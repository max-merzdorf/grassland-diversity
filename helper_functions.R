library(terra)
library(rasterdiv)
library(GLCMTextures)
library(stringr)

### coefficient of variation:
cv <- function(x){
  r <- sd(x, na.rm = T) / mean(x, na.rm = T)
  return(r)
}

### Quick NDVI:
calc_ndvi <- function(red, nir){
  ndvi <- (nir-red) / (nir+red)
  return(ndvi)
}

### Clip a list of UAV raster images to a matching subplot sf feature:
clip_to_subplot <- function(img_list, subplot_feature){
  
  res_stack <- terra::rast()
  
  for (i in 1:length(img_list)){
    site_raster <- terra::rast(img_list[i])
    
    # Control layer names here:
    newnames <- gsub("[NSE]_orthomosaic",
                     paste0("site", site_nr, "_", "B"),
                     names(site_raster)) # old naming convention
    newnames <- gsub("resampled_georef", "B", newnames) # new naming convention

    names(site_raster) <- newnames
    
    subplot_raster <- terra::crop(site_raster, subplot_feature)
    res_stack <- c(res_stack, subplot_raster)
    # NOTE: this will throw a warning that the first raster is empty, ignore!
  }
  
  return(res_stack)
}

### Aggregate a SpatRaster with multiple layers with a defined factor and function
agg_my_rasters <- function(rstack, mfact, mfun){
  res <- rast()
  for (i in 1:nlyr(rstack)){
    agged <- terra::aggregate(rstack[[i]], fact=mfact, fun=mfun)
    res <- c(res, agged)
  }
  return(res)
}


### Calculate GLCM metrics for a SpatRaster with multiple bands, return a SpatRaster
#   with named layers

calc_spat_metrics <- function(rst, nlevels){
  result <- terra::rast()
  
  for (i in 1:nlyr(rst)){
    lyr <- rst[[i]]
    lyrname <- names(lyr) # to name metrics
    mets <- GLCMTextures::glcm_textures(r = lyr,
                                        n_levels=nlevels,
                                        metrics = c("glcm_entropy", "glcm_mean", "glcm_dissimilarity"),
                                        quant_method = "range")
    metric_names <- paste0(lyrname,"_",names(mets))
    names(mets) <- metric_names
    result <- c(result, mets)
  }
  return(result)
}

### GET Y VAR:
# A function to easily retrieve a column for a specific site from the env_params dataframe
# to be used as a y variable in species_linear_modeling()

get_y_var_column <- function(site_nr, column_name){
  column <- env_params[,column_name][env_params$siteID == site_nr]
  return(column)
}


### LINEAR MODEL FUNCTIONS
linear_modeling <- function(png_name, metric_df, y_var, time_intervals, n_bands){
  
  # metric_df: generated from metrics_sd_mean_v2(), one column per metric/band/sd-mean combo,
  #   one row per time interval (=timestep)
  # y_var: vector of length(time_intervals) or dataframe with ncol() == time_intervals
  #   with recorded species per site per run or other variable
  # time_intervals: vector with times for the data, e.g. c(1,2) for a two-time measured relation,
  #   but if data is from april, may, aug, sep then: c(1,2,4,5) to make gaps
  
  if (nrow(metric_df) != length(time_intervals)) {
    stop("Number of rows in metric_df does not match time_intervals")
  }
  
  # arrange the graph.dev grid: 1 row per band, 1 col per metric / sd-mean combo:
  graph_rows <- n_bands
  graph_cols <- ncol(metric_df) / n_bands
  
  # MAKE LINEAR MODELS AND COMPARE COEFFICIENTS:
  # lm for species abundance / fractional cover / etc.
  y_linmod <- lm(y_var ~ time_intervals)
  y_linmod_slope <- coef(y_linmod)["time_intervals"] # slope of species / frac.cover / etc
  
  # lm for GLCM metric values (non-normalized output)
  x_linmod <- lapply(metric_df, function(col) lm(col ~ time_intervals)) # list of lm's
  x_linmod_slopes <- sapply(x_linmod, function(x) coef(x)["time_intervals"]) # list of slopes
  slopes_diff <- abs(x_linmod_slopes - y_linmod_slope)
  x_linmod_sorted <- x_linmod[order(slopes_diff)] # obsolete?
  result <- data.frame(slopes_diff = slopes_diff,
                       slope_x = x_linmod_slopes,
                       slope_y = rep(y_linmod_slope, length(x_linmod_slopes)))

  ### write image with centered data for slope comparability:
  # center data and create centered linmods:
  y_centered <- y_var - mean(y_var, na.rm=T)
  y_linmod_centered <- lm(y_centered ~ time_intervals)
  # center each column of x individually:
  x_centered <- x_centered <- as.data.frame(scale(metric_df, center = TRUE, scale = FALSE))
  x_linmod_centered <- lapply(x_centered, function(col) lm(col ~ time_intervals))
  
  # Multiplot image
  png(filename = png_name, height = graph_rows*300, width = graph_cols*400, res = 200)
  par(mfrow = c(graph_rows, graph_cols))
  
  for (i in 1:ncol(metric_df)){
    plot(metric_df[,i],
         ylim=c(-max(metric_df, na.rm = T) / 2, max(metric_df, na.rm = T)),
         xlab = "time",
         ylab="value",
         main=gsub("glcm_", "", colnames(metric_df)[i]))
    abline(reg = x_linmod_centered[[i]], col = "red")
    abline(reg = y_linmod_centered, col = "blue")
  }
  dev.off()
  
  # Single plots for all metrics
  for (i in 1:ncol(metric_df)){
    png(filename = paste0(gsub(".png", "", png_name), "_",
                          gsub("site\\d{1,2}_", "", colnames(metric_df)[i]),
                          ".png"))
    par(mfrow=c(1,1))
    
    plot(metric_df[,i],
         ylim=c(-max(metric_df, na.rm = T) / 2, max(metric_df, na.rm = T)),
         xlab = "time",
         ylab="value",
         main=gsub("glcm_", "", colnames(metric_df)[i]))
    abline(reg = x_linmod_centered[[i]], col = "red")
    abline(reg = y_linmod_centered, col = "blue")
    dev.off()
  }
  
  # Write list of all model slopes
  write.table(result,
              file = gsub(".png", "_slopes.xml", png_name), sep = ",",
              row.names = T, col.names = T)

  return(result)
}

