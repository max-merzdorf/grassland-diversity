library(GLCMTextures)
library(rasterdiv)
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(RStoolbox)
library(gridExtra)
library(tidyverse)
source("./helper_functions.R")

##### EDIT THESE VARIABLES #####

nlevels <- 32
analysis <- "species_richness"
# "species_richness"
# "species_abundance"
# "vegetation_height"
# ...

########## DON'T TOUCH: #############

##### CALCULATE METRICS AND PREPARE RASTERS ##############################

# load tables
env_params <- read.table("./data/_tables/_analysisready_env_params.csv")
allspecies <- read.table("./data/_tables/_analysisready_allspecies.csv")
plots <- sf::st_read("./data/_vector/sampling_sites_large_int_ext_seminat.gpkg")
plots <- st_transform(plots, "EPSG:32632")
sf_use_s2(FALSE) # turn off spherical geometry use
plots <- st_buffer(plots, dist = 15)

# load 8-bit planet data stack
p <- rast("./data/_raster/planet/Planet_8bit.tif")
sites <- vect(plots)

# crop and mask to the sites:
mask_by_polygons <- function(polygons, r) {
  n <- nrow(polygons)
  results <- vector("list", n)
  
  for (i in 1:n) {
    p <- polygons[i, ]
    r_crop <- crop(r, p)
    r_mask <- mask(r_crop, p)
    results[[i]] <- r_mask
    names(results)[i] <- paste0("site", polygons$Nummer[i])
  }
  
  return(results)
}
rl <- mask_by_polygons(sites, p)
# -> list is named by sites

# calculate glcm metrics and coVar
metrics <- lapply(rl, calc_spat_metrics, 32)
# unlist to one spatRaster with manymany layers doesn't work because
# extents never match (different sites)

# load pre-calculated rao's Q rasters:
raoq <- lapply(list.files("./data/_raster/planet/rao", pattern = "\\.tif$",
                        full.names = T), rast)

# name the rao spatRasters with their sources to match sites, so I can
# add the corresponding layer
new_names <- sapply(raoq, function(r) {
  fullname <- sources(r)
  n <- gsub("C:/EAGLE/InnoLab/data/_raster/planet/rao/", "", fullname)
  gsub("_paRao.tif", "", n)
})
names(raoq) <- new_names

# mask Rao again with the non-buffered sites
raoq <- lapply(raoq, function(x){
  x <- mask(x, sites)
  return(x)
})

###
# concatenating Rao Q and GLCM metric rasters doesn't work because
# extents do not match. we therefore concat later, with the data frames
# so I can just global() on the Rao rasters and match the resulting data
# frames by site I HOPE

##### RASTER STATISTICS ##################################################

# coefficient of variation for each band:
cvs <- lapply(rl, function(r) {
  layer_names <- names(r)
  vals <- values(r)
  covs <- apply(vals, 2, function(v)
    sd(v, na.rm = TRUE) / mean(v, na.rm = TRUE)
  )

  df <- data.frame(value = covs)
  rownames(df) <- layer_names
  df
})

cvs <- Map(function(x, nm) {
  rownames(x) <- paste0(nm,"_",rownames(x),"_cv")
  colnames(x) <- "value"
  x
}, cvs, names(cvs))

# get dates and names for row and column names:

# population standard deviation of GLCM textures
metrics_stats <- lapply(metrics, global, fun = "std", na.rm=T)
# rename rows:
metrics_stats <- Map(function(x, nm) {
  rownames(x) <- paste0(nm, "_", rownames(x), "_std")
  colnames(x) <- "value"
  x
}, metrics_stats, names(metrics_stats))

# pop sd for rao's Q layers:
rao_stats <- lapply(raoq, global, fun = "std", na.rm=T)

lapply(rao_stats, function(x){
  rownames(x) <- paste0(rownames(x), "_std")
  colnames(x) <- "value"
  return(x)
})

# write table per site
d <- rao_stats[[1]]
rn <- rownames(d)

rn_nosite <- sub("site\\d{1,2}_", "", rn)
date <- sub("_.*$", "", rn_nosite)
predictor <- sub("^[^_]*_", "", rn_nosite)

long_df <- data.frame(
  date = date,
  predictor = predictor,
  value = d[[1]],
  stringsAsFactors = FALSE
)

wide_df <- pivot_wider(
  long_df,
  names_from = predictor,
  values_from = value
)

#> check that the cols are named properly
#> 

# put this into a for loop to do for every site
for (i in new_names){
  
  # coefficients of variation
  s_cvs <- cvs[[as.character(i)]]
  rn_cvs <- sub("site\\d{1,2}_", "", rownames(s_cvs))
  date_cvs <- sub("_.*$", "", rn_cvs)
  predictor_cvs <- sub("^[^_]*_", "", rn_cvs)
  
  cvs_long <- data.frame(
    date = date_cvs,
    predictor = predictor_cvs,
    value = s_cvs,
    stringsAsFactors = FALSE
  )
  
  cvs_wide <- pivot_wider(
    cvs_long,
    names_from = predictor,
    values_from = value
  )
  
  # glcm metrics
  s_glcm <- metrics_stats[[as.character(i)]]
  rn_glcm <- sub("site\\d{1,2}_", "", rownames(s_glcm))
  date_glcm <- sub("_.*$", "", rn_glcm)
  predictor_glcm <- sub("^[^_]*_", "", rn_glcm)
  
  glcm_long <- data.frame(
    date = date_glcm,
    predictor = predictor_glcm,
    value = s_glcm,
    stringsAsFactors = FALSE
  )
  
  glcm_wide <- pivot_wider(
    glcm_long,
    names_from = predictor,
    values_from = value
  )
  
  # rao metrics
  s_rao <- rao_stats[[as.character(i)]]
  rn_rao <- sub("site\\d{1,2}_", "", rownames(s_rao))
  date_rao <- sub("_.*$", "", rn_rao)
  predictor_rao <- sub("^[^_]*_", "", rn_rao)
  
  rao_long <- data.frame(
    date = date_rao,
    predictor = predictor_rao,
    value = s_rao,
    stringsAsFactors = FALSE
  )
  
  rao_wide <- pivot_wider(
    rao_long,
    names_from = predictor,
    values_from = std
  )
  
  # check if dates match
  if (unique(cvs_wide$date == glcm_wide$date)){
    writeLines(paste0(i, " dates match!"))
  } else {
    stop("Error: dates do not match")
  }
  
  # rbind and drop duplicate date cols
  r <- cbind(cvs_wide,
             glcm_wide[,2:ncol(glcm_wide)],
             rao_wide[,2:ncol(rao_wide)])
  tabname <- paste0("Planet_",i, "_metrics.csv")
  write.table(r, file = paste0("C:/EAGLE/InnoLab/results/metric_tables", tabname),
              sep = ",", dec = ".")
}

##########################################################################
# change name to metrics_stat_derivatives so i don't have to change the
# following and transpose the data frame so that dates are in columns:
metrics_stat_derivs <- as.data.frame(t(metstats))

# remove june from env_params (no UAV image that month)
env_params <- subset(env_params, env_params$Col_run != 2)

# write the results as images and tables:
if (analysis == "species_richness") {
  
  png_name <- paste0("site", site_nr,"_","speciesRichness","_n",
                     nlevels,"_agg",agg_factor,".png")
  
  # get species richness data
  sp_rich <- get_y_var_column(site_nr, "species_on_run")
  y_points <- sp_rich[!is.na(sp_rich)]
  # insert NA for June:
  y_points <- c(y_points[1],
                NA,
                y_points[2])
  
  # only may and july have species data, insert NA row for June
  species_metrics <- metrics_stat_derivs[2:3,]
  species_metrics[3,] <- species_metrics[2,]
  species_metrics[2,] <- rep(NA, ncol(species_metrics))
  row.names(species_metrics) <- c(row.names(species_metrics)[1],
                                  "20240612",
                                  row.names(species_metrics)[2])
  
  res <- linear_modeling(png_name = png_name,
                         metric_df = species_metrics,
                         y_var = y_points,
                         time_intervals = c(1,2,3),
                         bands)
  
  
} else if (analysis == "vegetation_height") {
  
  png_name <- paste0("site", site_nr,"_","vegetationHeight","_n",nlevels,
                     "_agg",agg_factor,".png")
  
  y_points <- get_y_var_column(site_nr, "veg_height_cm")
  
  res <- linear_modeling(png_name = png_name,
                         metric_df = metrics_stat_derivs,
                         y_var = y_points,
                         c(1,3,4,5),
                         bands)
}
