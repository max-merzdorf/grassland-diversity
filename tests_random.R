library(sf)
library(terra)

plots <- st_read(dsn = "./data/_vector/sampling_sites_large_int_ext_seminat.gpkg")

subplots <- st_read("./data/_vector/subplots_rectangles.gpkg")
#st_write(subplots, dsn = "./data/_vector/subplots_2.gpkg", layer = "subplots")

st_crs(subplots)
plots <- st_transform(plots, st_crs(x = subplots))
st_crs(subplots)
st_crs(plots)

# now spatially extract info to subplots df
join <- st_join(subplots, y = plots, join = st_intersects)

###
# load an original img
img <- terra::rast("./data/_raster/site8_202409812_aligned.tif")
plot(img[[3]])
plotRGB(img, r=3,g=2,b=1, stretch="lin")

### Test Rao's Q and Shannon's H'
library(rasterdiv)
library(terra)
img <- terra::rast("./data/_raster/20240408_E_subplot.tif")
img <- terra::aggregate(img[[1]], 4)
plot(img)
img
rao <- paRao(x = test,
             window=c(3, 5),
             method = "classic",
             simplify = 2)

terra::plot(rao[[1]][[1]])
cv(values(rao[[1]][[1]]))

### Test more metrics workflow still works?
calc_spat_metrics <- function(myraster){
  result <- terra::rast()
  
  for (i in 1:nlyr(myraster)){
    lyr <- myraster[[i]]
    lyrname <- names(lyr) # to name metrics
    mets <- GLCMTextures::glcm_textures(r = lyr,
                                        n_levels=16,
                                        metrics = c("glcm_entropy", "glcm_mean", "glcm_dissimilarity", "glcm_variance"),
                                        quant_method = "range")
    metric_names <- paste0(lyrname,"_",names(mets))
    names(mets) <- metric_names
    result <- c(result, mets)
  }
  return(result)
}

##### MODIFIED METRICS SD MEAN FUNCTION #####

metrics_sd_mean_v2 <- function(metric_raster, n_bands=4){
  result <- c()
  cnames <- c()
  rnames <- c()
  for (i in 1:nlyr(metric_raster)){
    msd <- sd(values(metric_raster[[i]]), na.rm = T)
    mmean <- mean(values(metric_raster[[i]]), na.rm=T)
    msd_name <- paste0(names(metric_raster[[i]]), "_sd")
    mmean_name <- paste0(names(metric_raster[[i]]), "_mean")
    result <- c(result, msd, mmean)
    cnames <- c(cnames, substr(msd_name, 10, 100), substr(mmean_name, 10, 100))
    cnames <- gsub("_orthomosaic", "", cnames)
    rnames <- c(rnames, substr(msd_name, 1, 8), substr(mmean_name, 1, 8))
  }
  
  # Better to return a 2 row dataframe with every band/metric/sd/mean combo per column
  # so nrow= # of layers in metric_raster / 4 (bands per img) / 3 (metrics per img)
  res_df <- data.frame(matrix(result, nrow=4, byrow=T))
  colnames(res_df) <- unique(cnames)
  rownames(res_df) <- unique(rnames)
  
  return(res_df)
}


### TRY PCA ###
library(RStoolbox)
img <- terra::rast("./data/_raster/20240408_E_subplot.tif")
img
pca <- rasterPCA(img)
pca