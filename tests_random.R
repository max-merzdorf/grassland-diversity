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
img2 <- terra::rast("./data/_raster/original/20240408_E_orthomosaic.tif")
plot(img[[3]])
plotRGB(img, r=3,g=2,b=1, stretch="lin")

### Test Rao's Q and Shannon's H'
library(rasterdiv)
library(terra)
img <- terra::rast("./data/_raster/original/20240408_site8_resampled_georef_clipped_aligned.tif")
img <- terra::aggregate(img[[1]], 4)
plot(img)
img
rao <- paRao(x = img,
             window=c(9),
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

### LOAD RGB ###
s10_rgb <- terra::rast("./data/_raster/site10_20240812_RGB_mosaic.tif")
s10_rgb <- terra::crop(s10_rgb, plots[plots$siteID == 10,])


### why are rasters sometimes emptx?
img <- terra::rast("./data/_raster/resample_this/site8_20240408_aligned.tif")
img[[1]]
unique(img[[1]]) # -> 8 bit
plot(img[[1]])
subimg <- terra::crop(img, plots[plots$siteID==10])
plot(subimg)

agged <- terra::aggregate(img, fact = 0.03/res(img)[1])

aggregate_to_res <- function(r, targetres){
  rfact <- targetres / res(r)[1]
  result <- terra::aggregate(r, fact = rfact)
  return(result)
}
test <- aggregate_to_res(img, 0.03)

img2 <- terra::rast("./data/_raster/20240408_site8_3cm_aligned.tif")

test <- terra::resample(img, img2)
test

terra::writeRaster(test, "./data/_raster/resample_this/resampling_test_1.tif")


### TEST RESAMPLING ###
img <- rast("./data/_raster/original/20240408_N_orthomosaic.tif")
img2 <- terra::rast("./data/_raster/original/8bit/20240408_site10_3cm_orthomosaic.tif")
img_resamp <- rast("./data/_raster/original/20240523_N_orthomosaic.tif")

virtual_raster <- rast(nrows = 10, ncols = 10, crs = "epsg:32632", resolution = 0.03)

res <- terra::resample(img_resamp, virtual_raster)
terra::writeRaster(res, "./data/_raster/resampling_test.tif")
res <- terra::resample(img_resamp, img2)
terra::writeRaster(res, "./data/_raster/resampling_test_to_3cm_site8.tif")

# copy of input raster but with different resolution?
vrast <- rast(extent = ext(img_resamp), crs = crs(img_resamp), resolution = 0.03)
vrast
res <- terra::resample(img_resamp, vrast)
terra::writeRaster(res, "./data/_raster/resampling_virtual_reaster_copy.tif")


### CLIP ONE RASTER MANY SUBPLOTS ###
library(terra)
library(sf)
uas <- rast("./data/_raster/original/20240408_site10_resampled_georef_clipped_aligned.tif")
plots <- subplots <- st_read("./data/_vector/subplots_rectangles.gpkg")

### TEST RAO and SHANNON ###
rao <- rasterdiv::paRao(agg[[1]], alpha = 2)



###### Delta slopes absolute differneces (boxplots) ######
d_slopes_long <- pivot_longer(delta_slopes, cols=colnames(delta_slopes))

p <- ggplot(d_slopes_long, aes(x=name, y=value, fill=name)) +
  geom_violin() +
  scale_fill_viridis_d(labels = c("Site 10", "Site 14", "Site 8")) +
  labs(title = "Delta slopes species richness / texture metrics")
png(filename = "./images/graphs/d_slopes_speciesRichness_textureMetrics.png", width = 1980, height = 1080, res=300)
p
dev.off()
