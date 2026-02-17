library(GLCMTextures)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
source("./uav_multispectral_indices.R")

test_n <- rast("./data/_raster/20240408_N_subplot.tif")
test_e <- rast("./data/_raster/20240408_E_subplot.tif")

qtest_n <- quantize_raster(test_n[[4]], 8, quant_method = "range")
metrics_n <- glcm_textures(qtest_n, n_levels = 8, quant_method = "none")
terra::plot(metrics_n, main=c("verylonglonglonglongtext",2,3,4,5,6,7,8))
qtest_e <- quantize_raster(test_e[[4]], 16, quant_method = "range")
metrics_e <- glcm_textures(qtest_e, n_levels=16, quant_method = "none")

metrics_n$ndvi <- calc_ndvi(test_n[[3]], test_n[[4]])
metrics_e$ndvi <- calc_ndvi(test_e[[3]], test_e[[4]])
metrics_n$savi <- calc_savi(test_n[[3]], test_n[[4]], 0.5)
metrics_e$savi <- calc_savi(test_e[[3]], test_e[[4]], 0.5)

# try titles: # do main=c(title1, title2, ...)
plot(metrics_n, main = c()) 

ggplot(metrics_n) +
  geom_raster()

# try lapply for sd and mean

png("site8_site10_subplots_quant16.png", width = 3000, height = 800)
par(mfrow = c(2, 10), mar = c(1,1,1,1))  
for (i in 1:nlyr(metrics_n)) {
  plot(metrics_n[[i]], main = paste("site 8/agg 0/quant 16", names(metrics_n[[i]])), col=map.pal("viridis"))
}

for (i in 1:nlyr(metrics_e)) {
  plot(metrics_e[[i]], main = paste("site 10/agg 0/quant 16", names(metrics_e[[i]])), col=map.pal("viridis"))
}
dev.off()
par(mfrow = c(1,1))


### test with smaller plots:
plots <- st_read("./data/_vector/sampling_sites_rect_allfields.gpkg")
north <- rast("./data/_raster/20240408_N_orthomosaic.tif_clipped.tif")
north <- terra::project(north, "epsg:32632")
p8 <- plots[plots$No==8,]

smallnorth <- terra::crop(x = north, y=p8)

lapply(metrics_n, rstsd)
unlist(lapply(metrics_n, rstsd))
