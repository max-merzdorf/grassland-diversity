library(terra)
library(sf)

mosaics <- list.files("./data/_raster/", pattern = "orthomosaic.tif", full.names = T)
uav_sites <- sf::st_read("./data/_vector/uav_sites.gpkg")



single_site <- rast(mosaics[1])

red <- single_site[[3]]
nir <- single_site[[4]]

ndvi <- (nir - red) / (nir + red)
plot(ndvi)

# test aligned rasters:
ali1 <- terra::rast("./data/_raster/site10_20240408_aligned.tif")
ali1
terra::plot(ali1[[4]])
unali1 <- terra::rast("./data/_raster/20240408_E__repro.tif")
unali1
terra::plot(unali1[[4]])
