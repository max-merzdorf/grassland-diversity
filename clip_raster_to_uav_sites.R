library(terra)
library(sf)

mosaics <- list.files("./data/_raster/", pattern = "DEM.tif", full.names = T)
uav_sites <- sf::st_read("./data/_vector/uav_sites.gpkg")

for (i in 1:length(mosaics)) {
  om <- rast(mosaics[i])
  clipped <- mask(om, uav_sites)
  
  terra::writeRaster(clipped, filename = paste0(mosaics[i], "_clipped.tif"))
}
