library(terra)
library(sf)

site_nr <- 14

mosaics <- list.files("./data/_raster/original/",
                      pattern = paste0("site", site_nr, "_.*\\.tif$"),
                      full.names = T)
uav_sites <- sf::st_read("./data/_vector/uav_sites.gpkg")
uav_sites <- sf::st_transform(uav_sites, 32632)

for (i in 1:length(mosaics)){
  om <- rast(mosaics[i])
  clipped <- mask(om, uav_sites)
  
  terra::writeRaster(clipped, filename = gsub(".tif", "_clipped.tif", mosaics[i]))
}
