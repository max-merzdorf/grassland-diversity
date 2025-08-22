# reproject orthomosaics to epsg:32632 (UTM 32 N)
repro_32n <- function(rastfile){
  myrast <- terra::rast(paste0("C:/EAGLE/InnoLab/data/_raster/",rastfile))
  myrast <- terra::project(myrast, "epsg:32632")
  terra::writeRaster(myrast, filename=paste0("C:/EAGLE/InnoLab/data/_raster/", substr(rastfile, 1, 11), "_DEM_repro.tif"))
}

filesls <- list.files("./data/_raster/", pattern="DEM.tif_clipped.tif")

lapply(filesls, repro_32n)
