library(terra)
library(tictoc)

repro <- function(rfile, crs){
  tic("start")
  r <- terra::rast(rfile)
  r <- terra::project(r, crs)
  terra::writeRaster(r, rfile, overwrite = TRUE)
  toc()
}

rfiles <- list.files("./data/_raster/original/", pattern = ".tif$", full.names = T)
rfiles
lapply(rfiles, repro, "epsg:32632")
