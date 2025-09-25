library(terra)

# resamples a raster using a virtual clone of itself to a specified decimal
# target resolution

resamp <- function(path_to_raster, target_resolution){
  r <- terra::rast(path_to_raster)
  clone <- terra::rast(crs = terra::crs(r),
                       extent = terra::ext(r),
                       resolution = target_resolution)
  r <- terra::resample(r, clone)
  terra::writeRaster(r, filename = gsub(".tif",
                                        replacement = paste0("_resampled",
                                                             gsub(".", "", target_resolution),
                                                             ".tif"),
                                        x = path_to_raster))
}

rlist <- list.files("./data/_raster/original/", pattern=".tif$", full.names = T)
lapply(rlist, resamp, 0.03)
