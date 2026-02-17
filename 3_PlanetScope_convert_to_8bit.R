library(terra)

rstack <- terra::rast("./data/_raster/planet/Planet_stack_resampled.tif")

r8bit <- lapply(rstack, function(r){
  
  # replace values above 99th percentile threshold with NA
  pt <- quantile(matrix(r), probs = .99, na.rm = T)
  values(r)[values(r) > pt] <- NA
  
  # convert to 8-bit
  rmin <- min(global(r, "min", na.rm = TRUE)[[1]])
  rmax <- max(global(r, "max", na.rm = TRUE)[[1]])
  rscaled <- as.int(round( (r - rmin) / (rmax - rmin) * 255 ))
  rscaled
  
})

r8stack <- rast(r8bit)
terra::writeRaster(r8stack, "./data/_raster/planet/Planet_8bit.tif")
