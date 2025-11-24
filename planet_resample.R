# lapply load planet data and resample to same extent and resolution
# and write to disk
pf <- list.files(path = "./data/_raster/planet/",
                 pattern = "bandmath.tif$", full.names = TRUE)
rlist <- lapply(pf, rast)

# resample to same resolution & extent
resampled <- lapply(rlist, resample, rlist[[1]])
rstack <- c(resampled[[1]], resampled[[2]],
            resampled[[3]], resampled[[4]])
# rename bands
n <- gsub("_\\d+(?:_\\d+_\\d*[a-z]*_3B_[a-zA-Z]*_SR_8b_[a-z_]*)",
          "_B", names(rstack))
names(rstack) <- n
terra::writeRaster(rstack,
                   "./data/_raster/planet/Planet_stack_resampled.tif")
