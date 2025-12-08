# Calculate Rao's Q for planet data once and write to file (takes long)

library(terra)
library(sf)
library(rasterdiv)

rstack <- rast("./data/_raster/planet/Planet_8bit.tif")
plots <- sf::st_read("./data/_vector/sampling_sites_large_int_ext_seminat.gpkg")
plots <- st_transform(plots, "EPSG:32632")
sf_use_s2(FALSE) # turn off spherical geometry use
plots <- st_buffer(plots, dist = 30)

# convert sf to SpatVector
aoi_vec <- vect(plots)

# function to process a single AOI and write a multi-layer file
process_aoi_multilayer <- function(i) {
  
  message("Processing AOI ", i, " of ", nrow(plots))
  
  # 1. mask & crop raster to AOI
  aoi_i <- aoi_vec[i]
  r_i   <- crop(rstack, aoi_i)
  r_i   <- mask(r_i,  aoi_i)
  # to list
  lst <- lapply(1:nlyr(r_i), \(i) r_i[[i]])
  
  # 2. compute Rao entropy (returns a named list)
  rao_lst <- lapply(lst, paRao, dist_m  = "euclidean",
                    alpha = 1, window = c(3,5,7,9))
  # site name
  site <- paste0("site", aoi_vec[i]$Nummer)
  # rename spatRasters
  r <- rast()
  for(i in 1:length(rao_lst)){
    band <- names(lst[[i]])
    for(j in 1:length(rao_lst[[i]])){
      win_sizes <- gsub("\\.", "_", names(rao_lst[[i]]))
      ws <- win_sizes[[j]]
      for(k in 1:length(rao_lst[[i]][[j]])){
        lyrname <- paste0(site,"_",band,"_",ws)
        names(rao_lst[[i]][[j]][[k]]) <- lyrname
        r <- c(r, rao_lst[[i]][[j]][[k]])
      }
    }
  }
  
  # unlist and rename list entries:
  # 3. convert list to a multilayer SpatRaster
  #    Each element becomes a layer; names come from window sizes.
  rao_stack <- rast(unlist(rao_lst))
  nm <- unlist(lapply(unlist(rao_lst), names))
  names(rao_stack) <- nm
  
  # 4. write single multilayer GeoTIFF

  writeRaster(rao_stack,
              filename = paste0("./data/_raster/planet/rao/", site,
                                "_paRao.tif"), overwrite = TRUE)
  
  return()
}

# run for all AOIs -------------------------------------------------------
output_files <- lapply(1:nrow(plots), process_aoi_multilayer)
