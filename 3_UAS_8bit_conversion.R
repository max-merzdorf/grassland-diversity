# Cut the 99th percentile of UAS float data and convert to 8-bit,
# write to disk
# > Ran script for each site nr separately

library(terra)
site_nr <- 14

##########################################################################
############################ BAND HIST STRETCH ###########################
##########################################################################

uav_images <- list.files("./data/_raster/original/",
                         pattern = paste0("site", site_nr,
                                          "_resampled_georef",
                                          "_clipped_aligned",
                                          ".tif$"),
                         full.names = T)

for (j in 1:length(uav_images)){
  
  stack99 <- terra::rast()
  stack <- terra::rast(uav_images[j])
  
  for(i in 1:nlyr(stack)){
    
    # 99th percentile cut
    lyr <- stack[[i]]
    lyrperc <- quantile(matrix(lyr), probs = .99, na.rm = T)
    writeLines(paste0("layer ", names(lyr)," 99 percentile = ",lyrperc))
    values(lyr)[values(lyr) > lyrperc] <- NA
    
    # 8-bit conversion
    lyrmin <- min(global(lyr, "min", na.rm = TRUE)[[1]])
    lyrmax <- max(global(lyr, "max", na.rm = TRUE)[[1]])
    lyr_scaled <- round( (lyr - lyrmin) / (lyrmax - lyrmin) * 255 )
    
    # to integer
    stack99 <- c(stack99, as.int(lyr_scaled))
  }
  
  # write the stacked bands as raster
  terra::writeRaster(stack99,
                     filename = gsub("resampled_georef_clipped_aligned",
                                     "resampled_georef_aligned_8bit",
                                     uav_images[j]))
}


##########################################################################
######################### SCENE HIST STRETCH #############################
##########################################################################

for (j in 1:length(uav_images)){
  
  stack99 <- terra::rast()
  stack <- terra::rast(uav_images[j])
  
  # rescale to the values across all 4 bands
  stackmin <- min(global(stack, "min", na.rm = TRUE))
  stackmax <- max(global(stack, "max", na.rm = TRUE))
  
  for(i in 1:nlyr(stack)){
    
    # 99th percentile cut
    lyr <- stack[[i]]
    lyrperc <- quantile(values(lyr), probs = .99, na.rm = T)
    writeLines(paste0("layer ", names(lyr)," 99 percentile = ",lyrperc))
    values(lyr)[values(lyr) > lyrperc] <- NA
    
    # 8-bit conversion
    lyr_scaled <- round( (lyr - stackmin) / (stackmax - stackmin) * 255 )
    
    # to integer
    stack99 <- c(stack99, as.int(lyr_scaled))
  }
  
  # write the stacked bands as raster
  terra::writeRaster(stack99,
                     filename = gsub("resampled_georef_clipped_aligned",
                                     "resampled_georef_aligned",
                                     "_8bit_scene_stretch",
                                     uav_images[j]))
}
