rescale_to_8bit <- function(r){
  r_min <- min(terra::global(r, "min", na.rm = TRUE)[[1]])
  r_max <- max(terra::global(r, "max", na.rm = TRUE)[[1]])
  
  img_scaled <- round( (r - r_min) / (r_max - r_min) * 255)
  return(r)
}