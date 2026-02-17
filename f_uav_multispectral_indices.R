# This script contains utility functions to calculate several multispectral
# indices

library(terra)

calc_ndvi <- function(red, nir){
  ndvi <- (nir-red) / (nir+red)
  return(ndvi)
}

calc_savi <- function(red, nir, l){
  savi <- (1+l) * (nir-red) / (nir+red+l)
  return(savi)
}

# helper functions:
rstmean <- function(raster){
  r <- mean(as.matrix(raster), na.rm=T)
  return(r)
}

rstsd <- function(raster){
  r <- sd(as.matrix(raster), na.rm=T)
}