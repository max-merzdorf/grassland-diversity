### NDVI function:

calc_ndvi <- function(red, nir){
  ndvi <- (nir-red) / (nir+red)
  return(ndvi)
}