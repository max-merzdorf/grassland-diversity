library(terra)

sites <- c(8, 10, 14)
path <- "C:/EAGLE/InnoLab/data/_raster/20240408_"
dates <- c("0523", "0722", "0812")

for (i in sites){
  ref_img_file <- paste0(path, "site", i, "_3cm_orthomosaic.tif")
  ref_img <- terra::rast(ref_img_file)
  for (j in dates){
    trg_img_file <- gsub("0408", j, ref_img_file)
    trg_img_file <- gsub("orthomosaic", "georef", trg_img_file)
    
  }
}

# reproject RGB mosaics
rgbs <- list.files(path="./data/_raster", pattern = "RGB_mosaic.tif$", full.names = T)

for (i in rgbs){
  img <- terra::rast(i)
  repro <- terra::project(img, "epsg:32632")
  terra::writeRaster(repro, filename = i, overwrite=T)
}
