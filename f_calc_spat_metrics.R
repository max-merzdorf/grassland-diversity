### Calculate GLCM metric groups contrast and orderliness
# for a SpatRaster with multiple bands, return a SpatRaster with named layers

calc_spat_metrics <- function(rst, nlevels){
  
  result <- terra::rast()
  
  for (i in 1:nlyr(rst)){
    lyr <- rst[[i]]
    lyrname <- names(lyr) # to name metrics
    mets <- GLCMTextures::glcm_textures(r = lyr,
                                        n_levels=nlevels,
                                        metrics = c("glcm_contrast",
                                                    "glcm_homogeneity",
                                                    "glcm_dissimilarity",
                                                    "glcm_entropy",
                                                    "glcm_ASM"),
                                        quant_method = "range")
    
    # get layer names
    metric_names <- paste0(lyrname,"_",names(mets))
    names(mets) <- metric_names
    result <- c(result, mets)
  }
  
  return(result)
}