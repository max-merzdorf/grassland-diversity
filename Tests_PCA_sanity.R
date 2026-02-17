library(terra)

site_nr <- 10
processing <- "resampled_georef_clipped_aligned"
uav_images <- list.files("./data/_raster/original/",
                         pattern = paste0("site", site_nr, "_", processing, "\\.tif$"),
                         full.names = T)

stack <- terra::rast(uav_images)
unique(is.na(matrix(stack[[1]]))) #> TRUE

# don't set NA to 0 otherwise rasterPCA() breaks:
april <- stack[[1:4]]

# would be nice if i didnt have to replace NaN with NA
# values(april)[is.nan(values(april)) == T] <- NA
# unique(is.na(matrix(april[[1]]))) #> FALSE

# Create a mask of valid cells (non-NA in all layers)
#valid_mask <- app(april, fun = function(x) all(!is.na(x)))
# Apply mask
#april_masked <- mask(april, valid_mask)

pca <- RStoolbox::rasterPCA(april, nComp = 4, maskCheck = TRUE)
summary(pca$model)

### MANUAL PCA ###
r <- april

# Optional: trim or mask out empty edges
r <- trim(r)

# Convert to data.frame and drop NAs
v <- na.omit(as.data.frame(r, na.rm = TRUE))

# Run PCA
pca_model <- prcomp(v, scale. = TRUE)

# Map PCA scores back to raster
pca_rast <- predict(r, pca_model)

# Inspect results
summary(pca_model)

# Run terra PCA
pca_terra <- terra::princomp(v, scale. = TRUE)
pca_r2 <- terra::predict(r, pca_terra)
pca_r2

terra::writeRaster(pca_r2$Comp.1, filename = "./data/_raster/site10_terraPCA_comp1.tif")
terra::writeRaster(pca_r2$Comp.2, filename = "./data/_raster/site10_terraPCA_comp2.tif")
