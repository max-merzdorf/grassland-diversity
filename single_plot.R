source("./helper_functions.R")
library(terra)

# Use this to make a single plot example for InnoLab presentation:

subplot <- plots[plots$siteID == site_nr,]

rgb_img <- terra::rast("./data/_raster/site10_20240812_RGB_mosaic.tif")
rgb_clipped <- terra::crop(rgb_img, subplot)
terra::writeRaster(rgb_clipped, filename = "./data/_raster/InnoLab_example/s10_rgb_clipped.tif")

entropy <- metrics[[4]]
plot(entropy)
terra::writeRaster(entropy, "./data/_raster/InnoLab_example/entropy.tif")

cv(matrix(entropy))

# load a stack for a single plot and displayy different aggregation levels:

subplot <- plots[plots$siteID == 10,]
uas_img <- rast("./data/_raster/original/20240408_site10_resampled_georef_clipped_aligned.tif")
stack <- clip_to_subplot(uas_img, subplot)
agg4 <- terra::aggregate(stack, fact = 4)

png("./images/graphs/s14_agg0.png", width = 1980, height = 1080, res = 270)
plot(stack[[1:4]])
dev.off()
png("./images/graphs/s14_agg4.png", width = 1980, height = 1080, res = 270)
plot(agg4[[1:4]])
dev.off()
