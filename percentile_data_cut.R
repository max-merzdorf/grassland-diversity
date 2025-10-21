library(rasterdiv)
library(terra)
img <- terra::rast("./data/_raster/original/20240408_site8_resampled_georef_clipped_aligned.tif")

hist(values(img))

# Test quantiles:
# q <- quantile(values(img), probs = c(.95, .98, .99), na.rm=T)
# > works!

# Replace all values higher than the 99th percentile with NA
values(img)[values(img) >= quantile(values(img), probs = c(.99), na.rm = T)] <- NA
hist(values(img))

# Rescale to 8-bit:
r_min <- min(global(img, "min", na.rm = TRUE)[[1]])
r_max <- max(global(img, "max", na.rm = TRUE)[[1]])

img_scaled <- round( (img - r_min) / (r_max - r_min) * 255)
img_scaled
hist(img_scaled)

