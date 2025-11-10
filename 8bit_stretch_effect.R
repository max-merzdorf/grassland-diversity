# Visualize the effect of removing the 99th percentile and converting to
# 8-bit with ggplot density by comparing before (float) and after (int)
# of the red band of Site 10 in April:
library(terra)

before <- rast(paste0("./data/_raster/original/20240408_site10_",
                      "resampled_georef_clipped_aligned.tif"))
after_band <- rast(paste0("./data/_raster/original/20240408_site10_",
                          "resampled_georef_aligned_8bit.tif"))
after_scene <- rast(paste0("./data/_raster/original/20240408_site10_",
                           "resampled_georef_aligned_",
                           "8bit_scene_stretch.tif"))

make_long <- function(r){
  df <- as.data.frame(r)
  df <- pivot_longer(df, cols = colnames(df))
  return(df)
}
make_long(before[[2]])

b <- ggplot(data = make_long(before[[2]]),
            aes(x = value, group = name)) +
  geom_histogram(bins = 50) +
  labs(title = "a) Float reflectance") +
  ylim(c(0, 10000000))

ab <- ggplot(data = make_long(after_band[[2]]),
             aes(x = value, group = name)) +
  geom_histogram(bins = 50) + 
  labs(title = "b) Integer band-stretch") +
  ylim(c(0, 10000000))

as <- ggplot(data = make_long(after_scene[[2]]),
             aes(x = value, group = name)) +
  geom_histogram(bins = 50) +
  xlim(c(0, 255)) +
  ylim(c(0, 10000000)) +
  labs(title = "c) Integer scene-stretch")

ggsave("./images/graphs/float_value_dist.png", b, width = 7,
       height = 5, units = "cm", dpi = 300)
ggsave("./images/graphs/int_band_stretch_dist.png", ab, width = 7,
       height = 5, units = "cm", dpi = 300)
ggsave("./images/graphs/int_scene_stretch_dist.png", as, width = 7,
       height = 5, units = "cm", dpi = 300)

library(magick)

f <- magick::image_read("./images/graphs/float_value_dist.png")
i1 <- image_read("./images/graphs/int_band_stretch_dist.png")
i2 <- image_read("./images/graphs/int_scene_stretch_dist.png")

big <- image_append(c(f, i2, i1))
image_write(big, "./images/graphs/distribution_big.png")
