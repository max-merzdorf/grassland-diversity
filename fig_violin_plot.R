# 3.2.3. Figure:
# Distribution of float reflectance values for the April scene of Site 10
library(ggplot2)
library(tidyverse)

# load Site 10 (April)
img <- terra::rast(paste0("./data/_raster/original/20240408_site10_","
                          resampled_georef_clipped_aligned.tif"))
names(img) <- c("Green", "Red", "RE", "NIR")
m <- as.data.frame(img)
m <- pivot_longer(m, cols = colnames(m))

p <- ggplot(data = m, aes(x = factor(name,
                                     levels = c("Green", "Red",
                                                "RE", "NIR")),
                          y = value, fill = name)) +
  geom_violin() +
  labs(title = "2024-04-08 Site 10 reflectance value distribution",
       fill = "Band") +
  ylab("reflectance value") +
  xlab("Band") +
  scale_fill_viridis_d(guide = "none")
p
ggsave("./images/graphs/20240408_Site10_value_distribution.png", plot = p,
       width = 15.5, height = 6, units = "cm", dpi = 300)
