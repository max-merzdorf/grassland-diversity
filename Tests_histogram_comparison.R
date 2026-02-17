# Compare UAS and Planet imagery 8-bit comverted histograms to see if reflectance patterns
# match

library(GLCMTextures)
library(rasterdiv)
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(RStoolbox)
library(gridExtra)
library(tidyverse)
source("./helper_functions.R")

planet_files <- list.files("./data/_raster/planet/", pattern = "bandmath.tif$", full.names = TRUE)

planet_1 <- rast(planet_files[1])
planet_2 <- rast(planet_files[2])
planet_3 <- rast(planet_files[3])
planet_4 <- rast(planet_files[4])

#planet_stack <- rast(planet_files)
#> Extents do not match (currently)

uas_images <- list.files("./data/_raster/original/",
                         pattern = paste0("resampled_georef_clipped_aligned\\.tif$"),
                         full.names = T)

# highest value of all planet data:
maxval <- max(matrix(planet_1),matrix(planet_2),matrix(planet_3),matrix(planet_4))

bandnr <- 4

b <- as.data.frame(cbind(values(planet_1[[bandnr]]),
                         values(planet_2[[bandnr]]),
                         values(planet_3[[bandnr]]),
                         values(planet_4[[bandnr]])))
colnames(b) <- c("april", "may", "july", "august")
b_long <- pivot_longer(b, cols = colnames(b))

# Complete value distribution histogram:
p <- ggplot(data=b_long, aes(x=value, group=name, fill=name)) +
  geom_density(adjust=1.5, alpha=.4) +
  xlim(c(0, maxval)) +
  labs(title = paste0("Band ",bandnr, " value distribution")) +
  theme_minimal()

png(filename = paste0("./images/graphs/Planet_values_band",bandnr,".png"),
    width = 1980, height = 1080, res = 300)
p
dev.off()

# 99th percentile visualisation
p <- ggplot(data=b_long, aes(x=value, group=name, fill=name)) +
  geom_density(adjust=1.5, alpha=.4) +
  xlim(c(0, 5000)) +
  labs(title = paste0("Band ",bandnr, " zoomed")) +
  theme_minimal()

png(filename = paste0("./images/graphs/Planet_values_band",bandnr,"_zoomed.png"),
    width = 1980, height = 1080, res = 300)
p
dev.off()

# Complete values boxplots:
q <- ggplot(data=b_long, aes(x=name, y=value)) +
  geom_boxplot() +
  labs(title = paste0("Band ",bandnr, " value distribution")) +
  theme_minimal()

png(filename = paste0("./images/graphs/Planet_values_band",bandnr,"_box.png"),
    width = 1980, height = 1080, res = 300)
q
dev.off()

##############################################################
##### COMPARE SITE 10 UAS & PLANET HISTOGRAMS IN AUGUST ######
##############################################################
library(terra)
library(tidyverse)
library(sf)
planet <- terra::rast("./data/_raster/planet/20240812_104320_10_247a_3B_AnalyticMS_SR_8b_coreg_clip_reproject_bandmath.tif")
uas <- terra::rast("./data/_raster/original/20240812_site10_resampled_georef_clipped_aligned.tif")

# remove 99th percentile:
source("./f_remove_percentile.R")
planet_histcrop <- remove_percentile(planet, .99)
uas_histcrop <- remove_percentile(uas, .99)

# scale to 8bit:
source("./f_rescale_to_8bit.R")
planet_8bit <- rescale_to_8bit(planet_histcrop)
uas_8bit <- rescale_to_8bit(uas_histcrop)

# crop planet data to site 10:
sites <- sf::st_read("./data/_vector/sampling_sites_large_int_ext_seminat.gpkg")
sites <- st_transform(sites, crs(planet_8bit))

planet_8bit <- terra::mask(planet_8bit, sites[sites$Nummer == 10,])

planet_df <- as.data.frame(planet_8bit)
colnames(planet_df) <- c("planet_G", "planet_R", "planet_RE", "planet_NIR")
uas_df <- as.data.frame(uas_8bit)
colnames(uas_df) <- c("uas_G", "uas_R", "uas_RE", "uas_NIR")

planet_long <- pivot_longer(planet_df, cols = starts_with("planet"))
uas_long <- pivot_longer(uas_df, cols = starts_with("uas"))

p <- ggplot(data=planet_long, aes(x=value, group=name, fill=name)) +
  geom_density(adjust=1.5, alpha=.4) +
  xlim(0, 255) +
  labs(title = "Planet Histogram Site 10, August") +
  theme_minimal()
p

q <- ggplot(data=uas_long, aes(x=value, group=name, fill=name)) +
  geom_density(adjust=1.5, alpha=.4) +
  xlim(0, 255) +
  labs(title = "UAS Histogram Site 10, August") +
  theme_minimal()
q
