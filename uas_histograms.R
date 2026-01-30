library(terra)
library(patchwork)
library(ggplot2)
library(tidyverse)

source("./f_pixel_history.R")
uasf <- list.files("./data/_raster/original",
                   pattern = "site10_resampled_georef_aligned_8bit.tif$",
                   full.names = TRUE)

resampled <- lapply(uasf, rast)
green <- c(resampled[[1]][[1]], resampled[[2]][[1]], resampled[[3]][[1]],
           resampled[[4]][[1]])
red <- c(resampled[[1]][[2]], resampled[[2]][[2]], resampled[[3]][[2]],
         resampled[[4]][[2]])
re <- c(resampled[[1]][[3]], resampled[[2]][[3]], resampled[[3]][[3]],
        resampled[[4]][[3]])
nir <- c(resampled[[1]][[4]], resampled[[2]][[4]], resampled[[3]][[4]],
         resampled[[4]][[4]])

bandlist <- list(green, red, re, nir)
names(bandlist) <- c("Green", "Red", "RE", "NIR") # rename list items
# rename individual bands
bandlist <- lapply(bandlist, function(r) {
  names(r) <- gsub("_[a-zA-Z0-9_]*", "", names(r))
  r
})

violist <- Map(function(r, nm){
  df <- pivot_longer(as.data.frame(r), cols = names(r))
  
  ggplot(df, aes(x = name, y = value, fill = name)) +
    geom_violin() +
    ylim(c(0,255)) +
    scale_fill_viridis_d() +
    labs(title = paste0(nm, " band"), fill = "Scene") +
    xlab("Scene") +
    ylab("Reflectance value") +
    theme(legend.position = "none") +
    scale_x_discrete(label = c("Apr", "May", "Jul", "Aug"))
}, bandlist, names(bandlist))

wrapped <- wrap_plots(violist, nrow = 1, axes = "collect",
                      guides = "collect")

ggsave(wrapped, filename = "./images/graphs/UAS_violins.png",
       width = 21, height = 7, units = "cm")

df <- as.data.frame(green)
dflong <- pivot_longer(df, cols = colnames(df))
p <- ggplot(dflong, aes(x = name, y = value, fill = name)) +
  geom_violin()
ggsave(filename = "./images/vio_test.png", width = 1980, height = 1080,
       units = "px", plot = p)


### 20 Random pixels:

pxls <- pixel_history(red, 20)

histlist <- Map(function(r, nm){
  ggplot(pixel_history(r, 20), aes(x = time, y = value,
                                   group = pixel, color = pixel)) +
    geom_line() +
    geom_point() +
    ylim(c(0,255)) +
    scale_color_viridis_c() +
    labs(title = paste0(nm, " band")) +
    xlab("Scene") +
    ylab("Reflectance value") +
    scale_x_discrete(label = c("Apr", "May", "Jul", "Aug"))
}, bandlist, names(bandlist))

w <- wrap_plots(histlist, nrow=1, axes="collect", guides="collect")

ggsave(w, filename = "./images/graphs/UAS_pixHist_wrapped.png",
       width = 21, height = 6, units = "cm")
