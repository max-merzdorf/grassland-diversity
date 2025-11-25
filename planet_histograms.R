# Compare histograms of planet data to observe diffs in sensors and
# visual effects like haze etc.
library(terra)
library(ggplot2)
library(tidyverse)
source("./f_band_histograms.R")

# load rasters
pf <- list.files(path = "./data/_raster/planet/",
                 pattern = "bandmath.tif$", full.names = TRUE)
rlist <- lapply(pf, rast)

# resample to same resolution & extent
resampled <- lapply(rlist, resample, rlist[[1]])
# rename bands
pat <- "_\\d+(?:_\\d+_\\d*[a-z]*_3B_[a-zA-Z]*_SR_8b_[a-z_]*)"

resampled <- lapply(resampled, function(x){
  names(x) <- gsub(pat, "_B", names(x))
  x
})

stack <- c(resampled[[1]], resampled[[2]], resampled[[3]], resampled[[4]])

# stack per band:
green <- c(resampled[[1]][[1]], resampled[[2]][[1]], resampled[[3]][[1]],
           resampled[[4]][[1]])
red <- c(resampled[[1]][[2]], resampled[[2]][[2]], resampled[[3]][[2]],
           resampled[[4]][[2]])
re <- c(resampled[[1]][[3]], resampled[[2]][[3]], resampled[[3]][[3]],
           resampled[[4]][[3]])
nir <- c(resampled[[1]][[4]], resampled[[2]][[4]], resampled[[3]][[4]],
           resampled[[4]][[4]])

# lapply everything?
bandlist <- list(green, red, re, nir)
names(bandlist) <- c("Green", "Red", "RE", "NIR")
bandlist <- lapply(bandlist, function(r) {
  names(r) <- gsub(pat, "_B_", names(r))
  r
})

plotlist <- lapply(bandlist, function(r) {
  
  df <- pivot_longer(as.data.frame(r),
                     cols = colnames(as.data.frame(r)))
  p <- ggplot(data = df, aes(x=value, group=name, fill=name)) +
    geom_density() +
    xlim(c(0, quantile(values(r), probs = c(.99)))) + # 99th percentile
    scale_fill_viridis_d() +
    labs(title = paste0("Planet ", names(bandlist), " Band")) +
    theme(legend.position = "none")
  p
})

plotlist <- Map(function(r, nm) {
  df <- pivot_longer(as.data.frame(r),
                     cols = colnames(as.data.frame(r)))
  
  ggplot(df, aes(x = value, group = name, fill = name)) +
    geom_density() +
    xlim(c(0, quantile(values(r), probs = .99))) +
    scale_fill_viridis_d() +
    labs(title = paste0("Planet ", nm, " Band")) +
    theme(legend.position = "none")
}, bandlist, names(bandlist))

lapply(names(plotlist), function(p) {
  ggsave(filename = paste0("./images/graphs/planetHist_",p,"_band.png"),
         plotlist[[p]], width = 7, height = 5, units = "cm")
})

### Compare pixels per band across time
source("./f_pixel_history.R")

histlist <- Map(function(r, nm){
  ggplot(pixel_history(r, 20), aes(x = time, y = value,
                               group = pixel, color = pixel)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_c() +
    labs(title = paste0(nm, " band pixel sample")) +
    xlab("Scene") +
    ylab("Reflectance value") +
    theme(legend.position = "none") +
    scale_x_discrete(label = c("Apr", "May", "Jul", "Aug"))
}, bandlist, names(bandlist))

lapply(names(histlist), function(p) {
  ggsave(filename = paste0("./images/graphs/planetPixels_",p,"_band.png"),
         histlist[[p]], width = 7, height = 7, units = "cm")
})
