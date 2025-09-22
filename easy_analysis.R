library(GLCMTextures)
library(rasterdiv)
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
source("./helper_functions.R")

##### EDIT THESE VARIABLES #####
site_nr <- 10
agg_factor <- 2
bands <- 4
analysis <- "species_richness"
# "species_richness"
# "species_abundance"
# "vegetation_height"
# ...

##### DON'T TOUCH: #############

png_name <- paste0("site", site_nr,"_",analysis,"_agg",agg_factor,".png")

uav_images <- list.files("./data/_raster/",
                         pattern = paste0("site", site_nr, "_3cm_aligned\\.tif$"),
                         full.names = T)

stack <- clip_to_subplot(img_list = uav_images,
                         subplot_feature = plots[plots$siteID == site_nr,]) # get the subplot geometry from plots table

names(stack) <- gsub("3cm_aligned_",
                     paste0("B"), names(stack))

agg <- terra::aggregate(stack, fact = agg_factor, fun = "mean")

metrics <- calc_spat_metrics(agg)

metrics_stat_derivs <- metrics_sd_mean_v2(metrics, n_bands = bands)

# remove june from env_params (no UAV image)
env_params <- subset(env_params, env_params$Col_run != 2)

if (analysis == "species_richness") {
  
  # get species richness data
  sp_rich <- get_y_var_column(site_nr, "species_on_run")
  y_points <- sp_rich[!is.na(sp_rich)]
  # insert NA for June:
  y_points <- c(y_points[1],
                NA,
                y_points[2])
  
  # only may and july have species data, insert NA row for June
  species_metrics <- metrics_stat_derivs[2:3,]
  species_metrics[3,] <- species_metrics[2,]
  species_metrics[2,] <- rep(NA, ncol(species_metrics))
  row.names(species_metrics) <- c(row.names(species_metrics)[1],
                                  "20240612",
                                  row.names(species_metrics)[2])
  
  res <- linear_modeling(png_name = png_name,
                         metric_df = species_metrics,
                         y_var = y_points,
                         time_intervals = c(1,2,3),
                         bands)
  
  
} else if (analysis == "vegetation_height") {
  
  y_points <- get_y_var_column(site_nr, "veg_height_cm")
  
  res <- linear_modeling(png_name = png_name,
                         metric_df = metrics_stat_derivs,
                         y_var = y_points,
                         c(1,3,4,5),
                         bands)
}

# go to "single_plot.R" to make nicer plots for selected metrics