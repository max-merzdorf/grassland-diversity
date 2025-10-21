library(GLCMTextures)
library(rasterdiv)
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(RStoolbox)
library(gridExtra)
source("./helper_functions.R")

##### EDIT THESE VARIABLES #####
site_nr <- 10
agg_factor <- 1
nlevels <- 32
analysis <- "species_richness"
# "species_richness"
# "species_abundance"
# "vegetation_height"
# ...

processing = "resampled_georef_clipped_aligned"

##### DON'T TOUCH: #############

uav_images <- list.files("./data/_raster/original/",
                         pattern = paste0("site", site_nr, "_", processing, "\\.tif$"),
                         full.names = T)

stack <- terra::rast(uav_images)

stack <- terra::crop(stack, plots[plots$siteID == site_nr,])

# April bands have wrong names as they were not georeferenced
names(stack) <- gsub("[NSE]_orthomosaic",
                     paste0("site", site_nr, "_", "B"),
                     names(stack))
names(stack) <- gsub("resampled_georef_", "B_", names(stack))

agg <- terra::aggregate(stack, fact = agg_factor, fun = "mean")


metrics <- calc_spat_metrics(agg, nlevels = nlevels)
# get dates and names for row and column names:
dates <- unique(unlist(lapply(names(metrics), FUN = substr, 1, 8)))

metrics_stats <- global(x = metrics,
                        fun = "sd",
                        na.rm = TRUE)

predictor_names <- unique(gsub("\\d{8}_site\\d{1,2}_", "", rownames(metrics_stats)))

mat <- matrix(metrics_stats$sd,
              ncol = 4,
              byrow = FALSE)
metstats <- as.data.frame(mat)
colnames(metstats) <- dates
rownames(metstats) <- predictor_names

# remove june from env_params (no UAV image)
env_params <- subset(env_params, env_params$Col_run != 2)

# change name to metrics_stat_derivatives so i don't have to change the following stuff
# and transpose the data frame to fit with previous format

metrics_stat_derivs <- as.data.frame(t(metstats))

bands <- nlyr(metrics) / ncol(metrics_stat_derivs)

if (analysis == "species_richness") {
  
  png_name <- paste0("site", site_nr,"_","speciesRichness","_n",nlevels,"_agg",agg_factor,".png")
  
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
  
  png_name <- paste0("site", site_nr,"_","vegetationHeight","_n",nlevels,"_agg",agg_factor,".png")
  
  y_points <- get_y_var_column(site_nr, "veg_height_cm")
  
  res <- linear_modeling(png_name = png_name,
                         metric_df = metrics_stat_derivs,
                         y_var = y_points,
                         c(1,3,4,5),
                         bands)
}
