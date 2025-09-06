library(GLCMTextures)
library(rasterdiv)
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
source("./helper_functions.R")

# load rasters and exclude QGIS .tif.aux.xml files with regex:
s8_img_list <- list.files("./data/_raster/", pattern = "^site8_.*\\.tif$", full.names = T)
s14_img_list <- list.files("./data/_raster/", pattern = "^site14_.*\\.tif$", full.names = T)
s10_img_list <- list.files("./data/_raster/", pattern = "^site10_.*\\.tif$", full.names = T)

s8_stack <- clip_to_subplot(img_list = s8_img_list, subplot_feature = subplot8)
s10_stack <- clip_to_subplot(img_list = s10_img_list, subplot_feature = subplot10)
s14_stack <- clip_to_subplot(img_list = s14_img_list, subplot_feature = subplot14)

s8_agg2 <- agg_my_rasters(s8_stack, mfact = 2, mfun = "mean")
s10_agg2 <- agg_my_rasters(s10_stack, mfact = 2, mfun = "mean")
s14_agg2 <- agg_my_rasters(s14_stack, mfact = 2, mfun = "mean")

s8_met <- calc_spat_metrics(s8_agg2)
s10_met <- calc_spat_metrics(s10_agg2)
s14_met <- calc_spat_metrics(s14_agg2)

s8_met_sd_mean <- metrics_sd_mean_v2(s8_met, n_bands = 4)
s10_met_sd_mean <- metrics_sd_mean_v2(s10_met, n_bands = 4)
s14_met_sd_mean <- metrics_sd_mean_v2(s14_met, n_bands = 4)

# the y variable can be e.g. the # of species for species richness analysis:
s8_no_of_species <- get_y_var_column(8, "species_on_run")
s10_no_of_species <- get_y_var_column(10, "species_on_run")
s14_no_of_species <- get_y_var_column(14, "species_on_run")

# for species richness estimation the metrics data frame is shortened as only
# 2 months of sp.richn. observations are available:
s8_result <- species_linear_modeling("./images/s8_agg2_models",
                                     s8_met_sd_mean[2:3,],
                                     s8_no_of_species[!is.na(s8_no_of_species)],
                                     c(1,2), 4)
