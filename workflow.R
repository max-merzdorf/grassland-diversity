library(GLCMTextures)
library(rasterdiv)
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
source("./helper_functions.R")

s8_img_list <- list.files("./data/_raster/", pattern = "site8_*", full.names = T)
s14_img_list <- list.files("./data/_raster/", pattern = "site14_*", full.names = T)
s10_img_list <- list.files("./data/_raster/", pattern = "site10_*", full.names = T)

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

s8_result <- species_linear_modeling("s8_agg2_models", s8_met_sd_mean, )