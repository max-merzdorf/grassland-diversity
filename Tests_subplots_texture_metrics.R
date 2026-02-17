# This script calculates texture metrics from UAV imagery for only the 2x2 subplot areas
# of the Hubland meadows project. The resulting values are held in data frame format
# that can be appended to the subplot simple features, and from these the correlations
# between certain metrics and the %-cover of e.g. soil or herbs can be modelled

library(GLCMTextures)
library(rasterdiv)
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
source("./uav_multispectral_indices.R")
source("./helper_functions.R")

##### SITE 8 PROCESSING #####

# gather all images as one rastestack:
s8imgs <- list.files("./data/_raster/", pattern = "^site8_.*\\.tif$", full.names = T)
s8raster<- terra::rast(s8imgs[1])
s8stack <- rast()
# read, clip and stack every image tu the corresponding subplot feature:
for (i in 1:length(s8imgs)){
  s8raster <- terra::rast(s8imgs[i])
  s8clipped <- terra::crop(s8raster, subplot8)
  s8stack <- c(s8stack, s8clipped)
}
s8stack

# I should make this into a function:
clip_to_subplot <- function(img_list, subplot_feature){
  res_stack <- terra::rast()
  for (i in 1:length(img_list)){
    site_raster <- terra::rast(img_list[i])
    subplot_raster <- terra::crop(site_raster, subplot_feature)
    res_stack <- c(res_stack, subplot_raster)
    # NOTE: this will throw a warning that the first raster is empty, ignore!
  }
  return(res_stack)
}


# When aggregating I want to establish the effect of different aggregation functions
# (mean vs median), as well as the effect of different aggregation factors (2, 4, 8)

# wrapper function for aggregating:
agg_my_rasters <- function(rstack, mfact, mfun){
  res <- rast()
  for (i in 1:nlyr(rstack)){
    agged <- terra::aggregate(rstack[[i]], fact=mfact, fun=mfun)
    res <- c(res, agged)
  }
  return(res)
}
# -> because expanding raster layers by r[[i]] iteratively does not work

# 2 different aggregation functions:
s8_agg2_mean <- agg_my_rasters(s8stack, 2, "mean")
s8_agg2_median <- agg_my_rasters(s8stack, 2, "median")
# -> different aggregation functions produce absolutely minimal differences
# -> use mean for other steps
rm(s8_agg2_median)

# WHICH METRICS TO USE AS PREDICTOR VARIABLES?
# We select 4 metrics: Rao's Q, GLCM Entropy, GLCM Dissimilarity and GLCM Mean.
# We create a function for each metric that takes a rasterstack with 4 bands (layers)
#   and returns the sd, mean and median for every band.
# Then we can compare which band, which metric, and which aggregation resolution
#   best predict species richness in a regression

### 1. Rao's Q:
sd(values(s8_agg2_median), na.rm = T)
raotest <- rasterdiv::paRao(s8stack[[4]], window = 3)
raotest
# -> doesn't work on floating nums?? So have to multiply each cell by 10000

values(s8stack[[4]])
multiplied <- s8stack[[4]] * 10000
raotest <- rasterdiv::paRao(multiplied, window = 3)
raotest

### 2. GLCM Entropy (^= Shannon's H'?)

### 3. GLCM Dissimilarity

### 4. GLCM Mean

##### PROCESSING PER SITE #####
# The workflow is:
# 1. Clip images to site
#   (1.1 Make NDVI & RGB composite images as extra layers)
# 2. Aggregate to resolution of choice
# 3. Get sd, mean for the different texture metrics as dataframe

### Test functionality for site 14:
s14_images <- list.files("../InnoLab/data/_raster/", pattern = "^site14.*\\.tif$", full.names = T)
s14_subplots <- clip_to_subplot(s14_images, subplot_feature = subplot14)
met_s14_apr <- calc_spat_metrics(myraster = s14_subplots)

# select only april and july metric rasters to coincide with the species recording
# months:
s14_apr_jul_metrics <- c(met_s14_apr[[1:12]], met_s14_apr[[25:36]])

# calculate sd and mean for each metric raster:
test2 <- metrics_sd_mean_v2(s14_apr_jul_metrics)

#### LINEAR REGRESSION ####
# time variable for linear model:
time <- 1:nrow(test2)

# find min and max for dataset:
vmin <- min(test2[1,])
vmax <- max(test2[2,])

# set graphics device and model for one example:
par(mfrow=c(1,1))
plot(test2[,1])
abline(reg = lm(test2[,1] ~ time))

# plot all:
time <- c(1,2) # CHANGE FOR THE OTHER ANALYSES DUE TO MISSING MONTH
png(filename="./plots_test.png", width = 3000, height = 1500, res = 200)
par(mfrow=c(4,6)) # 4 rows for 4 bands

for (i in 1:ncol(test2)){
  plot(test2[,i],
       ylim=c(0, vmax),
       xlab = "time",
       ylab="value",
       main=colnames(test2)[i])
  abline(reg = lm(test2[,i] ~ time), col = "red")
}
dev.off()

# get species counts for site 14:
s14_species <- env_params[env_params$siteID == 14,] # -> Apr: 23; Jul: 17
s14_species <- s14_species$species_on_run[!is.na(s14_species$species_on_run)]
s14_species

# Compare 2 linear models on coefficients:
metrics_model <- lm(test2[,1]~time)
species_model <- lm(s14_species~time)
coef(metrics_model)
coef(species_model)
# ^ do this for every metric and then add the abline for the species lm to the plots

# can I create a list of linear models (-> list of lists)
lm_list_test <- list()
lm_list_test <- lapply(test2, function(col) lm(col ~ time)) # yes!
coef(lm_list_test[[1]])["time"]

# -> list of slope per model
# -> abs() on the list
# -> (named) list of aboslute differences
# -> sort ascending to find lm's with best fit
slopes <- sapply(lm_list_test, function(x) coef(x)["time"])
abs_diff <- abs(slopes - coef(species_model)["time"])
lm_list_sorted <- lm_list_test[order(abs_diff)]

# abs works properly:
abs_test <- abs(slopes - 1.5)
abs_test[order(abs_test)]

# can I combine the ordered abs_diff and slopes vectors ? yeees
combo <- data.frame(abs_diff=abs_diff[order(abs_diff)], slope=slopes[order(abs_diff)])

# can I display slopes above eachother?
par(mfrow=c(1,1))
plot(test2[,1])

glcm_model <- lm(test2[,1] ~ time)
abline(reg = glcm_model, col="red")
species_slope <- coef(species_model)["time"]
max(test2[,1])
species_intercept <- max(test2[,1]) / 2
abline(a = 0.8, b = 1, col="blue")

#> scales are mismatched
#> try centering:
s14_species_centered <- s14_species - mean(s14_species)
texture_centered <- test2[,1] - mean(test2[,1])
glcm_model_centered <- lm(texture_centered ~ time)
species_model_centered <- lm(s14_species_centered ~ time)
plot(texture_centered)
abline(reg=glcm_model_centered)
abline(species_model_centered)

#> try normalizing:
#> can't normalize it because of only 2 data points
s14_species_z <- (s14_species - mean(s14_species)) / sd(s14_species)

#> MAKE ONCE MANUALLY WITH BOTH SLOPES:
png(filename="./images/plots_test_centered.png", width = 3000, height = 1500, res = 200)
par(mfrow=c(4,6)) # 4 rows for 4 bands

for (i in 1:ncol(test2)){
  texture_centered <- test2[,i] - mean(test2[,i])
  glcm_model_centered <- lm(texture_centered ~ time)
  plot(test2[,i],
       ylim=c(0, 4),
       xlab = "time",
       ylab="value",
       main=colnames(test2)[i])
  abline(reg = glcm_model_centered, col = "red")
  abline(reg = species_model_centered, col="blue")
}
dev.off()

s14_species_df <- data.frame(s14_species)
species_linear_modeling("test1.png", test2, y_var = s14_species, time_intervals = c(1,2), n_bands = 4)



#> compare deltas:
d_species <- s14_species[2] - s14_species[1]
d_texture <- test2[2,1] - test2[1,1]


#> TODO:
#> try with logarithmic scaling:
glcm_model_centered_log <- lm(log(texture_centered) ~ log(time))
species_model_centered_log <- lm(log(s14_species_centered) ~ log(time))
plot(texture_centered)
abline(reg=glcm_model_centered_log)
abline(species_model_centered_log)