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

processing = "resampled_georef_aligned_8bit"
# "resampled_georef_clipped_aligned"

########## DON'T TOUCH: #############

# load tables
env_params <- read.table("./data/_tables/_analysisready_env_params.csv")
plots <- sf::st_read("./data/_vector/_analysisready_plots.gpkg")
allspecies <- read.table("./data/_tables/_analysisready_allspecies.csv")

# load and stack UAS images
uav_images <- list.files("./data/_raster/original/",
                         pattern = paste0("site", site_nr, "_",
                                          processing, "\\.tif$"),
                         full.names = T)
stack <- terra::rast(uav_images)

# crop to the subplot feature. apply a 0.1 m buffer for moving windows
# (largest window size is
# 9 px * 0.03m res = 0.27m / 2 = 0.135 -> 15 cm buffer)
stack <- terra::crop(stack, sf::st_buffer(plots[plots$siteID == site_nr,],
                                          dist = 0.15,
                                          endCapStyle = "SQUARE"))

# April bands have wrong names as they were not georeferenced
names(stack) <- gsub("[NSE]_orthomosaic",
                     paste0("site", site_nr, "_", "B"),
                     names(stack))
names(stack) <- gsub("resampled_georef_", "B_", names(stack))

# The raster is converted to 8-bit. High reflectance noise is
# removed by cutting the 99th percentile of values, to enhance contrast
# in non-noise values (this step was done in 'percentile_data_cut.R'
# and is read from disk)

# convert to integer
stack <- terra::as.int(stack)

# aggregate resolution
agg <- terra::aggregate(stack, fact = agg_factor, fun = "mean")

# calc GLCM Textures
metrics <- calc_spat_metrics(agg, nlevels = nlevels)

# add Rao's Q for every band as layer:
wsizes <- c(3,5,7,9)
raostack <- terra::rast()

for(i in 1:nlyr(agg)){
  lyrname <- names(agg)[i]
  
  for(j in 1:length(wsizes)){
    rao <- rasterdiv::paRao(agg[[i]], dist_m = "euclidean", alpha = 1,
                            window = wsizes[j])
    names(rao[[1]][[1]]) <- paste0(lyrname,"_RaoQ_",names(rao),"_",
                                   names(rao[[1]]))
    raostack <- c(raostack, rao[[1]][[1]])
  }
}

# merge rao results with metrics:
metrics <- c(metrics, raostack)

# coefficient of variation for each band:
cvs <- c()
for(i in 1:nlyr(agg)){
  covar <- cv(matrix(agg[[i]]))
  names(covar) <- paste0(names(agg[[i]]), "_coVar")
  cvs <- c(cvs, covar)
}

cvdf <- data.frame(cvs, row.names = names(cvs))

# get dates and names for row and column names:
dates <- unique(unlist(lapply(names(metrics), FUN = substr, 1, 8)))

# population standard deviation of GLCM textures
metrics_stats <- global(x = metrics,
                        fun = "std",
                        na.rm = TRUE)

# clarify population standard deviation in name, as we add coefficient of
# variation to the same dataframe later
rownames(metrics_stats) <- paste0(rownames(metrics_stats), "_std")
predictor_names <- unique(gsub("\\d{8}_site\\d{1,2}_", "",
                               rownames(metrics_stats)))

# bind the dataframes
colnames(cvdf) <- "std"
rbind(metrics_stats, cvdf)

mat <- matrix(metrics_stats$std,
              ncol = 4,
              byrow = FALSE)
metstats <- as.data.frame(mat)
colnames(metstats) <- dates
rownames(metstats) <- predictor_names

# change name to metrics_stat_derivatives so i don't have to change the
# following and transpose the data frame so that dates are in columns:
metrics_stat_derivs <- as.data.frame(t(metstats))

# remove june from env_params (no UAV image that month)
env_params <- subset(env_params, env_params$Col_run != 2)

# get number of spectral bands
bands <- nlyr(metrics) / ncol(metrics_stat_derivs)

### Species richness

png_name <- paste0("site", site_nr,"_","speciesRichness","_n",
                   nlevels,"_agg",agg_factor,".png")
# get y variable (sp.richness) data
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

### Vegetation height:

png_name <- paste0("site", site_nr,"_","vegetationHeight","_n",nlevels,
                   "_agg",agg_factor,".png")

y_points <- get_y_var_column(site_nr, "veg_height_cm")

res <- linear_modeling(png_name = png_name,
                       metric_df = metrics_stat_derivs,
                       y_var = y_points,
                       c(1,3,4,5),
                       bands)
