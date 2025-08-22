# This script defines a texture metrics calculation function and applies it to UAV
# multispectral data from the Hubland meadows project, and creates tables and 
# vectors that can be appended to the tables from the table_preparation script

library(GLCMTextures)
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
source("./uav_multispectral_indices.R")

################################################################################
##### TEXTURE METRICS FUNCTION #################################################
plotit = T

# no spectral indices because only one band used
texture_metrics <- function(sf_site, raster, band_no, agg, quant, plotit){
  small <- terra::crop(raster[[band_no]], sf_site$geom)
  agged <- terra::aggregate(small, fact = agg)
  metrics <- GLCMTextures::glcm_textures(agged, w = c(3,3), n_levels = quant,
                                         quant_method = "range",
                                         metrics = c("glcm_contrast", "glcm_dissimilarity",
                                                    "glcm_entropy", "glcm_mean", "glcm_variance"))
  
  # lapply stat derivatives:
  means <- unlist(lapply(metrics, rstmean))
  sds <- unlist(lapply(metrics, rstsd))
  
  if (plotit == T){
  # make png of plots:
  png(filename = paste0("C:/EAGLE/InnoLab/images/site", sf_site$No,"_band",band_no,"_agg",agg,"_quant",quant,".png"),
      res=200, 
      )
  plot(metrics,
       main = c(paste0("CON: ", "mean=",means[1], " / sd=",sds[1], " / agg=",agg, " / quant=",quant),
                paste0("DIS: ", "mean=",means[2], " / sd=",sds[2], " / agg=",agg, " / quant=",quant),
                paste0("ENT: ", "mean=",means[3], " / sd=",sds[3], " / agg=",agg, " / quant=",quant),
                paste0("AVG: ", "mean=",means[4], " / sd=",sds[4], " / agg=",agg, " / quant=",quant),
                paste0("VAR: ", "mean=",means[5], " / sd=",sds[5], " / agg=",agg, " / quant=",quant)
                ),
       col=map.pal("viridis"))
  dev.off()
  }
  
  # return stat derivatives? yeah
  result <- c(sf_site$siteID,band_no,agg,quant,means[1],sds[1],means[2],sds[2],means[3],sds[3],means[4],sds[4],means[5],sds[5])
  
  # convert all to numeric before return
  result[] <- lapply(result, as.numeric)
  return(result)
  
}

# column names for the returned vectors, then rbind them to a dataframe
cnames <- c("site_id","band_no","agg","quant","con_mean","con_sd","dis_mean","dis_sd","ent_mean","ent_sd","mea_mean","mea_sd","var_mean","var_sd", "month")

# split by site:
# we're gonna make the biggest data frame, you're gonna see... tremendous and big data frame, the most tremendous data frame of them all

# make empty dataframe first then rbind the results iteratively
agg_steps <- c(2, 4, 6, 8)
bands <- c(1,2,3,4)

# site 8:
df_site8 <- data.frame()
mosN <- list.files("./data/_raster/", pattern = "N__repro.tif", full.names = T)
# site to crop to:
s8 <- plots[plots$siteID==8,]

for (i in 1:length(mosN)){
  rst <- rast(mosN[i])
  mon <- substr(mosN[i], 21, 21)
  
  for (j in 1:length(agg_steps)){
    for (k in 1:length(bands)){
      r <- texture_metrics(s8, rst,agg =agg_steps[j], band_no = bands[k],quant = 16, plotit = F)
      r <- c(r, mon)
      df_site8 <- rbind(df_site8, r)
    }
  }
  
  # ndvi & savi to plots df
  mean_ndvi <- rstmean(calc_ndvi(rst[[3]], rst[[4]]))
  
}
colnames(df_site8) <- cnames

# site 10:
df_site10 <- data.frame()
mosE <- list.files("./data/_raster/", pattern = "E__repro.tif", full.names = T)
# site to crop to:
s10 <- plots[plots$No==10,]

for (i in 1:length(mosE)){
  rst <- rast(mosE[i])
  for (j in 1:length(agg_steps)){
    for (k in 1:length(bands)){
      r <- texture_metrics(s10, rst,agg =agg_steps[j], band_no = bands[k],quant = 16, plotit = F)
      mon <- substr(mosE[i], 21, 21)
      r <- c(r, mon)
      df_site10 <- rbind(df_site10, r)
    }
  }
}
colnames(df_site10) <- cnames

# site 14:
df_site14 <- data.frame()
mosS <- list.files("./data/_raster/", pattern = "S__repro.tif", full.names = T)
# site to crop to:
s14 <- plots[plots$No==14,]

for (i in 1:length(mosS)){
  rst <- rast(mosS[i])
  for (j in 1:length(agg_steps)){
    for (k in 1:length(bands)){
      r <- texture_metrics(s14, rst,agg =agg_steps[j], band_no = bands[k],quant = 16, plotit = F)
      mon <- substr(mosS[i], 21, 21)
      r <- c(r, mon)
      df_site14 <- rbind(df_site14, r)
    }
  }
}
colnames(df_site14) <- cnames

rm(i, j, k, rst, s10, s14, s8, mon, r)

# maybe:
# -> a function returning stat derivatives with different parameters would be nice
# like input=(large mosaic, a geometry to clip to, agg and quant), return=dataframe 

################################################################################
################# METRIC PLOTS #################################################

# texture metrics from GLCM::texture_metrics() arguments
mymetrics <- c("glcm_contrast", "glcm_dissimilarity", "glcm_entropy", "glcm_mean", "glcm_variance")

# just try apply texture_metrics first
# band 1:
r <- rast("./data/_raster/20240408_N_subplot.tif")
names(r) <- c("band1", "band2", "band3", "band4")
b1_metrics <- GLCMTextures::glcm_textures(r = r[[1]],
                            n_levels = 16, w = c(3, 3), metrics = mymetrics, quant_method = "range")
b2_metrics <- glcm_textures(r[[2]], n_levels=16,w=c(3,3),metrics=mymetrics,quant_method="range")
b3_metrics <- glcm_textures(r[[3]], n_levels=16,w=c(3,3),metrics=mymetrics,quant_method="range")
b4_metrics <- glcm_textures(r[[4]], n_levels=16,w=c(3,3),metrics=mymetrics,quant_method="range")

names(b1_metrics) <- paste0(names(b1_metrics), "_B1")
names(b2_metrics) <- paste0(names(b2_metrics), "_B2")
names(b3_metrics) <- paste0(names(b3_metrics), "_B3")
names(b4_metrics) <- paste0(names(b4_metrics), "_B4")

bandmetrics_combined <- c(b1_metrics, b2_metrics, b3_metrics, b4_metrics)
bandmetrics_combined

png("./images/graphs/multiplot_test.png", width = 5000, height = 3000, units = "px", res = 300)
par(mfrow = c(4, 5), mar = c(1, 1, 2, 1))
for (i in 1:nlyr(bandmetrics_combined)) {
  plot(bandmetrics_combined[[i]], main = names(bandmetrics_combined)[i], legend.width=0.6)
}
dev.off()
################################################################################
################# INTRA DF-CORRELATIONS ########################################

# when I plot a whole column of df_x you can see interesting patterns responding
# to the aggregation or bands used, investigate this here:
dftest <- df_site8
dftest[] <- lapply(dftest, as.numeric)
dftest$index <- seq(nrow(dftest))
plot(dftest$con_sd)
ggplot(dftest, aes(x=index,
                   y= con_mean,
                   color=as.character(agg))) +
  geom_point() +
  scale_y_continuous(limits = c(0.0,20.0))

intradf_metrics_comp <- function(df){
  metrics <- c("con_mean","con_sd","dis_mean","dis_sd","ent_mean","ent_sd","mea_mean","mea_sd","var_mean","var_sd")
  plts <- list()
  for(i in 1:length(metrics)){
    
    plts[[i]] <- ggplot(df, aes(x = index, y = .data[[metrics[i]]], color=as.character(band_no))) +
      geom_point() +
      scale_y_continuous(limits=c(0, 20)) +
      labs(title = as.character(metrics[i]), y = "value", x = "arbitrary index no", color = "band_no")
  }
  return(plts)
}

r <- intradf_metrics_comp(dftest)
#png("./images/graphs/site_8_metrics_depend_on_band.png", width = 4000, height = 2000, res = 300)
grid.arrange(grobs=r)
#dev.off()

################################################################################
################# CORRELATE WITH ENV PARAMS DATA FRAME #########################

# bind env_params to df_x data frames

s8 <- rast(mosN[1])[[4]]
plot(rast(mosE[1])[[4]])
plot(rast(mosS[1])[[4]])

