# Calculate all predictor variables for all UAS scenes at three different resolutions
# > Resolutions are set by terra::aggregate()
# > Factor 1 is native resolution (3 cm), Factor 2 is 6 cm and Factor 4 is 12 cm

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
sites <- c(8, 10, 14)
aggsteps <- c(1, 2, 4)
plots <- sf::st_read("./data/_vector/_analysisready_plots.gpkg")

for (i in sites){
  uav_images <- list.files("./data/_raster/original/",
                           pattern = paste0("site", i,
                                            "_",
                                            "resampled_georef_aligned_8bit",
                                            "\\.tif$"),
                           full.names = T)
  stack <- terra::rast(uav_images)
  stack <- terra::crop(stack, sf::st_buffer(plots[plots$siteID == i,],
                                            dist = 0.15,
                                            endCapStyle = "SQUARE"))
  
  names(stack) <- gsub("[NSE]_orthomosaic",
                       paste0("site", i, "_", "B"),
                       names(stack))
  names(stack) <- gsub("resampled_georef_", "B_", names(stack))
  stack <- terra::as.int(stack)
  
  for (j in 1:length(aggsteps)){
    agg <- terra::aggregate(stack, fact = aggsteps[j], fun = "mean")
    metrics <- calc_spat_metrics(agg, nlevels = 32)
    wsizes <- c(3,5,7,9)
    raostack <- terra::rast()
    
    for(k in 1:nlyr(agg)){
      lyrname <- names(agg)[k]
      
      for(l in 1:length(wsizes)){
        rao <- rasterdiv::paRao(agg[[k]], dist_m = "euclidean", alpha = 1,
                                window = wsizes[l])
        names(rao[[1]][[1]]) <- paste0(lyrname,"_RaoQ_",names(rao),"_",
                                       names(rao[[1]]))
        raostack <- c(raostack, rao[[1]][[1]])
      }
    }
    
    metrics <- c(metrics, raostack)
    
    # coefficient of variation for each band:
    cvs <- c()
    for(m in 1:nlyr(agg)){
      covar <- cv(matrix(agg[[m]]))
      names(covar) <- paste0(names(agg[[m]]), "_coVar")
      cvs <- c(cvs, covar)
    }
    cvdf <- data.frame(cvs, row.names = names(cvs))
    
    dates <- unique(unlist(lapply(names(metrics), FUN = substr, 1, 8)))
    
    # population standard deviation of GLCM textures
    metrics_stats <- global(x = metrics,
                            fun = "std",
                            na.rm = TRUE)
    
    # clarify population standard deviation in name, as we add coefficient of
    # variation to the same dataframe later
    rownames(metrics_stats) <- paste0(rownames(metrics_stats), "_std")
    
    # bind the dataframes
    colnames(cvdf) <- "std"
    metrics_stats <- rbind(metrics_stats, cvdf)
    predictor_names <- unique(gsub("\\d{8}_site\\d{1,2}_", "",
                                   rownames(metrics_stats)))
    
    mat <- matrix(metrics_stats$std,
                  ncol = 4,
                  byrow = FALSE)
    metstats <- as.data.frame(mat)
    colnames(metstats) <- dates
    rownames(metstats) <- predictor_names
    
    # change name to metrics_stat_derivatives so i don't have to change the
    # following and transpose the data frame so that dates are in columns:
    metrics_stat_derivs <- as.data.frame(t(metstats))
    
    tabname <- paste0("UAS_site",i,"_agg",aggsteps[j],"_metrics.csv")
    write.table(metrics_stat_derivs,
                paste0("./results/metric_tables/", tabname),
                sep = ",", dec = ".")
    
  }
}
