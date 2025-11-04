# Creates a linear model for (i) GLCM metrics and (ii) a corresponding
# dependant variable y (species richness, species abundance, etc.), sorts and
# ranks by slope difference, creates plots and outputs xml table

linear_modeling <- function(png_name, metric_df, y_var, time_intervals, n_bands){
  
  # metric_df: generated from metrics_sd_mean_v2(), one column per metric/band/sd-mean combo,
  #   one row per time interval (=timestep)
  # y_var: vector of length(time_intervals) or dataframe with ncol() == time_intervals
  #   with recorded species per site per run or other variable
  # time_intervals: vector with times for the data, e.g. c(1,2) for a two-time measured relation,
  #   but if data is from april, may, aug, sep then: c(1,2,4,5) to make gaps
  
  if (nrow(metric_df) != length(time_intervals)) {
    stop("Number of rows in metric_df does not match time_intervals")
  }
  
  # arrange the graph.dev grid: 1 row per band, 1 col per metric / sd-mean combo:
  graph_rows <- n_bands
  graph_cols <- ncol(metric_df) / n_bands
  
  # MAKE LINEAR MODELS AND COMPARE COEFFICIENTS:
  # lm for species abundance / fractional cover / etc.
  y_linmod <- lm(y_var ~ time_intervals)
  y_linmod_slope <- coef(y_linmod)["time_intervals"] # slope of species / frac.cover / etc
  
  # lm for GLCM metric values (non-normalized output)
  x_linmod <- lapply(metric_df, function(col) lm(col ~ time_intervals)) # list of lm's
  x_linmod_slopes <- sapply(x_linmod, function(x) coef(x)["time_intervals"]) # list of slopes
  slopes_diff <- abs(x_linmod_slopes - y_linmod_slope)
  x_linmod_sorted <- x_linmod[order(slopes_diff)] # obsolete?
  result <- data.frame(slopes_diff = slopes_diff,
                       slope_x = x_linmod_slopes,
                       slope_y = rep(y_linmod_slope, length(x_linmod_slopes)))
  
  ### write image with centered data for slope comparability:
  # center data and create centered linmods:
  y_centered <- y_var - mean(y_var, na.rm=T)
  y_linmod_centered <- lm(y_centered ~ time_intervals)
  # center each column of x individually:
  x_centered <- x_centered <- as.data.frame(scale(metric_df, center = TRUE, scale = FALSE))
  x_linmod_centered <- lapply(x_centered, function(col) lm(col ~ time_intervals))
  
  # Multiplot image
  png(filename = png_name, height = graph_rows*300, width = graph_cols*400, res = 200)
  par(mfrow = c(graph_rows, graph_cols))
  
  for (i in 1:ncol(metric_df)){
    plot(metric_df[,i],
         ylim=c(-max(metric_df, na.rm = T) / 2, max(metric_df, na.rm = T)),
         xlab = "time",
         ylab="value",
         main=gsub("glcm_", "", colnames(metric_df)[i]))
    abline(reg = x_linmod_centered[[i]], col = "red")
    abline(reg = y_linmod_centered, col = "blue")
  }
  dev.off()
  
  # Write list of all model slopes
  write.table(result,
              file = gsub(".png", "_slopes.xml", png_name), sep = ",",
              row.names = T, col.names = T)
  
  return(result)
}