band_histograms <- function(r, max_value = NULL){
  
  df <- pivot_longer(as.data.frame(r),
                     cols = colnames(as.data.frame(r)))

  if (is.null(max_value)){
    max_value <- max(df$value)
  }
  
  p <- ggplot(data = df, aes(x=value, group=name, fill=name)) +
    geom_density() +
    xlim(c(0, max_value)) +
    theme_minimal() +
    scale_fill_viridis_d()
  
  return(p)
}
