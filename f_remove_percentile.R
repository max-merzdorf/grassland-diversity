remove_percentile <- function(r, percentile){
  terra::values(r)[terra::values(r) >= quantile(values(r), probs = percentile, na.rm = T)] <- NA
  return(r)
}