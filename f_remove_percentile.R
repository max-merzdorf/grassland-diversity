remove_percentile <- function(r, percentile){
  values(r)[values(r) >= quantile(values(r), probs = percentile, na.rm = T)] <- NA
  return(r)
}