### coefficient of variation:
cv <- function(x){
  r <- sd(x, na.rm = T) / mean(x, na.rm = T)
  return(r)
}