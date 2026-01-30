# Samples n random pixels of a SpatRaster across layers (time) and
# plots the values like in GoogleEarthEngine

pixel_history <- function(r, npx){
  
  stopifnot(inherits(r, "SpatRaster"))
  
  all_vals <- terra::values(r)
  
  # Identify complete cells (no NA in ANY layer)
  valid_cells <- which(stats::complete.cases(all_vals))
  
  # randomly select cell numbers
  cells <- sample(valid_cells, npx)
  
  # extract values
  vals <- terra::extract(r, cells)
  
  # layer (time) names
  layer_names <- names(r)
  if (any(layer_names == "")) {
    layer_names <- paste0("t", seq_len(nlyr(r)))
  }

  vals$pixel <- seq_len(npx)
  
  df_long <- tidyr::pivot_longer(
    vals,
    cols = all_of(layer_names),
    names_to = "time",
    values_to = "value"
  )
  
  return(df_long)
}
