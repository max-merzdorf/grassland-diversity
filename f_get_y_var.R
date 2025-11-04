# A function to easily retrieve a column for a specific site from the
# env_params dataframe to be used as a y variable in species_linear_modeling()

get_y_var_column <- function(site_nr, column_name){
  column <- env_params[,column_name][env_params$siteID == site_nr]
  return(column)
}