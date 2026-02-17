calc_means <- function(df, dvs, ivs, res, value_col = abs_r) {
  df %>%
    filter(dependent_var %in% dvs,
           independent_var %in% ivs,
           agg %in% res) %>%
    group_by(dependent_var, independent_var, agg) %>%
    summarise(
      mean = mean({{ value_col }}, na.rm = TRUE),
      .groups = "drop"
    )
}
