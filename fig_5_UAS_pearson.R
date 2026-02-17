library(tidyverse)
library(ggplot2)
library(forcats)

# r value on y, predictor on x, 

uas_pearson <- read.csv("./results/ACTUAL_RESULTS/UAS_structure_vars_pearson.csv")

## Plot

plot_df <- uas_pearson %>%
  mutate(abs_r = abs(pearson_r)) %>%
  group_by(dependent_var) %>%
  mutate(mean_abs_r = mean(abs_r, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    dependent_var = fct_reorder(dependent_var, mean_abs_r, .desc = TRUE)
  ) %>%
  select(-n)

# bind rows:
plot_df <- plot_df %>%
  mutate(agg = factor(case_when(
    agg == "agg1" ~ "3 cm",
    agg == "agg2" ~ "6 cm",
    agg == "agg4" ~ "12 cm"
  ), levels = c("3 cm", "6 cm", "12 cm"))) %>%
  mutate(
    dependent_var = fct_reorder(dependent_var, mean_abs_r, .desc = F)
  )

p <- ggplot(plot_df, aes(
  x = independent_var,
  y = dependent_var,
  fill = abs_r
)) +
  geom_tile(color = "white", linewidth = 0.5) +
  facet_wrap(~ agg, nrow = 1) +
  scale_fill_viridis_c(
    name = "|r|",
    limits = c(0, 1)
  ) +
  labs(
    x = "Target variable",
    y = "Predictor name"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey50", color = "grey50")
  ) +
  labs(title = "UAS absolute difference of pearson correlation grouped by resolutions")
p
# sanity checks to see if factor ordering works in the plot
plot_df %>%
  filter(dependent_var == "B2_glcm_entropy") %>%
  select(mean_abs_r) %>%
  pull() %>%
  mean()

plot_df %>%
  filter(dependent_var == "B2_RaoQ_w3") %>%
  select(mean_abs_r) %>%
  pull() %>%
  mean()

plot_df %>%
  filter(dependent_var == "B2_RaoQ_w9") %>%
  select(mean_abs_r) %>%
  pull() %>%
  mean()

ggsave(filename = "./images/5_Results_UAS_pearson.png", plot = p,
       width = 3000, height = 2000, units = "px", dpi = 300)

dvs <- c("B2_glcm_entropy", "B2_glcm_ASM", "B3_glcm_homogeneity", "B3_glcm_entropy", "B3_glcm_dissimilarity", "B3_glcm_ASM")
ivs <- c("bare.soil", "moss")
res <- c("3 cm")

for(i in dvs){
  df <- plot_df %>%
    filter(dependent_var == i)
  for(j in ivs){
    m <- df %>%
      filter(independent_var == j) %>%
      select(abs_r) %>%
      pull() %>%
      mean()
    writeLines(paste0(i, " / ", j, " mean: ", m))
  }
}

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

calc_means(plot_df,
           dvs = c("B4_RaoQ_w5", "B4_RaoQ_w7"),
           ivs = c("litter"),
           res = c("3 cm", "12 cm"))
