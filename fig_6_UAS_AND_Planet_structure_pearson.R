library(tidyverse)
library(ggplot2)
library(forcats)

# r value on y, predictor on x, 

uas_pearson <- read.csv("./results/ACTUAL_RESULTS/UAS_structure_vars_pearson.csv")
planet_pearson <- read.csv("./results/ACTUAL_RESULTS/Planet_structure_vars_pearson.csv")

# add planet as fourth resolution:
planet_df <- planet_pearson %>%
  select(-Type) %>%
  mutate(agg = "agg10") %>%
  mutate(abs_r = abs(pearson_r)) %>%
  group_by(dependent_var) %>%
  mutate(mean_abs_r = mean(abs_r, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    dependent_var = fct_reorder(dependent_var, mean_abs_r)
  )

planet_collapsed <- planet_pearson %>%
  group_by(independent_var, dependent_var) %>%
  summarise(
    pearson_r = mean(pearson_r, na.rm = TRUE),
    p_value   = mean(p_value, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(agg = "agg10") %>%
  mutate(abs_r = abs(pearson_r)) %>%
  group_by(dependent_var) %>%
  mutate(mean_abs_r = mean(abs_r, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    dependent_var = fct_reorder(dependent_var, mean_abs_r)
  )

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
  bind_rows(planet_collapsed) %>%
  mutate(agg = factor(case_when(
    agg == "agg1" ~ "3 cm",
    agg == "agg2" ~ "6 cm",
    agg == "agg4" ~ "12 cm",
    agg == "agg10" ~ "300 cm"
  ), levels = c("3 cm", "6 cm", "12 cm", "300 cm"))) %>%
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
  labs(title = "UAS and PlanetScope absolute difference of pearson correlation grouped by resolutions")
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