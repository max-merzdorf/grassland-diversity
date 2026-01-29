library(tidyverse)
library(ggplot2)
library(forcats)

planet_pearson <- read.csv("./results/ACTUAL_RESULTS/Planet_structure_vars_pearson.csv")

plot_df <- planet_pearson %>%
  mutate(abs_r = abs(pearson_r)) %>%
  group_by(dependent_var) %>%
  mutate(mean_abs_r = mean(abs_r, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    dependent_var = fct_reorder(dependent_var, mean_abs_r)
  ) %>%
  mutate(Type = factor(Type, levels = c("int", "ext", "semi_nat")))

p <- ggplot(plot_df, aes(
  x = independent_var,
  y = dependent_var,
  fill = abs_r
)) +
  geom_tile(color = "white", linewidth = 0.5) +
  facet_wrap(~ Type, nrow = 1) +
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
  labs(title = "PlanetScope absolute difference of pearson correlation grouped by management type")
p

ggsave(filename = "./images/5_Results_Planet_pearson.png", plot = p,
       width = 3000, height = 2000, units = "px", dpi = 300)
