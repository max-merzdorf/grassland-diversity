library(tidyverse)
planet_metrics <- read.csv("./results/Planet_texture_metric_results.csv")

p <- planet_metrics %>%
  filter(site %in% c("site3", "site5", "site14")) %>%
  mutate(site = factor(case_when(
    site == "site3" ~ "semi_nat",
    site == "site5" ~ "int",
    site == "site14" ~"ext"
  ), levels = c("int", "ext", "semi_nat"))) %>%
  select(-contains("RaoQ")) %>%
  select(-contains("CV")) %>%
  select(-contains("SSD")) %>%
  select(-contains("contrast")) %>%
  pivot_longer(cols = -c(date, site, month))

plotty <- ggplot(p, aes(x = name, y = value, fill = site)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()
plotty
ggsave(filename = "./images/6_Discussion_Planet_GLCM_minus_contrast.png", plot = plotty,
       width = 3000, height = 1000, units = "px", dpi = 300)
