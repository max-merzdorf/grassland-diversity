library(ggplot2)
# how do the output values differ across bands grouped by predictor?
# do higher initial values in a band mean higher predictor values?

# predictor on x, value on y, grouped by band
planet_metrics <- read.csv("./results/Planet_texture_metric_results.csv")
uas_metrics <- read.csv("./results/UAS_texture_metric_results.csv")

p <- planet_metrics %>%
  select(-c(date, site, month, SSD)) %>%
  pivot_longer(cols = starts_with("B")) %>%
  mutate(band = factor(case_when(
    startsWith(name, "B1") ~ "Green",
    startsWith(name, "B2") ~ "Red",
    startsWith(name, "B3") ~ "RE",
    startsWith(name, "B4") ~ "NIR"
  ), levels = c("Green", "Red", "RE", "NIR")),
  name = str_remove(name, "^B\\d{1}_")) %>%
  mutate(name = sub("glcm", "GLCM", name))

plotty <- ggplot(p, aes(x = name, y = value, fill = band)) +
  geom_boxplot() +
  scale_fill_viridis_d(name = "Band") +
  theme_light() +
  xlab("Predictor name") +
  ylab("Value") +
  labs(title = "PlanetScope predictor values grouped by band") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))
plotty
ggsave(filename = "./images/5_Planet_predictorValues_by_band.png", plot = plotty,
       width = 3000, height = 2000, units = "px", dpi = 300)

############ UASSS

u <- uas_metrics %>%
  select(-c(date, site, month, SSD, agg)) %>%
  pivot_longer(cols = starts_with("B")) %>%
  mutate(band = factor(case_when(
    startsWith(name, "B1") ~ "Green",
    startsWith(name, "B2") ~ "Red",
    startsWith(name, "B3") ~ "RE",
    startsWith(name, "B4") ~ "NIR"
  ), levels = c("Green", "Red", "RE", "NIR")),
  name = str_remove(name, "^B\\d{1}_")) %>%
  mutate(name = sub("glcm", "GLCM", name))

plotty <- ggplot(u, aes(x = name, y = value, fill = band)) +
  geom_boxplot() +
  scale_fill_viridis_d(name = "Band") +
  theme_light() +
  xlab("Predictor name") +
  ylab("Value") +
  labs(title = "UAS predictor values grouped by band") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))
plotty
ggsave(filename = "./images/5_UAS_predictorValues_by_band.png", plot = plotty,
       width = 3000, height = 2000, units = "px", dpi = 300)
