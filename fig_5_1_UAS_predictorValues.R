uas_predictors <- read.csv("./results/UAS_texture_metric_results.csv")
##### 5.1 UAS results ######
# UAS predictor values depend on aggregate:
u <- uas_predictors %>%
  select(c(starts_with("B1"), "SSD", "agg")) %>%
  rename_with(~ gsub("B1_", "", .x, fixed = TRUE)) %>%
  mutate(resolution = case_when(
    agg == "agg1" ~ 3,
    agg == "agg2" ~ 6,
    agg == "agg4" ~ 12
  )) %>%
  pivot_longer(cols = -c(agg, resolution))
p <- ggplot(u, aes(x = name, y = value, fill = as.factor(resolution))) +
  geom_boxplot() +
  scale_fill_viridis_d() +
  labs(title = "UAS predictor output values") +
  guides(fill = guide_legend(title = "Resolution [cm]")) +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  xlab("Predictor") +
  ylab("Value") +
  theme_light()
p
#ggsave("./images/51_UAS_predictor_values.png", plot = p, width = 3000, height = 1500, units = "px", dpi = 300)

byagg <- u %>%
  filter(name == "RaoQ_w3") %>%
  group_by(agg) %>%
  summarise(mean_value = mean(value, na.rm = TRUE))

byname <- u %>%
  group_by(name) %>%
  summarise(mean_value = mean(value, na.rm = TRUE))
