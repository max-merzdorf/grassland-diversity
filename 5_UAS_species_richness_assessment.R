bands_UAS_Richness <- bands_UAS_Richness %>%
  add_row(band = "SSD",
          `mean(mean_diff_richness)` = NA)

combo <- bands_Planet_Richness %>%
  left_join(bands_Planet_Shannon, by = "band") %>%
  left_join(bands_Planet_Turnover, by = "band") %>%
  left_join(bands_UAS_Richness, by = "band") %>%
  left_join(bands_UAS_Shannon, by = "band") %>%
  left_join(bands_UAS_Turnover, by = "band")

clong <- combo %>%
  pivot_longer(-band) %>%
  group_by(band) %>%
  summarise(sum = sum(value))


# predictors:
preds_UAS_Richness <-  preds_UAS_Richness %>%
  add_row(pred = "SSD",
          `sum(n_sites_richness)` = 0)
combo <- preds_UAS_Richness %>%
  left_join(preds_UAS_Shannon, by = "pred") %>%
  left_join(preds_UAS_turnover, by = "pred")
clong <- combo %>%
  pivot_longer(-pred) %>%
  group_by(pred) %>%
  summarise(sum = sum(value))



combo <- preds_Planet_Richness %>%
  left_join(preds_Planet_Shannon, by = "pred") %>%
  left_join(preds_Planet_Turnover, by = "pred")
clong <- combo %>%
  pivot_longer(-pred) %>%
  group_by(pred) %>%
  summarise(sum = sum(value))
