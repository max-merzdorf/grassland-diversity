library(ggplot2)
library(terra)
library(sf)

planet_slopes <- read.csv("./results/ACTUAL_RESULTS/Planet_slopes_FINAL_Type_grouped.csv")

u <- planet_slopes %>%
  select(c(predictor, Type,
           starts_with("prop"), starts_with("mean"))) %>%
  group_by(predictor, Type) %>%
  summarise(
    across(
      where(is.numeric),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) %>%
  mutate(
    across(starts_with("prop_"), ~ na_if(., 0))
  ) %>%
  # can I use case_when to replace same_sign == 0 with NA and do the same in the rank rows of the same predictors?
  mutate(
    rank_ss_S = rank(prop_same_sign_shannon,  ties.method = "average", na.last = "keep"),
    rank_ss_T = rank(prop_same_sign_turnover, ties.method = "average", na.last = "keep"),
    rank_ss_R = rank(prop_same_sign_richness, ties.method = "average", na.last = "keep")
  ) %>%
  mutate(ranksum_ss_X = rowSums(
    select(., starts_with("rank_ss")),
    na.rm = T)
  ) %>%
  mutate(
    rank_delta_S = rank(-abs(mean_diff_shannon), ties.method = "average"),
    rank_delta_T = rank(-abs(mean_diff_turnover), ties.method = "average"),
    rank_delta_R = rank(-abs(mean_diff_richness), ties.method = "average")
  ) %>%
  mutate(ranksum_delta_X = rowSums(
    select(., starts_with("rank_delta")),
    na.rm = T
  )) %>%
  mutate(ranksum = ranksum_delta_X + ranksum_ss_X) %>%
  mutate(Type = factor(case_when(
    Type == "int_prox" ~ "int",
    Type == "int_cit" ~ "int",
    Type == "ext_prox" ~ "ext",
    .default = Type
  ), levels = c("int", "ext", "semi_nat")))

# group for interpretation of results:
u_bands <- u %>%
  mutate(band = case_when(
    str_starts(predictor, "B1") ~ "Band 1",
    str_starts(predictor, "B2") ~ "Band 2",
    str_starts(predictor, "B3") ~ "Band 3",
    str_starts(predictor, "B4") ~ "Band 4",
    str_starts(predictor, "SSD") ~ "SSD x 10" # x11 predictors per band
  )) %>%
  group_by(band, Type) %>%
  summarise(value = sum(ranksum_delta_X)) %>%
  mutate(value = if_else(
    band == "SSD x 10",
    value * 10,
    value))

u_predictor_x_bands <- u %>%
  mutate(predictor_x_bands = case_when(
    str_detect(predictor, "CV") ~ "CV",
    str_detect(predictor, "contrast") ~ "GLCM contrast",
    str_detect(predictor, "dissimilarity") ~ "GLCM dissimilarity",
    str_detect(predictor, "entropy") ~ "GLCM entropy",
    str_detect(predictor, "homogeneity") ~ "GLCM homogeneity",
    str_detect(predictor, "ASM") ~ "GLCM ASM",
    str_detect(predictor, "RaoQ_w3") ~ "RaoQ w3",
    str_detect(predictor, "RaoQ_w5") ~ "RaoQ w5",
    str_detect(predictor, "RaoQ_w7") ~ "RaoQ w7",
    str_detect(predictor, "RaoQ_w9") ~ "RaoQ w9",
    str_starts(predictor, "SSD") ~ "SSD x 4" # x4 bands per predictor
  )) %>%
  group_by(predictor_x_bands, Type) %>%
  summarise(value = sum(ranksum_delta_X)) %>%
  mutate(value = if_else(
    predictor_x_bands == "SSD x 4",
    value * 4,
    value))

bands_tbl <- u_bands %>%
  rename(Category = band) %>%
  pivot_wider(
    names_from = Type,
    values_from = value,
    names_prefix = "Typ "
  ) %>%
  arrange(Category)

pred_tbl <- u_predictor_x_bands %>%
  rename(Category = predictor_x_bands) %>%
  pivot_wider(
    names_from = Type,
    values_from = value,
    names_prefix = "Typ "
  ) %>%
  arrange(Category)

combined_tbl <- bind_rows(bands_tbl, pred_tbl) %>%
  rowwise() %>%
  mutate(order_key = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(order_key))

kable(
  combined_tbl,
  format = "latex",
  booktabs = TRUE,
  caption = "replace meee :(",
  col.names = c(
    "Type",
    "int",
    "ext",
    "semi_nat",
    "Sum across types"
  ),
  align = c("l", "r", "r", "r", "r")
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 12
  ) %>%
  pack_rows(
    "A) Rank aggregation by band",
    start_row = 1,
    end_row = nrow(bands_tbl),
    bold = TRUE
  ) %>%
  pack_rows(
    "B) Rank aggregation by predictor",
    start_row = nrow(bands_tbl) + 1,
    end_row = nrow(combined_tbl),
    bold = TRUE
  )
