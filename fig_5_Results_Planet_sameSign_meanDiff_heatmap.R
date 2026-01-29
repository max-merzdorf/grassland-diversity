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
  mutate(ranksum = ranksum_delta_X + ranksum_ss_X)

u %>%
  select(prop_same_sign_shannon, rank_ss_S)
u %>%
  select(mean_diff_shannon, rank_delta_S)
# -> all works as intended!!

u_both <- u %>%
  select(c(predictor, Type, starts_with("rank"))) %>%
  select(-c(ranksum_ss_X, ranksum)) %>%
  pivot_longer(-c(predictor, Type, ranksum_delta_X)) %>%
  # rename some stuff
  mutate(Type = factor(case_when(
    Type == "int_prox" ~ "int",
    Type == "int_cit" ~ "int",
    Type == "ext_prox" ~ "ext",
    .default = Type
  ), levels = c("int", "ext", "semi_nat")),
    name = str_remove(name, "^rank_"))

both <- ggplot(u_both, aes(x = name,
                           y = reorder(predictor, ranksum_delta_X, FUN = mean),
                           fill = value)) +
  geom_tile(color = "white", lwd = .5) +
  facet_wrap(~ Type, nrow = 1) +
  scale_fill_viridis_c(name = "Rank avg.") +
  theme_light() +
  ylab("Predictor name") +
  xlab("Ranked target variables delta of slopes and same sign ranks, per resolution") +
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 20)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title = "PlanetScope predictors ranked by sum of average scores")
both

# sanity chekcs:
u %>%
  filter(predictor == "B2_RaoQ_w9") %>%
  select(ranksum_delta_X) %>%
  sum()
u %>%
  filter(predictor == "B2_glcm_homogeneity") %>%
  select(ranksum_delta_X) %>%
  sum()
# crazy

ggsave(filename = "./images/5_Results_Planet_sameSign_meanDiff_ranks.png", plot = both,
       width = 3000, height = 2000, units = "px", dpi = 300)
write.csv(u , "./result_tables/Fig_5_4_ata.csv", row.names = F)
