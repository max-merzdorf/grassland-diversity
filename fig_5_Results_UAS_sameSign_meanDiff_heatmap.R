library(dplyr)
library(tidyverse)
library(xtable)
library(ggplot2)
library(terra)
library(sf)
uas_slopes <- read.csv("./results/ACTUAL_RESULTS/UAS_slopes_FINAL.csv")

u <- uas_slopes %>%
  select(c(predictor, agg,
           starts_with("prop"), starts_with("mean"))) %>%
  group_by(predictor, agg) %>%
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

#########
# visualise UAS slopes same sign
u_ss <- u %>%
  select(c(predictor, agg, ranksum_ss_X, starts_with("rank_ss"))) %>%
  pivot_longer(-c(predictor, agg, ranksum_ss_X))

p_ss <- ggplot(u_ss, aes(x = name, y = reorder(predictor, ranksum_ss_X, FUN = mean),
              fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Ranked predictors for same sign estimation of species variables",
       subtitle = "on UAS data, ordered by sum of ranks")

# visualise UAS slopes mean diff
u_mean <- u %>%
  select(c(predictor, agg, ranksum_delta_X, starts_with("rank_delta"))) %>%
  pivot_longer(-c(predictor, agg, ranksum_delta_X))

ggplot(u_mean, aes(x = name, y = reorder(predictor, ranksum_delta_X, FUN = mean),
                 fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal()

# both in one?
u_both <- u %>%
  select(c(predictor, agg, starts_with("rank"))) %>%
  select(-c(ranksum_ss_X, ranksum)) %>%
  pivot_longer(-c(predictor, agg, ranksum_delta_X)) %>%
  # rename some stuff
  mutate(res = factor(
    case_when(
    agg == "agg1" ~ "3 cm",
    agg == "agg2" ~ "6 cm",
    agg == "agg4" ~ "12 cm"
              ), levels = c("3 cm", "6 cm", "12 cm")),
  name = str_remove(name, "^rank_"))

both <- ggplot(u_both, aes(x = name,
                           y = reorder(predictor, ranksum_delta_X, FUN = mean),
                           fill = value)) +
  geom_tile(color = "white", lwd = .5) +
  facet_wrap(~ res, nrow = 1) +
  scale_fill_viridis_c(name = "Rank avg.") +
  theme_light() +
  ylab("Predictor name") +
  xlab("Ranked target variables delta of slopes and TTD ranks") +
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 20)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(title = "UAS predictors ranked by sum of average scores, grouped by resolution")
both
ggsave(filename = "./images/5_Results_UAS_sameSign_meanDiff_ranks.png", plot = both,
       width = 3000, height = 2000, units = "px", dpi = 300)
write.csv(u , "./result_tables/Fig_5_2_data.csv", row.names = F)
