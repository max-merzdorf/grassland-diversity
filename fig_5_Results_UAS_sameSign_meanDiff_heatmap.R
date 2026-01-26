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
    rank_ss_S = rank(prop_same_sign_shannon, ties.method = "average"),
    rank_ss_T = rank(prop_same_sign_turnover, ties.method = "average"),
    rank_ss_R = rank(prop_same_sign_richness, ties.method = "average"),
    ranksum_ss_X = rank_ss_S + rank_ss_T + rank_ss_R
  ) %>%
  mutate(
    rank_delta_S = rank(mean_diff_shannon, ties.method = "average"),
    rank_delta_T = rank(mean_diff_turnover, ties.method = "average"),
    rank_delta_R = rank(mean_diff_richness, ties.method = "average"),
    ranksum_delta_X = rank_delta_S + rank_delta_T + rank_delta_R
  )

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
  mutate(ranksum = ranksum_delta_X + ranksum_ss_X) %>%
  select(-c(ranksum_delta_X, ranksum_ss_X)) %>%
  pivot_longer(-c(predictor, agg, ranksum)) %>%
  # rename some stuff
  mutate(res = factor(
    case_when(
    agg == "agg1" ~ "3 cm",
    agg == "agg2" ~ "6 cm",
    agg == "agg4" ~ "12 cm"
              ), levels = c("3 cm", "6 cm", "12 cm")),
  name = str_remove(name, "^rank_"))

both <- ggplot(u_both, aes(x = interaction(name, res, sep = " | "),
                           y = reorder(predictor, ranksum, FUN = mean),
                           fill = value)) +
  geom_tile(color = "white", lwd = .5) +
  scale_fill_viridis_c(name = "Rank avg.") +
  theme_light() +
  ylab("Predictor name") +
  xlab("Ranked target variables delta of slopes and same sign ranks, per resolution") +
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 20)) +
  scale_x_discrete(guide = guide_axis(n.dodge=2))
both
ggsave(filename = "./images/5_Results_UAS_sameSign_meanDiff_ranks.png", plot = both,
       width = 3000, height = 2000, units = "px", dpi = 300)
