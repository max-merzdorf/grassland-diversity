library(ggplot2)
library(tidyverse)

agg_factor <- 1
nlevels <- 32

s8 <- read.table(paste0("./results/species_richness/site8_species_richness_n",nlevels,"_agg",agg_factor,"_slopes.xml"), sep=",")
s10 <- read.table(paste0("./results/species_richness/site10_species_richness_n",nlevels,"_agg",agg_factor,"_slopes.xml"), sep=",")
s14 <- read.table(paste0("./results/species_richness/site14_species_richness_n",nlevels,"_agg",agg_factor,"_slopes.xml"), sep=",")

rnames <- gsub("site8_","", rownames(s8))
rnames <- gsub(".time_intervals", "", rnames)

delta_slopes <- data.frame(s8$slopes_diff, s10$slopes_diff, s14$slopes_diff)
rownames(delta_slopes) <- rnames
d_slopes_ranked <- data.frame(lapply(delta_slopes, rank))
rownames(d_slopes_ranked) <- rnames
d_slopes_ranked$predictor <- rnames
df_long <- pivot_longer(d_slopes_ranked, cols = starts_with("s"),
                        values_to = "Rank",
                        names_to = "Site_deltaSlope")


# sum of ranks for performance index
d_slopes_ranked$sum <- rowSums(d_slopes_ranked[, c("s8.slopes_diff", "s10.slopes_diff", "s14.slopes_diff")])
df_long <- df_long %>%
  left_join(d_slopes_ranked[, c("predictor", "sum")], by = "predictor") %>%
  mutate(predictor = reorder(predictor, sum))  # reorder by sum


png(filename=paste0("./images/graphs/glcm_predictor_performance_n",nlevels,"_agg",agg_factor,".jpg"),
    width = 1980,
    height=1080,
    res = 210)
# ggplot heatmap:
ggplot(df_long, aes(x = Site_deltaSlope, y = predictor, fill = Rank)) +
  geom_tile(color = "white") +
#  scale_fill_gradient(low = "blue", high = "red",
#                      name = "Rank",
#                      guide = guide_colorbar(reverse = TRUE)) +
  scale_fill_viridis_c(guide = guide_colorbar(reverse = TRUE),
                      direction = -1) +
  scale_y_discrete(limits = rev(levels(factor(df_long$predictor)))) +
  theme_minimal(base_size = 14) +
  labs(title = "Ranked Performance Across Sites",
       x = "Site (delta slopes)",
       y = "Predictor")
dev.off()
