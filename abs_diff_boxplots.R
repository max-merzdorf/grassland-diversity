s10 <- read.table("./results/species_richness/site10_species_richness_agg1_slopes.xml",sep=",")
s10a2<- read.table("./results/species_richness/site10_species_richness_agg2_slopes.xml",sep=",")
s10a4 <- read.table("./results/species_richness/site10_species_richness_agg4_slopes.xml",sep=",")

boxplot(s10$slopes_diff,
        min = min(c(s10$slopes_diff, s10a2$slopes_diff, s10a4$slopes_diff)),
        max = max(c(s10$slopes_diff, s10a2$slopes_diff, s10a4$slopes_diff)),
        main = "S10 Aggregate 0 slope deltas")

boxplot(s10a2$slopes_diff,
        min = min(c(s10$slopes_diff, s10a2$slopes_diff, s10a4$slopes_diff)),
        max = max(c(s10$slopes_diff, s10a2$slopes_diff, s10a4$slopes_diff)),
        main = "S10 Aggregate 2 slope deltas")

boxplot(s10a4$slopes_diff,
        min = min(c(s10$slopes_diff, s10a2$slopes_diff, s10a4$slopes_diff)),
        max = max(c(s10$slopes_diff, s10a2$slopes_diff, s10a4$slopes_diff)),
        main = "S10 Aggregate 4 slope deltas")
