library(ggplot2)
library(tidyverse)

# Funtion by Jeromy Anglim via stackoverflow
# (https://jeromyanglim.tumblr.com/post/50228877196/round-numbers-in-data-frame-that-contains-non)
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

###### CSV RESULTS OUTPUT #######

# put all planet results in one big table:
files <- list.files("./results/metric_tables/", pattern="Planet", full.names = T)
combined <- do.call(
  rbind,
  lapply(files, function(f) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    id <- sub("Planet_", "", basename(f))
    id <- sub("_metrics.csv", "", id)
    df$site <- id
    df
  })
)

# add "month" column for easier results
combined <- combined %>%
  mutate(month = case_when(
    date == 20240408 ~ "April",
    date == 20240513 ~ "May",
    date == 20240709 ~ "July",
    date == 20240812 ~ "August"
  ))

#write.csv(round_df(combined, 4), "./results/Planet_texture_metric_results.csv", row.names = F)

# all UAS results into one csv file for github ---------------------------
uasfiles <- list.files("./results/metric_tables/", pattern="UAS", full.names = T)
uascombined <- do.call(
  rbind,
  lapply(uasfiles, function(f) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    site <- sub("UAS_", "", basename(f))
    site <- sub("_agg\\d{1}_metrics.csv", "", site)
    df$site <- site
    agg <- sub("UAS_site\\d{1,2}_", "", basename(f))
    agg <- sub("_metrics.csv", "", agg)
    df$agg <- agg
    df
  })
)

uascombined <- uascombined %>%
  mutate(month = case_when(
    date == 20240408 ~ "April",
    date == 20240523 ~ "May",
    date == 20240722 ~ "July",
    date == 20240812 ~ "August"
  ))

#write.csv(round_df(uascombined, 4), "./results/UAS_texture_metric_results.csv", row.names = F)


###### GROUPED VISUAL INSIGHTS #########
# Planet / UAS

# view metrics over time
ggplot(tl, aes(x = date, y = value)) +
  geom_line(aes(color = name))

# make graph for all results:

c <- read.csv("./results/Planet_texture_metric_results.csv", header = T, sep = ",", dec=".", )
cl <- pivot_longer(c, cols=starts_with("B"))
cl <- cl %>%
  mutate(bandgroup = case_when(
    startsWith(name, "B1")  ~ "Band 1",
    startsWith(name, "B2")  ~ "Band 2",
    startsWith(name, "B3")  ~ "Band 3",
    startsWith(name, "B4")  ~ "Band 4"
  ))

cl <- cl%>%
  mutate(predictorgroup = case_when(
    str_detect(name, "cv") ~ "Coef. of Variation",
    str_detect(name, "window") ~ "Rao's Quad. Entropy",
    str_detect(name, "glcm_entropy") ~ "GLCM Orderliness",
    str_detect(name, "glcm_ASM") ~ "GLCM Orderliness",
    str_detect(name, "glcm_contrast") ~ "GLCM Contrast group",
    str_detect(name, "glcm_dissimilarity") ~ "GLCM Contrast group",
    str_detect(name, "glcm_homogeneity") ~ "GLCM Contrast group"
  ))

#### Mann Whitney Test


#### Violin plots
p <- ggplot(cl, aes(x = site, y = value, group = predictorgroup)) +
  geom_violin() +
  scale_colour_viridis_d()
p

ggplot(cl, aes(x = bandgroup, y = value)) +
  geom_violin(trim = FALSE, fill = "grey80") +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  theme_bw() +
  labs(
    x = "Predictor group",
    y = "Value",
    title = "Values distribution per site"
  )

# get a shortened version of the long format data frame as example for LaTeX
cl_short <- cl[1:5,]
xtable(cl_short)
