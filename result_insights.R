library(ggplot2)
library(tidyverse)
t <- read.csv("./results/metric_tables/Planet_site10_metrics.csv")
tl <- pivot_longer(t, cols = -date)

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

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

write.csv(round_df(combined, 4), "./Planet_texture_metric_results.csv")

cl <- pivot_longer(combined, cols = -date)

ggplot(combined, aes(y = ))

# all UAS results into one csv file for github
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
write.csv(round_df(uascombined, 4), "./UAS_texture_metric_results.csv")


###### GROUPED VISUAL INSIGHTS #########

# view metrics over time
ggplot(tl, aes(x = date, y = value)) +
  geom_line(aes(color = name))

# add date as row to UAS 
uaslist <- list.files(path="./results/metric_tables/", pattern = "UAS",full.names = T)
for(i in uaslist){
  t <- read.csv(i)
  t$date <- rownames(t)
  write.csv(x = t, file = i, sep = ",", dec = ".", row.names = F)
}

# change colnames for UAS metrics from "B_1" to "B1" to match UAS tables
uaslist <- list.files(path="./results/metric_tables/", pattern = "UAS",full.names = T)
for(i in uaslist){
  t <- read.csv(i)
  colnames(t) <- sub("_", "", colnames(t))
  write.csv(x = t, file = i, row.names = F) # doesnt have rownames anymore but explicit > implicit
}

# make graph for all results:

reslist <- list.files(path="./results/metric_tables/", pattern = ".csv",full.names = T)

# group by band
for (i in reslist){
  t <- read.csv(i)
  tl <- pivot_longer(t, cols=-date)
  
  # group column
  tl <- tl %>%
    mutate(group = case_when(
      startsWith(name, "B1")  ~ "Band 1",
      startsWith(name, "B2")  ~ "Band 2",
      startsWith(name, "B3")  ~ "Band 3",
      startsWith(name, "B4")  ~ "Band 4"
    ))
  
  resname <- strsplit(i, split = "/")[[1]][4]
  p <- ggplot(tl, aes(x = as.Date(as.character(tl$date), format = "%Y%m%d"), y= value, group = name, color = group)) +
    geom_line() +
    labs(title = resname) +
    xlab("date") +
    scale_colour_viridis_d()
  ggsave(filename = paste0("./results/metric_tables/visualized/", resname, ".png"),
         p, width = 3000, height = 1000, units = "px", dpi = 300)
  
}

# group by predictor
for (i in reslist){
  t <- read.csv(i)
  tl <- pivot_longer(t, cols=-date)
  
  # group column
  tl_groups <- tl %>%
    mutate(group = case_when(
      str_detect(name, "cv") ~ "Coef. of Variation",
      str_detect(name, "window") ~ "Rao's Quad. Entropy",
      str_detect(name, "glcm_entropy") ~ "GLCM Orderliness",
      str_detect(name, "glcm_ASM") ~ "GLCM Orderliness",
      str_detect(name, "glcm_contrast") ~ "GLCM Contrast group",
      str_detect(name, "glcm_dissimilarity") ~ "GLCM Contrast group",
      str_detect(name, "glcm_homogeneity") ~ "GLCM Contrast group"
    ))
  
  resname <- strsplit(i, split = "/")[[1]][4]
  p <- ggplot(tl, aes(x = as.Date(as.character(tl$date), format = "%Y%m%d"), y= value, group = name, color = group)) +
    geom_line() +
    labs(title = resname) +
    xlab("date") +
    scale_colour_viridis_d()
  ggsave(filename = paste0("./results/metric_tables/visualized/predictor_grouped_", resname, ".png"),
         p, width = 4000, height = 3000, units = "px", dpi = 300)
  
}

