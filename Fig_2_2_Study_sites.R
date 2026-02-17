library(tidyverse)
library(sf)

sites <- st_read("./data/_vector/NOTREE_sites.gpkg") %>%
  st_transform("EPSG:32632")
sf_use_s2(FALSE) 

s <- sites %>%
  mutate(area_m2 = as.numeric(st_area(geom))) %>%
  mutate(Cat = factor(case_when(
    Cat == "intensive" ~ "int",
    Cat == "extensive" ~ "ext",
    Cat == "semi_nat" ~ "semi_nat"
  ), levels = c("int", "ext", "semi_nat"))) %>%
  select(-geom) %>%
  mutate(siteID = as.numeric(Nummer)) %>%
  select(-Nummer)

env_params <- read.table("./data/_tables/_analysisready_env_params.csv")

e <- env_params %>%
  filter(!siteID %in% c(seq(16, 20), seq(24, 27))) %>%
  mutate(Type = factor(case_when(
    Type == "int_prox" ~ "int",
    Type == "int_cit" ~ "int",
    Type == "ext_prox" ~ "ext",
    .default = Type
  ), levels = c("int", "ext", "semi_nat"))) %>%
  select(siteID, Type, species_on_run) %>%
  filter(!is.na(species_on_run),
         !is.na(Type)) %>%
  pivot_longer(cols = c(species_on_run))

combo <- e %>%
  left_join(y = s, by = "siteID") %>%
  select(-c(Type, name)) %>%
  pivot_longer(c("value", "area_m2"), names_to = "measure", values_to = "y")

plotty <- ggplot(combo, aes(x = Cat, y = y, fill = Cat)) +
  geom_violin() +
  facet_wrap(~ measure, scales = "free_y",
             labeller = labeller(measure = c(value = "No. of species",
                                             area_m2 = "Area [m²]"))) +
  geom_boxplot(width = 0.1, color = "black", fill = "grey") +
  scale_fill_viridis_d() +
  theme_light() +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey50", color = "grey50")
  ) +
  guides(fill = guide_legend(title = "Type")) +
  labs(x = "Site management type", y = NULL)
  
ggsave("./images/2_StudyArea_sites_area_and_no_of_species.png", plot = plotty,
       width = 3000, height = 1000, units = "px", dpi = 300)

s %>%
  st_drop_geometry() %>%
  group_by(Cat) %>%
  summarise(median(area_m2))


allspecies <- read.table("./data/_tables/_analysisready_allspecies.csv")

semispecies <- allspecies %>%
  filter(Type == "semi_nat")
