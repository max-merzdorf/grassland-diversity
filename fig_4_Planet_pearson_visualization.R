planet_metrics <- read.csv("./results/Planet_texture_metric_results.csv")
env_params <- read.table("./data/_tables/_analysisready_env_params.csv")

planet_metrics <- planet_metrics %>%
  filter(!site %in% c("site16", "site17", "site18", "site19", "site20",
                      "site24", "site25", "site26", "site27"))

# [Chapter 2] How many sites per type remain?
gjort <- env_params %>%
  filter(Col_run == 1) %>%
  select(siteID, Type) %>%
  filter(!siteID %in% c(16:20, 24:27)) %>%
  mutate(Type = factor(case_when(
    Type == "int_prox" ~ "int",
    Type == "int_cit" ~ "int",
    Type == "ext_prox" ~ "ext",
    Type == "ext_cit" ~ "ext",
    .default = Type
  ), levels = c("int", "ext", "semi_nat"))) %>%
  group_by(Type) %>%
  count()

e <- env_params %>%
  mutate(month = case_when(
    Col_run == 1 ~ "April",
    Col_run == 2 ~ "June",
    Col_run == 3 ~ "July",
    Col_run == 4 ~ "August",
    Col_run == 5 ~ "September",
  )) %>%
  filter(month %in% c("April", "July", "August")) %>%
  mutate(site = paste0("site", siteID)) %>% # same colname as planet_metrics for grouping -> can drop siteID
  select(-c(Col_run, all_vegetation, note, Sum.all, species_on_run, siteID, Sum.vegetation)) %>%
  mutate(Type = factor(case_when(
    Type == "int_prox" ~ "int",
    Type == "int_cit" ~ "int",
    Type == "ext_prox" ~ "ext",
    Type == "ext_cit" ~ "ext",
    .default = Type
  ), levels = c("int", "ext", "semi_nat")))

e_planet <- e %>%
  filter(!site %in% c("site16", "site17", "site18", "site19", "site20",
                      "site24", "site25", "site26", "site27")) %>%
  left_join(planet_metrics, by = c("site", "month"))

ivs <- c("veg_height_cm", "grasses", "herbs", "moss", "litter", "bare.soil")
dvs <- e_planet %>%
  select(-c(date, site, month, Type)) %>%
  names %>%
  setdiff(ivs)

# group by Type
planet_long <- e_planet %>%
  pivot_longer(cols = all_of(dvs),
    names_to = "dependent_var",
    values_to = "dependent_value") %>%
  pivot_longer(cols = all_of(ivs),
    names_to = "independent_var",
    values_to = "independent_value")

huaa <- planet_long %>%
  filter(dependent_var == "B1_glcm_homogeneity") %>%
  filter(independent_var == "veg_height_cm")

# scatterplot?
peloty <- ggplot(huaa, aes(x = dependent_value, y = independent_value, color = Type)) +
  geom_point() +
  scale_colour_viridis_d() +
  theme_light() +
  labs(x = "B1_glcm_homogeneity value", y = "Vegetation height [cm]") +
  geom_smooth(method = "lm")

ggsave("./images/fig_4_PlanetScope_Pearson_Example.png", peloty,
       width = 3000, height = 1000, dpi = 300, units = "px")

planet_cor_results <- planet_long %>%
  group_by(Type, independent_var, dependent_var) %>%
  summarise(
    test = list(
      cor.test(independent_value, dependent_value, method = "pearson")
    ),
    .groups = "drop"
  ) %>%
  mutate(
    pearson_r = sapply(test, \(x) x$estimate),
    p_value   = sapply(test, \(x) x$p.value),
    n         = sapply(test, \(x) x$parameter + 2)
  ) %>%
  select(-test)
