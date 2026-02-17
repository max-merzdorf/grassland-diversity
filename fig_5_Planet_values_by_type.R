planet_predictors <- read.csv("./results/Planet_texture_metric_results.csv")
mgmt_type <- read.table("./data/_tables/_analysisready_env_params.csv")

# get site management type
mgmt_type <- mgmt_type %>%
  filter(!siteID %in% c(16:20, 24:27)) %>%
  filter(Col_run == 1) %>%
  select(c(siteID, Type)) %>%
  mutate(site = paste0("site", siteID)) %>%
  select(-siteID)
  
u <- planet_predictors %>%
  filter(!site %in% c("site16", "site17", "site18", "site19", "site20",
                      "site24", "site25", "site26", "site27")) %>%
  left_join(mgmt_type, by = "site") %>%
  select(c(starts_with("B1"), "SSD", "Type")) %>%
  rename_with(~ gsub("B1_", "", .x, fixed = TRUE)) %>%
  pivot_longer(cols = -c(Type)) %>%
  mutate(Type = factor(case_when(
    Type == "int_prox" ~ "int",
    Type == "int_cit" ~ "int",
    Type == "ext_prox" ~ "ext",
    .default = Type
  ), levels = c("int", "ext", "semi_nat")))

p <- ggplot(u, aes(x = name, y = value, fill = as.factor(Type))) +
  geom_boxplot() +
  scale_fill_viridis_d() +
  labs(title = "PlanetScope predictor values grouped by site management type") +
  guides(fill = guide_legend(title = "Type")) +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  xlab("Predictor") +
  ylab("Value") +
  theme_light()
p
ggsave("./images/5_Planet_values_by_type.png", plot = p, width = 3000, height = 1500, units = "px", dpi = 300)

source("./f_calc_means.R")

zoomed <- planet_predictors %>%
  filter(!site %in% c("site16", "site17", "site18", "site19", "site20",
                      "site24", "site25", "site26", "site27")) %>%
  left_join(mgmt_type, by = "site") %>%
  select(c(starts_with("B1"), "SSD", "Type")) %>%
  rename_with(~ gsub("B1_", "", .x, fixed = TRUE)) %>%
  select(-contains("RaoQ")) %>%
  select(-contains("contrast")) %>%
  pivot_longer(cols = -c(Type)) %>%
  mutate(Type = factor(case_when(
    Type == "int_prox" ~ "int",
    Type == "int_cit" ~ "int",
    Type == "ext_prox" ~ "ext",
    .default = Type
  ), levels = c("int", "ext", "semi_nat")))

u %>%
  filter(str_detect(name, "SSD")) %>%
  group_by(name, Type) %>%
  summarise(mean(value))
