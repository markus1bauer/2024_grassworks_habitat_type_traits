#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types x Ecoregion ####
# Show figure 3C
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-05-22



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggeffects)
library(ggbeeswarm)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d", "m")))

### Functions ###
theme_mb <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 9, color = "black"),
    strip.text = element_text(size = 10),
    axis.text = element_text(angle = 0, hjust = 0.5, size = 9,
                             color = "black"),
    axis.title = element_text(angle = 0, hjust = 0.5, size = 9,
                              color = "black"),
    axis.line = element_line(),
    legend.key = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
}

#### Load data ###
sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = "f",
    site.type = col_factor(
      levels = c("positive", "restored", "negative"), ordered = TRUE
    ),
    obs.year = "f"
  )
) %>%
  mutate(
    esy4 = fct_recode(
      esy4, "Hay meadow" = "R22", "Calcareous\ngrassland" = "R1A"
      ),
    esy4 = fct_relevel(esy4, "Hay meadow", "Calcareous\ngrassland"),
    eco.id = fct_recode(
      eco.id, "North" = "664", "Centre" = "654", "South" = "686"
      ),
    eco.id = fct_relevel(eco.id, "North", "Centre", "South"),
    cwm.abu.seedmass = cwm.abu.seedmass * 1000
  ) %>%
  rename(y = cwm.abu.seedmass)

### * Model ####
load(file = here("outputs", "models", "model_seedmass_cwm_1.Rdata"))
m <- m1
m@call



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### * Preparation ####

data_summary <- sites %>%
  group_by(esy4) %>%
  summarize(median = median(y), sd = sd(y, na.rm = TRUE))

data_model <- ggemmeans(
  m, terms = c("esy4", "eco.id"), back.transform = TRUE, ci_level = .95
) %>%
  as_tibble() %>%
  rename(esy4 = x) %>%
  mutate(
    predicted = predicted * 1000,
    conf.low = conf.low * 1000,
    conf.high = conf.high * 1000,
    group = fct_recode(group, "North" = "664", "Centre" = "654", "South" = "686"),
    group = fct_relevel(group, "North", "Centre", "South")
  )

data_text <- tibble(
  y = c(5.5, 4.9),
  eco.id = c("South", "South"),
  label = c("Ecoregion **", "Interaction *"),
  esy4 = c("Calcareous\ngrassland", "Calcareous\ngrassland")
) %>%
  mutate(esy4 = fct_relevel(esy4, "Hay meadow", "Calcareous\ngrassland"))

### * Plot ####

graph <- ggplot() +
  geom_quasirandom(
    data = sites,
    aes(x = eco.id, y = y, color = eco.id),
    alpha = .2, shape = 16, size = 1
  ) +
  geom_boxplot(
    data = sites, aes(x = eco.id, y = y, fill = eco.id),
    alpha = .5
  ) +
  geom_text(
    data = data_text,
    aes(x = eco.id, y = y, label = label, group = esy4),
    hjust = .8, size = 3.1
  ) +
  facet_grid(~ esy4) +
  scale_color_manual(
    values = c(
      "North" = "#414487",
      "Centre" = "#22a884",
      "South" = "#FFA500"
    ), guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      "North" = "#414487",
      "Centre" = "#22a884",
      "South" = "#FFA500"
    ), guide = "none"
  ) +
  scale_y_continuous(limits = c(0, 5.5), breaks = seq(0, 6, 1)) +
  labs(
    x = "Ecoregion",
    y = expression( CWM ~ Seed ~ mass ~ "[" * mg * "]"),
    title = "Seed mass",
    tag = "C"
  ) +
  theme_mb(); graph

#### * Save ####

ggsave(
  here(
    "outputs", "figures", "figure_3_ecoregion_seedmass_cwm_300dpi_7x6cm.tiff"
    ),
  dpi = 300, width = 7, height = 6, units = "cm"
)

graph_c <- graph +
  theme(strip.text = element_blank())
