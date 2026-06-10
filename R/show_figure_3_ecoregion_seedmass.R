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

data_model <- ggemmeans(
  m, terms = c("eco.id", "esy4"), back.transform = TRUE, ci_level = .95
) %>%
  as_tibble() %>%
  mutate(
    predicted = predicted * 1000,
    conf.low = conf.low * 1000,
    conf.high = conf.high * 1000,
    x = fct_recode(
      x, "North" = "664", "Centre" = "654", "South" = "686"
      ),
    x = fct_relevel(x, "North", "Centre", "South"),
    group = fct_recode(
      group, "Hay\nmeadow" = "R22", "Calcareous\ngrassland" = "R1A"
    )
  )

data_text <- tibble(
  y = c(1.07, .8),
  x = c("South", "South"),
  label = c("Ecoregion **", "Interaction *"),
  group = c("Calcareous\ngrassland", "Calcareous\ngrassland")
) %>%
  mutate(group = fct_relevel(group, "Hay\nmeadow", "Calcareous\ngrassland"))

### * Plot ####

graph <- ggplot() +
  geom_errorbar(
    data = data_model,
    aes(x = x, ymin = conf.low, ymax = conf.high, color = x),
    width = 0.0, linewidth = 0.8
  ) +
  geom_point(
    data = data_model,
    aes(x = x, y = predicted, color = x),
    size = 2.5
  ) +
  geom_text(
    data = data_text,
    aes(x = x, y = y, label = label, group = group),
    hjust = .8, size = 3.1
  ) +
  facet_grid(~ group) +
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
  scale_y_continuous(limits = c(0.7, 2.8), breaks = seq(0, 6, .5)) +
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
    "outputs", "figures", "figure_3_ecoregion_seedmass_cwm_300dpi_8x6cm.tiff"
    ),
  dpi = 300, width = 8, height = 6, units = "cm"
)

graph_c <- graph +
  theme(strip.text = element_blank())
