#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types x Site type ####
# Show figure of canopy height
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
  here("data", "processed", "data_processed_sites_refs.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = col_factor(levels = c("664", "654", "686"), ordered = TRUE),
    site.type = col_factor(
      levels = c("negative", "restored", "positive"), ordered = TRUE
    ),
    hydrology = col_factor(levels = c("moist", "fresh", "dry"), ordered = TRUE),
    obs.year = "f"
  )
) %>%
  mutate(
    site.type = fct_recode(site.type, "+" = "positive", "−" = "negative"),
    hydrology = fct_recode(hydrology, "mesic" = "fresh")
  ) %>%
  rename(y = cwm.abu.height) %>%
  filter(y < 1)

### * Model ####
load(file = here("outputs", "models", "model_height_refs_1.Rdata"))
m <- m1
m@call



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### * Preparation ####

data_model <- ggemmeans(
  m, terms = c("hydrology", "site.type"), back.transform = TRUE, ci_level = .95
) %>%
  as_tibble() %>%
  rename(hydrology = x) %>%
  mutate(
    group = fct_recode(group, "+" = "positive", "−" = "negative"),
    hydrology = fct_recode(hydrology, "mesic" = "fresh")
  )

data_text <- tibble(
  y = c(0.37, 0.32, 0.27),
  site.type = c("+", "+", "+"),
  label = c("Hydrology **", "Site type **", "Interaction n.s."),
  hydrology = c("moist", "moist", "moist")
)

### * Plot ####

graph <- ggplot() +
  geom_errorbar(
    data = data_model,
    aes(x = group, ymin = conf.low, ymax = conf.high, color = group),
    width = 0.0, linewidth = 0.8
  ) +
  geom_point(
    data = data_model,
    aes(x = group, y = predicted, color = group),
    size = 2.5
  ) +
  geom_text(
    data = data_text,
    aes(x = site.type, y = y, label = label, group = hydrology),
    hjust = .8, size = 3.1
  ) +
  facet_grid(~ fct_relevel(hydrology, "moist", "mesic", "dry")) +
  scale_color_manual(
    values = c(
      "+" = "#7ad151",
      "restored" = "#2a788e",
      "−" = "#440154"
    ), guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      "+" = "#7ad151",
      "restored" = "#2a788e",
      "−" = "#440154"
    ), guide = "none"
  ) +
  scale_y_continuous(limits = c(0.26, 0.7), breaks = seq(0, 1, .1)) +
  labs(
    x = "",
    y = expression(CWM ~ canopy ~ height ~ "[" * m * "]"),
    title = "Canopy height",
    tag = "B"
  ) +
  theme_mb(); graph

#### * Save ####

ggsave(
  here("outputs", "figures", "figure_4_site.type_height_cwm_300dpi_9x6cm.tiff"),
  dpi = 300, width = 9, height = 6, units = "cm"
)

graph_b <- graph +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    strip.text = element_blank()
  )