#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types x Ecoregion ####
# Show figure of seed mass
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-05-05



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
  here("data", "processed", "data_processed_sites_esy4.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = col_factor(levels = c("664", "654", "686"), ordered = TRUE),
    site.type = col_factor(
      levels = c("positive", "restored", "negative"), ordered = TRUE
    ),
    fertilized = "f",
    freq.mow = "f",
    obs.year = "f"
  )
) %>%
  mutate(
    esy4 = fct_relevel(esy4, "R", "R22", "R1A")#,
    # esy4 = fct_recode(
    #   esy4, "Dry grassland\nR1A" = "R1A", "Hay meadow\nR22" = "R22",
    #   "Undefined\nR" = "R"
    # )
  ) %>%
  rename(y = cwm.abu.seedmass)

### * Model ####
load(file = here("outputs", "models", "model_seedmass_esy4_1.Rdata"))
m <- m1
m@call



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data_model <- ggemmeans(
  m, terms = c("eco.id", "esy4"), back.transform = TRUE, ci_level = .95
  ) %>%
  filter(!(group == "R1A" & x == "664")) #%>%
  # mutate(
  #   x = fct_recode(
  #     x,
  #     "Central European\nmixed forests" = "654",
  #     "European Atlantic\nmixed forests" = "664",
  #     "Western European\nbroadleaf forests" = "686"
  #   )
  # )

data <- sites %>%
  rename(predicted = y, x = eco.id, group = esy4)

data_text <- tibble(
  y = c(0.005, 0.0046),
  x = rep(c(2.6), 2),
  label = c("Ecoregion ***", "Interaction n.s."),
  group = c("R1A", "R1A")
  ) %>%
  mutate(group = fct_relevel(group, "R", "R22", "R1A"))

(graph_c <- ggplot() +
    geom_quasirandom(
      data = data,
      aes(x = x, y = predicted, color = x),
      dodge.width = .6, size = 1, shape = 16, alpha = .4
    ) +
    geom_errorbar(
      data = data_model,
      aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, color = x),
      width = 0.0, linewidth = 0.4
    ) +
    geom_point(
      data = data_model,
      aes(x = x, y = predicted, color = x),
      size = 2
    ) +
    geom_text(
      data = data_text,
      aes(x = x, y = y, label = label, group = group)
      ) +
    facet_grid(~ group) +
    scale_y_continuous(limits = c(0, .005), breaks = seq(0, 0.1, 0.001)) +
    scale_color_manual(
      values = c(
        "664" = "#440154",
        "654" = "#21918c",
        "686" = "orange"
      ), guide = "none"
    ) +
    scale_y_continuous(limits = c(0, .005), breaks = seq(0, 0.1, 0.001)) +
    labs(
      x = "Ecoregion",
      y = expression( CWM ~ Seed ~ mass ~ "[" * g * "]"),
      title = "Seed mass",
      tag = "C"
    ) +
    theme_mb() +
    theme(strip.text = element_blank()))

#### * Save ####

ggsave(
  here("outputs", "figures", "figure_3_ecoregion_seedmass_300dpi_15x8cm.tiff"),
  dpi = 300, width = 15, height = 8, units = "cm"
)
