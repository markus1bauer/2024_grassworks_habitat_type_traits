#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types x Site type ####
# Show figure of specific leaf area
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
  filter(esy4 %in% c("R", "R22", "R1A") & !(eco.id == 647)) %>%
  mutate(
    esy4 = fct_relevel(esy4, "R", "R22", "R1A")#,
    # esy4 = fct_recode(
    #   esy4, "Dry grassland\nR1A" = "R1A", "Hay meadow\nR22" = "R22",
    #   "Undefined\nR" = "R"
    # )
  ) %>%
  rename(y = cwm.abu.sla)

### * Model ####
load(file = here("outputs", "models", "model_sla_esy4_2.Rdata"))
m <- m2
m@call



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data_model <- ggemmeans(
  m, terms = c("site.type", "esy4"), back.transform = FALSE, ci_level = .95
)

data <- sites %>%
  rename(predicted = y, x = site.type, group = esy4)

graph_a <- ggplot() +
    geom_quasirandom(
      data = data,
      aes(x = x, y = predicted, color = x),
      dodge.width = .6, size = 1, shape = 16, alpha = .3
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
    facet_grid(~ group) +
    scale_color_manual(
      values = c(
        "positive" = "#440154",
        "restored" = "#21918c",
        "negative" = "orange"
      ), guide = "none"
    ) +
    labs(
      x = "",
      y = expression(CWM ~ specific ~ leaf ~ area ~ "[" * cm^2 ~ g^-1 * "]"),
      title = "Specific leaf area",
      tag = "A"
    ) +
    theme_mb() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
    ); graph_a

#### * Save ####

ggsave(
  here("outputs", "figures", "figure_4_site.type_sla_300dpi_15x8cm.tiff"),
  dpi = 300, width = 15, height = 8, units = "cm"
)
