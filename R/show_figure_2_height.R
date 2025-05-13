#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Show figure of canopy height
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
  rename(y = cwm.abu.height) %>%
  filter(y < 1)

### * Model ####
load(file = here("outputs", "models", "model_height_esy4_2.Rdata"))
m <- m2
m@call



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data_model <- ggemmeans(
  m, terms = "esy4", back.transform = FALSE, ci_level = .95
)

data <- sites %>%
  rename(predicted = y, x = esy4)

graph_b <- ggplot() +
  geom_violin(
    data = data,
    aes(x = x, y = predicted, fill = x),
    alpha = .4
  ) +
  geom_hline(yintercept = 0.4820903, linetype = "dashed") +
  geom_errorbar(
    data = data_model,
    aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, color = x),
    width = 0.0, linewidth = 0.4
  ) +
  geom_point(
    data = data_model,
    aes(x = x, y = predicted, color = x),
    size = 3
  ) +
  annotate("text", label = "a", y = 1, x = 1) +
  annotate("text", label = "a", y = 1, x = 2) +
  annotate("text", label = "b", y = 1, x = 3) +
  scale_fill_manual(
    values = c(
      "R" = "#440154",
      "R22" = "#21918c",
      "R1A" = "orange"
    ), guide = "none"
  ) +
  scale_color_manual(
    values = c(
      "R" = "#440154",
      "R22" = "#21918c",
      "R1A" = "orange"
    ), guide = "none"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) +
  labs(
    x = "",
    y = expression(CWM ~ canopy ~ height ~ "[" * m * "]"),
    title = "Canopy height",
    tag = "B"
  ) +
  theme_mb() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()
  ); graph_b

#### * Save ####

ggsave(
  here("outputs", "figures", "figure_2_height_300dpi_15x8cm.tiff"),
  dpi = 300, width = 15, height = 8, units = "cm"
)
