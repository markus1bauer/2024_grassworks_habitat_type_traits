#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Show figure of seed mass
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-05-14



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
    esy4 = fct_relevel(esy4, "R", "R22", "R1A"),
    esy4 = fct_recode(
      esy4, "Dry grassland\nR1A" = "R1A", "Hay meadow\nR22" = "R22",
      "Undefined\nR" = "R"
    ),
    cwm.abu.seedmass = cwm.abu.seedmass * 1000
  ) %>%
  rename(y = cwm.abu.seedmass)

### * Model ####
load(file = here("outputs", "models", "model_seedmass_esy4_2.Rdata"))
m <- m2
m@call



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data <- sites %>%
  group_by(esy4) %>%
  summarize(mean = mean(y), sd = sd(y, na.rm = TRUE), median = median(y))

graph_c <- ggplot() +
  geom_quasirandom(
    data = sites,
    aes(x = esy4, y = y, color = esy4),
    alpha = .2, shape = 16, size = 1
  ) +
  geom_hline(
    yintercept = 1.21, linetype = "solid", color = "grey70", size = .5
    ) +
  geom_hline(
    yintercept = 1.47+1.159, linetype = "dashed", color = "grey70", size = .5
    ) +
  geom_hline(
    yintercept = 1.47-1.159, linetype = "dashed", color = "grey70", size = .5
    ) +
  geom_errorbar(
    data = data,
    aes(x = esy4, ymin = mean-sd, ymax = mean+sd, color = esy4),
    width = 0.0, linewidth = 0.4
  ) +
  geom_point(
    data = data,
    aes(x = esy4, y = median, color = esy4),
    size = 2
  ) +
    annotate("text", label = "n.s.", y = 9, x = 3.4) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    scale_fill_manual(
      values = c(
        "Undefined\nR" = "#440154",
        "Hay meadow\nR22" = "#21918c",
        "Dry grassland\nR1A" = "orange"
      ), guide = "none"
    ) +
    scale_color_manual(
      values = c(
        "Undefined\nR" = "#440154",
        "Hay meadow\nR22" = "#21918c",
        "Dry grassland\nR1A" = "orange"
      ), guide = "none"
    ) +
    labs(
      x = "",
      y = expression(CWM ~ seed ~ mass ~ "[" * mg * "]"),
      title = "Seed mass",
      tag = "C"
    ) +
    theme_mb(); graph_c


#### * Save ####

ggsave(
  here("outputs", "figures", "figure_2_seedmass_300dpi_9x6cm.tiff"),
  dpi = 300, width = 9, height = 6, units = "cm"
)
