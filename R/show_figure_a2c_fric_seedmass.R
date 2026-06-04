#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# Functional diversity of habitats ####
# Show figure A2 functional richness Seed mass
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2026-06-03



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d", "graph_e",
                          "graph_f", "graph_g", "graph_h", "graph_i")))

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
  rename(y = fric.abu.seedmass)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



graph <- ggplot() +
  geom_quasirandom(
    data = sites,
    aes(x = site.type, y = y, color = site.type),
    alpha = .2, shape = 16, size = 1
  ) +
  geom_boxplot(
    data = sites, aes(x = site.type, y = y, fill = site.type),
    alpha = .5
  ) +
  facet_grid(~ hydrology) +
  scale_color_manual(
    values = c(
      "−" = "#440154",
      "restored" = "#2a788e",
      "+" = "#7ad151"
    ), guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      "−" = "#440154",
      "restored" = "#2a788e",
      "+" = "#7ad151"
    ), guide = "none"
  ) +
  scale_y_continuous(limits = c(0, 210), breaks = seq(0, 210, 50)) +
  labs(
    x = "",
    title = "Seed mass",
    y = expression("FRic Seed mass"),
    tag = "C"
  ) +
  theme_mb(); graph

#### * Save ####

ggsave(
  here("outputs", "figures", "figure_a2c_fric_seedmass_300dpi_9x6cm.tiff"),
  dpi = 300, width = 9, height = 6, units = "cm"
)

graph_c <- graph +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()
  )
