#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Show figure of CWM Canopy height ~ EUNIS
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-04-29



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
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

#### Load sites data and model ###

base::load(file = here("outputs", "models", "model_height_esy16_1.Rdata"))

sites <- read_csv(
  here("data", "processed", "data_processed_sites_esy16.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = "f",
    region = col_factor(levels = c("north", "centre", "south"), ordered = TRUE),
    site.type = col_factor(
      levels = c("positive", "restored", "negative"), ordered = TRUE
    )
  )
) %>%
  filter(esy16 %in% c("R", "R22", "R1A") & !(eco.id == 647)) %>%
  mutate(
    esy16 = fct_relevel(esy16, "R", "R22", "R1A"),
    esy16 = fct_recode(
      esy16, "Dry grassland\nR1A" = "R1A", "Hay meadow\nR22" = "R22",
      "Undefined\nR" = "R"
    )
  ) %>%
  mutate(y = cwm.abu.height.mean) %>%
  filter(y < 1) # see section Outliers: Exclude site N_DAM (more or less only the tall grass Arrhenatherum elatius germinated at this young restoration site)




#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



graph_b <- ggplot(sites, aes(y = y, x = esy16, fill = esy16)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(alpha = .5) +
  facet_grid(~ eco.id) +
  scale_fill_viridis_d(guide = "none") +
  labs(y = "CWM Height (abu) [m]", title = "Canopy height", tag = "B") +
  theme_mb() +
  theme(axis.title.x = element_blank()); graph_b

graph_b <- ggplot(sites, aes(y = y, x = eco.id, fill = eco.id)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(alpha = .5) +
  facet_grid(~ esy16) +
  scale_fill_viridis_d(guide = "none") +
  labs(y = "CWM Height (abu) [m]", title = "Canopy height", tag = "B") +
  theme_mb() +
  theme(axis.title.x = element_blank()); graph_b

#### * Save ####

ggsave(
  here("outputs", "figures", "figure_2_height_300dpi_10x10cm.tiff"),
  dpi = 300, width = 10, height = 10, units = "cm"
)
