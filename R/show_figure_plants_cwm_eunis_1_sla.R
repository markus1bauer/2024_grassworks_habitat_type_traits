#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types 
# Show figure of CWM SLA ~ EUNIS
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2024-08-12



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

# base::load(file = here("outputs", "models", "model_plants_nmds_presence.Rdata"))

sites <- read_csv(
  here("data", "processed", "sites_processed_environment_nms_20240812.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = "f",
    region = col_factor(levels = c("north", "centre", "south"), ordered = TRUE),
    site.type = col_factor(
      levels = c("positive", "restored", "negative"), ordered = TRUE
    ),
    freq.mow = "f",
    fertilized = "f"
  )
) %>%
  select(
    id.plot, longitude, latitude, region, eco.id, eco.name, obs.year,
    esy4, esy16,
    site.type, history, hydrology, land.use.hist, fertilized, freq.mow,
    cwm.abu.sla, cwm.abu.height, cwm.abu.seedmass,
    cwm.pres.sla, cwm.pres.height, cwm.pres.seedmass,
    fdis.abu.sla, fdis.abu.height, fdis.abu.seedmass,
    fric.abu.sla, fric.abu.height, fric.abu.seedmass
  ) %>%
  group_by(esy16) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 20) %>%
  mutate(
    esy16 = fct_relevel(esy16, "R", "R22", "R1A"),
    esy16 = fct_recode(
      esy16, "Dry grassland\nR1A" = "R1A", "Hay meadow\nR22" = "R22",
      "Undefined\nR" = "R"
    )
  )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



graph_a <- ggplot(sites, aes(y = cwm.abu.sla, x = esy16)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  labs(y = "CWM SLA (abu) [cm²/g]", title = "SLA", tag = "A") +
  theme_mb() +
  theme(axis.title.x = element_blank()); graph_a

#### * Save ####

ggsave(
  here(
    "outputs", "figures", "plants_eunis_cwm",
    "figure_cwm_eunis_1_sla_300dpi_10x10cm.tiff"
  ),
  dpi = 300, width = 10, height = 10, units = "cm"
)
