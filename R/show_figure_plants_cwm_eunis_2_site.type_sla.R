#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types 
# Show figure of SLA ~ EUNIS x site.type
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2024-09-05



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
  here("data", "raw", "sites_processed_environment_nms_20240813.csv"),
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
  mutate(
    n = n(),
    reference = if_else(
      site.type == "positive" | site.type == "negative", "reference", site.type
    )
  ) %>%
  ungroup() %>%
  filter(n > 20) %>%
  mutate(
    esy16 = fct_relevel(esy16, "R", "R22", "R1A"),
    esy16 = fct_recode(
      esy16, "Dry grassland\nR1A" = "R1A", "Hay meadow\nR22" = "R22",
      "Undefined\nR" = "R"
    )
  )

medians <- sites %>%
  group_by(reference, esy16) %>%
  summarise(median = median(cwm.abu.sla)) %>%
  filter(reference == "reference")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



p1 <- ggplot(sites, aes(y = cwm.abu.sla, x = reference, fill = reference)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(alpha = .5) +
  geom_hline(aes(yintercept = median), data = medians, linetype = "dashed") +
  facet_grid(~ esy16) +
  scale_fill_viridis_d(option = "inferno", guide = "none") +
  labs(y = "CWM SLA (abu) [cm²/g]", tag = "A") +
  theme_mb() +
  theme(axis.title.x = element_blank()); p1

#### * Save ####

ggsave(
  here(
    "outputs", "figures", "figure_cwm_eunis_2_site.type_sla_300dpi_17x10cm.tiff"
  ),
  dpi = 300, width = 17, height = 10, units = "cm"
)

(graph_a <- p1 +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank()
    ))