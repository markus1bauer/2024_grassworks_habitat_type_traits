#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types x Ecoregion ####
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



graph_a <- ggplot(sites, aes(y = y, x = eco.id, fill = eco.id)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(alpha = .5) +
  facet_grid(~ esy4) +
  scale_fill_viridis_d(guide = "none") +
  scale_y_continuous(limits = c(140, 340), breaks = seq(0, 400, 20)) +
  labs(
    y = expression(CWM ~ Specific ~ leaf ~ area ~ "[" * cm^2 ~ g^-1 * "]"),
    title = "SLA",
    tag = "A",
    x = ""
    ) +
  theme_mb() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank()
  ); graph_a



#### * Save ####

ggsave(
  here("outputs", "figures", "figure_3_ecoregion_sla_300dpi_15x8cm.tiff"),
  dpi = 300, width = 15, height = 8, units = "cm"
)
