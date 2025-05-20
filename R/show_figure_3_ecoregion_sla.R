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
    legend.position = "none",
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
  mutate(esy4 = fct_relevel(esy4, "R", "R22", "R1A")) %>%
  rename(y = cwm.abu.sla)

### * Model ####
load(file = here("outputs", "models", "model_sla_esy4_2.Rdata"))
m <- m2
m@call



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data_text <- tibble(
  y = c(340, 340, 340),
  eco.id = c("664", "654", "686"),
  label = c("", "", "Ecoregion ***"),
  esy4 = c("R", "R22", "R1A")
) %>%
  mutate(esy4 = fct_relevel(esy4, "R", "R22", "R1A"))

data <- sites %>%
  group_by(esy4, eco.id) %>%
  summarize(mean = mean(y), sd = sd(y, na.rm = TRUE))

graph_a <- ggplot() +
  geom_quasirandom(
    data = sites,
    aes(x = eco.id, y = y, color = eco.id),
    alpha = .2, shape = 16, size = 1
  ) +
  geom_errorbar(
    data = data,
    aes(x = eco.id, y = mean, ymin = mean-sd, ymax = mean+sd, color = eco.id),
    width = 0.0, linewidth = 0.4
  ) +
  geom_point(
    data = data,
    aes(x = eco.id, y = mean, color = eco.id),
    size = 2
  ) +
  geom_text(
    data = data_text,
    aes(x = eco.id, y = y, label = label, group = esy4),
    hjust = .8, size = 3.1
  ) +
  facet_grid(~ esy4) +
  scale_fill_viridis_d(guide = "none") +
  scale_y_continuous(limits = c(140, 340), breaks = seq(0, 400, 20)) +
  scale_color_manual(
    values = c(
      "664" = "#440154",
      "654" = "#21918c",
      "686" = "orange"
    ), guide = "none"
  ) +
  labs(
    y = expression(CWM ~ Specific ~ leaf ~ area ~ "[" * cm^2 ~ g^-1 * "]"),
    title = "Specific leaf area",
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
  here("outputs", "figures", "figure_3_ecoregion_sla_300dpi_9x6cm.tiff"),
  dpi = 300, width = 9, height = 6, units = "cm"
)
