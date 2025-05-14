#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types x Site type ####
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
  mutate(esy4 = fct_relevel(esy4, "R", "R22", "R1A")) %>%
  rename(y = cwm.abu.height) %>%
  filter(y < 1)

### * Model ####
load(file = here("outputs", "models", "model_height_esy4_2.Rdata"))
m <- m2
m@call



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data <- sites %>%
  group_by(esy4, site.type) %>%
  summarize(mean = mean(y), sd = sd(y, na.rm = TRUE))

data_line <- data %>%
  filter(site.type == "positive")

data_text <- tibble(
  y = c(1, 1, 1, .93),
  site.type = c("positive", "restored", "negative", "negative"),
  label = c("", "", "Site type **", "Interaction n.s."),
  esy4 = c("R", "R22", "R1A", "R1A")
) %>%
  mutate(esy4 = fct_relevel(esy4, "R", "R22", "R1A"))

graph_b <- ggplot() +
  geom_quasirandom(
    data = sites,
    aes(x = site.type, y = y, color = site.type),
    alpha = .2, shape = 16, size = 1
  ) +
  geom_hline(
    data = data_line,
    aes(yintercept = mean),
    linetype = "solid", color = "grey70"
  ) +
  geom_hline(
    data = data_line,
    aes(yintercept = mean+sd),
    linetype = "dashed", color = "grey70"
  ) +
  geom_hline(
    data = data_line,
    aes(yintercept = mean-sd),
    linetype = "dashed", color = "grey70"
  ) +
  geom_errorbar(
    data = data,
    aes(x = site.type, y = mean, ymin = mean-sd, ymax = mean+sd, color = site.type),
    width = 0.0, linewidth = 0.4
  ) +
  geom_point(
    data = data,
    aes(x = site.type, y = mean, color = site.type),
    size = 2
  ) +
  geom_text(
    data = data_text,
    aes(x = site.type, y = y, label = label, group = esy4),
    hjust = .8, size = 3.1
  ) +
  facet_grid(~ esy4) +
  scale_color_manual(
    values = c(
      "positive" = "#21918c",
      "restored" = "orange",
      "negative" = "#440154"
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
    axis.line.x = element_blank(),
    strip.text = element_blank()
  ); graph_b

#### * Save ####

ggsave(
  here("outputs", "figures", "figure_4_site.type_height_300dpi_9x6cm.tiff"),
  dpi = 300, width = 9, height = 6, units = "cm"
)
