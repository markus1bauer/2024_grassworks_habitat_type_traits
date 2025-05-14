#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types x Site type ####
# Show figure of seed mass
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
    esy4 = fct_relevel(esy4, "R", "R22", "R1A"),
    cwm.abu.seedmass = cwm.abu.seedmass * 1000,
    site.type = fct_recode(site.type, "+" = "positive", "−" = "negative")
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
  group_by(esy4, site.type) %>%
  summarize(mean = mean(y), sd = sd(y, na.rm = TRUE))

data_line <- data %>%
  filter(site.type == "+")

data_text <- tibble(
  y = c(10, 10, 10, 9),
  site.type = c("+", "restored", "−", "−"),
  label = c("", "", "Site type n.s.", "Interaction n.s."),
  esy4 = c("R", "R22", "R1A", "R1A")
) %>%
  mutate(esy4 = fct_relevel(esy4, "R", "R22", "R1A"))

graph_c <- ggplot() +
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
      "+" = "#21918c",
      "restored" = "orange",
      "−" = "#440154"
    ), guide = "none"
  ) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
  labs(
    x = "Restoration compared to references",
    y = expression(CWM ~ seed ~ mass ~ "[" * g * "]"),
    title = "Seed mass",
    tag = "C"
  ) +
  theme_mb() +
  theme(strip.text = element_blank()); graph_c


#### * Save ####

ggsave(
  here("outputs", "figures", "figure_4_site.type_seedmass_300dpi_9x6cm.tiff"),
  dpi = 300, width = 9, height = 6, units = "cm"
)
