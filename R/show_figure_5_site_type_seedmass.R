#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types x Site type ####
# Show figure of seed mass
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-05-22



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
load(file = here("outputs", "models", "model_seedmass_esy4_3.Rdata"))
m <- m3
m@call



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### * Preparation ####

data_summary <- sites %>%
  group_by(esy4, site.type) %>%
  summarize(median = median(y), sd = sd(y, na.rm = TRUE), mean = mean(y))

data_model <- ggemmeans(
  m, terms = c("esy4", "site.type"), back.transform = FALSE, ci_level = .95
) %>%
  as_tibble() %>%
  rename(esy4 = x) %>%
  mutate(
    predicted = predicted * 1000,
    conf.low = conf.low * 1000,
    conf.high = conf.high * 1000,
    group = fct_recode(group, "+" = "positive", "−" = "negative"),
    group = fct_relevel(group, "+", "restored", "−")
  )

data_line <- data_model %>%
  filter(group == "+")

data_text <- tibble(
  y = c(10, 10, 6.2, 5.7),
  site.type = c("+", "restored", "−", "−"),
  label = c("", "", "Site type n.s.", "Interaction n.s."),
  esy4 = c("R", "R22", "R1A", "R1A")
) %>%
  mutate(esy4 = fct_relevel(esy4, "R", "R22", "R1A"))

### * Plot ####

graph_c <- ggplot() +
  # geom_hline(
  #   data = data_line,
  #   aes(yintercept = predicted),
  #   linetype = "dashed", color = "grey70", size = .5
  # ) +
  geom_quasirandom(
    data = sites,
    aes(x = site.type, y = y, color = site.type),
    alpha = .2, shape = 16, size = 1
  ) +
  geom_boxplot(
    data = sites, aes(x = site.type, y = y, fill = site.type),
    alpha = .5
  ) +
  # geom_errorbar(
  #   data = data_model,
  #   aes(
  #     x = as.numeric(factor(group)) + 0.5, ymin = conf.low, ymax = conf.high,
  #     color = group
  #   ),
  #   width = 0.0, linewidth = 0.4
  # ) +
  # geom_point(
  #   data = data_model,
  #   aes(x = as.numeric(factor(group)) + 0.5, y = predicted, color = group),
  #   size = 1
  # ) +
  geom_text(
    data = data_text,
    aes(x = site.type, y = y, label = label, group = esy4),
    hjust = .8, size = 3.1
  ) +
  facet_grid(~ esy4) +
  scale_color_manual(
    values = c(
      "+" = "#21918c",
      "restored" = "#FFA500",
      "−" = "#440154"
    ), guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      "+" = "#21918c",
      "restored" = "#FFA500",
      "−" = "#440154"
    ), guide = "none"
  ) +
  scale_y_continuous(limits = c(0, 6.2), breaks = seq(0, 10, .5)) +
  labs(
    x = "Restoration compared to references",
    y = expression(CWM ~ seed ~ mass ~ "[" * mg * "]"),
    title = "Seed mass",
    tag = "C"
  ) +
  theme_mb() +
  theme(strip.text = element_blank()); graph_c


#### * Save ####

ggsave(
  here("outputs", "figures", "figure_5_site.type_seedmass_300dpi_9x6cm.tiff"),
  dpi = 300, width = 9, height = 6, units = "cm"
)
