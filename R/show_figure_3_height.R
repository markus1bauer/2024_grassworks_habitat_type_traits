#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Show figure of canopy height
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
    eco.id = "f",
    eco.id = col_factor(levels = c("664", "654", "686"), ordered = TRUE),
    # site.type = col_factor(
    #   levels = c("positive", "restored", "negative"), ordered = TRUE
    # ),
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
  group_by(esy4) %>%
  summarize(mean = mean(y), sd = sd(y, na.rm = TRUE))

graph_b <- ggplot() +
  geom_hline(
    yintercept = .488, linetype = "solid", color = "grey70", linewidth = .2
    ) +
  geom_hline(
    yintercept = .488+.1196,
    linetype = "dashed", color = "grey70", linewidth = .2
    ) +
  geom_hline(
    yintercept = .488-.1196,
    linetype = "dashed", color = "grey70", linewidth = .2
    ) +
  geom_quasirandom(
    data = sites, aes(x = esy4, y = y, color = esy4),
    alpha = .2, shape = 16, size = 1
  ) +
  geom_boxplot(
    data = sites, aes(x = esy4, y = y, fill = esy4),
    alpha = .5
    ) +
  geom_errorbar(
    data = data,
    aes(x = as.numeric(
      factor(esy4)) + 0.45, ymin = mean-sd, ymax = mean+sd, color = esy4
      ),
    width = 0.0, linewidth = 0.4
  ) +
  geom_point(
    data = data,
    aes(x = as.numeric(factor(esy4)) + 0.45, y = mean, color = esy4),
    size = 2
  ) +
  annotate("text", label = "a", y = 1, x = 1) +
  annotate("text", label = "a", y = 1, x = 2) +
  annotate("text", label = "b", y = 1, x = 3) +
  scale_fill_manual(
    values = c(
      "R" = "#440154",
      "R22" = "#21918c",
      "R1A" = "#FFA500"
    ), guide = "none"
  ) +
  scale_color_manual(
    values = c(
      "R" = "#440154",
      "R22" = "#21918c",
      "R1A" = "#FFA500"
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
    axis.line.x = element_blank()
  ); graph_b

#### * Save ####

ggsave(
  here("outputs", "figures", "figure_3_height_300dpi_9x6cm.tiff"),
  dpi = 300, width = 9, height = 6, units = "cm"
)
