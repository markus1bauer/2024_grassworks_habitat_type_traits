#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Functional trait space
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-09-04



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(plotly)
library(vegan)

### Start ###
rm(list = ls())

### Load data ###
sites <- read_csv(
  here("data", "processed", "data_processed_sites_esy4.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = "f",
    region = col_factor(levels = c("north", "centre", "south"), ordered = TRUE),
    site.type = col_factor(
      levels = c("positive", "restored", "negative"), ordered = TRUE
    ),
    fertilized = "f",
    obs.year = "f"
  )
) %>%
  mutate(esy4 = fct_relevel(esy4, "R", "R22", "R1A")) %>%
  select(id.plot, esy4, cwm.abu.sla, cwm.abu.height, cwm.abu.seedmass)

centroid <- sites %>%
  group_by(esy4) %>%
  summarise(
    across(
      c("cwm.abu.sla", "cwm.abu.height", "cwm.abu.seedmass"), ~mean(., na.rm = TRUE)
      )
    )

figure <- plot_ly() %>%
  add_trace(
    data = sites,
    x = ~cwm.abu.sla,
    y = ~cwm.abu.height,
    z = ~cwm.abu.seedmass,
    color = ~esy4,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 4, opacity = 0.2)
    ) %>%
  add_trace(
    data = centroid,
    x = ~cwm.abu.sla,
    y = ~cwm.abu.height,
    z = ~cwm.abu.seedmass,
    color = ~esy4,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 8)
  ) %>%
  layout(
    scene = list(
      xaxis = list(title = "CWM SLA"),
      yaxis = list(title = "CWM plant height"),
      zaxis = list(title = "CWM seed mass")
    )
    );figure

library(reticulate)
save_image(
  figure, file = "3d_plot.png",
  width = 1200, height = 900
  )

species2 <- sites %>%
  select(-esy4) %>%
  column_to_rownames(var = "id.plot")

sites2 <- sites %>%
  column_to_rownames(var = "id.plot") %>%
  select(esy4)

adonis <- adonis2(
  data = sites2,
  formula = species2~esy4,
  method = "bray",
  permutations = 999
)
adonis
