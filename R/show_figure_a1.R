#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Show figure 3
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-05-05



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(patchwork)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d", "graph_e",
                          "graph_f", "graph_g", "graph_h")))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot #######################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



(graph_a | graph_b) /
  (graph_c | graph_d) /
  (graph_e | graph_f) /
  (graph_g | graph_h) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

### Save ###

ggsave(
  here("outputs", "figures", "figure_a1_300dpi_17x20cm.tiff"),
  dpi = 300, width = 17, height = 20, units = "cm"
)
