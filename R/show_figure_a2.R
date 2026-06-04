#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# Functional diversity of habitats ####
# Show figure A2
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2026-06-04



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(patchwork)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d", "graph_e",
                          "graph_f", "graph_g", "graph_h", "graph_i")))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot #######################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



(graph_a | graph_b | graph_c) /
  (graph_d | graph_e | graph_f) /
  (graph_g | graph_h | graph_i) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

### Save ###

ggsave(
  here("outputs", "figures", "figure_a2_300dpi_25x15cm.tiff"),
  dpi = 300, width = 25, height = 15, units = "cm"
)
