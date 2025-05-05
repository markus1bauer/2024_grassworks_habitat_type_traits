#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Show figure of site types
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
library(cowplot)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d")))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot #######################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



p <- graph_a / graph_b / graph_c +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))
ggdraw(p) +
  draw_label("Site type ***\nInteraction n.s.", x = .9, y = .905, size = 11) +
  draw_label("Site type **\nInteraction n.s.", x = .9, y = .585, size = 11) +
  draw_label("Site type n.s.\nInteraction n.s.", x = .9, y = .26, size = 11)

### Save ###

ggsave(
  here("outputs", "figures", "figure_4_300dpi_15x24cm.tiff"),
  dpi = 300, width = 15, height = 24, units = "cm"
)
