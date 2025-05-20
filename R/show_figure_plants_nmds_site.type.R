# Grassworks Project
# Non-metric multidimensional scaling (NMDS) ordination ####
# Plants ~ site.type

# Markus Bauer
# 2024-05-02



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(vegan)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d", "ordi")))

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
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

vegan_cov_ellipse <- function(cov, center = c(0, 0), scale = 1, npoints = 100) {
  theta <- (0:npoints) * 2 * pi / npoints
  circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(circle %*% chol(cov)))
}

#### Load sites data and model ###

base::load(file = here("outputs", "models", "model_plants_nmds_abundance.Rdata"))
ordi_abundance

ordi_points <- ordi_abundance$points %>%
  as.data.frame() %>%
  rownames_to_column("id.plot")

sites <- read_csv(
  here("data", "processed", "sites_processed_environment_nms.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?"
  )
) %>%
  select(
    id.plot, veg.height, biomass.1, fdis_abu_oek_f, cwm_abu_oek_f,
    obs.year, site.type, history, changes, region
  ) %>%
  inner_join(ordi_points, by = "id.plot") %>%
  filter(!is.na(site.type)) # Check if necessary (last update 02.05.2024)
  
sites %>%
  group_by(site.type) %>%
  count()



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### 1 Preparation #############################################################


ellipses <- tibble()

data_nmds <- sites %>%
  ### Select variables to plot ###
  select(
    id.plot, MDS1, MDS2, site.type # modify group
  ) %>%
  mutate(
    group_type = site.type # modify group
  ) %>%
  group_by(group_type) %>%
  mutate(
    mean1 = mean(MDS1),
    mean2 = mean(MDS2),
    group_type = factor(group_type)
  )

### Calculate ellipses for plot ###
for (group in levels(data_nmds$group_type)) {
  
  ellipses_calc <- data_nmds %>%
    filter(group_type == group) %>%
    with(
      cov.wt(
        cbind(MDS1, MDS2),
        wt = rep(1 / length(MDS1), length(MDS1))
      )
    )
  
  ellipses <- vegan_cov_ellipse(
    cov = ellipses_calc$cov, center = ellipses_calc$center
  ) %>%
    as_tibble() %>%
    bind_cols(group_type = group) %>%
    bind_rows(ellipses)
  
  data_ellipses <- ellipses %>%
    mutate(
      group_type = fct_relevel(group_type, "negative", "restored", "positive")
      )
}

levels(data_ellipses$group_type) # check levels



## 2 Plot #####################################################################


#### * Site scores ####

(graph_a <- ggplot() +
   geom_vline(xintercept = 0, linetype = "dashed") +
   geom_hline(yintercept = 0, linetype = "dashed") +
   geom_point(
     aes(y = MDS2, x = MDS1,
         color = group_type, shape = group_type),
     data = data_nmds,
     cex = 2
   ) +
   
   #### * Ellipses ####
 
 geom_path(
   aes(x = MDS1, y = MDS2, color = group_type),
   data = data_ellipses,
   linewidth = 1,
   show.legend = FALSE
 ) +
   
   #### * Layout ####
 
   geom_label(
     aes(x = mean1, y = mean2, label = group_type, fill = group_type),
     data = data_nmds,
     color = "white",
     size = 3,
     show.legend = FALSE
   ) +
   coord_fixed() +
   
   #### * Design ####
 
 
 scale_shape_manual(
   values = c(
     negative = "circle open", restored = "circle", positive = "circle open"
   )
 ) +
   scale_color_manual(
     values = c(
       negative = "deepskyblue3", positive = "#F8766D", restored = "grey30"
     )
   ) +
   scale_fill_manual(
     values = alpha(c(
       negative = "deepskyblue3", positive = "#F8766D", restored = "grey30"
     ), alpha = .6)
   ) +
   #scale_alpha_manual(values = c(.7, .7, .7, .6, .3)) +
   labs(
     x = "NMDS1", y = "NMDS2", fill = "", color = "", shape = "", alpha = ""
   ) +
   theme_mb())


#### * Save ####

ggsave(
  here("outputs", "figures", "figure_plants_nmds_abundance_site_type_300dpi_16.5x12cm.tiff"),
  dpi = 300, width = 16.5, height = 12, units = "cm"
)
