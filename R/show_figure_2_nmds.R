#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# Non-metric multidimensional scaling (NMDS) ordination ####
# Plant communities
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2024-08-12



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(vegan)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d", "ordi")))
library(scales)
show_col(hue_pal()(3))

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

base::load(file = here("outputs", "models", "model_plants_nmds_presence.Rdata"))
ordi_presence

ordi_points <- ordi_presence$points %>%
  as.data.frame() %>%
  rownames_to_column("id.plot")

sites <- read_csv(
  here("data", "processed", "sites_processed_environment_nms_20240812.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?"
  )
) %>%
  select(
    id.plot, veg.height, biomass.1, eco.id, eco.name, esy4, esy16,
    longitude, latitude, fdis.abu.oek.f, cwm.abu.oek.f, obs.year, site.type,
    history, changes, region
  ) %>%
  mutate(
    esy16 = if_else(str_detect(esy16, "^V"), "V", esy16),
    reference = if_else(
      site.type == "positive" | site.type == "negative", "reference", site.type
    )
    ) %>%
  filter(esy16 %in% c("R", "R1A", "R22", "V")) %>%
  inner_join(ordi_points, by = "id.plot")

sites %>%
  group_by(site.type, esy16) %>%
  count() %>%
  pivot_wider(names_from = site.type, values_from = n)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### 1 Preparation #############################################################


ellipses <- tibble()

data_nmds <- sites %>%
  ### Select variables to plot ###
  select(
    id.plot, MDS1, MDS2, esy16, site.type, reference # modify group
  ) %>%
  mutate( # modify group
    group_type = if_else(
      site.type == "negative", site.type, if_else(
        site.type == "positive", site.type, esy16
    )
    )
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

  data_ellipses <- ellipses #%>%
    # separate( # separate group type
    #   group_type,
    #   sep = "\\.",
    #   c("site.type", "esy.simple")
    # ) %>%
    # mutate( # fct_relevel groups
    #   esy.simple = fct_relevel(esy.simple, "R3", "R2", "R1", "R", "Other"),
    #   site.type = fct_relevel(site.type, "negative", "restored", "positive")
    #   )
}



## 2 Plot points ###############################################################


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
     data = data_ellipses %>% filter(group_type != "Other"),
     linewidth = 1,
     show.legend = FALSE
   ) +
   
   #### * Layout ####
   
   # facet_wrap(
   #   ~ group_type,
   #   nrow = 2,
   #   labeller = as_labeller(
   #     c(reference = "Reference", restored = "Restoration")
   #     )
   # ) +
   geom_label(
     aes(x = mean1, y = mean2, label = group_type, fill = group_type),
     data = data_nmds %>% filter(group_type != "Other"),
     color = "white",
     size = 3,
     show.legend = FALSE
   ) +
   coord_fixed() +
   
   #### * Design ####
   
   scale_shape_manual(
     values = c(
       R22 = "circle", R1A = "circle", R = "circle",
       positive = "circle open", negative = "circle open", V = "cross"
     )
   ) +
   scale_color_manual(
     values = c(
       R22 = "#00BFC4", R1A = "#F8766D", R = "grey30",
       positive = "grey70", negative = "grey70", V = "grey90"
     )
   ) +
   scale_fill_manual(
     values = alpha(c(
       R22 = "#00BFC4", R1A = "#F8766D", R = "grey30",
       positive = "grey70", negative = "grey70", V = "grey90"
     ), alpha = .6)
   ) +
   #scale_alpha_manual(values = c(.7, .7, .7, .6, .3)) +
   labs(
     x = "NMDS1", y = "NMDS2", fill = "", color = "", shape = "", alpha = ""
   ) +
   theme_mb() +
   theme(legend.position = "right"))

#### * Save ####

ggsave(
  here(
    "outputs", "figures", "plants_nmds",
    "figure_plants_nmds_eunis16_presence_points_300dpi_30x12cm.tiff"
  ),
  dpi = 300, width = 30, height = 12, units = "cm"
)



## 3 Plot density #############################################################


graph_b <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_density_2d(
    aes(x = MDS1, y = MDS2, fill = ..level..),
    geom = "polygon", colour="transparent", data = data_nmds
  ) +
  facet_wrap(~ esy16) +
  coord_fixed() +
  labs(x = "NMDS1", y = "NMDS2", fill = "") +
  theme_mb() +
  theme(legend.position = "none"); graph_b


#### * Save ####

ggsave(
  here(
    "outputs", "figures", "plants_nmds",
    "figure_plants_nmds_eunis16_presence_density_300dpi_30x12cm.tiff"
    ),
  dpi = 300, width = 30, height = 12, units = "cm"
  )
