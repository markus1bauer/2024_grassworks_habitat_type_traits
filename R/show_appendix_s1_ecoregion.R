#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Show appendix S1 Amoun of plots
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-06-10



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(gt)

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
    )
  )
) %>%
  mutate(esy4 = fct_relevel(esy4, "R", "R22", "R1A"))

(data <- sites %>%
  count(esy4, region, eco.id, eco.name))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot with gt ##############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



(table <- data %>%
    gt() %>%
    opt_table_lines("none") %>% ### Set general style options ###
    tab_options(
      table.font.style = "Arial",
      table.font.size = px(11),
      table.font.color = "black",
      column_labels.font.weight = "bold",
      data_row.padding = px(4),
      table.align = "left",
      column_labels.border.top.style = "solid",
      table_body.border.top.style = "solid",
      table_body.border.bottom.style = "solid",
      column_labels.border.top.color = "black",
      table_body.border.top.color = "black",
      table_body.border.bottom.color = "black",
      column_labels.border.top.width = px(2),
      table_body.border.top.width = px(1),
      table_body.border.bottom.width = px(2)
    ) %>%
    tab_style(
      locations = cells_column_labels(),
      style = cell_borders(
        sides = "top", color = "black", style = "solid", weight = px(1)
        )
    ) %>%
    tab_style(
      locations = cells_column_labels(),
      style = cell_text(align = "center")
    ) %>%
    tab_style(
      locations = cells_body(),
      style = cell_text(align = "center")
    ) )%>%
    tab_style(
      locations = cells_body(columns = "esy4"),
      style = cell_text(align = "left")
    ) %>%
    cols_label( ### Rename column names ###
      esy4 = md("Habitat type"),
      eco.id = md("Ecoregion ID"),
      eco.name = md("Ecoregion"),
      region = md("Region")
    )


### Save ###

write_csv(data, here("outputs", "tables", "table_s1_ecoregion.csv"))
gtsave(table, here("outputs", "tables", "table_s1_ecoregion.html"))
