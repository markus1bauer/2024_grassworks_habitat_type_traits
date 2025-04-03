#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# Traits ~ Habitat types ####
# Prepare data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-04-03



### Packages ###
library(renv)
library(here)
library(tidyverse)

### Start ###
rm(list = ls())
# installr::updateR(
#   browse_news = FALSE,
#   install_R = TRUE,
#   copy_packages = TRUE,
#   copy_site_files = TRUE,
#   keep_old_packages = FALSE,
#   update_packages = FALSE,
#   start_new_R = FALSE,
#   quit_R = TRUE
#   )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Load data #################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



species <- read_csv(
  here("data", "raw", "data_processed_species_plants_plot_20240813.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?"
  )
)

sites <- read_csv(
  here("data", "raw", "sites_processed_environment_nms_20240813.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = "f",
    region = col_factor(levels = c("north", "centre", "south"), ordered = TRUE),
    site.type = col_factor(
      levels = c("positive", "restored", "negative"), ordered = TRUE
    ),
    freq.mow = "f",
    fertilized = "f"
  )
)

### sPlotOpen: Sabatini et al. (2021) Global Ecol Biogeogr:
### https://doi.org/10.1111/geb.13346

sites_splot <- read_delim(
  here("data", "raw", "database_splotopen", "sPlotOpen_header.txt"),
  col_names = TRUE, na = c("", "NA", "na"),
  col_types = cols(
    .default = "?",
    Cover_algae_layer = "d"
  )
)

species_splot <- read_delim(
  here("data", "raw", "database_splotopen", "sPlotOpen_DT.txt"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(
      .default = "?"
    )
) %>%
  filter(Abundance_scale == "CoverPerc")


rm(list = setdiff(ls(), c(
  "sites", "sites_splot", "species", "species_splot",
  "traits"
  )))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Surveys from sPlotOpen ####################################################


### Sabatini et al. (2021) Global Ecol Biogeogr
### https://doi.org/10.1111/geb.13346

data_sites <- sites_splot %>%
  filter(
    # Chytry et al. 2020 Appl Veg Sci
    # https://doi.org/10.1111/avsc.12519
    # Hay meadow: EUNIS2007 code E2.2; Dry grassland: EUNIS2007 code E1.2a:
    (ESY == "E22" | ESY == "E12a") &
      Releve_area >= 2 &
      Releve_area <= 16 &
      #Country == "Germany" &
      Elevation < 700
  ) %>%
  rename_with(tolower) %>%
  rename(
    id = plotobservationid, survey_year = date_of_recording,
    plot_size = releve_area, reference = country
  ) %>%
  mutate(
    id = paste0("splot", id),
    survey_year = year(survey_year),
    longitude = longitude * 10^5,
    latitude = latitude * 10^5
  ) %>%
  select(
    id, givd_id, longitude, latitude, elevation, plot_size, survey_year,
    reference, esy
  ) %>%
  mutate(
    survey_year = as.character(survey_year),
    source = "Sabatini et al. (2021) Global Ecol Biogeogr https://doi.org/10.1111/geb.13346"
  )
sites_splot <- data_sites

data_species <- species_splot %>%
  rename(
    id = PlotObservationID, name = Species, abundance = Original_abundance
  ) %>%
  mutate(id = paste0("splot", id)) %>%
  semi_join(data_sites, by = "id") %>%
  select(id, name, abundance) %>%
  pivot_wider(
    names_from = "id", values_from = "abundance", values_fn = sum
  ) %>%
  mutate(
    name = factor(name)
  ) %>%
  group_by(name) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))
species_splot <- data_species

### Check species name congruency ###
data <- data_species %>%
  anti_join(traits, by = "name") %>%
  select(name) %>%
  print(n = 50)

rm(list = setdiff(ls(), c(
  "sites", "sites_splot", "species", "species_splot",
  "traits"
)))

## 2 Ecoregions ###############################################################


## 3 Traits ###################################################################


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save processed data #######################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
