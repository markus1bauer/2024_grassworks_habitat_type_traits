#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# Traits ~ Habitat types ####
# Prepare data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-03-20



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

### Sabatini et al. (2021) Global Ecol Biogeogr:
### https://doi.org/10.1111/geb.13346
sites_splot <- read_delim(
  here("data", "raw", "database_splotopen", "sPlotOpen_header.txt"),
  col_names = TRUE, na = c("", "NA", "na"),
  col_types = cols(
    .default = "?",
    Cover_algae_layer = "d"
  )
)

### Sabatini et al. (2021) Global Ecol Biogeogr:
### https://doi.org/10.1111/geb.13346
species_splot <- read_delim(
  here("data", "raw", "database_splotopen", "sPlotOpen_DT.txt"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(
      .default = "?"
    )
) %>%
  filter(Abundance_scale == "CoverPerc")


rm(list = setdiff(ls(), c(
  "sites_experiment", "sites_splot", "sites_bauer",
  "species_experiment", "species_splot", "species_bauer",
  "traits"
)))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Reference sites ##########################################################


### Sabatini et al. (2021) Global Ecol Biogeogr
### https://doi.org/10.1111/geb.13346

data_sites <- sites_splot %>%
  filter(
    # Chytry et al. 2020 Appl Veg Sci
    # https://doi.org/10.1111/avsc.12519
    # Hay meadow: EUNIS2007 code E2.2:
    (ESY == "E22" |
       # Dry grassland: EUNIS2007 code E1.2a:
       ESY == "E12a") &
      Releve_area >= 1 &
      Releve_area <= 25 &
      #Country == "Germany" &
      Elevation < 700
  ) #%>%
  rename_with(tolower) %>%
  rename(id = plotobservationid, survey_year = date_of_recording,
         plot_size = releve_area, reference = country) %>%
  mutate(
    id = paste0("S", id),
    reference = str_replace(reference, "Germany", "reference"),
    survey_year = year(survey_year),
    source = if_else(
      givd_id == "EU-DE-014",
      "Jandt & Bruelheide (2012) Biodivers Ecol https://doi.org/10.7809/b-e.00146",
      "other"
    ),
    longitude = longitude * 10^5,
    latitude = latitude * 10^5
  ) %>%
  select(id, givd_id, source, longitude, latitude, elevation, plot_size,
         survey_year, reference, esy) %>%
  mutate(
    survey_year = as.character(survey_year),
    source = "Sabatini et al. (2021) Global Ecol Biogeogr https://doi.org/10.1111/geb.13346",
    reference = if_else(
      esy == "E12a", "positive_reference", if_else(
        esy == "E22", "positive_reference", "other"
      )
    ),
    target_type = if_else(
      esy == "E12a", "dry_grassland", if_else(
        esy == "E22", "hay_meadow", "other"
      )
    ),
    esy = if_else(
      esy == "E12a", "R1A", if_else(
        esy == "E22", "R22", "other"
      )
    ),
    exposition = "other"
  )
sites_splot <- data_sites

data_species <- species_splot %>%
  rename(id = PlotObservationID, name = Species,
         abundance = Original_abundance) %>%
  mutate(id = paste0("S", id)) %>%
  semi_join(data_sites, by = "id") %>%
  select(id, name, abundance) %>%
  pivot_wider(names_from = "id",
              values_from = "abundance",
              values_fn = sum) %>%
  mutate(
    name = str_replace(name, " ", "_"),
    name = str_replace(name, "Helianthemum_ovatum", "Helianthemum_nummularium"),
    name = str_replace(name, "Galium_album", "Galium_mollugo"),
    name = str_replace(name, "Taraxacum", "Taraxacum_campylodes"),
    name = str_replace(
      name, "Cerastium_fontanum", "Cerastium_fontanum_ssp_vulgare"
    ),
    name = str_replace(name, "Leucanthemum_ircutianum", "Leucanthemum_vulgare"),
    name = str_replace(name, "Tragopogon_orientalis", "Tragopogon_pratensis"),
    name = factor(name)
  ) %>%
  group_by(name) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))

### Check species name congruency ###
data <- data_species %>%
  anti_join(traits, by = "name") %>%
  select(name) %>%
  print(n = 50)

species_splot <- data_species



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save processed data #######################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
