#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Prepare data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-04-03



### Packages ###
library(renv)
library(here)
library(tidyverse)
library(installr)

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



sites <- read_csv(
  here("data", "raw", "data_processed_environment_nms_20250603.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = "f",
    region = col_factor(levels = c("north", "centre", "south"), ordered = TRUE),
    site.type = col_factor(
      levels = c("positive", "restored", "negative"), ordered = TRUE
    ),
    freq.mow = "f",
    fertilized = "f",
    obs.year = "f",
    land.use.hist = "f"
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
) %>%
  filter(!(ESY %in% c(NA, "?"))) %>%
  filter(ESY %in% c("E12a", "E22") & Country == "Germany")

species_splot <- read_delim(
  here("data", "raw", "database_splotopen", "sPlotOpen_DT.txt"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(
      .default = "?"
    )
) %>%
  filter(Abundance_scale == "CoverPerc") %>%
  semi_join(sites_splot, by = "PlotObservationID")


rm(list = setdiff(ls(), c(
  "sites", "sites_splot", "species", "species_splot")))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Calculate variables #######################################################


data <- sites %>%
  filter(
    eco.id %in% c(664, 654, 686) &
      !(eco.id == 686 & region == "centre") &
      !(eco.id == 664 & region == "centre")
  )

sites_esy16 <- data %>%
  select(
    id.plot, id.site, longitude, latitude, region, eco.id, eco.name, obs.year,
    esy16, site.type, hydrology, fertilized, mngm.type,
    cwm.abu.sla, cwm.abu.height, cwm.abu.seedmass,
    cwm.pres.sla, cwm.pres.height, cwm.pres.seedmass
  ) %>%
  group_by(
    id.site, region, eco.id, eco.name, obs.year, esy16, site.type,
    hydrology, fertilized, mngm.type,
    ) %>%
  summarize(
    cwm.abu.sla.mean = mean(cwm.abu.sla),
    cwm.abu.height.mean = mean(cwm.abu.height),
    cwm.abu.seedmass.mean = mean(cwm.abu.seedmass),
    cwm.pres.sla.mean = mean(cwm.pres.sla),
    cwm.pres.height.mean = mean(cwm.pres.height),
    cwm.pres.seedmass.mean = mean(cwm.pres.seedmass),
  ) %>%
  filter(esy16 %in% c("R", "R22", "R1A"))
  
sites_esy4 <- data %>%
  select(
    id.plot, id.site, longitude, latitude, region, eco.id, eco.name, obs.year,
    esy4, site.type, hydrology, fertilized, mngm.type,
    cwm.abu.sla, cwm.abu.height, cwm.abu.seedmass,
    cwm.pres.sla, cwm.pres.height, cwm.pres.seedmass
  ) %>%
  filter(esy4 %in% c("R", "R22", "R1A"))

table(sites_esy16$esy16)
table(sites_esy4$esy4)



## 2 Surveys from sPlotOpen ####################################################


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

rm(list = setdiff(ls(), c(
  "sites", "sites_splot", "species", "species_splot", "sites_esy4", "sites_esy16"
)))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save processed data #######################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



write_csv(
  sites_esy4, here("data", "processed", "data_processed_sites_esy4.csv")
  )

write_csv(
  sites_esy16, here("data", "processed", "data_processed_sites_esy16.csv")
)
