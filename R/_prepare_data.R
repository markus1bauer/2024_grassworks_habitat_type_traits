#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Prepare data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2026-05-28



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
  here("data", "raw", "data_processed_environment_nms_20260528_subset.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = "f",
    region = col_factor(levels = c("north", "centre", "south"), ordered = TRUE),
    site.type = col_factor(
      levels = c("positive", "restored", "negative"), ordered = TRUE
    ),
    obs.year = "f",
    land.use.hist = "f"
  )
)


rm(list = setdiff(ls(), c("sites")))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data <- sites %>%
  filter(
    eco.id %in% c(664, 654, 686) &
      !(eco.id == 686 & region == "centre") &
      !(eco.id == 664 & region == "centre")
  )

# sites_esy16 <- data %>%
#   select(
#     id.plot, id.site, longitude, latitude, region, eco.id, eco.name, obs.year,
#     esy16, site.type, hydrology, mngm.type,
#     cwm.abu.sla, cwm.abu.height, cwm.abu.seedmass,
#     fric.abu.sla, fric.abu.height, fric.abu.seedmass#,
#     #c.n, ph.value, c.perc, toc.perc, n.perc, clay.perc, silt.perc, sand.perc
#   ) %>%
#   group_by(
#     id.site, region, eco.id, eco.name, obs.year, esy16, site.type,
#     hydrology, mngm.type#,
#     #c.n, ph.value, c.perc, toc.perc, n.perc, clay.perc, silt.perc, sand.perc
#     ) %>%
#   summarize(
#     cwm.abu.sla.mean = mean(cwm.abu.sla),
#     cwm.abu.height.mean = mean(cwm.abu.height),
#     cwm.abu.seedmass.mean = mean(cwm.abu.seedmass),
#     fric.abu.sla.mean = mean(cwm.abu.sla),
#     fric.abu.height.mean = mean(cwm.abu.height),
#     fric.abu.seedmass.mean = mean(cwm.abu.seedmass)
#   ) %>%
#   filter(esy16 %in% c("R22", "R1A"))
# table(sites_esy16$esy16)
  
sites_esy4 <- data %>%
  select(
    id.plot, id.site, longitude, latitude, region, eco.id, eco.name, obs.year,
    esy4, site.type, hydrology, mngm.type,
    cwm.abu.sla, cwm.abu.height, cwm.abu.seedmass,
    fric.abu.sla, fric.abu.height, fric.abu.seedmass#,
    #c.n, ph.value, c.perc, toc.perc, n.perc, clay.perc, silt.perc, sand.perc
  ) %>%
  filter(esy4 %in% c("R22", "R1A"))
table(sites_esy4$esy4)

sites_refs <- data %>%
  select(
    id.plot, id.site, longitude, latitude, region, eco.id, eco.name, obs.year,
    esy4, site.type, hydrology, mngm.type,
    cwm.abu.sla, cwm.abu.height, cwm.abu.seedmass
  ) %>%
  filter(
    ((hydrology == "dry" & site.type == "positive" &
       (esy4 == "R1A" | esy4 == "R21" | esy4 == "R22")) |
      (hydrology == "fresh" & site.type == "positive" &
         (esy4 == "R1A" | esy4 == "R21" | esy4 == "R22")) |
      (hydrology == "moist" & site.type == "positive" &
         (esy4 == "R21" | esy4 == "R22" | esy4 == "R35" | esy4 == "R37"))) |
      site.type == "restored" |
      site.type == "negative",
    !(hydrology == "dry" & site.type == "negative" &
        esy4 == "R1A"),
    !(hydrology == "fresh" & site.type == "negative" &
        (esy4 == "R21" | esy4 == "R22")),
    !(hydrology == "moist" & site.type == "negative" &
        (esy4 == "R21" | esy4 == "R22" | esy4 == "R36"))
  )
sites_refs %>%
  group_by(hydrology, site.type, esy4) %>%
  count() %>%
  print(n = 70)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save processed data #######################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# write_csv(
#   sites_esy16, here("data", "processed", "data_processed_sites_esy16.csv")
# )

write_csv(
  sites_esy4, here("data", "processed", "data_processed_sites_esy4.csv")
  )

write_csv(
  sites_refs, here(
    "data", "processed", "data_processed_sites_refs.csv"
    )
)
