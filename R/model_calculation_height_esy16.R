#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Canopy height for ESY16
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-04-29



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(patchwork)
library(DHARMa)

### Start ###
rm(list = ls())

#### * Load data sites ####

sites <- read_csv(
  here("data", "processed", "data_processed_sites_esy16.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = "f",
    region = col_factor(levels = c("north", "centre", "south"), ordered = TRUE),
    site.type = col_factor(
      levels = c("positive", "restored", "negative"), ordered = TRUE
    )
  )
) %>%
  filter(esy16 %in% c("R", "R22", "R1A") & !(eco.id == 647)) %>%
  mutate(esy16 = fct_relevel(esy16, "R", "R22", "R1A")) %>%
  rename(y = cwm.abu.height.mean)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Data exploration ##########################################################


### a Graphs of raw data -------------------------------------------------------

ggplot(sites, aes(y = y, x = esy16)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent") +
  labs(
    y = "CWM canopy height (abu) [m]",
    x = "Habitat type per block calculated from 4 plots per block"
  )

ggplot(sites, aes(y = y, x = eco.id)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM canopy height (abu) [m]", x = "Ecoregion")

ggplot(sites, aes(y = y, x = site.type)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM canopy height (abu) [m]", x = "Survey year")

ggplot(sites, aes(y = y, x = factor(obs.year))) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM canopy height (abu) [m]", x = "Survey year")

sites %>%
  filter(site.type == "restored" & !(str_detect(history, "ie Fläche"))) %>%
  mutate(history = as.numeric(history)) %>%
  ggplot(aes(y = y, x = history)) +
  geom_quasirandom(color = "grey") +
  geom_smooth() +
  facet_grid(~ esy16) +
  labs(y = "CWM canopy height (abu) [m]")


### b Outliers, zero-inflation, transformations? ------------------------------

sites %>% count(eco.id)
sites %>% count(site.type)
sites %>% count(esy16)
sites %>% count(esy16, eco.id)
plot1 <- ggplot(sites, aes(x = region, y = y)) + geom_quasirandom()
plot2 <- ggplot(sites, aes(x = y)) + geom_histogram(binwidth = 0.01)
plot3 <- ggplot(sites, aes(x = y)) + geom_density()
plot4 <- ggplot(sites, aes(x = log(y))) + geom_density()
(plot1 + plot2) / (plot3 + plot4)


### c Check collinearity ------------------------------------------------------

# -> No continous explanatory variables

# sites %>%
#   select() %>%
#   GGally::ggpairs(lower = list(continuous = "smooth_loess")) +
#   theme(strip.text = element_text(size = 7))
#--> exclude r > 0.7
# Dormann et al. 2013 Ecography
# https://doi.org/10.1111/j.1600-0587.2012.07348.x



## 2 Model building ###########################################################


### a Candidate models ---------------------------------------------------------

m1 <- lm(y ~ esy16 * (eco.id + site.type), data = sites)
simulateResiduals(m1, plot = TRUE)
m2 <- lm(y ~ esy16 + eco.id + site.type, data = sites)
simulateResiduals(m2, plot = TRUE)
m3 <- lm(y ~ esy16 + eco.id + site.type + fertilized, data = sites)
simulateResiduals(m3, plot = TRUE)
m4 <- lm(y ~ esy16 + eco.id + site.type + freq.mow, data = sites)
simulateResiduals(m4, plot = TRUE)
m5 <- lm(y ~ esy16 + eco.id + site.type + hydrology, data = sites)
simulateResiduals(m5, plot = TRUE)
m6 <- lm(y ~ esy16 * eco.id, data = sites)
simulateResiduals(m6, plot = TRUE)
m7 <- lm(y ~ esy16 * site.type, data = sites)
simulateResiduals(m7, plot = TRUE)


### b Save ---------------------------------------------------------------------

save(m1, file = here("outputs", "models", "model_height_esy16_1.Rdata"))
save(m2, file = here("outputs", "models", "model_height_esy16_2.Rdata"))
