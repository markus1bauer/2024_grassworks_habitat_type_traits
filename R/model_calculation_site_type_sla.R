#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Specific leaf area (SLA) ~ Ecoregion
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-04-28



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(patchwork)
library(blme)
library(DHARMa)
library(emmeans)

### Start ###
rm(list = ls())

#### * Load data sites ####

sites <- read_csv(
  here("data", "raw", "data_processed_environment_nms_20250306.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = "f",
    region = col_factor(levels = c("north", "centre", "south"), ordered = TRUE),
    site.type = col_factor(
      levels = c("positive", "restored", "negative"), ordered = TRUE
      )
  )
) %>%
  select(
    id.plot, id.site, longitude, latitude, region, eco.id, eco.name, obs.year,
    esy4, esy16,
    site.type, history, hydrology, land.use.hist, fertilized, freq.mow,
    cwm.abu.sla, cwm.abu.height, cwm.abu.seedmass,
    cwm.pres.sla, cwm.pres.height, cwm.pres.seedmass,
    fdis.abu.sla, fdis.abu.height, fdis.abu.seedmass,
    fric.abu.sla, fric.abu.height, fric.abu.seedmass
  ) %>%
  group_by(esy16) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 20 & !(eco.id == 647)) %>%
  mutate(esy16 = fct_relevel(esy16, "R", "R22", "R1A")) %>%
  rename(y = cwm.abu.sla)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Data exploration ##########################################################


### a Graphs of raw data -------------------------------------------------------

ggplot(sites, aes(y = y, x = esy16)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent") +
  labs(
    y = "CWM SLA (abu) [cm²/g]",
    x = "Habitat type per plot calculated from block (combined 4 plots)"
    )

ggplot(sites, aes(y = y, x = eco.id)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM SLA (abu) [cm²/g]", x = "Ecoregion")

ggplot(sites, aes(y = y, x = site.type)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM SLA (abu) [cm²/g]", x = "Survey year")

ggplot(sites, aes(y = y, x = factor(obs.year))) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM SLA (abu) [cm²/g]", x = "Survey year")

sites %>%
  filter(site.type == "restored" & !(str_detect(history, "ie Fläche"))) %>%
  mutate(history = as.numeric(history)) %>%
  ggplot(aes(y = y, x = history)) +
  geom_quasirandom(color = "grey") +
  geom_smooth() +
  facet_grid(~ esy16) +
  labs(y = "CWM SLA (abu) [cm²/g]")


### b Outliers, zero-inflation, transformations? ------------------------------

sites %>% count(eco.id)
sites %>% count(site.type)
sites %>% count(esy16)
sites %>% count(esy16, eco.id)
plot1 <- ggplot(sites, aes(x = region, y = y)) + geom_quasirandom()
plot2 <- ggplot(sites, aes(x = y)) + geom_histogram(binwidth = 0.7)
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


### a Random structure ---------------------------------------------------------

# -> id.site used as random intercept


### b Fixed effects ------------------------------------------------------------

m1 <- lmer(y ~ esy16 * eco.id + (1 | id.site), data = sites)
simulateResiduals(m1, plot = TRUE)
m2 <- lmer(y ~ esy16 + eco.id + (1 | id.site), data = sites)
simulateResiduals(m2, plot = TRUE)


### c Save ---------------------------------------------------------------------

save(m1, file = here("outputs", "models", "model_ecoregion_sla_1.Rdata"))
save(m2, file = here("outputs", "models", "model_ecoregion_sla_2.Rdata"))
