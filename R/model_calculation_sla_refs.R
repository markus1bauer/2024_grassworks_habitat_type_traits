#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Specific leaf area and reference sites
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2026-06-02



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(patchwork)
library(lme4)
library(DHARMa)

### Start ###
rm(list = ls())

### Load data ###
sites <- read_csv(
  here("data", "processed", "data_processed_sites_refs.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types = cols(
    .default = "?",
    eco.id = "f",
    region = col_factor(levels = c("north", "centre", "south"), ordered = TRUE),
    hydrology = col_factor(levels = c("moist", "fresh", "dry"), ordered = TRUE),
    site.type = col_factor(
      levels = c("negative", "restored", "positive"), ordered = TRUE
    ),
    obs.year = "f"
  )
) %>%
  rename(y = cwm.abu.sla)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Data exploration ##########################################################


### a Graphs of raw data -------------------------------------------------------

ggplot(sites, aes(y = y, x = hydrology)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent") +
  labs(y = "CWM SLA (abu) [cm²/g]", x = "Hydrology")

ggplot(sites, aes(y = log(y), x = site.type)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ hydrology) +
  labs(y = "CWM SLA (abu) [cm²/g]", x = "Site type")

ggplot(sites, aes(y = y, x = obs.year)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ hydrology) +
  labs(y = "CWM SLA (abu) [cm²/g]", x = "Survey year")


### b Outliers, zero-inflation, transformations? ------------------------------

sites %>% count(eco.id)
sites %>% count(site.type)
sites %>% count(hydrology, site.type)
sites %>% select(id.site, site.type) %>% unique() %>% count(site.type)
plot1 <- ggplot(sites, aes(x = region, y = y)) + geom_quasirandom()
plot2 <- ggplot(sites, aes(x = y)) + geom_histogram(binwidth = 10)
plot3 <- ggplot(sites, aes(x = y)) + geom_density()
plot4 <- ggplot(sites, aes(x = log(y))) + geom_density()
(plot1 + plot2) / (plot3 + plot4)


### c Check collinearity ------------------------------------------------------

# sites %>%
#   select() %>%
#   GGally::ggpairs(lower = list(continuous = "smooth_loess")) +
#   theme(strip.text = element_text(size = 7))
#--> exclude r > 0.7
# Dormann et al. 2013 Ecography
# https://doi.org/10.1111/j.1600-0587.2012.07348.x

# -> No continuous explanatory variables



## 2 Model building ###########################################################


### a Candidate models ---------------------------------------------------------

m1 <- lmer(
  y ~ hydrology * site.type + eco.id + obs.year + (1|id.site),
  REML = FALSE,
  data = sites
)
simulateResiduals(m1, plot = TRUE)
m2 <- lmer(
  y ~ (hydrology + site.type + eco.id + obs.year)^2 + (1|id.site),
  REML = FALSE,
  data = sites
)
simulateResiduals(m2, plot = TRUE)


### b Save ---------------------------------------------------------------------

save(m1, file = here("outputs", "models", "model_sla_refs_1.Rdata"))
save(m2, file = here("outputs", "models", "model_sla_refs_2.Rdata"))
