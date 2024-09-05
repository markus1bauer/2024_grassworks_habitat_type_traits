#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types 
# Community weighted mean of specific leaf area (SLA), canopy height
# and seed mass
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2024-07-16



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(patchwork)

### Start ###
rm(list = ls())

#### * Load data sites ####

sites <- read_csv(
  here("data", "processed", "sites_processed_environment_nms_20240716.csv"),
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
) %>%
  select(
    id.plot, longitude, latitude, region, eco.id, eco.name, obs.year,
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
  filter(n > 20) %>%
  mutate(esy16 = fct_relevel(esy16, "R", "R22", "R1A"))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Data exploration ##########################################################


### a Graphs of raw data -------------------------------------------------------

### General CWM ####

plot1 <- ggplot(sites, aes(y = cwm.abu.sla, x = esy16)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent") +
  labs(y = "CWM SLA (abu) [cm²/g]")
plot2 <- ggplot(sites, aes(y = cwm.abu.height, x = esy16)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent") +
  labs(y = "CWM Height (abu) [m]")
plot3 <- ggplot(sites, aes(y = cwm.abu.seedmass, x = esy16)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent") +
  labs(y = "CWM Seed mass (abu) [g]")
plot1 / plot2 / plot3

### General FDis and FRic ####

plot1 <- ggplot(sites, aes(y = fdis.abu.sla, x = esy16)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent")
plot2 <- ggplot(sites, aes(y = fdis.abu.height, x = esy16)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent")
plot3 <- ggplot(sites, aes(y = fdis.abu.seedmass, x = esy16)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent")
plot1 / plot2 / plot3

plot1 <- ggplot(sites, aes(y = fric.abu.sla, x = esy16)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent") +
  scale_y_continuous(limits = c(0,100))
plot2 <- ggplot(sites, aes(y = fric.abu.height, x = esy16)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent") +
  scale_y_continuous(limits = c(0,100))
plot3 <- ggplot(sites, aes(y = fric.abu.seedmass, x = esy16)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent") +
  scale_y_continuous(limits = c(0,100))
plot1 / plot2 / plot3

### EUNIS x region ####

ggplot(sites, aes(y = cwm.abu.sla, x = region)) + # Interesting differene of R1A and R22
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM SLA (abu) [cm²/g]")
ggplot(sites, aes(y = cwm.abu.height, x = region)) + # Interesting differene of R1A and R22
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM Height (abu) [m]")
ggplot(sites, aes(y = cwm.abu.seedmass, x = region)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM Seed mass (abu) [g]")

### EUNIS x site.type ####

ggplot(sites, aes(y = cwm.abu.sla, x = site.type)) + #intersting R
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM SLA (abu) [cm²/g]")
ggplot(sites, aes(y = cwm.abu.height, x = site.type)) + #interesting R, R1A
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM Height (abu) [m]")
ggplot(sites, aes(y = cwm.abu.seedmass, x = site.type)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM Seed mass (abu) [g]")

### FDis ~ EUNIS x site.type ####

ggplot(sites, aes(y = fdis.abu.sla, x = site.type)) + #interesting R
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) #+
  #scale_y_continuous(limits = c(0,100))
ggplot(sites, aes(y = fdis.abu.height, x = site.type)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16)
ggplot(sites, aes(y = fdis.abu.seedmass, x = site.type)) + # interesting R
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16)

### EUNIS x history ####

sites %>%
  filter(
    !str_detect(history, "[:alpha:]") & !str_detect(history, "[:punct:]")
    & history > 1980
    ) %>%
  mutate(history = as.numeric(history)) %>%
  ggplot(aes(y = cwm.abu.sla, x = history, color = esy16)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm") +
  facet_grid(~ site.type) +
  labs(y = "CWM SLA (abu) [cm²/g]") +
  theme(axis.text.x = element_text(angle = 90))
sites %>%
  filter(
    !str_detect(history, "[:alpha:]") & !str_detect(history, "[:punct:]")
    & history > 1980
  ) %>%
  mutate(history = as.numeric(history)) %>%
  ggplot(aes(y = cwm.abu.height, x = history, color = esy16)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm") +
  facet_grid(~ site.type) +
  labs(y = "CWM Height (abu) [m]") +
  theme(axis.text.x = element_text(angle = 90))
sites %>%
  filter(
    !str_detect(history, "[:alpha:]") & !str_detect(history, "[:punct:]")
    & history > 1980
  ) %>%
  mutate(history = as.numeric(history)) %>%
  ggplot(aes(y = cwm.abu.seedmass, x = history, color = esy16)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm") +
  facet_grid(~ site.type) +
  labs(y = "CWM Seed mass (abu) [g]") +
  theme(axis.text.x = element_text(angle = 90))

### EUNIS x land.use.hist ####

ggplot(sites, aes(y = cwm.abu.sla, x = land.use.hist)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM SLA (abu) [cm²/g]")
ggplot(sites, aes(y = cwm.abu.height, x = land.use.hist)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM Height (abu) [m]")
ggplot(sites, aes(y = cwm.abu.seedmass, x = land.use.hist)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM Seed mass (abu) [g]")

### EUNIS x fertilized ####

ggplot(sites, aes(y = cwm.abu.sla, x = fertilized)) + # interesting R
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM SLA (abu) [cm²/g]")
ggplot(sites, aes(y = cwm.abu.height, x = fertilized)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM Height (abu) [m]")
ggplot(sites, aes(y = cwm.abu.seedmass, x = fertilized)) +
  geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM Seed mass (abu) [g]")

### EUNIS x freq.mow ####

sites %>%
  filter(!is.na(freq.mow)) %>%
  ggplot(aes(y = cwm.abu.sla, x = freq.mow, color = fertilized)) + #intersting R
  geom_quasirandom(color = "grey") +
  #geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM SLA (abu) [cm²/g]")
ggplot(sites, aes(y = cwm.abu.height, x = freq.mow, color = fertilized)) +
  #geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM Height (abu) [m]")
ggplot(sites, aes(y = cwm.abu.seedmass, x = freq.mow, color = fertilized)) +
  #geom_quasirandom(color = "grey") +
  geom_boxplot(fill = "transparent") +
  facet_grid(~ esy16) +
  labs(y = "CWM Seed mass (abu) [g]")


## 2 Model building ###########################################################


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

