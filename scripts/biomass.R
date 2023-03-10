#____________________----
# Packages ----
library(tidyverse)
library(rstatix)
library(performance)

#___________________----
# Import Data
biomass <- read_csv(here("data", "biomass.csv"))

#___________________----
# Tidy data
glimpse(biomass)

head(biomass)

biomass <- janitor::clean_names(biomass)
# check for duplication
biomass %>%
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
biomass %>% 
  summarise(min=min(biomass_m2, na.rm=TRUE), 
            max=max(biomass_m2, na.rm=TRUE))

# check for typos by looking at distinct characters/values

biomass %>% 
  distinct(fert)

biomass %>% 
  distinct(light)

biomass %>% 
  distinct(fl)

# missing values
biomass %>% 
  is.na() %>% 
  sum()

# quick summary

summary(biomass)

#____________________----
# One-Way ANOVA ----


ls_1 <- lm(biomass_m2 ~ fl, data = biomass)
summary(ls_1)
