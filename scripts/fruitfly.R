#____________________----
# Packages ----
library(tidyverse)
library(rstatix)
library(performance)

#___________________----
# Import Data ----
fruitfly <- read_csv(here("data", "fruitfly.csv"))

#___________________----
# Tidy data ----
glimpse(fruitfly)

head(fruitfly)

fruitfly <- janitor::clean_names(fruitfly)
# check for duplication
fruitfly %>%
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
fruitfly %>% 
  summarise(min=min(thorax, na.rm=TRUE), 
            max=max(thorax, na.rm=TRUE))

# check for typos by looking at distinct characters/values

fruitfly %>% 
  distinct(type)

# missing values
fruitfly %>% 
  is.na() %>% 
  sum()

# quick summary

summary(fruitfly)

#______________________----
# Visualisations ----

GGally::ggpairs(fruitfly)
