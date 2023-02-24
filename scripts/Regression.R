# Packages ----
library(tidyverse)
library(rstatix)
library(performance)

janka <- read_csv(here("data", "Janka.csv"))

#Data Analysis----

# check the structure of the data
glimpse(janka)

# check data is in a tidy format
head(janka)

# check variable names
colnames(janka)


# clean up column names


janka <- janitor::clean_names(janka)

# check for duplication
janka %>%
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
janka %>% 
  summarise(min=min(dens, na.rm=TRUE), 
            max=max(dens, na.rm=TRUE))
janka %>% 
  summarise(min=min(hardness, na.rm=TRUE), 
            max=max(hardness, na.rm=TRUE))

# check for typos by looking at distinct characters/values

janka %>% 
  distinct(dens)

janka %>% 
  distinct(hardness)

# missing values
janka %>% 
  is.na() %>% 
  sum()

# quick summary

summary(janka)


