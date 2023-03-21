#____________________----
# Packages ----
library(tidyverse)
library(rstatix)
library(performance)
library(ggridges)
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

# Activity 1 ----

colours <- c("cyan", "darkorange", "purple")

fruitfly %>% 
  ggplot(aes(x = longevity, y = type, fill = type))+
  geom_density_ridges(alpha = 0.5)+
  scale_fill_manual(values = colours)+
  theme_minimal()+
  theme(legend.position = "none")
# looks like treatment affects longevity

fruitfly %>% 
  ggplot(aes(x = thorax, y = longevity, group = type, colour = type))+
  geom_point()+
  geom_smooth(method = "lm",
              se = FALSE)+
  scale_colour_manual(values = colours)+
  theme_minimal()
  
# looks like size affects longevity, and that it has a similar affect across treatments

fruitfly %>% 
  ggplot(aes(x = sleep, y = longevity, group = type, colour = type))+
  geom_point(alpha = 0.6)+
  geom_smooth(method = "lm",
              se = FALSE)+
  scale_colour_manual(values = colours)+
  theme_minimal()
# does look like sleep interacts with treatment to affect lifespan
# in order to know the strength of this association, and if it is significantly different from what we might observe under the null hypothesis, we will have to build a model.

#____________________________----
# Designing a Model ----


