# PACKAGES ----
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)

# Lm function ----
lsmodel0 <- lm(formula = height ~ 1, data = darwin)

summary(lsmodel0) #The 18.9 is the estimate of the model coefficient (in this case it is the overall mean),
                  #together with its standard error. 
mean(darwin$height) # can see that the intercept in the summay represents the mean in this case

# Comparing means ----

lsmodel1 <- lm(height ~ type, data=darwin) # analyses the difference in average plant height as a function of pollination type

# note that the following is identical

# lsmodel1 <- lm(height ~ 1 + type, data=darwin) - same as above code

broom::tidy(lsmodel1) # broom summarizes key information about models in tidy tibbles

# the intercept now represents the mean height for type cross plants 
# type self row represents the difference in mean height for self plants compared to type cross
#This linear model indicates the average height of Crossed plants is 20.2 inches, and Selfed plants are an average of 2.6 inches shorter.

darwin %>% 
  group_by(type) %>% 
  
  summarise(mean=mean(height))
# shows same difference in mean height

# Standard error of the difference ----

summary(lsmodel1) # tells us SED in the type self row (standard error difference) but our model assumes equal variance,
                  # to take advantage of the larger sample size through 'pooling', to generate a more accurate SED.
                  # But this assumption needs to be checked in our model
darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

# plotting calculated means

# Confidence intervals ----
confint(lsmodel1) # intercept row gives a 95% CI for the height of the crossed plants,
                  # and the second row gives a 95% interval for the difference in height between crossed and selfed plants.
 















