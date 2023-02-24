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

#Data Visualization ----

janka %>% 
  ggplot(aes(x= dens,
             y= hardness ))+
  geom_point()

# Shows a positive correlation between the two 

# Pearsons R ----

# Pearson's R generation to measure linear dependence between both variables
# cor() does not have a data option so need to use the with() function

with(janka, cor(dens, hardness))
# r = 0.97, very strong correlation (linear dependence)

janka_ls1 <- lm(hardness ~ dens, data = janka) 
# be careful here. left of the tilde is the response variable and right is the predictor

# specify linear model method for line fitting

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")

# blue line represents the regression line, and the shaded interval is the 95% confidence interval band
# 95% confidence interval band is narrowest in the middle and widest at either end of the regression line
# This is because of the two types of uncertainty in the prediction of linear regression
# First is the prediction of the overall mean of the estimate (ie the center of the fit). 
# The second is the uncertainly in the estimate calculating the slope.
# When combined, you can see that the further away you get from the center of the data, 
# the uncertainty of the slope becomes larger and CI's widen

summary(janka_ls1)
# very similar output as seen with darwin data (coefficient estimates, standard error, t-statistic and P-value. 
# The first row is the intercept, and the second row is the difference in the mean from the intercept caused by our explanatory variable.)
# the intercept describes the value of y (timber hardness) when x (wood density) = 0, 
# and the standard error around this value is shown
# this intercept value of Y is impossible but the intercept can be made more valuable by using a technique called centering
# By subtracting the average (mean) value of x from every data point, 
# the intercept (when x is 0) can effectively be right-shifted into the centre of the data.

# Mean Centred Regression ----

dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))
# 45.73333

janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()

# the effect of density on timber hardness has not changed, 
# but the intercept now represents the estimated mean timber hardness for the mean wood density 
# e.g. at a density of ρ 45.73 the average timber hardness on the janka scale is 1469.
# if 57.5 is the value of the regression slope (with its standard error),
# then the timber hardness is predicted to increase by 57.5 on the janka scale for every unit change of density.

# According to our model summary, this estimated change in the mean is statistically significant,
# so for this effect size and sample size it is unlikely that we would observe this relationship
# - if the null hypothesis (that we cannot predict timber hardness from wood density) were true.

# Confidence intervals 
broom::tidy(janka_ls1, conf.int=T, conf.level=0.95)
