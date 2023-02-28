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
# minimum effect size (at 95% confidence) of density on the janka scale = 52.9

# Effect size
summary(janka_ls1)
# 95% of the variance in timber hardness can be explained by wood density (R^2 = 0.95)

# Assumptions ----
janka_ls1 %>% # ls = least squares (referring to ordianry least squares method)
  broom::augment() %>% 
  head()
# shows residuals of each data point

augmented_ls1 <- janka_ls1 %>% 
  broom::augment()

augmented_ls1 %>% 
  ggplot(aes(x=dens, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=dens, 
                 y=hardness))+
  geom_segment(aes(x=dens, 
                   xend=dens, 
                   y=.fitted, 
                   yend=hardness), 
               linetype="dashed", colour="red")
# graph showing residuals

model_plot <- function(data=augmented_ls1, 
                       x="dens", 
                       y="hardness", 
                       title="Full data"){
  ggplot(aes(x=.data[[x]], 
             y=.data[[y]]), 
         data=data)+
    geom_line()+
    theme_bw()+
    ggtitle(title)
}

p1 <- model_plot()
p2 <- model_plot(y=".fitted", title="Linear prediction")
p3 <- model_plot(y=".resid", title="Remaining pattern")

library(patchwork)
p1+p2+p3

# normal distribution?
performance::check_model(janka_ls1, check=c("normality","qq"))

# Equal variance?
performance::check_model(janka_ls1, check="homogeneity")
# the plot we constructed earlier we had the 'raw' residuals as a function of the fitted values. 
# the plot we have produced now is the 'standardized residuals',
# this is the raw residual divided by the standard deviation.
# suggests that the residuals do not have constant variance, broadly speaking the amount of variance y increases as x increases. 
# This means we have less confidence in our predictions at high values of density.

# Outliers
performance::check_model(janka_ls1, check="outliers")
# 1 potential outlier

# Prediction ----
# Using the coefficients of the intercept and the slope,
# we can make predictions on new data

coef(janka_ls1)
# imagine we have a new wood samples with a density of 65, 
# how can we use the equation for a linear regression to predict what the timber hardness for this wood sample should be?
# y = a + bx

# -1160.49970 + 57.50667 * 65 = 2577.434

coef(janka_ls1)[1] + coef(janka_ls1)[2] * 65
# can use the coefficients of the model directly to make predictions
# or we can use the functions predict and broom::augment

predict(janka_ls1, newdata=list(dens=c(22,35,65)))

broom::augment(janka_ls1, 
               newdata=tibble(dens=c(22,35,65)))
# We can also add standard error and confidence intervals to these predictions

broom::augment(janka_ls1, newdata = tibble(dens=c(22,35,65)), se=TRUE)
# standard error

broom::augment(janka_ls1, newdata=tibble(dens=c(22,35,65)), interval="confidence")
# confidence intervals

# Add these to a plot

pred_newdata <- broom::augment(janka_ls1, 
                               newdata=tibble(dens=c(22,35,65)))

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_point(data=pred_newdata, aes(y=.fitted, x=dens), colour="red")+
  geom_label(data=pred_newdata, (aes(y=(.fitted+10), x=(dens+3), label=round(.fitted, digits=0))))+
  theme_bw()+
  labs(x="Density", y="Timber Hardness")+
  scale_x_continuous(limits=c(20,80), expand=expansion(add=c(0,5)))
