# PACKAGES ----
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom.helpers)


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

# Answering the question ----

# Darwin's original hypothesis was that self-pollination would reduce fitness (using height as a proxy for this)
# The null hypothesis is that there is no effect of pollination type, and therefore no difference in the average heights.
# we can simply determine whether or not the predicted value of our null hypothesis (a difference of zero),
# lies inside the 95% CI for the difference of the mean.
# In this case we can see that the upper and lower bounds of the confidence intervals do not cross zero.



 
GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)   # produces a graph of the estimated mean difference with an approx 95% CI.
                                        # As we can see we are able to reject the null hypothesis at a 95% confidence level.
                                        # when changing to 99% CI, the upper bound crosses zero - cannot rejct null
broom::tidy(lsmodel1, conf.int=T, conf.level=0.99) # represents same argument but with 99% CI - cannot reject null

# Getting other treatment mean and standard error ----

darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy() # tells us mean and SE of type self

# emmeans function ----

means <- emmeans::emmeans(lsmodel1, specs = ~ type) # does similar thing to previous code

means

means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL)) # can use emmeans as a handy summary to include in data visuals that combine raw data and statistical inferences.

# as variance is not being calculated separately, and instead is assumed to be roughly equal (pooled estimate),
# the standard error (and so 95% CI) are the same. This assumption still needs to be checked to trust results.

# Assumption Checking ----











