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

# a full model
flyls1 <- lm(longevity ~ type + thorax + sleep + type:sleep, data = fruitfly)

flyls1 %>% 
  broom::tidy()

# intercept
coef(flyls1)[1]+ 
  # 1*coefficient for virgin treatment  
  coef(flyls1)[3] + 
  # 0.79 * coefficient for thorax size  
  (coef(flyls1)[4]*0.79) + 
  # 22 * coefficient for sleep  
  (coef(flyls1)[5]*22) + 
  # 22 * 1 * coefficient for interaction
  (coef(flyls1)[7]*22*1)

# 43.66 = the mean longevity of a male with a 0.79mm thorax, that sleeps for 22% of the day and is paired with virgin females

#______________________________----
# Checking Assumptions and Collinearity ----

performance::check_model(flyls1)

# Activity 2 ----

# Homogeneity of Variance:

# the reference line is fairly flat (slight curve). There might be some increasing heterogeneity with larger values, though very minor. Should be fine for making inferences. 
# With a slight curvature this could indicate that it might get a better fit with a transformation, or perhaps that there is a missing variable that if included in the model would improve the residuals. 

# Are residuals normally distributed?

# Yes

# Any issues with collinearity?

# No, Graph clearly shows there is collinearity. But this is not unusual when we include an interaction term, if we see evidence of collinearity in terms that are not part of an interaction then we should take another look
# If there is an issue, you can either do nothing (and explain it in report), transform data or drop the term.

#___________________________----
# Data Transformations ----

# Most common issue when trying to fit simple linear regression models is that our response variable is not normally distributed, which violates our modelling assumption.
# Variable transformation e.g lm(sqrt(x) ~ y, data = data), can sometimes fix linearity and can sometimes fix non-normality and heteroscedasticity (i.e non-constant variance)
# Generalized Linear Models (GLMs) can be used to change the error structure (i.e the assumption that residuals need to be normal)


# run this, pick a transformation and retest the model fit
MASS::boxcox(flyls1)

# suggests that square rooting may be beneficial to model

flyls_sqrt <- lm(sqrt(longevity) ~ type + thorax + sleep + type:sleep, data = fruitfly)

performance::check_model(flyls_sqrt)

# residual fits are not really any better, so will stick with original model

# use drop1 function to remove top-level terms
drop1(flyls1, test = "F")

# Based on this ANOVA, the interaction term does not need to be kept in the model, and so we will remove it 

flyls2 <- lm(longevity ~ type + thorax + sleep, data = fruitfly)

drop1(flyls2, test = "F")

#___________________________----
# Post Hoc ----

emmeans::emmeans(flyls2, specs = pairwise ~ type + thorax + sleep)

# the emmeans package is a very easy way to produce the estimate mean values (rather than mean differences) for different categories.
# If the term pairwise is included then it will also include post-hoc pairwise comparisons between all levels with a tukey test
# For continuous variables (sleep and thorax) - emmeans has set these to the mean value within the dataset, so comparisons are constant between categories 

#_____________________________----
# Activity 3 - Write Up ----

# Analysis Section:
# I constructed an ordinary least squares model to investigate the effects of sleep, mating type and body size on longevity in adult Drosophila melanogaster. I also included an interaction term between sleep and mating type. All Analyses and data cleaning was carried out in R ver 4.1.2 with the tidyverse range of packages (Wickham et al 2019), model residuals were checked with the performance package (Lüdecke et al 2021), and summary tables produced with broom (Robinson et al 2022) and kableExtra (Zhu 2020).

# Results Section:
# I tested the hypothesis that sexual activity is costly for male Drosophila melanogaster fruitflies. Previous research indicated that sleep deprived males are less attractive to females, this would indicate that levels of sexual activity might be affected by sleep and impact the effect on longevity, as such this was included as an interaction term in the full model. Body size is also known to affect lifespan, as such this was included as a covariate in the mode.

# There was a small interaction effect of decreased lifespan with increasing sleep in the treatment groups compared to control in our samples, but this was not significantly different from no effect (F2,118 = 0.512, P = 0.6), and was therefore dropped from the full model

# There was a significant overall effect of treatment on male longevity (Linear model: F2,120 = 30.1, P < 0.001), with males paired to virgin females having the lowest mean longevity (48 days, [95%CI: 44.9 - 51.2]) (when holding body size and sleep constant), compared to control males (61.3 days [56.8 - 65.8]) and males paired with inseminated females (64.9 days [61.8 - 68.1 days]).

# Post hoc analysis showed that these differences were statistically significant for males paired with control females compared to the inseminated (Tukey test: t120 = 4.8, P < 0.001) and virgin groups (t120 = 7.5, P < 0.001), but there was no overall evidence of a difference between inseminated and virgin groups (t120 = -1.309 P < 0.3929)

# Comparing the treatment effects against other predictors of longevity such as body size and sleep, I found that sleep had a very small effect on longevity (mean change -0.05 days [-0.18 - 0.07]) which was not significantly different from no effect (Linear model: F1,120 = 0.68, P = 0.41). Body size (taken from thorax length) was a significant predictor of longevity (F1,120 = 121, P < 0.001), with each 0.1 mm increase in body size adding 14.4 days to the individual lifespan [11.8 - 17]. It appears as though body size has a stronger effect on longevity than treatment, indicating that while there is a measurable cost of sexual activity to males, it may be less severe than in females (not compared here), and less severe than other measurable predictors.

