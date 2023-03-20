#____________________----
# Packages ----
library(tidyverse)
library(rstatix)
library(performance)

#___________________----
# Import Data ----
biomass <- read_csv(here("data", "biomass.csv"))

#___________________----
# Tidy data ----
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
# We can see an increase in biomass with the addition of light (30.12g) but with a standard error of 32.72g, further analysis of confidence intervals reveals that they go past 0 (-35.3 - 95.6) and so there is no real confidence that this increase of biomass is of true effect.
# In contrast the addition of fertiliser shows a mean increase of 93.7[28.2-159 95% CI] and the addition of both showing a mean increase of 219 [154-285 95% CI].
broom::tidy(ls_1, conf.int = T)

GGally::ggcoef_model(ls_1,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)
# can see that for the addition of light, the lower CI goes past 0.

# combine the average mean differences of the light effect and fertiliser effect
coef(ls_1)[2] + coef(ls_1)[3] 

# compare this to the average difference of the combined treatment
coef(ls_1)[4]

# we would expect that adding the mean difference caused by light and the mean difference caused by fertiliser would be close to the combined mean difference shown in column 4 (F+L+)
# as we can see 123.8 and 219.2 do not match
# This suggests there may be a positive interaction (that light and fertiliser treatments produce a sum effect that is greater than could be predicted by looking at their individual effects).
# The one-way ANOVA only tells us if there is a difference between the control group and the combined group but it does not tell us the strength of the interaction effect (combined F+ L+), i.e. is their a true interaction, how confident are we, and how does the strength of the intercation effect differ to the main effects (addition of F/L on their own)
# To test for interactions, we use a factorial design

#__________________________----
# Factorial Design ----

biomass %>% ggplot(aes(x=fert, y=biomass_m2, colour = light, fill = light, group = light))+
  geom_jitter(width=0.1) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 3,
    shape = 23
  )+stat_summary(
    geom = "line",
    fun = "mean",
    size = 1, linetype = "dashed"
  )


#if we suspect an interaction term we add this as Light:Fert to our linear model

ls_2 <- lm(biomass_m2 ~ fert + # main effect
             light + # main effect
             fert:light, # interaction term
           data = biomass)

summary(ls_2)
# The fourth line now indicates how much of an effect the two factors interacting changes the mean. Also note the standard error is larger, there is less power to accurately estimate an interaction over a main effect.
# indicates that the size of the interaction effect is around 95.4g
# If combining the light and fertiliser treatments produced an equivalent biomass as whe they are added alone, yu woud expect this to be 0g.
# Instead it is 95g more than we would expect from the additive effects alone.
GGally::ggcoef_model(ls_2,
                     show_p_values=FALSE,
                     signif_stars = FALSE,
                     conf.level=0.95)

# in order to work out the estimated biomass for the treatment of light and fertiliser we must sum the additive effects of Light+ Fert and the interaction effect Light:Fert.
# model 1
coef(ls_1)[4]

# model 2
coef(ls_2)[2] + coef(ls_2)[3] + coef(ls_2)[4]

#______________________----
# ANOVA Tables ----

# we previously ran the anova() function directly on our linear model, but this only works for simple and balanced designs (same number of sample sizes in each level of a factor).
# when working with unbalanced or complex designs (including those where you suspect an interaction effect) you need to use the drop1() function.
# This reports an F statistic for the interaction effect, carrying out an F-test of two models, one with and one without the interaction effect.
# It is a good idea to always use drop1 instead of anova to reduce the risk of human error.
drop1(ls_2, test = "F")

#This F-test is testing the null hypothesis that there is no true interaction effect. The significance test rejects the null hypothesis (just).

#There was an interactive effect of light and fertiliser treatments (ANOVA F1,60 = 4.25, P = 0.044) in which combining treatments produced substantially more biomass (95.4g [95% CI: 2.8 - 188]) than expected from the additive effects alone (Fertiliser 93.7g [28.2 - 159.2], Light 30.1g [-35.3 - 95.6]).

# main effects ----

# If there is interactions in the model, then the drop1 function stops, this is because if this interaction is significant, then the main effects need to be included, even if they are not significant on their own.
# If we decide to include reports of the main effect then estimates and confidence intervals should come from the full model(one-way ANOVA in this ex), but we need to produce an interaction free model to produce accurate F-values (especially for unbalanced designs)

# we have to remove the interaction term before we can keep using drop1()

ls_3 <- lm(biomass_m2 ~ fert + light, data = biomass)

drop1(ls_3, test = "F")

# provides more accurate F values for the main effects

#_____________________----
# Balanced/Unbalanced Designs ----

# When designs are not balanced then the order matters when we use anova() - this is because the sum of squares is calculated sequentially (in the order of the formula), and so we could get different results depending on the order in which we assemble predictors into our model.

# make three vectors and combine them into a new tibble

#________________________----
# Activity 1: Sum of Squares ----
height <- c(50,57,91,94,102,110,57,71,85,105,120)
size <- c(rep("small", 2), rep("large", 4), rep("small", 3), rep("large", 2))
treatment <- c(rep("Control", 6), rep("Removal", 5))

unbalanced <- tibble(height, size, treatment)

unbalanced

ls_4 <- lm(height ~ size + treatment, data = unbalanced)
anova(ls_4)

ls_5 <- lm(height ~ treatment + size, data = unbalanced)
anova(ls_5)
# when comparing the two models, using the same data and predictors but due to the different order, the conclusions are different
drop1(ls_4, test = "F")

drop1(ls_5, test = "F")

# does not matter what order the terms are for drop1

#_____________________----
# post hoc ----

emmeans::emmeans(ls_2, specs = pairwise ~ light + fert + light:fert) %>% 
  confint()
# including the argument pairwise in front of the ~ prompts the post-hoc pairwise comparisons.
# $emmeans contains the estimate mean values for each possible combination (with confidence intervals)
# $ contrasts contains tukey test post hoc comparisons between levels

# In this example it is unnecessary to spend time looking at pairwise comparisons between the four possible levels, the interesting finding is to report the strength of the interaction effect. 
# however you can do this using the emmeans:emmeans function

#_____________________----
# Continuous Linear models ----
# Previously looked at interaction between two categorical variables but can also look at interactions between a factor and continuous variable (ANCOVA)
# Activity 2 ----
# import data
pollution <- read_csv(here("data", "pollution.csv"))

# Hypotheses:
# well-watered plants will have a higher yield than stressed plants.
# plants under stress will respond more negatively to pollution than non-stressed plants.

# check the structure of the data
glimpse(pollution)

# check data is in a tidy format
head(pollution)

# check variable names
colnames(pollution)

# check for duplication
pollution %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
# quick summary

summary(pollution)

# check for typos by looking at distinct characters/values
pollution %>% 
  distinct(Stress)


# missing values
pollution %>% 
  is.na() %>% 
  sum()

# Visualization
pollution %>% 
  ggplot(aes(x = O3, y = William))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ Stress)+
  labs(x = expression(paste(Ozone~mu~L~L^-1)),
       y = expression(paste(Log~Yield~(kg~ha^-1))))
# gradient of slope does not seem to change, suspect there is not an interaction effect.

# Linear model
pollution_ls1 <- lm(William ~ O3 + Stress + O3:Stress, data = pollution)

pollution_ls1 %>% 
  broom::tidy(conf.int = T)

# does not seem to be an interaction effect (differs -1.35 kg from mean, but conf intervals cross 0 )

# check model fit

performance::check_model(pollution_ls1)

# everything looks pretty good

# Simplifying the model

drop1(pollution_ls1, test = "F")

# accept the null hypothesis that there is not interaction effect and so we can remove this from the model

pollution_ls2 <- lm(William ~ O3 + Stress, data = pollution) 

drop1(pollution_ls2, test = "F")
# F values of simpler model

pollution_ls2 %>% 
  broom::tidy(conf.int = T)
# estimates and confidence intervals

# Report ----

# I hypothesized that well watered plants would have a greater yield than stressed plants. We found well-watered plants had a greater yield (mean = 0.178 kg [95% CL = 0.068-0.288kg])(F = 11, p = 0.03, df = 1,28). I hypothesized that stressed plants would respond more negatively to pollution than well watered plants. However we found no evidence of a negative interaction effect between stress and ozone levels (F = 0.4612, p = 0.5 df = 1,27). Although individually the plants suffered a mean decrease in yield of 7.14 kg ha-1 [5.13 - 9.15] (F1,28 = 53, P < 0.001) for every unit increase (μL L-1) of Ozone. 
