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


