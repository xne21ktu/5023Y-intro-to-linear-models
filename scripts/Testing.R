# Packages ----
library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom.helpers)

# Students t-test ----
# base r
x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

# tidyverse version
x <- seq(-4, 4, length=100)
z_dist <- dnorm(x)

values <- tibble(x,z_dist)

# map_dfc combines values returned into a dataframe
t <- map_dfc(degf, ~dt(x, .x))
colnames(t) <- degf

combined <- cbind(values,t)

combined %>% 
  pivot_longer(cols=!x, names_to="distribution") %>% 
  mutate(distribution=factor(distribution, levels=c("z_dist", "1", "3", "8", "30"))) %>%  
  mutate(distribution=fct_recode(distribution, "z distribution" = "z_dist", "df = 1" = "1", "df = 3" = "3", "df = 8" = "8", "df = 30" = "30")) %>% 
  ggplot(aes(x=x, y=value, colour=distribution))+
  geom_line(linetype="dashed")+
  theme_classic()

# values for critical t up to 30 df
df <- c(1:30)

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

lsmodel1 <- lm(height ~ type, data = darwin)

broom::tidy(lsmodel1) # summary of model

# first row performs a t-test, it tests a null hypothesis that the intercept (here the mean height of the cross-pollinated plants) is zero.
# the second row of our table is a test we planned to perform, 
# it tests our null hypothesis by comparing the average observed difference in plant heights between cross and self-pollinated plants, 
# it calculates the average difference (estimate), the amount of uncertainty (std. error), 
# then calculates an observed t value and determines the probability of observing an effect of at least this size (at this sample size) if the null hypothesis is true.

tidy_model1 <- broom::tidy(lsmodel1)

tidy_model1[[2,2]] / tidy_model1[[2,3]] # calculates t statistic - same as second row on summary

# Paired t-test ----

#To generate the equivalent of a paired t-test, we simply have to add the factor for pairs to our linear model formula
# base r
lsmodel_darwin <- lm(height ~ type + factor(pair), data = darwin)
summary(lsmodel_darwin)

# tidyverse
darwin %>% 
  mutate(pair = as_factor(pair)) %>% 
  lm(height ~ type + pair, data = .) %>% 
  broom::tidy()

#the intercept is the height of the crossed plant from pair 1:
#The second row now compares the mean heights of Crossed and Selfed plants when they are in the same pair
#rows three to 16 compare the average difference of each pair (Crossed and Selfed combined) against pair 1

#If we ignore the pair comparisons the second row gives us a paired t-test: 
# 'What is the difference in height between Cross and Self-pollinated plants when we hold pairs constant.'

lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(1:2) # just show first two rows
# generates confidence intervals for paired t-test

# When comparing to studen t-test, we can see that estimate of the mean difference is identical 
# but the 95% confidence intervals are now slightly different. 
# So in this particular version we have actually increased our level of uncertainty by including the pair parameter.
m1 <- lm(height ~ type, data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="unpaired")

m2 <- lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="paired")

rbind(m1,m2) %>% 
  ggplot(aes(model, estimate))+
  geom_pointrange(aes(ymin=conf.high, ymax=conf.low))+
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_minimal()+
  coord_flip()

# In this case we had a good reason to include pair in our initial model, 
# there are then simple tests we can do to see if it is safe to remove it, 
# if it doesn't appear to be adding to our explanation of the difference in heights between self and cross-fertilised plants.

# Effect Sizes ----
# When our 95% confidence intervals do not overlap the intercept, 
# this indicates we have difference in our means which is significant at α= 0.05.
# this allows us to talk about the 'amount of difference' between our treatments, 
# the lower margin of our confidence intervals is the smallest/minimum effect size.
# It is not that interesting if a result is statistically significant, but the effect size is tiny.
# We can report that there is at least a 0.43 inch height difference between self and crossed fertilised plants at α= 0.05.


# Type 1 and 2 errors ----
# Statistical tests provide you with the probability of making a Type 1 error (rejecting the null hypothesis incorrectly) in the form of P.
# we know if we set an α= 0.05, that we run the risk of rejecting the null hypothesis incorrectly in 1 in 20 of our experiments (A Type 1 error).
# Type 2 errors = Keeping the null hypothesis, when we should be rejecting it? Or not finding an effect.
# The probability of making a Type 2 error is known as 1−β, where βrefers to your statistical 'power'.
# On the other side of the coin is experimental power - this is strength of your experiment to detect a statistical effect when there is one. 
# Power is expressed as 1-β. You want beta error typically to be less than 20%. 
# So, you want a power of about 80%. That is you have an 80% chance of finding an effect if it's there.

# Repeatability ----
# It is not possible for you to know from a single experiment whether you have made Type 1 or Type 2 errors.
# over time as experiments are eventually repeated the literature builds up allowing us to synthesise the evidence.

set.seed(1234)

myList <- vector("list", 20)
y <- tibble()

for (i in 1:length(myList)) { 
  
  x <-  rnorm(n=12, mean=2.6, sd=2.83)
  data <- tibble(x)
  temp <- lm(x~1, data=data) %>% 
    broom::tidy(conf.int=T) 
  y <- rbind(y,temp)  
  
}

y$`experiment number` <- rep(1:20)

# the new dataframe y contains the results of 20 new experiments
y %>% 
  mutate(`p value < 0.05` = if_else(p.value > 0.049, "non-significant", "significant")) %>% 
  group_by(`p value < 0.05`) %>% 
  summarise(`number of experiments`=n())
# nearly a third of the experiments did not find a statistically significant difference.
# A less formal review of the research might tally these P-values and conclude that there are inconsistent results in the literature.
# A better way would be to look at the estimates and calculated confidence intervals.

y %>% 
  ggplot(aes(x=`experiment number`, y=estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax=conf.high))+
  labs(y = "Estimated mean effect of outcrossing")+
  geom_hline(linetype="dashed", yintercept=0.05)+
  theme_minimal()
# it is clearer to see that the results are not really inconsistent, 
# the negative effects of inbreeding depression are clear to see in all of the experiments,
# we are simply observing the effect of sampling error.
# All 20 studies showed the effect of inbreeding depression, and all the experiments have identical levels of uncertainty.