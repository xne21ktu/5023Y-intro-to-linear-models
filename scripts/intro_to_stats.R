#Intro to Stats----

library(tidyverse)
library(here)
library(kableExtra)

darwin <- read_csv(here("data", "darwin.csv"))
                   
#Data Analysis----

# check the structure of the data
glimpse(darwin)

# check data is in a tidy format
head(darwin)

# check variable names
colnames(darwin)


# clean up column names

darwin <- janitor::clean_names(darwin)

# check for duplication
darwin %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE))

# check for typos by looking at distinct characters/values

darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)

# missing values
darwin %>% 
  is.na() %>% 
  sum()

# quick summary

summary(darwin)

#Data Visualisation----

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_point()

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height)) # tells us the mean and SD for btoh groups

# make a new object for summary stats
darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# make a summary plot
darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

# use kable extra functions to make a nice table (could be replaced with kable() if needed)
darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

# Estimations----
# estimating mean differences in height of two groups and our confidence in these differences

# pivot data to wide format then subtract Selfed plant heights from Crossed plant heights
darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)

difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary

difference_summary %>% 
  mutate(se= sd/sqrt(n)) # calculate standard error as a step in estimating our confidence in differences

# the average difference in height was 2.62 ± 1.22 inches (mean ± SE).

# Create a sequence of 100 equally spaced numbers between -4 and 4
x <- seq(-4, 4, length=100)

# create a vector of values that shows the height of the probability distribution
# for each value in x
y <- dnorm(x)

# plot x and y as a scatterplot with connected lines (type = "l") and add
# an x-axis with custom labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))

# calculate confidence intervals

lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI

# common mistake it to state that we are 95% confident that the 'true' mean lies within our interval.
# technically it refers to the fact that if we kept running this experiment again, 
# and again the intervals we calculate would capture the true mean in 95% of experiments. 
# So really we are saying that we are confident we would capture the true mean in 95% of our experiments.
# The maize plants that have been cross pollinated were taller on average than the self-pollinated plants, 
# with a mean difference in height of 2.62 [0.18, 5.06] inches (mean [95% CI]).


