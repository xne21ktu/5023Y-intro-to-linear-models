# Packages ----
library(tidyverse)
library(rstatix)
library(performance)

lsmodel1 <- lm(height ~ type, data = darwin)

# ANOVA table ----
anova(lsmodel1)
summary(lsmodel1)
# Compared to sumary table, ANOVA has less information in it, as it contains no information about the biology (estimates in difference, or uncertainty such as standard errors etc.) but they can help us simplify effects where there are more than two levels
# The first column contains the information source, here the signal/regression (first row) and error/noise (second row).
# The second column gives the df, in the first row this is the number of predictors(treatments) (k - 1, including intercept), and in the second row the residual degrees of freedom (N - k).
# The third column is the sum of squares, in the first row this is the SSR (sum of square residuals), and the second row SSE (sum of squared error), (remember that SST = SSR + SSE).
# The fourth column is the mean sum of squares ( average variability per treatment level), the first row is MSR (mean sum of sqaure residuals) and the second row is te MSE (mean sum of squared error). MSR = SSR/(k-1) & MSE = SSE/(N-k).
# to generate MSE, the linear model has pooled the SSE from the two treatments, this gives it greater power for detecting effects with smaller sample sizes, but is the reason why we need homogeneity of variance as an assumption of our model.
# The fifth column is our F statistic (the signal-to-noise ratio). MSR/MSE (treatment variance/residual error variance)
# F = 5.9, which means that the estimated signal is nearly six times larger than the estimated noise.

pf(5.9395, 1, 28, lower.tail=FALSE)
# Can use F test (pf function), taking into account sample size and df, to calculate the probability of observing this ratio of signal to noise if the null hypothesis that there were no effect of treatment is true
# The self pollinated maize plants measured an average of 17.6 [16-19.1] (mean[95% CI]) inches high, while the cross-pollinated plants had a mean height of 20.2 [18.6-21.7] inches - a difference of 2.6 [-0.4-4.8] inches (one-way ANOVA: F1,28 = 5.9, P = 0.02).
