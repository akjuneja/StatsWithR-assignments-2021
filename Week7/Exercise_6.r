### Stats with R Exercise sheet 6

##########################
# ANOVA
##########################

## This exercise sheet contains the exercises that you will need to complete and 



#######################
### Exercise 1: Preparation
#######################


library(boot)
library(ggplot2)
library(dplyr)
library(car)
library(reshape)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, 
# Cambridgeshire County Council considered 14 pairs of locations. The locations were 
# paired to account for factors such as traffic, volume and type of road. One site in 
# each pair had a sign erected warning of the dangers of speeding and asking drivers 
# to slow down. No action was taken at the second site. Three sets of measurements 
# were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the 
# erection of the sign, shortly after the erection of the sign, and again after 
# the sign had been in place for some time.

## a) For the further reference please use ?amis. 
## It may take some time to understand the dataset. 
?amis

## b) Load the dataset, store it into a variable called "data", and briefly inspect it. 
## Feel free to make some plots and calculate some statistics in order to understand 
## the data.
data <- amis
str(data)

## c) All our columns have numeric type. Convert the categorical columns to factors.
# Following are categorical columns: period, warning, pair
data$period <- factor(data$period)
data$warning <- factor(data$warning)
data$pair <- factor(data$pair)

## d) Plot boxplots for the distribution of `speed` for each of the `period` values 
## (before, immediately after and after some time). Build 2 plots (each containing 3 
## boxplots) side by side depending on the `warning` variable.
## (For all plots here and below please use ggplot)
data %>% filter(warning == 1) %>% ggplot(aes(x=period, y=speed)) + 
  geom_boxplot()

data %>% filter(warning == 2) %>% ggplot(aes(x=period, y=speed)) + 
  geom_boxplot()

## e) What can you conclude looking at the plots? What can you say about people's 
## behaviour in different periods: before, immediately after and after some time?

# Warning sign value 1 (warning sign is erected):
# We can observe that speed value decreases immediately after warning sign is placed (period2) but after some time 
# (period 3) there is again increase in speed values of car observed. So people are vigilant and careful when speed 
# warning sign is placed but again after some time they become careless and don't give attention to it and 
# drive fast.

# Warning sign value 2 (warning sign is not erected):
# We can observe that in sites where no sign was placed, we have more speed values during period 2 
# than period 1 and slight increase in period 3 also.

## f) What are your ideas about why the data with warning==2 (sites where no sign was 
## erected) was collected?
# The data was collected with warnings==2 to check how general speed observations will be during different
# periods even if there are no warning signs erected.

#######################
### Exercise 2: 1-way ANOVA
#######################

## a) First let's create a new data frame which will be used for all exercise 2.
## For the 1-way ANOVA we will be working with a subset of `amis` using only the 
## data for sites where warning signs were erected, which corresponds to warning==1. 
## Therefore first subset your data to filter out warning==2 and then apply group_by() and summarize() 
## to average "speed" over each "pair" and "period". 
## Assign this new data frame to the variable casted_data.
casted_data <- filter(data, warning == 1) %>% group_by(pair, period) %>%
  summarize(avg_speed = mean(speed))

## b) Build boxplots of the average speed depending on "period".
ggplot(casted_data, aes(x=period, y=avg_speed, fill = "period")) + 
  geom_boxplot()+labs(title = "Average speed for each period")

## c) Looking at the boxplots, is there a difference between the periods?
# A: By looking at the boxplots, we can say that average speed in period 2 is greater than period 1
#    and avergae speed in period 3 is greater than both 1 & 2.

## d) Now we are ready to perform 1-way ANOVA: please use the function aov() on the 
## speed depending on the period and assign the result to aov1way
aov1way = aov(avg_speed~period, data = casted_data)

## Before we interpret the results, let's check the ANOVA assumptions and whether 
## they are violated or not and why.

## e) Independence assumption
## (Figure out the best way to check this assumption and give a detailed justified 
## answer to whether it is violated or not.)
# A: Each observation from the given data set, is from a distinct pair of locations in 
#    order to be independent from each other. Thus the independence assupmtion is not
#    violated.

## f) Normality of residuals
##  First add the residuals to your casted data set, you find them in model$residuals
##  next, make a qqplot (using qqnorm() or geom_qq() ina ggplot) for the residuals and 
##  run the shapiro wilk test.
casted_data$residuals = residuals(aov1way)
ggplot(casted_data, aes(sample=residuals)) +  geom_qq() + geom_qq_line()
shapiro.test(residuals(aov1way))

## g) What do you conclude from your results in f?
## A: The p-value is 0.1662 so we do not reject the null hypothesis, i.e., we have a 
##    normal distribution since the p-value is large enough to satisfy the normality of 
##    residuals assumption.

## h) Homogeneity of variance of residuals
##  First, plot the residuals by period (boxplots) to see whether variance differs between groups
##  Next, run Levene's test using the function leveneTest() (from library car) with the same syntax
##  as aov(). It indicates whether the variance is significantly different between groups (= not
##  homogeneous).
library(car)
ggplot(casted_data, aes(x=period, y=residuals)) + geom_boxplot()
leveneTest(avg_speed~period, data=casted_data)

## i) What do you conclude from your results in h?
## A: The p-value is computed to be 0.8383, which means the assumption homogeneity 
##     of variance holds.

## j) Now we turn to the results. Look at the summary of aov1way
summary(aov1way)

## k) State your conclusion
## A: The p-value is computed to be 0.382, i.e, we cannot reject the null hypothesis.
##    The f-value is 0.986, this indicates that there is a clear difference between
##    means of the two groups.

## l) Please do pairwise t-tests of the same variables as in d) using pairwise.t.test().
pairwise.t.test(casted_data$avg_speed, casted_data$period, p.adjust.method = "none")

## m) Try to use no adjustment for pairwise testing and then the Bonferroni correction.
pairwise.t.test(casted_data$avg_speed, casted_data$period, p.adjust.method = "bonferroni")

## n) If the results change  in m, why do they? What does Bonferroni correction do?
## A: Yes, the results vary from that of the previous approach. The Bonferroni correction has 
##    inflated the previously present Type 1 error. While conducting multiple statistical
##    tests, there is an increasing risk of Type 1 error. The Bonferroni correction basically
##    adjusts the p-values to prevent misinterpretations.

#######################
### Exercise 3: 2-way ANOVA
#######################
## a) Now we want to analyze the influence of 2 categorical variables 
## (period and warning) on the speed.
## So let's turn back to our initial dataset amis (not its subset with warning==1).
## First, we need to average the speed over each `pair`, `warning` and `period
## Cast your data again and assign the results to casted_data2.
casted_data2 = cast(amis, period + warning + pair ~., mean, value = "speed")
names(casted_data2)[4] = "avg_speed"

## b) State the main difference between the applicability of 1-way and 2-way ANOVA.
# The main difference is in the number of independent variables. 
# One way has 1 independent variable where as 2 way has two independent variable.

## c) Do you think, we need to include an interaction term in the ANOVA?
# Yes, we need to include interaction term in the 2-way ANOVA because we have two predictors
# and impact of one predictor depends on the level of the other predictor.

## e) Now apply the 2-way ANOVA: please use the function aov() with mean speed as the
## dependent variable, period and warning as predictor (independent) variables and depending on your
## answer in c) either including an interaction term, or not.
aov2way = aov(avg_speed ~ period * warning, data = casted_data2)
summary(aov2way)

## f) Report the p-values and interpret the results in detail. Properly formulate the findings
##  with regard to the research question!
# Period has p-value 0.18 and Warning has p-value 0.00463
# p-value of period is large and p-value of Warning is very small 
# Hence Warning is significant.
# With respect to the research question we can say the drivers speed depends on the warning but 
# not on the period. Period is not adding anything significant to the result