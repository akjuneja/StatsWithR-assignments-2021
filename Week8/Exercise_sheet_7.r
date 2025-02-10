### Stats with R Exercise sheet 7

#############################################################################
#Week8: Checking Assumptions underlying ANOVA and linear regression
#############################################################################


###############################################################################
###############################################################################

## The following line of code clears your workspace.

rm(list = ls())


#############################################################################
### Exercise 1
#############################################################################

#############################################################################
### Please, use ggplot to make plots in all exercises unless specified differently!
#############################################################################


##  We will again be using the lexdec dataset from library languageR. 
##  In sheet 4, we ran a t-test to test for differences in RT after a word vs after a non word.
##  In sheet 5, we looked at correlations between RT and frequency and length. In this sheet we will 
##  combine these analyses, look for interactions and again look at model assumptions
##  and model diagnostics

library(languageR)
library(ggplot2)

## a) Load the dataset lexdec from package languageR and store it in a variable called data
data <- lexdec

## b) Run a simple regression, just including Frequency as predictor and RT as the dependent variable
##  Store it in lm1
lm1 = lm(RT ~ Frequency, data=data)

## c) Report and explain the effect of Frequency
summary(lm1)
# The p-value for Frequency (<2e-16) is less than 0.1% and considered highly statistical. This means Frequency
# can give us reliable estimate for RT.
# The coefficient for Frequency is negative (-0.042872), which means for every one unit of frequency 
# there will be decrease of (0.042872) in RT.

## d) Make a scatterplot of RT by Frequency, including the regression line
ggplot(data, aes(x=Frequency, y=RT)) + geom_point() + geom_smooth(method=lm, se=FALSE)

## e) Next, fit a model including Frequency and PrevType as predictors, store it in lm2
lm2 = lm(RT ~ Frequency+PrevType, data=data)

## f) Report and explain the effects of Frequency and PrevType.
summary(lm2)
# The p-value for Frequency and PrevType model is less than 0.1% and is considered statistically significant.
# This means that this model can give us good estimations.

##  Next we want to plot a model where both predictors are shown. For those we first store the predicted values
## of our model:
 data$RT_pred = fitted(lm2)

## g) Now, plot the original data (RT by Frequency with different colors for PrevType), but use the 
## fitted values (RT_pred) inside geom_smooth() or geom_line(), otherwise, it will display regression lines 
## assuming an interaction
## The resulting plot should show the data points in different colors and two parallel regression lines.
ggplot(data, aes(x=Frequency, y=RT, color=PrevType)) + geom_point() +
   geom_smooth(aes(y = RT_pred))
 
## h) Run a regression model that includes also the interaction between Frequency and PrevType and store it
##  as lm3
lm3 = lm(RT ~ Frequency*PrevType, data=data)
 
## i) Plot the results of the model! (This time no need to specify the pred data set)
ggplot(data, aes(x=Frequency, y=RT, color=PrevType)) + geom_point() +
  geom_smooth(method=lm, se=FALSE)

## j) Report the results of lm3 and interpret with the help of the graph in i)
summary(lm3)
# We can observe that p-value for Frequency and PrevType interaction is less than 5% and is considered 
# statistically significant. This means that interaction of two predictors have a significant 
# effect on our model and the estimations.

## k) Do model checking on your model lm3, i.e. inspect the standard model plots provided by R (no ggplot, 
## see lecture notes for syntax)
par(mfcol=c(2,2))
plot(lm3)

## l) Interpret what you see in k) and possibly suggest further steps
# We observe following plots:

# Residuals v/s fitted: For higher range of fitted values there is very little variance observed in 
#                       residuals values as we can see the plotted line drafting away from dotted line.
#                       But overall, we can say that residuals are evenly distributed.

# Scale-Location: It is similar to previous graph but instead of raw residuals, we have standardized
#                 absolute value of residuals. It also suggests same interpretation as of previous graph.

# Normal Q-Q: We can observe that data points are plotted on the line except for the higher 
#             theoretical quantiles, so residuals are not following proper normal distribution. 

# Residuals v/s Leverage: We can see some data points which have high leverage and high residuals, 
#                         have higher influence on the estimation than other data points. 
 

## m) So, what assumptions are violated in the model as it is? Consider both your results from l and what you know 
##  about the data set from previous analyses.
# Following assumptions are violated in the model:

# Normality of residuals - In normal Q-Q plot we can see that plotted graph is deviating from the straight 
#                          line, hence our assumption of normality of residuals is violated.

# No bad outliers - In residuals vs leverage graph, we can see some points with high leverage and high residuals
#                   which are acting as residuals.

