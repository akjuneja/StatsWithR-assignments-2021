### Stats with R Exercise sheet 5

##########################
#Correlation and Regression
##########################


###########################################################################################
###########################################################################################

library(languageR)
library(ggplot2)
library(dplyr)

#######################
### Exercise 1: Correlation
#######################

## a) We will again use lexdec from languageR. This week, we will look at the variables 
##  RT, FreqSingular, Trial, Frequency, and Length. Create a dataset called data with just
##  those variables

data = select(lexdec, c('RT', 'FreqSingular', 'Trial', 'Frequency','Length'))


## b) Take a look at the data frame.
glimpse(data)
summary(data)

## c) Let's say you're interested in whether there is a linear relationship between 
## Frequency and Length 
## Take a look at the relationship between the frequency and length by 
## means of a scatter plot (use the ggplot library for this).
ggplot(data, aes(x = Length, y = Frequency)) + geom_point()

## d) Judging from the graph, do you think that word frequency and word length are 
## in any way correlated with one another?
## A: If we look at the graph, we notice that the words with more length (> 8) have very
##    less frequency.

## e) We are also interested in correlations between RT and all other variables in
## data. Get the correlations between all variables in the data set using cor().
## Tell R to only include complete pairs of observations. Is the correlation between
## Frequency and Length like you expected?
cor(data, use = "pairwise.complete.obs")
## A: We notice that the variables Frequency and Length are negatively correlated.
##    That is, as the length of the words increase, their frequency to occur decreases.
##    This matches our expectation.

## f) What is the range of a correlation, what is the meaning of a correlation of 0,
## 1, and -1 respectively?
## A: The computed Pearson correlation ranges from -1 to +1. 
##    -1 value denotes a perfect negative correlation. (negative direction)
##     0 value denotes no correlation between those variables.
##    +1 value denotes a perfect positive correlation. (positive direction)

## g) Going back to the correlation matrix obtained above, please describe the correlations
##  between RT and the other variables in words.
## A: We notice a positive correlation between RT and Length with a small effect size. 
##    That is, as the length of the words increase, the value of RT increases with it.
##    Since their Pearson correlation is 0.155, the effect size is small.
##    In case of FreqSingular, Trial and Frequency w.r.t. RT, there is a negative 
##    correlation between them. That is, as the value of RT increases, the other variable
##    value decreases and vice-versa. Also, the effect size of aforementioned 3 variables 
##    have small effect size.


## h) Is the correlation between RT and FreqSingular significant? Use cor.test()
cor.test(data$RT, data$FreqSingular)
## A: We notice that the p-value of small (1.149e-08). Thus we have to reject the null
##    hypothesis and accept the alternate hypothesis, that is, 

## i) Calculate the Spearman rank correlation between RT and FreqSingular. 
## What do you observe?
cor(data$RT, data$FreqSingular, method = "spearman")
## A: The Spearman correlation is computed to be -0.2287. This gives us the strength 
##    of the relationship between the variables and its direction of association, that
##    is in our case, negative. 

## j) To get a better overview, we will create a plot between FreqSingular and 
## the mean RT per FreqSingular level.  
## Use group_by() and summarize() to obtain mean RTs by FreqSingular.
## Make a scatter plot between the two variables of the resulting data set.
mean_RT = mean(data$RT)
data %>% group_by(data$FreqSingular) %>% summarise(mean_RT)
ggplot(data, aes(x= mean_RT, y=FreqSingular)) + geom_point()

## k) Looking at the graph, why do you think Spearman's rho is better suited than the Pearson 
## correlation to describe the relationship between the two variables?
## A: mean_RTs is a monotonously non-linear decreasing function with respect to FreqSimilar. Therefore,
##    Spearman's rho metho will get a better estimate of correlation value compared to Pearson's correlation method. 

## l) Calculate Kendall's tau for the same relationship. 
cor(x = data$RT, y = data$FreqSingular, method = "kendall")

## m) Is there any reason to prefer this correlation measure in the current context? 
##  In general, in what contexts would you use Kendall's tau?
## A: The Kendall's correlation is -0.156, which suggests negative correlation between
##    RT and FreqSingular.

################################
### Exercise 2: Regression
################################


## a) Read in the data set lexicalDecision2.csv provided on cms and turn the variable Word 
##  into a factor. This data set is similar to the one used above in that it looks at lexical decision 
##  times for different words with the explanatory variables Frequency, Length and SynsetCount.

lexicalDecision <- read.csv("lexicalDecision2.csv")
lexicalDecision$Word <- factor(lexicalDecision$Word)

## b) First, we will investigate the relationship between meanRT and Length, which gives the length 
##  of the word in letters. Make a scatter plot of meanRT and Length (as always: ggplot). You can use
##  geom_jitter() to avoid overplotting
ggplot(lexicalDecision, aes(x=meanRT, y=Length)) + geom_jitter()

## c) Run a regression model with meanRT and Length and look at the summary.
## General form: 
## "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
## "summary(modelname)"
lm.fit <- lm(meanRT ~ Length, data = lexicalDecision)
summary(lm.fit)

## d) Interpret the model from c. What do intercept and the coefficient of Length tell you?

# The coefficients obtained are significant which is prompted by the p-values of each coefficient. 
# All these p values are much less than 0.05. Also, the coefficient of Length variable is positive(0.0194). This means
# that mean_RT increases by 0.0194 with unit increase in Length.

## e) What about the model fit: What proportion of the total variance is explained by your model?

# R squared value = 0.1773. This means that only 17.73% of the total variance is explained by the model.

## f) Now let's turn to the relationship between meanRT and Frequency. Run the regression and 
## interpret.
lm1.fit <- lm(meanRT ~ Frequency, data = lexicalDecision)
summary(lm1.fit)

# The coefficients obtained are significant which is prompted by the p-values of each coefficient. 
# All these p values are much less than 0.05. Also, the coefficient of Frequency variable is negative(-0.0342). 
# This means that mean_RT decreases by 0.0342 with unit increase in Frequency. 

# R squared value = 0.2877 This means that only 28.77% of the total variance is explained by the model.

## g) Plot meanRT by Frequency and add a regression line to your plot

predicted_df <- data.frame(meanRT_pred = predict(lm1.fit, lexicalDecision), Frequency=lexicalDecision$Frequency)

ggplot(lexicalDecision, aes(x= Frequency, y = meanRT)) +
  geom_point() +
  geom_line(color='red',data = predicted_df, aes(x=Frequency, y=meanRT_pred))


## h) Redo the plot, but instead of points, plot the Word value.
## Do you think there are any "bad" outliers, i.e. highly influential data points in your data set? 

ggplot(lexicalDecision, aes(x= Frequency, y = meanRT)) +
  geom_point() +
  geom_text(
    label= lexicalDecision$Word,
    nudge_x = 0.05 
  ) +
  geom_line(color='red',data = predicted_df, aes(x=Frequency, y=meanRT_pred))

# yes, there are outliers in the data - eggplant, vulture can be considered as outliers.

## i) Rerun the model excluding the data point for the word "egplant". Compare the results.

filtered_lexicalDecision <- lexicalDecision %>% filter(Word != "egplant")
lm2.fit <- lm(meanRT ~ Frequency, data = filtered_lexicalDecision)
summary(lm2.fit)

## j) Given the difference between the two models and the peculiarities that you observe for this 
## data point, would you exclude this data point from further analysis?

# Yes, this data can be excluded as it appears to be some erroneous data entry with frequency = 0. 
# Also it's hampering the accuracy of the model.


###################################
### Exercise 3: Multiple Regression
###################################

## We will use the same data set as in 2. This time, we will look at the effect of Frequency and Length
## on meanRT simultaneously. 

## a) Run a multiple regression model with Frequency and Length as predictors
## General form: 
## "modelname <- lm(outcome ~ predictor1+predictor2+.., data = dataFrame, na.action = an action)"
## "summary(modelname)"
lexical_data <- read.csv("lexicalDecision2.csv")
multiple_regression <- lm(lexical_data$meanRT ~ lexical_data$Frequency + lexical_data$Length, data = lexical_data)
summary(multiple_regression)
## b) Interpret the model: what do intercept and the 2 coefficients tell you? What about significance?
# Intercept value is 6.452. This indicates the value of meanRT when frequency and length both are 0
# From the coefficients we can say that, Frequency is negatively corelated and length is poitively co related.
# For one unit change in Frequency when length is 0, meanRT on an average changes by -0.0277 units
# For one unit change in Length when frequency is 0, meanRT on an average changes by +0.0108 units
# Both the variables have p values less than alpha values 0.05 and are hence significant for the prediction of meanRT
# Also from the F-statistic test and p-value is significant. Hence we can say the vaiables are significant for meanRT prediction.
## c) Compare to the model in 2c (only including Length), has the model fit improved? How about
## the model in 2f (only including Frequency)?
# The multiple regression model has RSE= 0.07119 and Adjusted R2 = 0.3153
# The model 2c has RSE = 0.07854 and Adjusted R2 = 0.1666
# The model 2f has RSE = 0.07308 and Adjusted R2 = 0.2784
# Hence the multiple regression model has RSE less than 2c and 2f as weel has higher R2 than 2c and 2f. Hence,
# it provides better accuracy and explains more variance. Hence is a better fit.
## d) Using the model from 3 a: What is the predicted meanRT for the word "giraffe", which has a Frequency 
## of 3.33. Calculate "by hand", i.e. do not use predict() and show your calculation.
# meanRT("giraffe") = 6.452 - (0.027795 * 3.33) + (0.010814 * 7) = 6.435