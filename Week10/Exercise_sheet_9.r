####################################################
#Week 11: Model Families and Logistic Regression
####################################################




##################################################################################

##################################################################################
## Exercise 1: Logistic regression
##################################################################################

require(carData)
require(dplyr)
require(lme4)
require(ggplot2)

## Look at the dataset TitanicSurvival from the carData package.

## a) Build a simple logistic regression model that models the probability of survival (binary) based on 
##  sex (categorical) and passengerClass (categorical) without an interaction and store it in mSurv. 
##  You have to use the glm() function and specify the family correctly.
mSurv <- glm(survived ~ sex + passengerClass, data = TitanicSurvival, family = binomial(link = 'logit'))

## b) Look at the summary. What group does the intercept correspond to?
# intercept correspond to females belonging to first class 

## c) Were men more likely to survive than women? Is the effect significant?
# No, women were more likely to survive than men. The effect is significant since the p-values(<0.05) are significant

## d) Imagine two passengers: Rose (female, 1st class passenger) and Jack (male, 3rd class passenger).
##  Calculate their expected survival on the logit scale (i.e. the scale of the model) either by hand or 
##  using predict() with a new data.frame
gender <- c("male","female")
pclass <- c("3rd","1st")
test = data.frame(gender,pclass)
names(test) = c("sex","passengerClass")
results = predict(mSurv, newdata = test)

## e) Transform your results from d) to the probability scale, using the formula given on the slides. 
##  You can check your calculation by asserting the probabilities lie in the 0-1 range. For whom does 
##  the model predict the higher probability of survival?

transform_to_prob <- function(logits){
  odds <- exp(logits)
  prob <- odds / (1 + odds)
  return(prob)
}
transform_to_prob(results)
# The model provides higher probability of survival for Rose who is female and 1st class passanger


##################################################################################
## Exercise 2: Generalized Linear Mixed effect models
##################################################################################

## In this exercise, we will again look at connections between coffee consumption and sleep (among others). 
## The data set "coffee.csv" contains data from 10 students, who reported on 10 randomly chosen days of the year: 
##  sleep: how many hours of sleep they had in the previous night
##  mood: how happy they felt on a scale from 1 (very unhappy)-10 (extremely happy)
##  coffee: how many cups of coffee they had on that day
##  In addition, the maximal temperature on that day was entered into the dataset.

## Our research hypotheses are: 
## students consume more coffee, when they are tired
## students consume more coffee, if they are in a bad mood.
## students consume more coffee, when it is cold outside

## a) Download the data set from cms and read it in, store it in a variable called: coffeedat
coffeedat = read.csv("coffee.csv")

## b) Plot the number of consumed cups of coffee in three individual scatterplots by sleep, mood, and temperature. 
##  You can use geom_jitter() to get a nicer plot
ggplot(coffeedat, aes(sleep, coffee)) + geom_point() + geom_jitter()
ggplot(coffeedat, aes(mood, coffee)) + geom_point() + geom_jitter()
ggplot(coffeedat, aes(temperature, coffee)) + geom_point() + geom_jitter()

## c) Can you detect an obvious relationship in any of the plots? Which direction does it have?
# There is no obvious relationship visible in the plots.

## d) fit a simple linear regression model with all three predictors and store it in linmod
linmod = lm(coffee ~ sleep + mood + temperature, coffeedat)
summary(linmod)

## e) fit a generalized linear model with the appropriate family (hint: coffee is a count variable) and
##  store it in poimod
poimod = glm(coffee ~ sleep + mood + temperature, coffeedat, family = poisson())
summary(poimod)

## f) Look at the two summaries, what changed?
# In poimod model, p-value for all the predictors(sleep, mood, temperature) had decreased in comparison
# to linmod.
# For example: p-value for mood in linmoid is 0.011737 wheres in poimodel it is 0.000273.

## g) In fact, we have repeated measures in our design, so refit the model including a random intercept for
##  subject using glmer() with the correct family specification and store it in mixedpoi
mixedpoi = glmer(coffee ~ sleep + mood + temperature + (1|subj), coffeedat, family = poisson())
summary(mixedpoi)

## h) Look at the summary and report what changed in comparison to both linmod and poimod.
# In mixedpoi model, we can observe that p-value of mood (5.96e-05) has decreased significantly in comparison to
# both linmoid and poimod.

## i) Finally, to make it complete, also run a mixed model using the gaussian family and store it in mixedlin
mixedlin = glmer(coffee ~ sleep + mood + temperature + (1|subj), coffeedat, family = gaussian())
summary(mixedlin)

## j) Compare the AIC for all four models. Which one has the best fit?
AIC(linmod, poimod, mixedpoi, mixedlin)
# The model with lower AIC is considered good fit. 
# mixedpoi has lowest AIC and is the best fit model.

## k) And which model is conceptually the appropriate one? Why?
# Conceptually, mixedpoi is the appropriate model because it uses the general linear model 
# including random effects with poisson family. 
# General linear model including random effects is the ideal choice because there are multiple
# observations from same subject so we can not assume observations are independent.
# Choosing family as poisson is ideal because response variable is discrete. 

## l) Finally, report on the effects of interest in light of our research hypotheses specified above for the 
##  model you chose in k)
# We choose mixedpoi model.

# Hypothesis: students consume more coffee, when they are tired.
# Null Hypotheis: students consume more coffee, when they are not tired.
# We reject the null hypothesis because p-value for sleep (0.007) is less than significant value (0.05). 

# Hypothesis: students consume more coffee, if they are in a bad mood.
# Null Hypothesis: students consume more coffee, if they are not in a bad mood.
# We reject the null hypothesis because p-value for mood (5.96e-05) is less than significant value (0.05).

# Hypothesis: students consume more coffee, when it is cold outside
# Null Hypothesis: students consume more coffee, when it is not cold outside
# We did not reject the null hypothesis because p-value for temperature (0.214) is more than significant value (0.05)
