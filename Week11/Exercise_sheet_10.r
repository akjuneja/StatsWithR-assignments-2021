### Stats with R Exercise sheet 10

####################################################
# Week 11: Model Selection, Transformations, Power
####################################################


###############################################################################
###############################################################################

# The following line of code clears your workspace.

rm(list = ls())


#########################################################################
### Exercise 1  Simplifying random effect structures
#########################################################################

library(lme4)
library(languageR)

##  Using the lexdec data set again, you want to fit the model that tests for effects of Frequency, the type of the 
##  previous Word and the native language of the participant:

m = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(PrevType+Frequency|Subject) + (PrevType+NativeLanguage| Word), lexdec, REML=F)

## a) Unfortunately, the maximal model given above gives a warning that indicates that the model is too complex for the data.
##  In order to get a model that converges without warnings, try to use backwards selection on the random effects. 
##  First exclude the random effect that is least contributing to the model fit and so on (this may require multiple 
##  steps and a large number of fitted models!). Use model comparison to decide which effects
##  can be excluded.
##  You may exclude random effects only, if they don't contribute significantly with alpha set to 0.1
m1 = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(PrevType|Subject) + (PrevType+NativeLanguage| Word), lexdec, REML=F)
anova(m, m1)

m2 = lmer(RT ~ PrevType+  Frequency+ NativeLanguage+(PrevType+Frequency|Subject) + (NativeLanguage| Word), lexdec, REML=F)
anova(m, m2)

m3 = lmer(RT ~ PrevType + Frequency + NativeLanguage + (Frequency|Subject) + (PrevType+NativeLanguage| Word), lexdec, REML=F)
anova(m, m3)

m4 = lmer(RT ~ PrevType+  Frequency+ NativeLanguage + (PrevType+NativeLanguage| Word), lexdec, REML=F)
anova(m4, m)

step(m, direction = "backward")

## b) Comment on your result in a): were you able to produce a suitable model without convergence problems?


## c) Another approach is to simplify the random effect structure by excluding correlations. 
##  Try out whether this would have solved the problem.


#########################################################################
### Exercise 2  Simulations and power
#########################################################################

## In the following we provide you with code for simulations. The goal of the exercise is for you to try out
## the code and understand what it does.
## Please always execute the code at least 5 times for each subquestion, to see how stable the results are 
##  -- this is necessary because we are sampling the data randomly, so it could be that we sometimes get more or 
##  less "lucky" draws. 


n <- 200 # number of observations to be simulated
predA <- rnorm(n, 80, 20)
predB <- rnorm (n, 30, 30)
interact <- 0.02*(predA*predB) 
error <- rnorm (n, 0, 50)

resp <- 32 + 0.02*predA - 2.4*predB + interact + error

d <- data.frame(predA, predB, resp)

## a) Write down what values you would hope for the model to estimate in the ideal case:
##   i)  intercept= 32
##   ii) predA= 0.02
##   iii)predB= 2.4
##   iv) predA:predB= 0.02 

m1<- lm(resp~predA*predB, data=d)
summary(m1)  

## b) Can the model recover the original model structure and estimate correct coefficients 
##  for the predictors?
# The model cannot recover the original model structure and cannot estimate correct coefficients for the predictors
# as we can see from the summary of m1 that estimated coefficients do not match the original values.

## c) What happens if you change the number of subjects? (specify the numbers you tried out)
# If we increase the number of subjects (n=400, 500, 600, 700) then we move closer to estimating correct coefficients for the 
# predictors but our adjusted R-squared value decreases. 

## d) What happens if you change the variance of the error term? (specify the numbers you tried out)
# If we reduce the variance of error term then residual standard error decreases and adjusted R-squared value 
# increases and vice-versa.
# Tried for sd = 20, 40, 50, 70, 80 

## e) What happens if you change the effect sizes?
# We change effect size by changing the sd for both predA and predB. 
# If we decrease sdA(10) and keep sdB(30) as it is then we observed decrease in both residual standard error 
# and adjusted R-squared. 
# If we keep sdA and sdB equal to 20 then we observe that residual standard error is almost same but there is 
# decrease in adjusted R-squared value.
# If we increase sdA(50) and decrease sdB(20) then we observe that residual standard error is almost same but 
# there is increase in adjusted R-squared value.

##  Next we include the above code into a loop to calculate the power of the experiment 

# number of simulated data sets
sim = 1000
n = 100
# results matrix
results = matrix(nrow=sim, ncol=4)
colnames(results) <- c("Intercept", "predA", "predB", "interaction")
for(i in c(1:sim)){
  predA <- rnorm(n, 80, 20)
  predB <- rnorm (n, 30, 30)
  interact <- 0.02*(predA*predB) 
  error <- rnorm (n, 0, 50)
  resp <- 32 + predA - 2.4*predB + interact + error
  d <- data.frame(predA, predB, resp)
  m1<- lm(resp~predA*predB, data=d)
  # store the resulting p-values in the results matrix
  results[i,] = summary(m1)$coefficients[,4]
}

## f. We use the above code and the results matrix to calculate power. Recall that the power is 
##  the probability of rejecting the Null hypothesis, given a specific effect size.
##  We can approximate this by calculating the proportion of simulated datasets, 
##  where the effect comes out significant, i.e. below 0.05. 
##  Calculate the power based on the simulations for all three effects of interest 
##  (i.e., predA, predB and the interaction) individually.
a = 0.05
power_predA = sum(results[,2] < a)/(1000)
power_predB = sum(results[,3] < a)/(1000)
power_interaction = sum(results[,4] < a)/(1000)

## g. How does power change when you decrease your alpha level to 0.01?
# If we decrease the alpha level then value of power across all three effects of interest (predA, predB and the interaction) 
# decreases.


## h. How does power change, when you decrease the number of participants in each simulated data 
##  set to 80? (alpha-level = 0.05)
# There is slight decrease in value of power for all three effects of interest if we decrease the number of 
# participants in each simulated data to 80.
