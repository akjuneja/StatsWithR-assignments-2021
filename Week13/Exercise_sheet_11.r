### Stats with R Exercise sheet 11

##########################
#Week 13: Bayesian statistics 2
##########################


###############################################################################
###############################################################################

# The following line of code clears your workspace.

rm(list = ls())


########################################
### Exercise 1
########################################


##  We will again be using the lexdec dataset from library languageR. 
##  In sheet 6, we ran a multiple regression model, which we will now repeat as a Bayesian 
##  analysis using the package brms.

## a) Load the dataset lexdec from package languageR and store it in a variable called data

library("languageR")
data = lexdec


## b) Load the package brms

#install.packages("brms")
library("brms")


## c) Fit a (frequentist) linear model of RT including Frequency and PrevType as predictors, store it in lm1

lm1 = lm(RT ~ Frequency * PrevType, data = data)


## d) Fit the same model as a Bayesian regression using the function brm() and using only defaults (you don't need
##  to specify priors or fitting parameters like chains and iterations). Store it in bm1

bm1 = brm(RT ~ Frequency * PrevType, data = data)


## e) Look at the summaries of bm1 and lm1

summary(lm1)
summary(bm1)


## f) How do the parameter estimates compare?

# We have almost same estimated values of all the coefficients(parameter) of both model lm1 and bm1. 


## g) store the posterior samples of b_Frequency in the variable ps_freq. Use the function as_draws_df()'

ps_freq = as_draws_df(bm1)$b_Frequency 
ps_freq


## h) Your colleague claims that the effect of frequency has to be smaller (meaning more negative) than -0.03.
##  What is the probability of the frequency effect being more negative than -0.03 given your posterior samples?
##  Do you agree with your colleague?

prob = sum(ps_freq < -0.03) / length(ps_freq)
prob
# Probability of having frequency effect more negative than -0.03 is 69.65%.
# Yes, I agree with my colleague because we have high percentage of having more negative frequency effect. 


## i) Derive 95% and 80% credible intervals from ps_freq. Compare to the results above.

ci95 <- quantile(ps_freq, probs = c(0.025, 0.975))
ci95
# ci95 = [-0.04584152, -0.02096935]

ci80 <- quantile(ps_freq, c(0.10, 0.90))
ci80
# ci80 = [-0.04154734, -0.02540221]

# ci95 align almost with the summary results of bm1

## j) What is the meaning of a credible interval compared to the confidence interval in the frequentist's approach?

# credible interval - Probability that the true value is within a range

# confidence interval - Probability that a range contains the true value


## k) Plot the model using the default function, this will give you the posteriors of the model parameters
##   as well as the trace plot, which give you an indication of the convergence of your model. The trace 
##   plot is supposed to look like a "fat hairy caterpillar", i.e. the different chains should not be 
##   separated in any part of the plot and there should not be a general pattern. Is this the case?

plot(bm1)
# Yes, this is the case.


## l) We want the model to run quicker. Change the settings such that each chain only has 180 iterations with 1/4 of
# them as warmup. Store the result in bm2 and look at summary and trace plots. Use the provided seed to be able to 
# better compare your results (or try a different one, but provide it together with your answer!)

set.seed(1111)
bm2 <- brm(RT ~ Frequency * PrevType, data = data, chains=4, iter=180, warmup=45)
summary(bm2)
plot(bm2)


## m) Do you think reducing the iterations was a good idea? Give reasons!

# No,reducing the iterations was not a good idea because chains in some parameters did not converge.  


## n) Another colleague of yours said 2 months ago to you that the effect of frequency is most likely at -0.01 +-0.005
##  Use these numbers for a normal prior of Frequency (with 0.005 as sd). Assign the model to bm3.

bm3 <- brm(RT ~ Frequency * PrevType, data = data, prior= prior(normal(-0.01, 0.005), class="b", coef="Frequency"))
summary(bm3)


## o) How did the estimate and credible interval of frequency change?

# The coefficient estimate of frequency is changed from -0.03 to -0.02 i.e. less negative.
# Credible interval of frequency is changed from [-0.05, -0.02] to [-0.03, -0.01].   


## p) What class of priors does the above one belong to? 

# Class of prior is informative priors.
