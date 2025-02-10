### Stats with R Exercise sheet 2

###############################################################
# Deriving sampling distribution and confidence intervals
###############################################################


###############################################################
### Exercise 1: Deriving sampling distributions
###############################################################
## In this exercise, we're going to derive sampling distributions for the mean with 
## different sizes.

## a) We will not use data from a normal distribution, but work with the poisson distribution, which is 
### often used for count data. We start by generating a large random sample of a poisson distribution
### with lambda = 1. Use rpois and create a sample of 1000 values, assign them to 'pdata'.
### Please first set a seed of 555 to have comparable results
set.seed(555)
pdata <- rpois(1000, lambda=1)

# Why we use seed ? To obtain same random observations.
# https://www.youtube.com/watch?v=zAYzAZwufKI

## b) Take a look at your sample using the table() function and histogram and boxplot.
table(pdata)
hist(pdata)
boxplot(pdata)

## c) In what ways does this diverge from a normal distribution?
### Name at least 2 differences in reference to the plots and/or table

## Histogram plot is right skewed 
## Mean is right to median

## d) Now, we are going to draw a smaller sample from our generated data set.
### Use the function sample() to create a sample of five instances from pdata.
### assign it to sample5
sample5 <- sample(pdata, 5)

## e) draw another sample of 5 called sample5b
sample5b <- sample(pdata, 5)

## f) calculate the mean of the two samples and store them in the vector means5
means5 <- c(mean(sample5), mean(sample5b))

## g)   In order to draw a distribution of such a sample, we want to calculate the
###   mean of 1000 samples, each of size 5. However, we don't want to repeat 
###   question e and f 1000 times. Use a for loop to draw 1000 samples of size 5
###   and store the mean of each sample in the vector means5.
means5 <- c()
for (i in 1:1000){
  sample <- sample(pdata, 5)
  means5 <- append(means5, mean(sample))
}

## h) Repeat the for-loop in question h, but use a sample size of 20. 
##    Assign this to 'means50' instead of 'means5'.
means20 <- c()
for (i in 1:1000){
  sample <- sample(pdata, 20)
  means20 <- append(means20, mean(sample))
}

means50 <- c()
for (i in 1:1000){
  sample <- sample(pdata, 50)
  means50 <- append(means50, mean(sample))
}

## i) Explain in your own words what 'means5' and 'means50' now contain. 
##    How do they differ?

## means5 contain 1000 observations of mean of sample size 5.
## means50 contain 1000 observations of mean of sample size 50.
## means50 consider larger sample size so it is a better representation.

## j) Draw histograms of means5 and means20. Describe in what way they differ
hist(means5)
hist(means20)
hist(means50)

## hist(means5) is more right skewed compared to hist(means20).

## k) Why do you observe a skew for means5, but not for means20?

## Means20 is a better representation of population than means5 because of greater sample size. 
## Being the small sample size, means5 might not cover the values of population accurately and 
## may contain similar values resulting in variation of mean values. This explains the skewness in means5.



###############################################################
### Exercise 2: Confidence interval
###############################################################

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.


## a) What does a confidence interval mean from the perspective of experiment replication?
## A: Due to the fact that a replication experiment gives extremely varied p-values for every instance,
##    researchers rely on confidence interval, because the value of the mean of replication
##    is likely to fall within the range of the "Original Confidence Interval". So, for every replication
##    experiment, we would get a 95% confidence interval of the mean, in other words, for 95% of the time,
##    the true mean population would range within this obtained interval. This way, the researchers
##    would get statistically significant results instead of extreme p-values.

## b) please install and load packages sciplot and lsr
install.packages("sciplot")
library(sciplot)
install.packages("lsr")
library(lsr)

## c) calculate 95% Confidence Intervals for pdata, sample5 and sample5b. You can
##    use the function ciMean()
print(ciMean(pdata, na.rm = TRUE))
print(ciMean(sample5, na.rm = TRUE))
print(ciMean(sample5b, na.rm = TRUE))

## d) Why are these intervals so different?
# confidence intervals are different due to variation in different sample data

## e) Is the true mean contained in the interval?
## A: The obtained true mean for pdata, sample5 and sample5b, all lie within their respective confidence
##    intervals. 

## f) In the following, we will check whether the CI behaves as expected.
### What is the true mean in our example?
## A: In our case, the true mean of pdata is 1.027, which falls within the CI range of value 0.9634(2.5%) 
##    to 1.0905(97.5%).  

## g) Change your for loop from above (means20) to calculate the confidence interval 
### insetad of the mean. Then check whether the confidence interval contains the
### true mean and save the result in the variable TrueMeanContained
### Hint: You will need to compare the mean to the lower and the upper bound of the confidence interval
### ciMean(YOURSAMPLE)[1] gives you the lower bound and ciMean(YOURSAMPLE)[2] the upper bound
TrueMeanContained <- c()
true_mean = mean(pdata)
for (i in 1:1000){
  sample <- sample(pdata, 20)
  ci = ciMean(sample , na.rm = TRUE)
  is_contained = ((ci[1] <= true_mean) && (true_mean <= ci[2]))
  TrueMeanContained <- append(TrueMeanContained, is_contained)
}
## h) Given your results in TrueMeanContained, you now need to check, whether the interval really contains
### the mean 95% of the time. Does it?
print((sum(TrueMeanContained)/1000)*100)
# From this experiment we find the interval contains mean 94% of the times. Hence the answer is No
## i) Confidence intervals are often used in plots. Lets construct a barplot with confidence intervals for
### the dataset chickwts, which contains weight of chickens after being administered different kinds of 
### food for 6 weeks.
### Use the function bargraph.CI to plot weight by feed, using the arguments response and x.factor
bargraph.CI(x.factor = chickwts$feed, response = chickwts$weight,
            xlab="Feed",
            ylab = "weight",
            main = "Bar Graph")
## j) Now additionally set the optional argument ci.fun to ciMean. How does the graph change and why?
### Hint: Look into the documentation of bargraph.CI.
bargraph.CI(x.factor = chickwts$feed, response = chickwts$weight, ci.fun = ciMean,
            xlab="Feed",
            ylab = "weight",
            main = "Bar Graph")
# The difference between using ci.fun and not is that when ci.fun is set to ciMean the standard errors are
# set to upper and lower limit of confidence intervals.Otherwise when ci.fun is not set to ciMean the standard 
# error is set to +/- 1 of mean
