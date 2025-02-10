### Stats with R Exercise sheet 1 



## Many of the things on this exercise sheet have not been discussed in class. 
## The answers will therefore not be on  the slides. You are expected to find 
## the answers using the help function in R, in the textbooks and online. If you 
## get stuck on these exercises, remember: Google is your friend.
## If you have any questions, you can ask these during the tutorial, or use the 
## cms discussion board for the course.

###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
#getwd()


## b) Get help with this function.
#?getwd


## c) Change your working directory to another directory.
#setwd("..")

###############
### Exercise 2: Normal distribution plotting
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -4 to 4, by 0.1. Assign this to the 
##    variable x.
x <- seq(from=-4, to=4, by=0.1)
print(x)

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.
z <- dnorm(x, mean = 0, sd = 1, log = FALSE)

## c) Now use plot() to plot the normal distribution for z values of "x". 
plot(x,z)

## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.6, and plot a line 
##    instead of the circles.
plot(x,z,type="l",ylim=c(0,0.6))

## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the mean of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.
abline(v=mean(x), lty=2)

## f) Take a look at the beaver2 dataset. (You can see it by typing "beaver2".) 
##    Then select only the temperature part and store it in a variable "b2temp".
beaver2
b2temp = beaver2$temp

## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.
b2mean <- mean(b2temp)
b2sd <- sd(b2temp) 
print(b2mean)
print(b2sd)
norm_dist <- dnorm(b2temp, mean = b2mean, sd = b2sd, log = FALSE)
plot(b2temp, norm_dist, type="l")

## h) We observe two additional temperature values (38.13 an 36.81). What's the 
##    likelihood that these temperatures (or more extreme ones) respectively 
##    come from the normal distribution from g)?
1 - pnorm((38.13-b2mean)/b2sd)
1 - pnorm((36.81-b2mean)/b2sd)

## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. Set the range of the x-axis between 36 to 39 using xlim. 
##    Fix the number of breaks to 10 using breaks
##    What do you observe?

## sample 1
rand_sample_1 <- sample(b2temp, size=20)
hist(rand_sample_1, breaks=10, xlim=c(36,39))

rand_sample_2 <- sample(b2temp, size=20)
hist(rand_sample_2, breaks=10, xlim=c(36,39))

rand_sample_3 <- sample(b2temp, size=20)
hist(rand_sample_3, breaks=10, xlim=c(36,39))

rand_sample_4 <- sample(b2temp, size=20)
hist(rand_sample_4, breaks=10, xlim=c(36,39))

rand_sample_5 <- sample(b2temp, size=20)
hist(rand_sample_5, breaks=10, xlim=c(36,39))

#####
# Most of the plots looks left skewed.
###

###############
### Exercise 3: data exploration and more histograms
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
install.packages("languageR")
library(languageR)

## b) Specifically, we will deal with the dataset 'selfPacedReadingHeid'. 
##    This dataset should be available to you once you've loaded languageR.
##    Find out what experiment the data comes from
##    Inspect 'selfPacedReadingHeid'. Look at the head, tail, 
##    and summary. What do head and tail show you?
head(selfPacedReadingHeid)
tail(selfPacedReadingHeid)
summary(selfPacedReadingHeid)

## c) The file contains multiple observations for each participant. Create a 
##   subset only including subject number PP002 and assign it to PP002.
##   How many observations are there for this participant, i.e. how many rows 
##   are in your subset?
PP002 <- selfPacedReadingHeid[selfPacedReadingHeid$Subject == "PP002", ]
# There are 80 rows in the subset

## d) Create a histogram (using hist()) of "RT" (logarithm of reading time) 
##    for PP002
PP002_RT <- PP002$RT 
hist(PP002_RT)

## e) Create a kernel density plot for this data using density()
den_lot <- density(PP002_RT)
plot(den_lot)

## f) What is the difference between the two?
# both the plot are alike.

## g) Is this data likely from a normal distribution? How would you check ?
##    (describe in words, remember to comment out text)
# Yes, this data likely form a normal distribution. We checked the histogram plot and kernel density plot 
# and observe the bell curve. Other way to check if it is normal distribution is by using Q-Q plot. 

###### Correction #######
#No, not normal distribution, not symmetrical graph for kernel density

###############
### Exercise 4: Dataframes and boxplots
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 26 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 18 19 22 17 18 26 17 14 16 16 17 21 23 16 20 21 20 20 15 17 17 18 20 24


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why?
# This data is discrete and measurement scale is ordinal because you can order the data.

## b) The researcher is also interested in whether story telling is related to 
##    their reading habits. As a proxy, she asked the children, whether they have 
##    a library card. The following line codes that the first 13 observations are
##    from children with library card (Y) and the remaining 13 from children 
##    without (N). What measurements scale does this variable have?
lib = c(rep("Y",13),rep("N",13))
# Measurement scale is nominal


## c) You will now create a dataframe of this data. Start by creating a vector 
##    with participant IDs. Your vector should be named 'pps', and your 
##    participants should be labeled from 1 to 26
pps <- c(seq(1,26))

## d) Next, create a vector containing all the observations. Name this vector 'obs'.
obs <- c(18, 15, 18, 19, 22, 17, 18, 26, 17, 14, 16, 16, 17, 21, 23, 16, 20, 21, 20, 20, 15, 17, 17, 18, 20, 24)

## e) Create a dataframe including pps, obs and lib. Assign this to 'stories'. 
stories <- data.frame(pps, obs, lib)

## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class are the variable 'pps' and 'lib'?
summary(stories)
class(pps)
# pps class is integer
class(lib)
# lib class is character

## g) Change the class of 'pps' and 'lib' to factor. Why is factor a better
##     class for these variables? (answer for both separately)
pps <- factor(pps)
class(pps)
# Each observation is unique participation ID.
lib <- factor(lib)
class(lib)
# There are two factors in the we can divide the complete observations.

## h) Create a boxplot of obs for the two lib groups
install.packages("ggplot2")
library(ggplot2)
ggplot(stories, aes(x=lib, y=obs)) +  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=7, color="red", fill="red")

## i) Are there outliers in one of the lib groups?
# Yes, there are two outliers in group "Y"

## j) Which group shows the larger interquartile range? Which one has the 
##    greater overall range?
# Group "N" has the greatest interquartile range. Group "Y"
# has greater overall range including the outliers

## k) Which group shows a negative or positive skew?
# Group "N" is negatively skewed. Group "Y" is positively skewed

# A left-skewed(Negative Skew), negative distribution will have the mean to the left of the median.
# A right-skewed(Positive Skew), distribution will have the mean to the right of the median.

## l) What is a whisker? Why is the upper whisker of group "Y" so short?
# whisker indicates the minimum/maximum values of the data. Upper whisker of "Y" is short because the 
# highest value within the (Q3 + 1.5 IQR = 21) is 19 which is less by highest possible value by 3 points.
# Where as for lower whisker the lowest possible vales is 13 and lowest value in observations is 14 which is only
# one point less than the lowest possible value. In other words, variance among lower values 
# is greater than variance among higher values since the distance between Q1 and lowest value is greater than distance between
# Q3 and highest value.

# The whiskers are the two lines outside the box, that go from the minimum to the lower quartile 
# (the start of the box) and then from the upper quartile (the end of the box) to the maximum.
# length of the whiskers is restricted to a maximum of 1.5 times the interquartile range

## m) Compare the median of group Y with the mean - which one is plotted in your
##    boxplot? Why are they different?
# Generally box plot plots median only, but in the box we explicitly also plotted mean. Mean
# for group "Y" is greater than the median. They are different because the data is not symmetric.
# The mean is affected by existance of exceptionally high valued outliers on the upper tail.