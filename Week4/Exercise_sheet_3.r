### Stats with R Exercise sheet 3

#################################################################################
#Tests for Categorical Data and cleaning data
#################################################################################


#################################################################################
##  Exercise 1: Cleaning data
#################################################################################

## download the file insomnia.csv from cms
## The made-up dataset insomnia contains data of a survey on 60 students. 
## They are asked two questions: whether they regularly encounter sleep problems
## and what their preferred and most consumed drink is.

## a. Load the libraries stringr, dplyr and tidyr.
install.packages("stringr")
install.packages("dplyr")
install.packages("tidyr")
library(stringr)
library(dplyr)
library(tidyr)

## b. read in the data
data <- read.csv("insomnia.csv")
print(data)

## c. get a summary of the dataset
summary(data)

## d. how many students encounter sleep problems?
sum(data$sleepProblem)
## 22 students encounter sleep problems

## e. how many different drinks do students name? (transform the variable into a 
## factor first)
factor_drink <- factor(data$drink)
print(factor_drink)
## Students have named 8 different drinks

## f. collapse factor levels which were spelled wrong. Make sure you first handle
## case and whitespace incongruencies, before you fix individual misspellings
## with gsub
factor_drink <- lapply(factor_drink, str_remove_all, " ")
factor_drink <- lapply(factor_drink, tolower)
factor_drink <- gsub("coffe$", "coffee", factor_drink)
factor_drink <- gsub("tee", "tea", factor_drink)

factor_drink = factor(factor_drink)
print(factor_drink)

## You realize that most students had multiple exams in the week from Feb 22 to 
## Feb 26. As students had to learn a lot and were possibly worried, they might 
## misjudge or exaggerate their sleep problems as occurring "regularly"
## We want to exclude all data that was collected between and including Feb 15 
## and Feb 26!

## g.  First show how many data points will be concerned, you need to transform
##     the date column to a Date object first!
data$date <- as.Date(data$date, "%Y-%m-%d")
count(filter(data, date >= as.Date("2021-02-15", "%Y-%m-%d") | date <= as.Date("2021-02-26", "%Y-%m-%d")))
## 60 data points will be concerned
 
## h. Now filter out this part of the data and assign the result to clean
clean <-  filter(data, date < as.Date("2021-02-15", "%Y-%m-%d") | date > as.Date("2021-02-26", "%Y-%m-%d"))
print(clean)

#################################################################################
### Exercise 2: chi-squared test
#################################################################################

## consider the data set from above. If you had problems performing the
## required cleaning steps, note that you can also do them by hand
## Now consider we want to see whether the preferred drink influences sleep problems

## a. formulate in plain English what the Null hypothesis is in this context
# Null hypothesis can be formulated as  preferred drink does not influence sleep problems
## b. conduct a chi-squared test to test this hypothesis using the function chisq.test()
##    and assign the result to chi
clean$drink <- lapply(clean$drink, str_remove_all, " ")
clean$drink <- lapply(clean$drink, tolower)
clean$drink <- gsub("coffe$", "coffee", clean$drink)
clean$drink <- gsub("tee", "tea", clean$drink)
contingency <- table(clean$drink, clean$sleepProblem)
chi <- chisq.test(contingency)


## c. the last call produced a warning. To understand why this warning arises, look
##    at observed and expected frequencies of chi. Why do you think it produced the error?
chi$observed
chi$expected
#The error could be because of low expected frequencies. In other words we have few observations. 
#We would require more samples so that approximations of p is reliable.

## d. What are the expected frequencies? Do we need to look at expected or 
##    observed frequencies?
# expected frequencies indicate the frequencies necessary in indivial category so that the null hypothesis holds good.
# We need to look at both expected and observed frequencies in order to determine if there is dependency
# between the variables tested for. But t fix the error we have to look at the expected frequencies.

## e. a possible solution is to sample more participants. Given that the smallest 
##    admissible value is 5, from which group(s) in terms of preferred drinks do
##    we have to sample more?
# Looking at the expected fequencies, we can say more participants needs to be sampled for all the three groups, coffee, tea and water

## f. Assume we don't have the possibility to sample more students. Which test do
##    you have to run instead? How does it work roughly? Perform a suitable test
##  fisher's exact test is preferred over chi square test when number of samples is less than 1000. Hence if we cannot sample more then fisher test ##  can be performed.
## A: fisher's exact test is preferred over chi square test when number of samples is less than 1000. Hence if we cannot sample more then fisher test can be performed.


## g. Lastly, what is the conclusion of your test? What have you learned and what 
##    have you not learned? 




#################################################################################
##Exercise 3. Binomial distribution
#################################################################################
## Suppose there are 18 multiple choice questions in a quiz. 
## Each question has 5 possible answers, and only one of them is correct. 

## a. Please calculate the probability of getting exactly 5 answers right 
##    if you answer by chance. Calculate this using the dbinom() function.
dbinom(5, 18, 0.2)
# 0.1507299

## b. Next please calculate the probability of answering 6 or more questions 
##    correctly by chance.
1 - pbinom(5, 18, 0.2)
# 0.1329163


#################################################################################
##Exercise 4
#################################################################################
##   Consider the data set used in Ex 1&2. How would the experiment have to change
##   in order for you to choose McNemar's test over the ChiSquare test? 
##   What would be the problem of using the normal ChiSquare test in a case where 
##   McNemar's test would be more appropriate?
## A: (1) In the insomnia dataset, if we make a contingency matrix of the data, we would be able to 
##        measure the difference or change in the observations using McNemar's test, as it would still 
##        retain the indepence assumption.
##    (2) Let us say in an instance, where we have to measure the effectiveness (Before & After) of an 
##        advertisement of a company with responses (Yes/No). The problem of using Chi-squared test 
##        here is that the situations are not independent as the subject for the Before and After test
##        remains the same. Chi-squared test would not give significant results because the assumption
##        for independence is violated here.
