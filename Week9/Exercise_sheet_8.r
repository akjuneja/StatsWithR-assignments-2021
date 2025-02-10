### Stats with R Exercise sheet 8

##########################
# Linear Mixed Effects Models
##########################



###########################################################################################
###########################################################################################
library(lme4)
library(lattice)
library(Matrix)
library(ggplot2)
library(dplyr)


#####################################################
### 1. Linear mixed model for chicken growth 
#####################################################

## a) We will first look at the dataset ChickWeight, which is already loaded in base R. Check out 
##  the help page of the data set to understand how the data was collected and look at the summary
help("ChickWeight")
summary(ChickWeight)

## b) Let's plot the data. We will first follow the strategy from sheet 4, i.e. 
##  1. group the data by Diet and Time and use summarise() to get the mean and se (se() as provided below)
##    of weight. Assign resulting data set to aggData
se = function(x){sd(x)/sqrt(length(x))}
aggData <- ChickWeight %>% 
  group_by(Diet,Time) %>%
  summarize(mean_weight = mean(weight), se_weight = se(weight))

##  2. Create a line plot of aggData, plotting weight on the y axis, time on the x-axis and color by Diet. 
##    Also add errorbars (mean+/-1.96*se)
p <- ggplot(aggData, aes(x=aggData$Time, y=aggData$mean_weight,  color=aggData$Diet)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=aggData$mean_weight-aggData$se_weight, ymax=aggData$mean_weight+aggData$se_weight), width=.2,
                position=position_dodge(0.05))
print(p)
p+labs(title="Mean weight per Time", x="Time", y = "Mean Weight")

## c) The above plot ignored that the data comes from a fixed set of chicks. Let's look at individual growth
##  by coloring by Chick instead of by Diet, but faceting by Diet (side by side). You have to use ChickWeight 
##  instead of aggData here! Also you don't need error bars, because you are not plotting the mean, but the
##  actual data
p <- ggplot(ChickWeight, aes(x=Time, y=weight,  color=Chick)) + 
  geom_line() +
  geom_point()+
  facet_wrap(~Diet) 
print(p)
p+labs(title="Weight per Time", x="Time", y = "Mean Weight")

## d) What do you observe, looking at c?
# From c, we can observe that, each Chick growth varies. Different Chick's growth  varies within a particular diet
# In addition, the amount of growth (the general and slope) by days varies between different diets.

## e) We want to investigate whether the type of diet has an effect on the chick's growth, i.e. we are
##  looking for an interaction between time after birth and the diet type. Before running the model,
##  specify:

##  1) What fixed effect(s) do you enter into the model?
# Diet and time are fixed effects.

##  2) what random effect(s) should be included to account for the repeated measures structure of the data?
# Individual chick will be random effects

##  3) In addition to random intercept(s), which random slope(s) should you add to get a maximal model?
# random slope will be weight change over time. 

## f) Run the model you specified in e) using lmer() and assign it to chickmod
chickmod <- lmer(weight ~ Time + Diet + Time:Diet + (1+ Time|Chick), data = ChickWeight, REML=F)

## g) Rerun the model leaving out the interaction between Time and Diet and assign it to chicknull
chicknull <- lmer(weight ~ Time + Diet + (1+ Time|Chick), data = ChickWeight, REML=F)

## h) compare the two models using the anova() function, which performs a likelihood ratio test
anova(chickmod, chicknull)

## i) Report the p-value (from h) and the conclusion with respect to the research hypothesis
# p-value is 0.0012. Which is highly significant. Hence we can reject the Null hypoyhesis. 
# We used R to lme4 to perform a linear mixed effect analysis of the relationship between weight and Diet.
# As fixed effect we entered Diet and Time with interation term into the model. As random effects we had
# intercepts for chicks as well as by-Time randome slope for the effect of weight. p-values were obtained 
# by liklihood ratio tests with model with interaction term against model without interaction term. p-values
# are significant and AIC value for model with interaction (chickmod) is lower and hence we can conclude 
# that interaction between time and Diet does effect the growth(weight) of chicks.

## j) The following code creates a plot of all chick specific intercepts and slopes. What do you see?
print(dotplot(ranef(chickmod,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["Chick"]])
# We can interpret from the dot plot there is good variation in intercept and slope for individual chicks.
# Hence using them as random effects does improve the model.

#####################################################
### 2. Random effect structures 
#####################################################

## a) Let's return to the lexdec data set and suppose, we want to look at effects of the word type of the 
## previously presented word (each subject saw a different randomized sequence) and effects of the complexity
## of the word itself, while taking into account the dependence between data points collected on the same 
## word and from the same subject. 
## Which of the following models has a maximal random effect structure given the experimental design?
## Motivate your choice.

## m1 = lmer(RT ~ PrevType+ Complex+ (PrevType|Subject) + (Complex| Word), lexdec)
## m2 = lmer(RT ~ PrevType+ Complex+ (PrevType+Complex|Subject) + (PrevType| Word), lexdec)
## m3 = lmer(RT ~ PrevType+ Complex+ (PrevType+Complex|Subject) + (PrevType+Complex| Word), lexdec)
## m4 = lmer(RT ~ PrevType+ Complex+ (Complex|Subject) + (PrevType| Word), lexdec)
## m5 = lmer(RT ~ PrevType+ Complex+ (PrevType+Complex|Subject) + (1| Word), lexdec)

# Following model will have a maximal random effect structure:
# m2 = lmer(RT ~ PrevType+ Complex+ (PrevType+Complex|Subject) + (PrevType| Word), lexdec)

# In our model, observations with same subject can have variation in Prev Type and Complex.
# Observations with same word can have variation in Prev Type. 
# Hence, we will have by-subject random intercept and random slope (Complex, PrevType) and by-word random
# random intercept and random slope (Prev Type).  


## b) You want to relate students' performance in the advanced algebra course in a summer school in SaarbrÃÂ¼cken
##  to their final math grade in school. Performance is measured as the overall score in the final exam.
##  The summer school course has 200 participants, coming from 8 different partner Universities from all
##  over Germany. These 200 participants were randomly split into 10 tutorial groups, where each tutorial 
##  was held by a different tutor.
##  Given the design of your study, what random effects should you add to the model below?
##  Explain!!! If you want to, you can additionally add the random effects into the formula

# Random effects for our model are - university and tutorial.
# Observations with same university will have variation in mathGrade and observation with same tutorial 
# will also have variation in mathGrade.
# Hence we will have random intercept and random slope for both university and tutorial.

# lmer(advancedalgebrascore ~ mathGrade, (1+mathGrade|tutorial), (1+mathGrade|university), someData)

