### Stats with R Exercise sheet 4

##########################
# Week 5: t-test and friends
##########################

###########################################################################################
###########################################################################################

#####################################################
### 1. Restructuring, plotting, and t tests
#####################################################

library(lsr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(languageR)

## We will be working with the dataset lexdec from the package languageR
## In short, this data set contains reaction times from an experiment, where participants had 
## to decide whether something is a word or not (nonword). Only responses for real words are
## included and there were 79 measurements per participant.
## 
## Variables we will be interested in include 
## Subject (code for a participant)
## NativeLanguage (native language of participant)
## RT (log reaction time)
## Sex (of the participant)
## PrevType (whether the preceding word was a real word or a nonword)
## Class (whether the target word was denoting an animal or a plant)


## 1. Create the dataset lexdat, which is a copy of lexdec, but only includes the columns 
##  indicated above
lexdat = select(lexdec, c('Subject','NativeLanguage','RT','Sex','PrevType','Class'))

## Say you are interested in the influence of the previous word type on lexical decision time.
## Before we start testing, we want to get an impression of the data and create a barplot of 
## the mean by prevType, including error bars that show the 95% CI.
## Here, we define a function to calculate the standard error, which is needed for the CI.
## (just execute the next line, as you will need the function in 2.)
se = function(x){sd(x)/sqrt(length(x))}

## 2. To start, we need to summarize the data. Use the functions group_by() in combination with
##  summarise(). In particular, you need to group by prevType and get the mean as well as the
##  se of RT. Store the result to summaryByPrevType
##  You will find examples of how the summarizing can be done here:
##  https://datacarpentry.org/R-genomics/04-dplyr.html#split-apply-combine_data_analysis_and_the_summarize()_function
summaryByPrevType <-  lexdat %>% 
                      group_by(PrevType) %>%
                      summarize(mean_RT = mean(RT), se_RT = se(RT))

## 3. Describe the resulting data set (summaryByPrevType) in your own words
### The prevType is of two categories: word and non word.
### From this resulting data we can observe the mean reaction time for each of this category.
### Mean reaction time is greater for non word type than that of word type (6.416 > 6.351)
### Std error is very less approximately 0.0083 for both indicating mean_RT to be accurate with less randomness.

## 4. Now use summaryByPrevType to create the barplot with error bars denoting the 95% CI
##  (i.e. mean +/-1.96 * se)
ggplot(summaryByPrevType, aes(x=PrevType, y=mean_RT)) + 
  geom_bar(stat="identity", width=0.3) +
  geom_errorbar( aes(x=PrevType, ymin=mean_RT-(1.96*se_RT), ymax=mean_RT+(1.96*se_RT)), width=0.1, colour="orange", alpha=0.9, size=1)

## 5. The barplot always starts at zero, which makes the portion of the graph, we are most 
##  interested in (i.e. the spread of the error bars) hard to perceive. As an alternative,
##  construct a line plot of the same data, again including error bars.
##  Hint: if you get a complaint, try to add group = 1 to your aes
ggplot(summaryByPrevType, aes(x=PrevType, y=mean_RT, group=1)) + 
  geom_line() +
  geom_point()+
  geom_errorbar( aes(x=PrevType, ymin=mean_RT-(1.96*se_RT), ymax=mean_RT+(1.96*se_RT)), width=0.1, colour="orange", alpha=0.9, size=1)

## 6. Gauging from the plot, does it look like there's an important difference in mean RT 
##  after words compared to nonwords?
### From the plots, we can say there is no any easily gaugable difference between the mean reaction time after word and non word.

## 7. Let's go back to the original data frame "lexdat".
##  Now that you've taken a look at the data, you want to get into the stats.
##  You want to compute a t-test for the average RT after words vs nonwords.
##  Why can't you compute a t-test on the data as they are now? 
##  Hint: Which assumption is violated?
### Right now, the data violates the independence between the observations assumption of the t-test. 
### having same subjects multiple times makes the data correlated. Hence by taking the average as mentioned below help remove the correlation.

## 8. We need to restructure the data to only one observation (average RT) per subject 
##  and word/nonword condition (PrevType). We will again use group_by and summarize, but
##  this time we have to group by Subject and PrevType, while we only need the mean to be 
##  stored, not the se. Assign the result to bySubj
bySubj <- lexdat %>% 
  group_by(Subject, PrevType) %>%
  summarize(mean_RT = mean(RT))

## 9. Create histograms of the RT data in bySubj depending on the preceding word 
##  type and display them side by side. Set the binwidth to 0.08
ggplot(bySubj, aes(x=mean_RT,fill=PrevType)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position = 'dodge',binwidth = 0.08)+
  ggtitle("Histogram showing the mean RT")

## 10. Display the same data in density plots. 
ggplot(bySubj, aes(x=mean_RT,fill=PrevType)) +
  geom_density(color="#e9ecef", alpha=0.6)+
  ggtitle("densilty plot showing the mean RT")

## 11. Based on the histograms and the density plots - are these data normally 
##  distributed?
###Yes, the data is normally distributed

## 12. Create boxplots of the mean RT in bySubj by PrevType
ggplot(bySubj, aes(x = PrevType, y = mean_RT), na.rm = TRUE) + 
  geom_boxplot()

## 13. Compute the t-test to compare the mean RT between decisions following on a word
##  vs a nonword using the data in bySubj.
##  Do you need a paired t-test or independent sample t-test? why?
prev_word <- bySubj %>% filter(PrevType == "word")
prev_nonword <- bySubj %>% filter(PrevType == "nonword")
t.test(prev_word$mean_RT, prev_nonword$mean_RT, paired = TRUE)

### We need a paired t-test because we have two measurements of average reaction time from the same subject -
### one measurement is when preceding word is real word and other when it is nonword.

## 14. What does the output tell you? What conclusions do you draw?
### p-value = 5.472e-05. This means that the preceding word have significant
### effect on response time of the current word.

## 15. In addition to the long-format data we've just been working on, you may also 
## encounter data sets in a wide format (this is the format we have been using in 
## class examples.)
## Let's look at a different variable, namely the semantic class (Class) of the target 
## word instead of the type of the previous word. Again, summarize the dataset
## to obtain the mean RT by subject and class and transform the dataset to a 
## wide format. In addition to group_by() and summarize(), you will need the function 
## spread(). Assign the result to wide
wide <- lexdat %>% group_by(Subject, Class) %>% summarise(mean_RT = mean(RT)) %>% spread(Class, mean_RT)

## 16. Compute a t-test on the wide format data - note that for wide-format 
##  data you need to use a different syntax inside t.test()
t.test(wide$animal, wide$plant, paired = TRUE)

## 17. What do you conclude from this?
### p value = 0.426 
### p-value is grater than the significance level(0.05). Hence we can concludes that the 
### class of word does not have any significant impact on the average response time.

## 18. Now let's look at yet another question, namely whether the native language 
##  of the participant influences their reaction time. Check out the variable
##  NativeLanguage. Can you use a t-test to pursue this question and which type
##  of t-test would you use? Can you think of a situation, where a t-test would not 
##  be enough to test for a difference depending on the native language?
### We can use independent Sample t-test (student t-test) to check if native language influences the response time.
### Paired t test should not work here since the native language force the group to be divided into 
### 2 mutually exclusive scenarios.

## 19. Use again group_by and summarize to obtain by subject means of RT, but
## this time with regard to NativeLanguage and assign it to bySubjNatLang
## Perform the t-test you decided for.
bySubjNatLang <- lexdat %>% group_by(Subject, NativeLanguage) %>% summarize(mean_RT = mean(RT)) %>% spread(NativeLanguage, mean_RT)

t.test(bySubjNatLang$English, bySubjNatLang$Other)

## 20. What do you conclude?
### p-value = 0.03572 is less than the significance level(0.05), hence we reject the null hypothesis. 

## 21. Compute the effect size using Cohen's D.
cohensD(bySubjNatLang$English, bySubjNatLang$Other)

## 22.  Which effect size do we get? How do you interpret this result?
### We get an effect size of 1.135061 can be roughly interpreted as "large".

## 23. Choose an appropriate plot to visualize the difference between group
### We are using line plot to visualize the difference
bySubjNatLang_se <- lexdat %>% group_by(NativeLanguage) %>% summarize(sd = se(RT), mean_RT = mean(RT))

ggplot(bySubjNatLang_se, aes(x=NativeLanguage, y=mean_RT, group = 1)) + 
  geom_line() +
  geom_errorbar(aes(ymin = mean_RT - 1.96 * sd, ymax = mean_RT + 1.96 * sd), width=.2)

###############
### 2. T-Test
###############
## In this exercise we will try to explore the independent samples t-test 
## and its affect on different samples. 
## We will take the same example discussed in the lecture. A class has two tutors, and we want 
## to find out which tutor is better by comparing the performance of the students in the final 
## exam by tutor group. First set a seed to make sure your results can be reproduced

set.seed(8254)
## 1. Generate 15 samples from a normal distribution with mean 20 and sd 8 and save it in a variable 
##  called "tutor1_grades"
set.seed(15)
grades <- runif(15,0,100)
tutor1_grades = rnorm(grades, mean=20, sd=8)

## 2. Now we generate our second sample of size 15, this time for tutor 2 and with mean 35 and 
## sd 15
tutor2_grades = rnorm(grades, mean=35, sd=15)

## 3. Combine the two samples and store the result into one vector called "score" (it should 
##    first show all scores from tutor1 followed by the scores of tutor2)
score = data.frame(tutor1_grades, tutor2_grades)

## 4. Create a vector called tutor indicating which tutor the score belongs to: it should show 
##   "tutor1" 15 times followed by "tutor2" 15 times
tutor1 = c(replicate(15,"tutor1"))
tutor2 = c(replicate(15,"tutor2"))
df1 = data.frame(tutor1, tutor1_grades)
df2 = data.frame(tutor2, tutor2_grades)

df1 <- df1 %>% rename(tutor = tutor1, score = tutor1_grades)
df2 <- df2 %>% rename(tutor = tutor2, score = tutor2_grades)

final_df = rbind(df1,df2)
## 5. Create a data frame named "data_frame" having 2 columns "tutor", "score" created above.
data_frame = data.frame(final_df)

## 6. Run the independent samples TTest (independentSamplesTTest()) and formulate the findings as discussed 
###  in the lecture. 
##	independentSamplesTTest() also provides the effect size (Cohen's d). How do you interpret the effect size?

independentSamplesTTest(formula = score ~ tutor, data = data_frame, var.equal = TRUE)
## A: The Cohen's d gives us the estimate effect size which is formulated to be 1.026. As the size is greater than 0.8, 
##    we can infer that the there is a significant difference between the tutor1 and tutor2 grades.

## 7. Time to play around!
##	repeat the whole experiment you performed above with different sample size, mean and standard deviation  
##	repeat it 3 times changing all the values (sample size, mean, sd) and formulate the findings.  
##	what do you observe when we keep the means and sd same?
new_val <- function(sample_size, mean, sd){
  grades = runif(sample_size,0,100)
  t1_scores = rnorm(grades, mean=0, sd=10)
  t1 = c(replicate(sample_size, "tutor1")) 
  df_1 = data.frame(t1,t1_scores)
  df_1 = df_1 %>% rename(tutor = t1, score = t1_scores)
  
  t2_scores = rnorm(grades, mean=mean, sd=sd) 
  t2 = c(replicate(sample_size, "tutor2"))
  df_2 <- data.frame(t2,t2_scores)
  df_2 <- df_2 %>% rename(tutor = t2, score = t2_scores)
  
  comb_df <- rbind(df_1, df_2)
  independentSamplesTTest(formula = score ~ tutor, data = comb_df, var.equal = TRUE)
  
}

new_val(10,10,25)
new_val(1000,10,25)
new_val(10,0,10)

## A: We notice that the cases where we have same mean and standard deviation, the Cohen's d value is quite low.
##    This is due to the fact that the effect size is directly proportional to the difference in the means of t1 and t2 groups.
##    And it is indirectly proportional to their respective standard deviations.
##    Thus, as the sample size increases, the t-value and degrees of freedom increases. 

##Ans: The effect size for same mean and sd is very low. The effect size is directly proportional to 
##the difference in the means of the two groups and indirect. It proportional to their standard deviations.
##With the increase in the sample size the t value increases as the degrees of freedom increases. The corresponding 
##p-values are less indicating highly significant data as it is closer to the true population.
