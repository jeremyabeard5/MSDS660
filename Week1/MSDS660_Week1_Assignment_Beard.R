# 20220706
# MSDS660
# Week 1 - Homework Assignment
# Jeremy Beard

# To start, I will list what variables I will focus on for the assignment

# V10: happiness
# V25: 1st priority for children to learn
# V72: work vs. leisure scale
# V227: income
# V228: tv

# V10 Taking all things together, would you say you are: 
# 1 Very happy 
# 2 Quite happy 
# 3 Not very happy 
# 4 Not at all happy 
# 9 Don't know

# V25 Here is a shorter list of things that children can be encouraged to learn. If you had 
# to choose, which one of these do you consider to be the most important thing for a child 
# to learn at home?
# 1=Thrift, saving money and things
# 2=Obedience
# 3=Determination, perseverence
# 4=Religious faith
# 9=Don't know

# V72. Which point on this scale most clearly describes how much weight you place on 
# work (including housework and school work), as compared with leisure or recreation? 
# 1. It's leisure that makes life worth living, not work 
# 2. 
# 3. 
# 4. 
# 5. Work is what makes life worth living, not leisure 
# 9. DK 

# V227 Here is a scale of incomes. We would like to know in what group your household 
# is, counting all wages, salaries, pensions and other incomes that come in. Just give 
# the letter of the group your household falls into, before taxes and other deductions. 
# 1 2 3 4 5 6 7 8 9 10 
# C D E F G H I J K L 
# No answer = 98
# [CODE INCOME CATEGORIES BY DECILES FOR YOUR SOCIETY, 
# 1=LOWEST DECILE, 10=HIGHEST DECILE]


##### HYPOTHESIS
# 1. A person who prioritizes thrift/saving money more will have a higher income
# 2. A person who prioritizes work more has a higher income

##### Setup, Reading data

setwd("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week1") #this will change in your environment

# read in our .rdata file
load("WorldValues_Data.rdata")

# load necessary packages
library(data.table)
library(dplyr)
library(ggplot2)

# this converts to a data.table, which is easier
# to work with than other formats in R
wv3 = data.table(`WV3_Data_R_v_2015-04-18`)
rm("WV3_Data_R_v_2015-04-18")

##### EDA
# V10 (happiness)
hist(wv3$V10)
summary(wv3$V10)
# NOTE: there are negative values which must be cleaned

# V25 (priority)
hist(wv3$V25)
summary(wv3$V25)
# NOTE: there are negative values which must be cleaned

# V72 (work priority)
hist(wv3$V72)
summary(wv3$V72)
# NOTE: there are negative values which must be cleaned

# V227 (income)
hist(wv3$V227)
summary(wv3$V227)
# NOTE: there are negative values which must be cleaned

##### PROBLEMS OR CONCERNS WITH THE DATA
# From the summary, we can see that the data has some negative values
# These negative values do not correspond to any responses from the survey

##### DATA CLEANING
# Let's remove the negative values
wv3_pos <- wv3[V10 > 0 & V25 > 0 & V72 > 0 & V227 > 0]

# Let's see how the distributions changed
# V10 (happiness)
hist(wv3_pos$V10)
summary(wv3_pos$V10)

# V25 (priority)
hist(wv3_pos$V25)
summary(wv3_pos$V25)

# V72 (work priority)
hist(wv3_pos$V72)
summary(wv3_pos$V72)

# V227 (income)
hist(wv3_pos$V227)
summary(wv3_pos$V227)

# Looks much better now, no more erroneous data

# Let's use the as.factor function to provide labels to the variables
wv3_pos$happy <- as.factor(wv3_pos$V10)
wv3_pos$priority <- as.factor(wv3_pos$V25)
wv3_pos$work <- as.factor(wv3_pos$V72)
wv3_pos$income <- as.factor(wv3_pos$V227)

# Let's give labels to the amount of TV people watch
wv3_pos$priority <- plyr::revalue(wv3_pos$priority,
                                        c("1" = "Thrift/Saving Money",
                                          "2" = "Obedience",
                                          "3" = "Determination",
                                          "4" = "Religion"))

wv3_pos$work <- plyr::revalue(wv3_pos$work,
                                  c("1" = "Leisure makes life worth living, not work",
                                    "2" = "Leisure is more important than work",
                                    "3" = "Work and Leisure are equally important",
                                    "4" = "Work is mostly what makes life worth living",
                                    "5" = "Work is what makes life worth living"))

# Now let's plot the correlations between life priorities, priority of work, and income
ggplot(data=wv3_pos, aes(x=V227, fill=priority)) + 
  geom_histogram(aes(y=..density..), binwidth=0.5) +
  facet_wrap(~priority, nrow=4) + 
  #scale_x_continuous(breaks = 1:4, labels=c("1" = "Very happy", "2" = "Quite happy",
  #                                          "3" = "Not very happy", "4" = "Not at all happy")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Income Level',
       title = 'Effect of Priority in Life on Income level' ,
       subtitle = 'Focusing on teaching determination is associated with higher income')

ggplot(data=wv3_pos, aes(x=V227, fill=work)) + 
  geom_histogram(aes(y=..density..), binwidth=0.5) +
  facet_wrap(~work, nrow=5) + 
  #scale_x_continuous(breaks = 1:4, labels=c("1" = "Very happy", "2" = "Quite happy",
  #                                          "3" = "Not very happy", "4" = "Not at all happy")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Income Level',
       title = 'Effect of Work Priority on Income level' ,
       subtitle = 'Interestingly enough, prioritizing work above everything is correlated with a lower income level')


##### CONCLUSIONS
# In this analysis of the WVS data, I chose to analyze life priorities, and work priority, and how they
#   both are correlated with income level. I found that both my hypotheses were incorrect! I hypothesized
#   that wanting to teaching children thrift/saving money would be correlated most with a higher income, 
#   but this was actually false. Teaching children determination was most correlated with a higher income. 
#   This does make sense, determination would have been my second choice for being highly correlated with
#   a higher income level.
# Additionally, people who stated "work is what makes life worth living" were found to have lower income
#   levels than any other category. This may be due to the fact that they choose a job which they love but
#   which may have a lower income level, over another job which pays well but they hate. 

# Thanks!
# Jeremy B
