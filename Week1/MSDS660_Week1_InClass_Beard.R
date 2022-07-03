# 20220702
# MSDS660
# Week 1 - In-Class Assignment
# Jeremy Beard

# To start, I will list the variables from the Questionnaire
# which pertain to this assignment

# V10: happiness
# V227: income
# V228: tv

# V10 Taking all things together, would you say you are: 
# 1 Very happy 
# 2 Quite happy 
# 3 Not very happy 
# 4 Not at all happy 
# 9 Don't know

# V227 Here is a scale of incomes. We would like to know in what group your household 
# is, counting all wages, salaries, pensions and other incomes that come in. Just give 
# the letter of the group your household falls into, before taxes and other deductions. 
# 1 2 3 4 5 6 7 8 9 10 
# C D E F G H I J K L 
# No answer = 98
# [CODE INCOME CATEGORIES BY DECILES FOR YOUR SOCIETY, 
# 1=LOWEST DECILE, 10=HIGHEST DECILE]

# V228 Do you ever watch television? IF YES: How much time do you usually spend 
# watching television on an average weekday (NOT WEEKENDS)?
# 1. Do not watch TV or do not have access to TV 
# 2. 1 - 2 hours per day 
# 3. 2 - 3 hours per day 
# 4. More than 3 hours per day 
# 9. DK 

##### HYPOTHESIS
# 1. A person who watches more TV will have a lower happiness level
# 2. A person who watches more TV will have a lower income level

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

# V227 (income)
hist(wv3$V227)
summary(wv3$V227)

# V228 (TV)
hist(wv3$V228)
summary(wv3$V228)

##### PROBLEMS OR CONCERNS WITH THE DATA
# From the summary, we can see that the data has some negative values
# These negative values do not correspond to any responses from the survey

# Let's remove the negative values
wv3_pos <- wv3[V10 > 0 & V227 > 0 & V228 > 0]

# Let's see how the distributions changed
# V10 (happiness)
hist(wv3_pos$V10)
summary(wv3_pos$V10)

# V227 (income)
hist(wv3_pos$V227)
summary(wv3_pos$V227)

# V228 (TV)
hist(wv3_pos$V228)
summary(wv3_pos$V228)

# Looks much better now, no more erroneous data

# Let's use the as.factor function to provide labels to V10, 227, 228
wv3_pos$happy <- as.factor(wv3_pos$V10)
wv3_pos$income <- as.factor(wv3_pos$V227)
wv3_pos$tv <- as.factor(wv3_pos$V228)

# Let's give labels to the amount of TV people watch
wv3_pos$tv <- plyr::revalue(wv3_pos$tv,
                                        c("1" = "Do not watch TV/Do not have access to TV",
                                          "2" = "1-2 Hours/Day",
                                          "3" = "2-3 Hours/Day",
                                          "4" = ">3 Hours/Day"))

# Now let's plot the correlations between TV, and happiness/income
ggplot(data=wv3_pos, aes(x=V10, fill=tv)) + 
  geom_histogram(aes(y=..density..), binwidth=0.5) +
  facet_wrap(~tv, nrow=4) + 
  scale_x_continuous(breaks = 1:4, labels=c("1" = "Very happy", "2" = "Quite happy",
                                            "3" = "Not very happy", "4" = "Not at all happy")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Happiness Level',
       title = 'Effect of TV-watching on Happiness level' ,
       subtitle = 'There is no noticeable effect of TV-watching on happiness')

ggplot(data=wv3_pos, aes(x=V227, fill=tv)) + 
  geom_histogram(aes(y=..density..), binwidth=0.5) +
  facet_wrap(~tv, nrow=4) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Income (Left=less income, Right=more income',
       title = 'Effect of TV-watching on Income level' ,
       subtitle = 'People who watched 1-3 hours/day of TV have higher income levels than 0 hours, or >3 hours')

##### CONCLUSIONS
# HAPPINESS LEVEL
# It appears that there is not much effect of TV-watching on happiness level.
# It can be noted that people who watched 0 hours/day of TV had the lowest happiness level.
# Therefore, any amount of TV is correlated to a higher happiness level than 0 hours/day of TV.

# INCOME LEVEL
# It also appears that there is an ideal TV-watching level of 1-3 hours/day, where
#   people who watched 1-2 hours/day of TV, or 2-3 hours/day, had a higher income level
#   than people who watched 0 hours/day or >3 hours/day of TV 
# It should also be noted that people who watched >3 hours/day of TV had a higher income
#   distribution than people who watched 0 hours/day of TV. Therefore, any amount of TV-watching
#   can be correlated to a higher income level than people who watch 0 hours/day of TV.

# Thanks!
# Jeremy B
