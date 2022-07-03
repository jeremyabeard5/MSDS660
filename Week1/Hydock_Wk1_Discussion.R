# WEEK ONE DEMO # 

if (!require(data.table)) {
  install.packages('data.table')
  require(data.table)
}

setwd("~/MSDS/MSDS660/wk1") #this will change in your environment

# read in our .rdata file
load("WorldValues_Data.rdata")

library(data.table)

# this converts to a data.table, which is easier
# to work with than other formats in R
wv3 = data.table(`WV3_Data_R_v_2015-04-18`)
rm("WV3_Data_R_v_2015-04-18")

# Inquiry : The more time one spends watching TV, the less income they generate
# Inquiry : The more time one spends watching TV, the happier they are

# Question V10. Taking all things together, would you say you are:
# 1 Very happy
# 2 Quite happy
# 3 Not very happy
# 4 Not at all happy
# 9 Don't know

# Question V227 : Here is a scale of incomes. We would like to know in what group 
# your household is, counting all wages, salaries, pensions and other incomes that 
# come in. Just give the letter of the group your household falls into, before 
# taxes and other deductions.
#
# [CODE INCOME CATEGORIES BY DECILES FOR YOUR SOCIETY, 1=LOWEST DECILE, 10=HIGHEST DECILE]

# Question V228. Do you ever watch television? IF YES: How much time do you usually spend
# watching television on an average weekday (NOT WEEKENDS)?
#
# 1. Do not watch TV or do not have access to TV
# 2. 1 - 2 hours per day
# 3. 2 - 3 hours per day
# 4. More than 3 hours per day
# 9. DK

##### Remove the erratic data #####
# Check for erratic data in the 3 questions under observation
summary(wv3$V10)
summary(wv3$V227)
summary(wv3$V228)

# All questions show erratic data, e.g. out of the grading scale. In this case
# it's only negative numbers which have no meaning in the survey. We can start by
# taking out those records

wv3_work <- wv3[V10 > 0 & V227 > 0 & V228 > 0]

summary(wv3_work$V10)
summary(wv3_work$V227)
summary(wv3_work$V228)
# From the previous summaries, we can see there are no outliers in these questions
# as the ranges fall within the boundaries of acceptable answers

# Using the basic histogram function we can see the distributions
hist(wv3_wrk$V10, breaks = 6, main = "Happiness", xlab = "Rating 1: Very happy to 4: Not at all happy")
hist(wv3_wrk$V227, main = "Income", xlab = "Scale of 1 (lowest) - 10 (highest")
hist(wv3_wrk$V228, breaks = 6, main = "Television Consumption", xlab = "Rating 1: Do not watch to 4: 3 or more hours per weekday")

# To utilize 2 or more variables we must use ggplot
library(dplyr)
library(ggplot2)

# I believe this works as a pseudonym for easier reference
wv3_work$Income <- as.factor(wv3_work$V227)

wv3_work$TV <- as.factor(wv3_work$V228)
# Assigning a label to values for representation within the plot
wv3_work$TV <- plyr::revalue(wv3_work$TV,
                             c("1" = "Do not watch TV or do not have access to TV",
                               "2" = "1 - 2 hours per day",
                               "3" = "2 - 3 hours per day",
                               "4" = "More than 3 hours per day"))

# Plot for TV consumption to income ratio
ggplot(data=wv3_work, aes(x=V227, fill=TV)) + 
  geom_histogram(aes(y=..density..), binwidth=0.5) +
  facet_wrap(~TV, nrow=2) + 
  #scale_x_continuous(breaks = 1:4, labels=c("1" = "Do not watch TV", "2" = "1 - 2 hours",
  #                                          "3" = "2 - 3 hours", "4" = "More than 3 hours")) +
  scale_y_continuous("Density") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Income scale 1 (lowest) - 10 (highest)',
       title = 'How TV consumtion affects income' ,
       subtitle = 'Poor respondents seem to no own as many TVs, but once owned, consumption remains relatively normal')

# Plot for TV consumption to happiness ratio 
ggplot(data=wv3_work, aes(x=V10, fill=TV)) + 
  geom_histogram(aes(y=..density..), binwidth=0.5) +
  facet_wrap(~TV, nrow=2) + 
  scale_x_continuous(breaks = 1:4, labels=c("1" = "Very happy", "2" = "Quite happy",
                                            "3" = "Not very happy", "4" = "Not at all happy")) +
  scale_y_continuous("Density") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Taking all things together, would you say you are',
       title = 'How TV consumption affects happiness' ,
       subtitle = 'Having a TV generally increases happiness, regardless of time spent watching')

##### Conclusion ##### 
# After tidying the respective data, we can see that the average respondent is
# generally happy with life (mean 2.04), is truly in the median income range 
# (mean 4.539) and watches an average of 1-2 hours of TV per day (mean 2.569).

# Inquiry : The more time one spends watching TV, the less income they generate
# It seems that the lowest income respondents have a higher chance of not owning 
# a TV, but of those who seem to own TVs, consumption remains equally distributed
# amongst different income levels. The only other point to note is that lower
# with a TV tend to watch more than 3 hours per day at a higher rate than other
# income levels.
# Conclusion: Based on this chart, I believe it can safely be assumed that higher
# rates of TV consumption trend with lower incomes, with the exception of those
# too poor to afford a TV.

# Inquiry : The more time one spends watching TV, the happier they are
# Conclusion: This one is pretty cut and dry. TV consumption does not play a major
# factor in overall happiness as the distributions are quite even throughout the
# levels of consumption.

# Problems or Concerns with the data #
# Overall, the dataset is extremely messy, but easy to clean up to a usable state
# The income scale (V227) seems a little arbitrary as there is no reference given.
# In my opinion, This would give less weight to conclusions drawn solely from this point.
