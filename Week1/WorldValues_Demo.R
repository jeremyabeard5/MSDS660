# WEEK ONE DEMO # 

##### How To #####
# Use 5 pound signs to section out code 
# To comment out multiple lines of code, select area and press ctrl+c
# To run code press ctrl+enter - also works for multiple lines
# rm() removes object from global environment
# To search for help on a function use ?
# To install packages use install.packages('packageName')
# this will check if the data.table package is installed
# If not, it'll install it
if (!require(data.table)) {
  install.packages('data.table')
  require(data.table)
}

# To load in packages use require() or library() 
# List of commonly used packages: 
# dplyr 
# reshape2
# tidyverse - downloads a ton of dependent packages
  # includes: readr, readxl, stringr, stringi, ggplot2, knitr, lubridate 
# Cheat sheets: https://www.rstudio.com/resources/cheatsheets/

##### Setting file paths and reading data #####
# set working directory 

setwd("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week1") #this will change in your environment

# read in our .rdata file
load("WorldValues_Data.rdata")

library(data.table)

# this converts to a data.table, which is easier
# to work with than other formats in R
wv3 = data.table(`WV3_Data_R_v_2015-04-18`)
rm("WV3_Data_R_v_2015-04-18")

##### Exploratory functions #####
# dim() : Columns and rows
# summary() : Five number summary of each column - outplut dependent on class type
# head() : First five rows of data 
# str() : class composition of data.frame 
# colnames() : column names of data.frame 
# hist() : histogram of column 
# boxplot() : boxplot of column 
# plot() : bar plot of colum 
# IQR() : interquartile range 
# View() : View data in new tab 
# cor() : correlation plot 

# How interested was the respondent? Problem?
hist(wv3[, V231])
hist(wv3[V231 > 0, V231])

# How long did interview take?
hist(wv3$V230)
summary(wv3$V230)

# Sort the values and count the frequency
sortedTable = sort(table(wv3[, V230]), decreasing=T)
View(sortedTable)

# Remove the outliers
wv3_zero <- wv3[V230 > 0, ]
summary(wv3_zero$V230)

IQR(wv3_zero$V230)

# minimum = Q1 - 1.5*IQR
# maximum = Q3 + 1.5*IQR 
wv3_upper <- 100+(1.5*IQR(wv3_zero$V230))
wv3_lower <- 45-(1.5*IQR(wv3_zero$V230))
wv3_clean <- wv3_zero[V230 < wv3_upper & V230 > wv3_lower, ]

# Does that seem reasonable?
wv3_clean2 <- wv3_zero[V230 < wv3_upper, ]
hist(wv3_clean2[, V230])

# Inquiry : As a person's education level increases
# they are more likely to believe women are equitable political leaders

# Question V101 : On the whole, men make better political leaders than women do 
# 1 = Strongly agree; 4 = Strongly Disagree'

table(wv3[, V101])
hist(wv3[, V101])
hist(wv3[V101 > 0, V101])

#Boxplot is not very useful because of the catagorial nature of the respose.
boxplot(wv3[V101 > 0, V101])
summary(wv3[V101 > 0, V101])

# Question V217: What is the highest educational level that you have attained? 
# 1 = No formal Education; 9 = University level with degree
table(wv3[, V217])
hist(wv3[V217 > 0, V217])
boxplot(wv3[V217 > 0, V217])

library(dplyr)
library(ggplot2)


# Two good pages to help plot histograms with multiple groups
# https://uc-r.github.io/histograms#groups 
# https://stackoverflow.com/questions/22181132/normalizing-y-axis-in-histograms-in-r-ggplot-to-proportion-by-group


wv3_filtered <- wv3[V217 > 0 & V101 > 0]

wv3_filtered$Education <- as.factor(wv3_filtered$V217)
wv3_filtered$Education <- plyr::revalue(wv3_filtered$Education,
                                        c("1" = "No formal education",
                                          "2" = "Incomplete primary school",
                                          "3" = "Complete primary school",
                                          "4" = "Incomplete secondary school: tech/vocational",
                                          "5" = "Complete secondary school: tech/vocational",
                                          "6" = "Incomplete secondary school: preparatory",
                                          "7" = "Complete primary school: preparatory",
                                          "8" = "Some university wuthout a degree",
                                          "9" = " University with degree"))


ggplot(data=wv3_filtered, aes(x=V101, fill=Education)) + 
  geom_histogram(aes(y=..density..), binwidth=0.5) +
  facet_wrap(~Education, nrow=4) + 
  scale_x_continuous(breaks = 1:4, labels=c("1" = "Stongly agree", "2" = "Agree",
                                            "3" = "Disagree", "4" = "Strongly disagree")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'On the whole, men make better political leaders than women do',
       title = 'How education changes peoples opinion of women in poltics' ,
       subtitle = 'As education increases people are more accepting of women in politics')

# It appears there is a small trend indicating the more educated a person becomes
# the more they disagree that men make better politicians. 

# Continue using this R file for your in class assignment 


