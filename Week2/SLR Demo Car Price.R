library(data.table)
library(dplyr)

# data from here
# http://mlr.cs.umass.edu/ml/datasets/Automobile
# We want to predict Price of automobiles based on other variables

setwd("~/Work/Regis/MSDS660_Instructor/Week2")
dt <- read.csv("imports-85.data")

# add column names from documentation
column_names <- c('symbolizing', 'normalized-losses', 'make', 'fuel-type','aspiration','num-of-doors','body-style','drive-wheels','engine-location', 'wheel-base','length','weight','height','curb-weight','engine-type','num-of-cyliners','engine-size','fuel-system', 'bore','stroke','comression-ratio','horsepower','peak-rpm','city-mpg','highway-mpg','price')
colnames(dt) <- column_names

# Check the structure
dim(dt)
str(dt)

# Some numerical columns were entered as characters for some reason
dt$price <- as.numeric(dt$price)
dt$bore <- as.numeric(dt$bore)
dt$stroke <- as.numeric(dt$stroke)
dt$horsepower <- as.numeric(dt$horsepower)
dt$`peak-rpm` <- as.numeric(dt$`peak-rpm`)

summary(dt)

# We want to remove symbolizing because that is a insurance code
dt <- dt[, -1]

#looks like there are some NAs so let's remove them
dt <- dt[complete.cases(dt),]

#Look a the data summary, histogram, and boxplot of price
summary(dt$price)
hist(dt$price)
boxplot(dt$price)

#Everything looks pretty normal.  Remember this data is from 1987

#What was the most expensive and least expensive car on the list?

dt[which.max(dt$price),]
dt[which.min(dt$price),]

# Good to plot all the data at once but cannot plot characters so we will remove those
dtnum <- select_if(dt, is.numeric)
plot(dtnum)
# We are looking for variables that are stongly correlated with one another
# wheel-base:length and city-mpg:highway-mpg look strongly correlated

# Let's start by making a simple linear regression.  What is the single most correlated variable with price?
# Looks like engine size or horsepower

englm <- lm(price ~ dt$`engine-size`, data = dt)
summary(englm)

# Manipulating plot space
par(mfrow=c(2,2))
hist(dt$price)
plot(dt$price)
boxplot(dt$price)


plot(englm)

hplm <- lm(price ~ dt$horsepower, data = dt)
summary(hplm)
plot(hplm)



