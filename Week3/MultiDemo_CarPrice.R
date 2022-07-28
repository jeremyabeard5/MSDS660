library(data.table)
library(dplyr)

# data from here
# http://mlr.cs.umass.edu/ml/datasets/Automobile
# We want to predict Price of automobiles based on other variables

setwd("~/Work/Regis/MSDS660_Instructor/Week2")


dt <- imports.85
rm(imports.85)

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
# We are looking for variables that are stongle correlated with one another
# wheel-base:lenght and city-mpg:highway-mpg look strongle correlated

# Let's start by making a simple linear regression.  What is the single most correlated variable with price?
# Looks like engine size or horsepower

englm <- lm(price ~ dt$`engine-size`, data = dt)
summary(englm)
par(mfrow=c(2,2))
plot(englm)

hplm <- lm(price ~ dt$horsepower, data = dt)
summary(hplm)
plot(hplm)

##############################################################
# Let's fit price to all the numeric variables to make a MLR model
fit <- lm(price ~ ., data = dtnum)
summary(fit)
plot(fit)
# Already we see an improvement with MLR vs the simple engine size model but there is some
# strange distribution of residuals. This data may not be perfectly liner but we will try to model
# the data using a MLR. Oberservation #47 seems like the only gross outlier.  We will leave it in for now.
# But out of curiousity what is that oberservation?

dt[47,]


# you can do the same thing with glm (general linear model), although the p-values are calculated slightly differently
# it will show AIC and not F-score by default
fit_glm <- glm(price ~ ., data = dtnum)
summary(fit_glm)

# Remember some of the variables were looked highly correlated.
# Let's use VIF to see if they are highly correlated
# need the car package for vif
library(car)

# anything above 5 or 10 indicates problematic variables
# page 101-102 in ISLR
vif(fit)

# corrplot plots correlations between variables
# If it is close to 1 or -1 it might be problematic
par(mfrow=c(1,1))
library(corrplot)
cor(dtnum)
corrplot(cor(dtnum))
corrplot(cor(dtnum), method='number')

# look at original fit and vif again
summary(fit)
vif(fit)

# Looks like there are some colinear terms. We will combine them instead of removing them
dtnum$mpg <- (dtnum$`highway-mpg` + dtnum$`city-mpg`)/2
dtnum$len <- (dtnum$length + dtnum$`wheel-base`)/2
dtnum$mass <- (dtnum$weight + dtnum$`curb-weight`)/2
dtnum$engine <- (dtnum$`engine-size` + dtnum$horsepower)/2

todrop <- c('highway-mpg', 'city-mpg', 'length', 'wheel-base', 'weight' , 'curb-weight', 'engine-size', 'horsepower')

dtdiv <- dtnum[, -c(1:3,5,6,10,12,13)]

fit2 <- lm(price ~ ., data = dtdiv)
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)
vif(fit2)

# Looks like we can remove length or mass. Let's try removing mass

fit3 <- lm(price ~ . -mass, data = dtdiv)
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)
vif(fit3)

AIC(fit)
AIC(fit2)
AIC(fit3)


# We can try to remove variables that are detrimental to the model.
# we can use setpAIC() funcionin the MASS library
# 6.1.2 - stepwise fit selection (page 207 ISLR)
# basically try adding or removing predictors, and pick the one with the 
# best score
library(MASS)

# It removes the variables that have an AIC (when removed) lower than the current AIC 
stepAIC(fit3, direction = 'both')
fit4 <- lm(formula = price ~ height + bore + stroke + `comression-ratio` + `peak-rpm` + 
             mpg + len + mass + engine - mass, data = dtdiv)


summary(fit4)
plot(fit4)

# Compare all the models we have made
AIC(englm)
AIC(fit)  
AIC(fit2)
AIC(fit3)
AIC(fit4)

# Look at the VIF of the last model
vif(fit4)

# We are missing a lot of information because we have excluded the character variables.
# We will create dummy variabels for the charactor variables.
# We do not want to make factors because factors imply ordinality to the data.
# But we do not want to include too many dummy varialbes because it increases computational strain
# and we begin to get the curse of dimensionality https://en.wikipedia.org/wiki/Curse_of_dimensionality

# To help make dummy varialbes we load the dummies library
#library('dummies')

library(rigr)

# Change the charaters to factors
str(dt)
dt$`fuel-type` <- as.factor(dt$`fuel-type`)
dt$`aspiration` <- as.factor(dt$`aspiration`)
dt$`engine-type` <- as.factor(dt$`engine-type`)
dt$`fuel-system` <- as.factor(dt$`fuel-system`)

str(dt)

# Make the factors into the dummy variables
### having trouble with names ####

fuel_dummies <- dummy(dt$`fuel-type`)
aspiration_dummies <- dummy(dt$`aspiration`)
engine_type_dummies <- dummy(dt$`engine-type`)
fuel_system_dummies <- dummy(dt$`fuel-system`)

# Add the new dummy variables to the data structure
dt2 <- cbind(dtdiv, fuel_dummies, aspiration_dummies, engine_type_dummies, fuel_system_dummies)
str(dt2)

# Make a new MLR model with the dummy variables added
newfit <- lm(price ~ ., data = dt2)
summary(newfit)
plot(newfit)

#We will remove these large outliers
dt2 <- dt2[-c(29,44,47),]

newfit <- lm(price ~ ., data = dt2)
summary(newfit)
plot(newfit)

# The last dummy variable is listed as NA because it is coinsidered the baseline.
# The baseline variable is incorporated into the intercept.
# We can remove the baseline variable using the stepAIC() function

library(MASS)

stepAIC(newfit, direction = 'both')

newfit2 <- lm(formula = price ~ height + bore + stroke + `comression-ratio` + `peak-rpm` + 
                mpg + len + mass + engine + `dummy(dt$\`fuel-type\`).gas vs diesel` + 
                `dummy(dt$aspiration).turbo vs std` + `l vs dohc` + `ohc vs dohc` + 
                `ohcf vs dohc` + `ohcv vs dohc` + `2bbl vs 1bbl` + `idi vs 1bbl` + 
                `mfi vs 1bbl` + `mpfi vs 1bbl` + `spdi vs 1bbl` + `spfi vs 1bbl`, data = dt2)


summary(newfit2)
plot(newfit2)
vif(newfit2)

newfit3 <- lm(formula = price ~ bore + stroke + `peak-rpm` + mass + engine + 
                `\`fuel-type\`_diesel` + `\`engine-type\`_dohc` + `\`engine-type\`_l` + 
                `\`engine-type\`_ohc` + `\`engine-type\`_ohcf` + `\`fuel-system\`_1bbl` + 
                `\`fuel-system\`_2bbl`, data = dt2)


summary(newfit3)
vif(newfit3)
stepAIC(newfit3, dirrection = 'both')

newfit4 <- lm(formula = price ~ bore + stroke + `peak-rpm` + mass + engine + 
                `\`fuel-type\`_diesel` + `\`engine-type\`_dohc` + `\`engine-type\`_l` + 
                `\`engine-type\`_ohc` + `\`engine-type\`_ohcf`, data = dt2)


summary(newfit4)
par(mfrow=c(2,2))
plot(newfit4)
vif(newfit4)

AIC(englm)
AIC(fit)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(newfit)
AIC(newfit2)
AIC(newfit3)
AIC(newfit4)

# Looks lke we have found the best model.

# So how might we describe this?  For every stroke increase, 
# the price decreases by about $5500 on average. If the engine type is ohc
# the price increase by about $7900 on average.

