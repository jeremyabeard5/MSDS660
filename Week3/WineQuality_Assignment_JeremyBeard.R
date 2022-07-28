##### Wine Quality In Class Assignment ##### 
##### 20220720
##### Jeremy Beard

### Loading Data ### 
# Load datatable, corrplot, MASS, dplyr, and car libraries
library(data.table)
library(dplyr)
library('ggplot2')
library('corrplot')
library('Rmisc')
library('MASS')
library('car')

# Load data (pick either white or red wine)
dt <- read.csv("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week3\\winequality-white.csv", sep = ";")

# Change data to datatable or dataframe 
dt <- as.data.frame(dt)

### Exploratory ### 
# Look at summary to see if there are any NAs or characters in the dataset. 
  # If there are NAs and/or characters remove them.
head(dt)
nrow(dt)
ncol(dt)
str(dt)
summary(dt)

which(is.na(dt$fixed.acidity))
which(is.na(dt$volatile.acidity))
which(is.na(dt$citric.acid))
which(is.na(dt$residual.sugar))
which(is.na(dt$chlorides))
which(is.na(dt$free.sulfur.dioxide))
which(is.na(dt$total.sulfur.dioxide))
which(is.na(dt$density))
which(is.na(dt$pH))
which(is.na(dt$sulphates))
which(is.na(dt$alcohol))
which(is.na(dt$quality))

# there are NO null values!

## Plot ## 
# We want to predict wine quality. Plot a histogram and box plot of Quality to look for outliers.
  # If there are any outliers in quality comment on them and discuss how you will deal with outliers
hist(dt$quality, main="Wine Quality (White)")

par(new = TRUE)

boxplot(dt$quality, horizontal = TRUE, col = rgb(0, 0.8, 1, alpha = 0.5))
#title("Wine Quality (White)")
box()


#I will now remove the outliers
Q1 <- quantile(dt$quality, .25)
Q3 <- quantile(dt$quality, .75)
IQR <- IQR(dt$quality)
#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(dt, dt$quality> (Q1 - 1.5*IQR) & dt$quality< (Q3 + 1.5*IQR))

hist(no_outliers$quality, main="Wine Quality (White) w/o Outliers")
par(new = TRUE)
boxplot(no_outliers$quality, horizontal = TRUE, col = rgb(0, 0.8, 1, alpha = 0.5))
box()


par(mfrow = c(1,1))

# Plot all the data to look for correlations
boxplot(dt, las=2)
title("White Wine Quality Variables")


# Plot a correlation matrix
corrplot(cor(dt), method = 'number')
#corrplot.mixed(cor(d, order = 'AOE')

### Linear Model ###
# Fit linear model to quality and all variables
m1 <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = no_outliers)


## Check Assumptions ## 
# View the summary, VIF, and residual plots of the fit
summary(m1)
par(mfrow = c(2,2))
plot(m1)

vif_values <- vif(m1)
vif_values

par(mfrow = c(1,1))
barplot(vif_values, main= "VIF Values", horiz = TRUE, col = "steelblue", las=2)
abline(v = 10, lwd = 3, lty = 2)

## Modifying Data ## 
# Are there any variables or observations you want to combine or remove based on VIF, p-value, and corrplot?
  # If so adjust the variables and create a new model and view the summary, VIF, and residuals plots.

#Using the VIF score, it would be smart to drop residual.sugar and density as they both have VIF values above 10.

#Using corrplot, it would be wise to drop citric.acid, free.sulfur.dioxide, pH, residual.sugar and sulphates as they all have nearly 0 correlation with quality
dt2 = no_outliers[ , c('fixed.acidity', 'volatile.acidity', 'chlorides', 'total.sulfur.dioxide', 'alcohol', 'quality')]



corrplot(cor(dt2), method = 'number')
m2 <- lm(quality ~ fixed.acidity + volatile.acidity + chlorides + total.sulfur.dioxide + alcohol, data = dt2)
summary(m2)
par(mfrow = c(2,2))
plot(m2)

vif_values2 <- vif(m2)
vif_values2

par(mfrow = c(1,1))
barplot(vif_values2, main= "VIF Values", horiz = TRUE, col = "steelblue", las=2)
abline(v = 10, lwd = 3, lty = 2)

## AIC Plots ## 
# Use step AIC to see if there are any variables to remove and create a new model and view the summary, VIF, and residuals plots (if necessary).


# View the AIC score of all the models
stepAIC(m1, direction = "both")
#using the stepAIC, i should remove citric.acid, total.sulfur.dioxide, chlorides
dt3 = no_outliers[ , c('fixed.acidity', 'volatile.acidity', 'residual.sugar', 'free.sulfur.dioxide', 'density', 'pH', 'sulphates', 'alcohol', 'quality')]
corrplot(cor(dt3), method = 'number')
#using correlation, I should remove residual.sugar, free.sulfur.dioxide, pH, sulphates
m3 <- lm(quality ~ fixed.acidity + volatile.acidity + residual.sugar + free.sulfur.dioxide + density + pH + sulphates + alcohol, data = dt3)
summary(m3)
par(mfrow = c(2,2))
plot(m3)

vif_values3 <- vif(m3)
vif_values3

par(mfrow = c(1,1))
barplot(vif_values3, main= "VIF Values", horiz = TRUE, col = "steelblue", las=2)
abline(v = 10, lwd = 3, lty = 2)
#using VIF, I should remove density and residual.sugar

dt4 = dt3[ , c('fixed.acidity', 'volatile.acidity', 'alcohol', 'quality')]
corrplot(cor(dt4), method = 'number')

m4 <- lm(quality ~ fixed.acidity + volatile.acidity + alcohol, data = dt4)
summary(m4)
par(mfrow = c(2,2))
plot(m4)

vif_values4 <- vif(m4)
vif_values4

par(mfrow = c(1,1))
barplot(vif_values4, main= "VIF Values", horiz = TRUE, col = "steelblue", las=2)
abline(v = 10, lwd = 3, lty = 2)

stepAIC(m4, direction = "both")

### Discussion 
# Post your R file to the discussion board and write a brief summary that includes:
# Which wine data you used. An interpretation of the model and which variable is most positively 
# correlated to quality and which variable is most negatively correlated with quality. 
# Which variables (if any) did you remove or combine. 
# Why (if applicable) did you remove/combine the variables.
# Also, post your final AIC and adjusted R^2 value.

