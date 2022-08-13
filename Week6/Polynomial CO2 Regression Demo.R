#### POLYNOMIAL REGRESSION DEMO ####

##### Libraries #####
library(data.table)


##### Reading in Data ##### 
setwd("~/Work/Regis/MSDS660_Instructor/Week6")

dt <- read.csv('co2data.csv', header = TRUE)
dt <- as.data.table(dt)

## Cleaning 

dt$Date <-  as.Date(dt$Date, format = "%m/%d/%Y") # Dates usually suck - package Lubritate can help
dt$Date <- as.numeric(dt$Date)
plot(dt)


##### Modeling #####
# Start by plotting a linear model of CO2 concentration vs date
linear <- lm(CO2ppm ~ Date, data = dt)
abline(linear, col = 'red')
summary(linear)

# Assumptions 
par(mfrow=c(2, 2))
plot(linear) # Looks like a curve in the residuals vs fitted values.


# Let's try a few polynomial fits...
poly <- lm(CO2ppm ~ Date + I(dt$Date^2), data = dt)
poly2 <- lm(CO2ppm ~ I(dt$Date^2), data = dt)

# Plot the data and the lines of fit
par(mfrow=c(1, 1))
plot(dt)
abline(linear, col = 'red')
lines(sort(dt$Date), fitted(poly)[order(dt$Date)], col='green')
lines(sort(dt$Date), fitted(poly2)[order(dt$Date)], col='blue')

# Look at the summary of the 3 fits
library(stargazer)
stargazer(linear, poly, poly2, type = 'text') # The residual error(RSE and Adjusted R^2) has improved over the linear

# Use anova to check if there is a significant difference between the linear and polynominal fit
anova(poly, poly2)

# Plot the residuals
par(mfrow=c(2, 2))
plot(poly) # summary plots show a much better fit

# Model Evaluation 
AIC(linear)
AIC(poly) # AIC score confirms that poly is a much better fit of the data.
AIC(poly2) 

anova(linear, poly)
# The anova table shows there is a significant difference in how the linear model and the polynominal model fit the data.

anova(poly, poly2)
# The anova table shows there is a significant difference in how the polynominal model and the other polynominal model fit the data.


