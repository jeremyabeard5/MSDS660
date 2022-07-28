# Pick a dataset of your choosing that is appropriate for MLR. (Should be numerical and not have a lot of missing values). Make sure to comment in your report where you downloaded the dataset.
# 
# I'm choosing an air quality dataset from the following link: https://archive.ics.uci.edu/ml/datasets/Air+Quality#
#
# Attributes
# 0 Date (DD/MM/YYYY)
# 1 Time (HH.MM.SS)
# 2 True hourly averaged concentration CO in mg/m^3 (reference analyzer)
# 3 PT08.S1 (tin oxide) hourly averaged sensor response (nominally CO targeted)
# 4 True hourly averaged overall Non Metanic HydroCarbons concentration in microg/m^3 (reference analyzer)
# 5 True hourly averaged Benzene concentration in microg/m^3 (reference analyzer)
# 6 PT08.S2 (titania) hourly averaged sensor response (nominally NMHC targeted)
# 7 True hourly averaged NOx concentration in ppb (reference analyzer)
# 8 PT08.S3 (tungsten oxide) hourly averaged sensor response (nominally NOx targeted)
# 9 True hourly averaged NO2 concentration in microg/m^3 (reference analyzer)
# 10 PT08.S4 (tungsten oxide) hourly averaged sensor response (nominally NO2 targeted)
# 11 PT08.S5 (indium oxide) hourly averaged sensor response (nominally O3 targeted)
# 12 Temperature in Â°C
# 13 Relative Humidity (%)
# 14 AH Absolute Humidity
#
#
#

# Objective: Find the best multilinear regression model of the dataset of your choosing. ?
# 
# You must include:
#   1. Box plot and histogram of the dependent variable

# First we load the libraries and data
library(data.table)
library(dplyr)
library('ggplot2')
library('corrplot')
library('Rmisc')
library('MASS')
library('car')

# Load data (pick either white or red wine)
dt <- read.csv2("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week3\\AirQualityUCI\\AirQualityUCI.csv", sep = ";")

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

which(is.na(dt$Date))
which(is.na(dt$Time))
which(is.na(dt$CO.GT.))
which(is.na(dt$PT08.S1.CO.))
which(is.na(dt$NMHC.GT.))
which(is.na(dt$C6H6.GT.))
which(is.na(dt$PT08.S2.NMHC.))
which(is.na(dt$N0x.GT.))
which(is.na(dt$PT08.S3.N0x.))
which(is.na(dt$N02.GT.))
which(is.na(dt$PT08.S4.N02.))
which(is.na(dt$PT08.S5.03.))
which(is.na(dt$T))
which(is.na(dt$RH))
which(is.na(dt$AH))
#which(is.na(dt$X))
#which(is.na(dt$X.1))




sum(is.na(dt$PT08.S1.CO.))
sum(is.na(dt$NMHC.GT.))
sum(is.na(dt$PT08.S2.NMHC.))
sum(is.na(dt$X))
sum(is.na(dt$X.1))

# It looks like we need to remove the X and X.1 columns as they contain null values only.

dtnona <- dt[-c(16, 17)]

names(dt)
names(dtnona)
nrow(dtnona)
dtnona <- dtnona[complete.cases(dtnona), ]
names(dtnona)
nrow(dtnona)

which(is.na(dtnona$Date))
which(is.na(dtnona$Time))
which(is.na(dtnona$CO.GT.))
which(is.na(dtnona$PT08.S1.CO.))
which(is.na(dtnona$NMHC.GT.))
which(is.na(dtnona$C6H6.GT.))
which(is.na(dtnona$PT08.S2.NMHC.))
which(is.na(dtnona$N0x.GT.))
which(is.na(dtnona$PT08.S3.N0x.))
which(is.na(dtnona$N02.GT.))
which(is.na(dtnona$PT08.S4.N02.))
which(is.na(dtnona$PT08.S5.03.))
which(is.na(dtnona$T))
which(is.na(dtnona$RH))
which(is.na(dtnona$AH))

# Okay! so we've cleaned all null values. Now I'm going to remove the date column and convert time to a numerical hour

dtfinal <- dtnona[-c(1)]
names(dtfinal)
head(dtfinal)

dtfinal$Time = as.numeric(substr(dtfinal$Time, start = 1, stop = 2))
head(dtfinal)

summary(dtfinal)
str(dtfinal)

# Awesome, all the data is clean and numeric now

hist(dtfinal$CO.GT., main="Air Quality, CO2 Concentration")
par(new = TRUE)
boxplot(dtfinal$CO.GT., horizontal = TRUE, col = rgb(0, 0.8, 1, alpha = 0.5))
box()

# it looks like we want to remove all the negative concentrations, these are erroneous

dtfinal <- subset(dtfinal, dtfinal$CO.GT. >= 0)

hist(dtfinal$CO.GT., main="Air Quality, CO2 Concentration")
par(new = TRUE)
boxplot(dtfinal$CO.GT., horizontal = TRUE, col = rgb(0, 0.8, 1, alpha = 0.5))
box()

# now let's remove outliers

Q1 <- quantile(dtfinal$CO.GT., .25)
Q3 <- quantile(dtfinal$CO.GT., .75)
IQR <- IQR(dtfinal$CO.GT.)
#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
dtfinal <- subset(dtfinal, dtfinal$CO.GT.> (Q1 - 1.5*IQR) & dtfinal$CO.GT.< (Q3 + 1.5*IQR))

hist(dtfinal$CO.GT., main="Air Quality, Outliers Removed")
par(new = TRUE)
boxplot(dtfinal$CO.GT., horizontal = TRUE, col = rgb(0, 0.8, 1, alpha = 0.5))
box()

# 2. A correlation plot of all the numerical variables

corrplot(cor(dtfinal), method = 'square')
corrplot(cor(dtfinal), method = 'number')

# it looks like "True hourly averaged concentration CO in mg/m^3 (reference analyzer)" is our dependent variable, CO.GT

#Correlations
#most positive
# N0x.GT. (0.72), PT08.S5.03. (0.65), PT08.S2.NMHC. (0.61), N02.GT. (0.49), PT08.S1.CO. (0.46), PT08.S4.N02. (0.37), Time (0.36)
#most negative
# PT08.S3.N0x. (-0.57)

# 3. A MLR model that has a summary, residual plots, and a VIF analysis

# First, I'm going to create a model that takes every variable

m1 <- lm(CO.GT. ~ Time + PT08.S1.CO. + NMHC.GT. + C6H6.GT. + PT08.S2.NMHC. + NOx.GT. + PT08.S3.NOx. + NO2.GT. + PT08.S4.NO2. + PT08.S5.O3. + T + RH + AH, data = dtfinal)
summary(m1)
par(mfrow = c(2, 2))
plot(m1)
vif_values1 <- vif(m1)
vif_values1
par(mfrow = c(1,1))
barplot(vif_values1, main= "VIF Values m1", horiz = TRUE, col = "steelblue", las=2)
abline(v = 10, lwd = 3, lty = 2)

# Next, I want to create a model that just takes the most highly correlated variables with CO.GT, mentioned above
names(dtfinal)
dtm2 <- dtfinal[ , c('CO.GT.','NOx.GT.', 'PT08.S5.O3.', 'PT08.S2.NMHC.', 'NO2.GT.', 'PT08.S1.CO.', 'PT08.S4.NO2.', 'Time', 'PT08.S3.NOx.')]
names(dtm2)
corrplot(cor(dtm2), method = 'square')
corrplot(cor(dtm2), method = 'number')
m2 <- lm(CO.GT. ~ NO2.GT. + NOx.GT. + PT08.S5.O3. + PT08.S2.NMHC. + PT08.S1.CO. + PT08.S4.NO2. + Time + PT08.S3.NOx., data = dtm2)
summary(m2)
par(mfrow = c(2, 2))
plot(m2)
vif_values2 <- vif(m2)
vif_values2
par(mfrow = c(1,1))
barplot(vif_values2, main= "VIF Values m2", horiz = TRUE, col = "steelblue", las=2)
abline(v = 10, lwd = 3, lty = 2)

# Now based on the VIF value I will remove PT08.S1.CO., PT08.S2.NMHC., and PT08.S5.O3. as they all have VIF values above 10
dtm25 <- dtm2[ , c('CO.GT.','NOx.GT.', 'NO2.GT.', 'PT08.S4.NO2.', 'Time', 'PT08.S3.NOx.')]
names(dtm25)
corrplot(cor(dtm25), method = 'square')
corrplot(cor(dtm25), method = 'number')
m25 <- lm(CO.GT. ~ NO2.GT. + NOx.GT. + PT08.S4.NO2. + Time + PT08.S3.NOx., data = dtm25)
summary(m25)
par(mfrow = c(2, 2))
plot(m25)
vif_values25 <- vif(m25)
vif_values25
par(mfrow = c(1,1))
barplot(vif_values25, main= "VIF Values m25", horiz = TRUE, col = "steelblue", las=2)
abline(v = 10, lwd = 3, lty = 2)


# 4. Comment in the R code a justification on why you removed, combined, or left all variables/observations.

# I chose to only keep values which had a high degree of correlation with CO.GT. (abs>0.3) and which had a VIF value under 10. 
# This led to only choosing 'CO.GT.','NOx.GT.', 'NO2.GT.', 'PT08.S4.NO2.', 'Time', and 'PT08.S3.NOx.'

# 5. Kudos bonus if you create dummy variables from character values.

# This step was not needed as the character values (strings) could be converted to numeric as it was a Time variable

# 6. a stepAIC analysis

fit1 <- stepAIC(m1, direction = "both")
fit1
summary(fit1)

dtm3 = dtfinal[ , c('CO.GT.', 'Time', 'NMHC.GT.', 'PT08.S1.CO.', 'PT08.S2.NMHC.', 'C6H6.GT.', 'NOx.GT.', 'NO2.GT.', 'PT08.S4.NO2.', 'PT08.S5.O3.', 'T', 'RH', 'AH')]
corrplot(cor(dtm3), method = 'square')
corrplot(cor(dtm3), method = 'number')
m3 <- lm(formula = CO.GT. ~ Time + PT08.S1.CO. + NMHC.GT. + C6H6.GT. + PT08.S2.NMHC. + NOx.GT. + NO2.GT. + PT08.S4.NO2. + PT08.S5.O3. + T + RH + AH, data = dtm3)
summary(m3)
par(mfrow = c(2, 2))
plot(m3)
vif_values3 <- vif(m3)
vif_values3
par(mfrow = c(1,1))
barplot(vif_values3, main= "VIF Values m3", horiz = TRUE, col = "steelblue", las=2)
abline(v = 10, lwd = 3, lty = 2)

# based on the correlation values and VIF values, I will drop  from dtm3 to get dtm4 model

dtm4 = dtm3[ , c('CO.GT.', 'Time', 'NOx.GT.', 'NO2.GT.')]
corrplot(cor(dtm4), method = 'square')
corrplot(cor(dtm4), method = 'number')
m4 <- lm(formula = CO.GT. ~ Time + NOx.GT. + NO2.GT., data = dtm4)
summary(m4)
par(mfrow = c(2, 2))
plot(m4)
vif_values4 <- vif(m4)
vif_values4
par(mfrow = c(1,1))
barplot(vif_values4, main= "VIF Values m4", horiz = TRUE, col = "steelblue", las=2)
abline(v = 10, lwd = 3, lty = 2)




# 7. Summary, residual plots, and a VIF analysis of the final best model you created.

anova(m1, m2)
# so m2 is better than m1, p < 2.2e-16
anova(m1, m25)
# m25 is better than m1, p < 2.2e-16
anova(m1, m3)
# m1 is better than m3, p=0.7365
anova(m1, m4)
# m4 is better than m1, p < 2.2e-16
anova(m2, m25)
# m25 is better than m2, p < 2.2e-16
anova(m2, m3)
# m3 is better than m2, p < 2.2e-16
anova(m2, m4)
# m4 is better than m2, p < 2.2e-16
anova(m25, m3)
# m3 is better than m25, p < 2.2e-16
anova(m25, m4)
# m4 is better than m25, p < 2.2e-16
anova(m3, m4)
# m4 is better than m3, p < 2.2e-16

# Adjusted R^2
# The bigger adjusted R^2, the better the fit
# m1: 0.8696
# m2: 0.7689
# m25: 0.7493
# m3: 0.8696
# m4: 0.5636

# RSE
# The smaller the RSE, the better the fit
# m1: 0.4343
# m2: 0.578
# m25: 0.6021
# m3: 0.4342
# m4: 0.7944

# So the best model I created was m3, the model suggested by the stepAIC analysis!
summary(m3)
par(mfrow = c(2, 2))
plot(m3)
vif_values3 <- vif(m3)
vif_values3
par(mfrow = c(1,1))
barplot(vif_values3, main= "VIF Values m3", horiz = TRUE, col = "steelblue", las=2)
abline(v = 10, lwd = 3, lty = 2)

# 8. AIC scores of all models built
# the smaller the AIC, the better the fit
AIC(m1)
AIC(m2)
AIC(m25)
AIC(m3)
AIC(m4)

