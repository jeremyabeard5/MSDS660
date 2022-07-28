##### In Class Assignment 4 - ANOVA #####

# Load the required libraries
# import and load "BusinessStartupCosts.csv" data 
dt <- read.csv("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week4\\BusinessStartupCosts.csv", sep = ",")

# Convert data to data table or data frame or whatever 
dt <- as.data.frame(dt)

head(dt)
nrow(dt)
ncol(dt)
str(dt)
summary(dt)

# Plot the dependent variable vs the categorical variables (should be a boxplot)
par(mfrow = c(1,1))
boxplot(Cost.in.Thousands.of.Dollars ~ Business, data = dt)

# Fit the dependent variable to the categorical variables using ANOVA
# First I will just fit a regression model to the two variables
fit <- lm(Cost.in.Thousands.of.Dollars ~ Business, data = dt)
summary(fit)
anova(fit)
par(mfrow=c(2,2))
plot(fit)

# Now I will perform a fit using ANOVA
afit <- aov(Cost.in.Thousands.of.Dollars ~ Business, data = dt)

# View the ANOVA summary
summary(afit)

# View the coefficients of the ANOVA fit
coefficients(afit)

# Change the plot window to a 2x2
par(mfrow=c(2,2))

# Plot the residuals
plot(afit)

# Perform the post hoc analysis that you were assigned.
# pairwise.t.test(dt$Cost.in.Thousands.of.Dollars, dt$Business)
# pairwise.t.test(dt$Cost.in.Thousands.of.Dollars, dt$Business, p.adjust.method = 'bonferroni')

TukeyHSD(afit) # TukeyHSD pairwise comparison
par(mfrow=c(1,1))
plot(TukeyHSD(afit))
# Which post hoc analysis did you perform and which variables(s) have means that are significantly different? 

# I chose the Tukey HSD test to perform and it showed that only the Pet Store - Baker test had a p adj value under 0.05.
# Every other comparison in the Tukey HSD test had a p adj value above 0.05 which implied their means are not significantly different
# source: https://stats.stackexchange.com/questions/253588/interpreting-tukeyhsd-output-in-r

