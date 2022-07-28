##### MSDS660 Homework Assignment - Week 4 - ANOVA #####
##### Jeremy Beard

# Load the required libraries
# import and load "ds_salaries.csv" data 
# It is a data science salary dataset from : https://www.kaggle.com/datasets/ruchi798/data-science-job-salaries
dt <- read.csv("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week4\\ds_salaries.csv", sep = ",")

# Convert data to data table or data frame or whatever 
dt <- as.data.frame(dt)

head(dt)
nrow(dt)
ncol(dt)
str(dt)
summary(dt)

which(is.na(dt$work_year))
which(is.na(dt$experience_level)) # EN entry-level, MI mid-level, SE senior, EX executive
which(is.na(dt$employment_type)) # PT part-time FT full-time CT contract FL freelance
which(is.na(dt$job_title))
which(is.na(dt$salary))
which(is.na(dt$salary_currency))
which(is.na(dt$salary_in_usd))
which(is.na(dt$employee_residence))
which(is.na(dt$remote_ratio))
which(is.na(dt$company_location))
which(is.na(dt$company_size)) # S: <50, M: 50<x<250, L: 250+

# it looks like the data is clean already! Thank goodness

# Plot the dependent variable vs the categorical variables (should be a boxplot)
# in this case, the dependent variable is salary_in_usd and i will choose the categorical 
#   variable of experience_level
par(mfrow = c(1,1))
#specify logical order for box plots, per here: https://r-graph-gallery.com/9-ordered-boxplot.html
dt$experience_level <- factor(dt$experience_level , levels=c("EN", "MI", "SE", "EX"))
boxplot(salary_in_usd ~ experience_level, data = dt)

# Fit the dependent variable to the categorical variables using ANOVA
# First I will just fit a regression model to the two variables
fit <- lm(salary_in_usd ~ experience_level, data = dt)
summary(fit)
anova(fit)
par(mfrow=c(2,2))
plot(fit)

# Now I will perform a fit using ANOVA
afit <- aov(salary_in_usd ~ experience_level, data = dt)

# View the ANOVA summary
summary(afit)

# View the coefficients of the ANOVA fit
coefficients(afit)

# Change the plot window to a 2x2
par(mfrow=c(2,2))

# Plot the residuals
plot(afit)

# Perform the post hoc analysis that you were assigned.

# I'm choosing to perform a TukeyHSD pairwise comparison
tfit <- TukeyHSD(afit, conf.level = 0.95) # TukeyHSD pairwise comparison
str(tfit)
print(tfit,digits=15)

par(mfrow=c(1,1))
plot(tfit)
# Which post hoc analysis did you perform and which variables(s) have means that are significantly different? 
# I performed a TukeyHSD pairwise comparison post hoc analysis. This analysis showed that 
#   it seems all p adj values are under p=0.05. this is interesting, this implies that
#   all differences between means are significant. I tried using difference confidence levels
#   of 0.90, 0.95, 0.97, and 0.99 and received the same p values using all of these. 
#   What could be causing these universally low p values?


# source: https://stats.stackexchange.com/questions/253588/interpreting-tukeyhsd-output-in-r

