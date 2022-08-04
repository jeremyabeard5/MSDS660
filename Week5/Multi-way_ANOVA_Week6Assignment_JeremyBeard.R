## MULTI WAY ANOVA WEEK 6 HOMEWORK PROJECT ASSIGNMENT ## 

# Load the libraries
library(ggplot2)
library(devtools)
library(data.table)
library(ggpubr)
library('magrittr')
library('dplyr')

dt <- read.csv("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week5\\engineer.csv", sep = ",")
# Load 'data set to data.table
dt <- as.data.table(dt)

# Check structure of dt
head(dt)
nrow(dt)
ncol(dt)
summary(dt)
str(dt)

# I have visually looked at the data and there are no null values!

# Plot histogram of Salary
hist(dt$Salary, main="Salary")
par(new = TRUE)
boxplot(dt$Salary, horizontal = TRUE, col = rgb(0, 0.8, 1, alpha = 0.5))
box()

# Convert Profession and Region to factors
cols <- c("Profession", "Region")
dt %<>% mutate_each_(funs(factor(.)),cols)


# Plot Salary vs the 2 other factors 
plot.design(Salary ~ ., data = dt)




# Plot Individual Boxplots with means
boxplot(Salary ~ Profession, data = dt)
points(dt[, mean(Salary), by=Profession])
boxplot(Salary ~ Region, data = dt)
points(dt[, mean(Salary), by=Region])

ggboxplot(dt, x = "Profession", y = "Salary", color = "Region")

# Create interaction plot looking at Region and Profession
interaction.plot(x.factor = dt$Profession,
                 trace.factor = dt$Region, 
                 response = dt$Salary,
                 fun = mean, 
                 type = "b",  # shows each point
                 main = "Interaction Plot",
                 legend = TRUE,
                 trace.label = "Region",
                 xlab = "Profession",
                 ylab="Salary",
                 pch=c(1),
                 col = c("Red"))



# Build ANOVA model - the * is giving interactions. Show anova fit summary
model <- aov(Salary ~ Profession * Region, data = dt)
summary(model)

# Everything is significant! 


model2 <- aov(Salary ~ Profession + Region + Profession:Region, data = dt)
summary(model2)

# Based on the model people like hot dogs and ice cream the same.  There is a Profession Salary depends on Region and
# Profession and Region together interact and affect people Salary

# Perform TukeyHSD to check if which interactions have a significant difference
TukeyHSD(model)

#We can see Seattle-San Francisco is not significant. 
#We can also see this when we look at the interaction plot. 
#The lines of Seattle and San Francisco were very similar.

# Plot the residuals of the fit
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))



# Perform Shapiro test to see if residuals are normally distributed.
shapiro.test(residuals(model))
hist(residuals(model))


