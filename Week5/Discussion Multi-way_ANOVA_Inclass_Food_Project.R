## MULTI WAY ANOVA IN CLASS ASSIGNMENT ## 

# For this week discussion i have worked on Interactions_Categorical dataset which is provided and loaded
# the necessary libraries and set the working directory. In this our dependent variable is enjoyment and 
# food and condiment are two independent variables.

# Load the libraries
library(devtools)
library(qdata)
library(data.table)

# Load 'Interactions_Categorical.csv' data set
setwd("~/Downloads/Interactions_Categorical.csv")
dt <- read.csv("Interactions_Categorical.csv")

# Here i have converted the dataset to the data table()
dt <- as.data.table(dt)

# Here we are checking the structure of the object and we found that there are two characters in the
# dataset.

str(dt)

# Now we are converting these characters into the factors.

# Convert Food and Condiment to factors
dt$Food <- as.factor(dt$Food)
dt$Condiment <- as.factor(dt$Condiment)

str(dt)

# Want to see which food and condiment people enjoy most.  Do people enjoy hot dogs or ice cream more and which condiment do
# people prefer with their food.  It depends! People probably do not want mustard with ice cream and chocolate sauce with 
# hot dogs

# Plot histogram of Enjoyment

hist(dt$Enjoyment,
     prob=TRUE,
     main = "Histogram of Enjoyment",
     xlab = "Enjoyment",
     ylab = "Density",
     col="Blue")


# Plot Enjoyment vs the 2 other factors 

plot.design(Enjoyment ~ ., data = dt)

# From the above plot i see that most of the people enjoy chocolate sauce more than Mustard, where as
# people enjoy more Hot Dog compared to Ice Cream.

# Plot Individual Boxplots with means
boxplot(Enjoyment ~ Food, data = dt, ylab = 'Enjoyment', xlab = 'Food')
points(dt[, mean(Enjoyment), by=Food])
boxplot(Enjoyment ~ Condiment, data = dt, ylab = 'Enjoyment', xlab = 'Condiment')
points(dt[, mean(Enjoyment), by=Condiment])

# From the above boxplot we can again clearly say that people enjoy more chocolate sauce compared to
# Mustard.

# Create interaction plot looking at Condiment and Food
interaction.plot(x.factor = dt$Food,
                 trace.factor = dt$Condiment, 
                 response = dt$Enjoyment,
                 fun = mean, 
                 type = "b",  # shows each point
                 main = "Interaction Plot",
                 legend = TRUE,
                 trace.label = "Condiment",
                 xlab = "Food",
                 ylab="Enjoyment",
                 pch=c(1),
                 col = c("Red"))



# Build ANOVA model - the * is giving interactions. Show anova fit summary

fit <- aov(Enjoyment ~ Food * Condiment, data = dt)
summary(fit)

# Based on the model people like hot dogs and ice cream the same.  There is a food enjoyment depends on condiment and
# food and condiment together interact and affect people enjoyment

# Perform TukeyHSD to check if which interactions have a significant difference

TukeyHSD(fit)

# Plot the residuals of the fit

par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))


# Perform Shapiro test to see if residuals are normald distributed.
shapiro.test(residuals(fit))
hist(residuals(fit), breaks=40)

# Here we are testing the residuals and can see that the p value is 0.229 which is good.

