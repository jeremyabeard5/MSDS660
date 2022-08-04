## MULTI WAY ANOVA IN CLASS ASSIGNMENT ## 

# Load the libraries
library(ggplot2)
library(devtools)
library(data.table)
library(ggpubr)
library('magrittr')
library('dplyr')

dt <- read.csv("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week5\\Interactions_Categorical.csv", sep = ",")
# Load 'data set to data.table
dt <- as.data.table(dt)

# Check structure of dt
head(dt)
nrow(dt)
ncol(dt)
summary(dt)
str(dt)

# I have visually looked at the data and there are null values!

# Plot histogram of Enjoyment
hist(dt$Enjoyment, main="Food Enjoyment")
par(new = TRUE)
boxplot(dt$Enjoyment, horizontal = TRUE, col = rgb(0, 0.8, 1, alpha = 0.5))
box()

# Convert Food and Condiment to factors
cols <- c("Food", "Condiment")
dt %<>% mutate_each_(funs(factor(.)),cols)


# Plot Enjoyment vs the 2 other factors 
plot.design(Enjoyment ~ ., data = dt)



# Plot Individual Boxplots with means
boxplot(Enjoyment ~ Food, data = dt)
points(dt[, mean(Enjoyment), by=Food])
boxplot(Enjoyment ~ Condiment, data = dt)
points(dt[, mean(Enjoyment), by=Condiment])

ggboxplot(dt, x = "Food", y = "Enjoyment", color = "Condiment")

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
model <- aov(Enjoyment ~ Food * Condiment, data = dt)
summary(model)


# it seems that Food's p-value was greater than 0.05 
# which means the difference between the means of Food and Enjoyment is insignificant
# Let's create a model with just the significant variables
model2 <- aov(Enjoyment ~ Condiment + Food:Condiment, data = dt)
summary(model2)

# Based on the model people like hot dogs and ice cream the same.  There is a food enjoyment depends on condiment and
# food and condiment together interact and affect people enjoyment

# Perform TukeyHSD to check if which interactions have a significant difference
TukeyHSD(model2)


# Plot the residuals of the fit
par(mfrow = c(2,2))
plot(model2)
par(mfrow = c(1,1))



# Perform Shapiro test to see if residuals are normally distributed.
shapiro.test(residuals(model2))
hist(residuals(model2))


