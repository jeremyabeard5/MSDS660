## TWO WAY ANOVA DEMO ## 

#install.packages('devtools')
library(devtools)
devtools::install_github(repo = "quantide/qdata")
library(qdata)
library(data.table)

?distance
data(distance)
dt <- distance
rm(distance)

dim(dt)
str(dt)
dt <- as.data.table(dt)

# How does tire tread, tire type, and ABS affect breaking distance?

# Plot histogram of braking distance
hist(dt$Distance,
     prob=TRUE,
     main = "Histogram of braking distance",
     xlab = "Breaking distance",
     ylab = "Density",
     col="Blue")

# Distance vs the 4 factors 
plot.design(Distance ~ ., data = dt)

# Individual Boxplots with means
boxplot(Distance ~ Tire, data = dt, ylab = 'Breaking distance', xlab = 'Tire')
points(dt[, mean(Distance), by=Tire])
boxplot(Distance ~ Tread, data = dt, ylab = 'Breaking distance', xlab = 'Tread')
points(dt[, mean(Distance), by=Tread])
boxplot(Distance ~ ABS, data = dt, ylab = 'Breaking distance', xlab = 'Abs')
points(dt[, mean(Distance), by=ABS], col ="Red")


# create interaction plot looking at ABS and Tire
interaction.plot(x.factor = dt$ABS,
                 trace.factor = dt$Tire, 
                 response = dt$Distance,
                 fun = mean, 
                 type = "b",  # shows each point
                 main = "Interaction Plot",
                 legend = TRUE,
                 trace.label = "Tire",
                 xlab = "ABS",
                 ylab="Breaking Distance",
                 pch=c(1),
                 col = c("Red"))

# create interaction plot looking at ABS and Tread
interaction.plot(x.factor = dt$ABS,
                 trace.factor = dt$Tread, 
                 response = dt$Distance,
                 fun = mean, 
                 type = "b",  # shows each point
                 main = "Interaction Plot",
                 legend = TRUE,
                 trace.label = "Tread",
                 xlab = "ABS",
                 ylab="Breaking Distance",
                 pch=c(1),
                 col = c("Red"))

# create interaction plot looking at Tread and Tire
interaction.plot(x.factor = dt$Tread,
                 trace.factor = dt$Tire, 
                 response = dt$Distance,
                 fun = mean, 
                 type = "b",  # shows each point
                 main = "Interaction Plot",
                 legend = TRUE,
                 trace.label = "Tire",
                 xlab = "Tread",
                 ylab="Breaking Distance",
                 pch=c(1),
                 col = c("Red"))

# Build ANOVA model the * is giving interactions
model <- aov(Distance ~ ABS * Tire * Tread, data = dt)
summary(model)
# Based on the model there are no significant interactions between ABS, Tire, and Tread

TukeyHSD(model)

fit <- aov(Distance ~ ABS+Tire+Tread, data = dt)
summary(fit)

TukeyHSD(fit)

fit2 <- aov(Distance ~ ABS+Tire, data = dt)
summary(fit2)


TukeyHSD(fit2)

par(mfrow = c(2,2))
plot(fit2)
par(mfrow = c(1,1))


# null is that the distribution is normal, alternative is that it's not normal
shapiro.test(residuals(model))
hist(residuals(model), breaks=40)
# We fail to reject the null hypothesis and distribution is normal



