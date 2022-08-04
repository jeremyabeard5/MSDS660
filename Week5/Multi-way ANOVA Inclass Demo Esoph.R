## TWO WAY ANOVA DEMO ## 

library(data.table)
?esoph
dt <- esoph
dim(dt)
str(dt)
dt <- as.data.table(dt)

str(dt)

# remove the ncontrols column
dt <- dt[, 'ncontrols' := NULL]


## Predicting esophagus cancer based on patient age, tobacco and alcohol consumption

# Plot histogram of number of cancer cases
hist(dt$ncases,
     prob=TRUE,
     main = "Histogram of Cancer Cases",
     xlab = "Number of cases",
     ylab = "Density",
     col="Blue")


# Plots the average number of cases vs. factor level(s)
plot.design(ncases ~ ., data = dt)

boxplot(ncases ~ agegp, data = dt, ylab = 'number of cases', xlab = 'age group')
boxplot(ncases ~ alcgp, data = dt, ylab = 'number of cases', xlab = 'alcohol consuption')
boxplot(ncases ~ tobgp, data = dt, ylab = 'number of cases', xlab = 'tobaco consuption')

library(ggpubr)
ggboxplot(dt, x = "agegp", y = "ncases", color = "tobgp")

# let's throw out that outlier of 16 cases

dt_cln <- dt[ncases < 15,]

# check box plot again
ggboxplot(dt_cln, x = "agegp", y = "ncases", color = "alcgp")
boxplot(ncases ~ agegp, data = dt_cln)

# add means to box plot
points(dt_cln[, mean(ncases), by=agegp], col = 'Red')


# Build ANOVA model the * is giving interactions
model <- aov(ncases ~ alcgp * agegp * tobgp, data = dt_cln)
summary(model)

# Model is saturated and p-values cannot be calculated Df = observations. Need more observations than degrees of freedom (DF)
# model always takes one degree of freedom, so we need atleast one extra datapoint.  We will remove one interaction.
3+5+3+15+9+15+36+1

fit <- aov(ncases ~ alcgp * tobgp * agegp -alcgp:tobgp:agegp, data = dt_cln)
summary(fit)
# Looks like alcohol interaction with tobacco and tobacco interaction with age are not signification.  We will remove them

fit2 <- aov(ncases ~ alcgp + tobgp + agegp + alcgp:agegp, data = dt_cln)
summary(fit2) 
# All factors and interactions on our model are significant now.

# create interaction plot looking at alcohol consumption on age
interaction.plot(x.factor = dt_cln$agegp,
                 trace.factor = dt_cln$alcgp, 
                 response = dt_cln$ncases,
                 fun = mean, 
                 type = "b",  # shows each point
                 main = "Interaction Plot",
                 legend = TRUE,
                 trace.label = "Alcohol Group",
                 xlab = "Age Group",
                 ylab="Number of cancer cases",
                 pch=c(1, 2, 3, 4),
                 col = c("Red", "Blue", "Green","Black"))
# At least one of these interactions is significant.

# create interaction plot looking at tobacco consumption on age
interaction.plot(x.factor = dt_cln$agegp,
                 trace.factor = dt_cln$tobgp, 
                 response = dt_cln$ncases,
                 fun = mean, 
                 type = "b",  # shows each point
                 main = "Interaction Plot",
                 legend = TRUE,
                 trace.label = "Tobaco Group",
                 xlab = "Age Group",
                 ylab="Number of cancer cases",
                 pch=c(1, 2, 3, 4),
                 col = c("Red", "Blue", "Green","Black"))
# Even though the plot indicates there is an interaction the p-value says it is not significant


# create interaction plot looking at tobacco and alcohol consumption
interaction.plot(x.factor = dt_cln$alcgp,
                 trace.factor = dt_cln$tobgp, 
                 response = dt_cln$ncases,
                 fun = mean, 
                 type = "b",  # shows each point
                 main = "Interaction Plot",
                 legend = TRUE,
                 trace.label = "Tobaco Group",
                 xlab = "Alcohol Group",
                 ylab="Number of cancer cases",
                 pch=c(1, 2, 3, 4),
                 col = c("Red", "Blue", "Green","Black"))
# Even though the plot indicates there is an interaction the p-value says it is not significant

res <- TukeyHSD(fit2, which = 'alcgp:agegp')
res2 <- as.data.table(res$`alcgp:agegp`, keep.rownames = TRUE)
res2[res2$`p adj` < .05]


par(mfrow = c(2,2))
plot(fit2)
par(mfrow = c(1,1))



# null is that the distribution is normal, alternative is that it's not normal
shapiro.test(residuals(fit2))
hist(residuals(fit2), breaks=40)
# We fail to reject the null hypothesis and distribution is normal


