# Load the required libraries
library(data.table)
library(agricolae)

dt <- chickwts # load data from R library
dt <- as.data.table(dt) # convert data to data table
plot(weight ~ feed, data = dt) # plot weight vs feed

fit <- lm(weight ~ feed, data = dt) # Fit the chick weight to feed type.
summary(fit) # View the fit summary
anova(fit) # View ANOVA p-value for the fit
par(mfrow=c(2,2)) # Change the plot window to a 2x2
plot(fit) # plot the residuals

afit <- aov(weight ~ feed, data = dt) # Create an ANOVA fit of the data
summary(afit) # View the summary
coefficients(afit) # View the coefficients of the ANOVA fit
plot(afit)

library(stats)
# Does anyone know what pairwise means?
pairwise.t.test(dt$weight, dt$feed) # Pairwise t-test comparison
pairwise.t.test(dt$weight, dt$feed, p.adjust.method = 'bonferroni') # Bonferroni pairwise t-test comarison
TukeyHSD(afit) # TukeyHSD pairwise comparison
print(LSD.test(afit, 'feed')) # Fisher LSD pairwise comparison
#oneway.test(dt$weight ~ dt$feed, posthoc = 'games-howell') # Games-Howell pairwise comparison
#oneway(dt$weight, dt$feed, posthoc = 'tukey') # Another TukeyHSD pairwise comparison
duncan.test(afit, 'feed', DFerror = 5, alpha = 0.05, console = TRUE) # Duncan pairwise comparison

