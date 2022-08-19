library(data.table)
library(BSDA)
library(agricolae)

# Flu shot problem.  Is there a significant difference in median flu antibodies detected with the new flu vaccine?

# Load the flu_shot.csv data:
dt <- read.csv("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week7\\placebo_new_drug.csv")
dt <- as.data.table(dt)

# View the structure of the data
dt
summary(dt)
str(dt)
nrow(dt)
ncol(dt)

boxplot(dt)
title("Placebo vs. New Drug")
#Hmm should I remove the outlier 12? There are only 5 datapoints....hmm.....

hist(dt$Placebo)
hist(dt$New.Drug)

# Use the shapiro-wilks test to see if they are from a normal distribution
shapiro.test(dt$Placebo)
shapiro.test(dt$New.Drug)
# we have a small sample size so determining the distribution of the data is important
# using the shapireo wilk test, the p values of each variable were above 0.05 which
# does not indicate non-normality. The data may be called normal

# Are the data paired or unpaired?
# The data is paired because each placebo datapoint can be paired with the new.drug datapoint
# this will make comparison easier

# i will perform a Wilcoxon Signed Test to see if the two populations have a significant different 
median(dt$Placebo)
median(dt$New.Drug)

wilcox.test(dt$Placebo, dt$New.Drug)
# the wilcoxon test produced a p value greater than 0.05, so no conclusions can be made from this test

# Let's perform a sign test now
SIGN.test(x = dt$Placebo, y = dt$New.Drug, alternative = 'less')
# This p value is huge, so we cannot say that the two median's are any different really. 
# We fail to reject the null hypothesis in this case


# Will you use a one sided or two sided test?
# Which test will you use?  
# Is there a significant difference in median flu antibodies detected with the new flu vaccine? 



