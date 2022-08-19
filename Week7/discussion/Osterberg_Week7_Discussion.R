library(data.table)
library(BSDA)
library(agricolae)
library(readr)

# Flu shot problem.  Is there a significant difference in median flu antibodies detected with the new flu vaccine?

# Load the flu_shot.csv data:

flu <- read_csv("~/Backup/StatMethods/new_flu_shot.csv")

# View the structure of the data

str(flu)

# Convert the 'undetectable to a 0 and convert column to numeric

flu$new.vaccine[flu$new.vaccine == 'undetectable'] <- 0
flu$new.vaccine <- as.numeric(flu$new.vaccine)

# View structure again to confirm:

str(flu)

# Box plot the data

boxplot(flu)

# Use the shapiro-wilks test to see if they are from a normal distribution

shapiro.test(flu$old.vaccine)
shapiro.test(flu$new.vaccine)

# Plot the density distribution plots too.

hist(flu$old.vaccine)
hist(flu$new.vaccine)

old_density <- density(flu$old.vaccine)
plot(old_density)
new_density <- density(flu$new.vaccine)
plot(new_density)

# Are they from a normal distribution?

# No, it failed the Shapiro-Wilks test for both old and new vaccines

# Are the data paired or unpaired?

# I am assuming that they are paired because my guess at how the data is organized
# is that each record is a different participant in the experiment.

# Will you use a one sided or two sided test?

# I will use a two sided test because we do not know which direction the median could
# have shifted

# Which test will you use?  
# Is there a significant difference in median flu antibodies detected with the new flu vaccine? 

# I will use the Sign test to see if the medians are significantly different
SIGN.test(x = flu$old.vaccine, y = flu$new.vaccine, alternative = 'two.sided')

# It appears that the difference between the new and old flu vaccines was not significant


###############################################################################################
# Japanese car problem.  Is there a difference in median mpg between Japanese and US cars?

# Load the mpg.csv data:
mpg <- read.table("~/Backup/StatMethods/mpg.csv")

# View the structure

str(mpg)

# View a summary of the data

summary(mpg)

# View box plot of the data

boxplot(mpg)

# Create a US data set from the V1 data and a Japanese data set from the V2 data

names(mpg)[names(mpg) == "V1"] <- "US"
names(mpg)[names(mpg) == "V2"] <- "JP"

# Remove the negative -999 outlier

mpg <- subset(mpg, JP!=-999 & US!=-999)

# Use the shapiro-wilks test to see if they are from a normal distribution

shapiro.test(mpg$US)
shapiro.test(mpg$JP)

# Plot the density ditribution plots too.

hist(mpg$US)
hist(mpg$JP)
 
# Are they from a normal distribution?  

# One is and one isn't

# Are the data paired or unpaired?

# Unpaired

# Will you use a one sided or two sided test?

# Two sided test, it could be higher or lower, we aren't deciding that now

# Which test will you use?  
# Is there a difference in median mpg between Japanese and US cars?

# Kolmogorov-Smirnov Test
ks.test(mpg$US, mpg$JP, alternative="two.sided")

# Yes there is a difference and they come from two different distributions

###########################################################################################
### vocab training problem.  Does vocab training significantly improve median test scores?

# Load the vocab.csv data:
voc <- read_csv("~/Backup/StatMethods/vocab.csv")
# View the structure of the data
str(voc)
# View box plot of the data
boxplot(voc)
# Use the shapiro-wilks test to see if they are from a normal distribution
shapiro.test(voc$before.training)
shapiro.test(voc$after.training)

# Plot the density distribution plots too.
hist(voc$before.training)
hist(voc$after.training)


#Are they from a normal distribution?  
# Yes
#Are the data paired or unpaired? 
# Paired
# One sided or two sided test? 
# One sided
# Which test will you use?  
# Wilcoxon
wilcox.test(voc$before.training, voc$after.training, alternative = 'greater')
# Does vocab training significantly improve median test scores?
# No statistically significant change in median score


#################################################################################################################

# mcdonald's menu problem.  Is there a significant difference in salt in the menu items?

# Load the menu.csv data
menu <- read_csv("~/Backup/StatMethods/menu.csv")

# View the structure of the data

str(menu)

# View box plot of sodium vs the category


boxplot(Sodium ~ Category, data=menu)

# Use the shapiro-wilks test to see if the sodium content in each categories are from a normal distribution.
fit <- aov(Sodium ~ Category, data=menu)
shapiro.test(residuals(fit))

# Are they from a normal distribution?  
# No
# Do you have more than 2 groups?
duncan.test(fit, 'Category', DFerror = 5, alpha = 0.05, console = TRUE)
# According to the Duncan Test we have 5 groups
# Which test will you use?  
# Kruskal-Wallis
# Is there a significant difference in salt in the menu items? 
kruskal.test(menu, Sodium ~ Category)
# Yes there is a significant difference in salt in the menu items
