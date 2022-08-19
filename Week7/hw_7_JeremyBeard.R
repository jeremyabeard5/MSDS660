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

boxplot(dt)
title("Placebo vs. New Drug")

# Convert the 'undetectable to a 0 and convert column to numeric
dt[dt$new.vaccine == 'undetectable'] <- 0
dt$new.vaccine <- as.numeric(dt$new.vaccine)

# View structure again to confirm:
dt
summary(dt)
str(dt)

# Box plot the data


# Use the shapiro-wilks test to see if they are from a normal distribution
shapiro.test(dt$old.vaccine)
shapiro.test(dt$new.vaccine)

# Plot the density distribution plots too.
hist(dt$old.vaccine)
hist(dt$new.vaccine)

# Are they from a normal distribution?
# No, both p values are under 0.05 which indicates that the variable is NOT normally distributed

# Are the data paired or unpaired?
# The data is paired because all old vaccine data can be compared with new vaccines data.

# Will you use a one sided or two sided test?
# Which test will you use?  
# Is there a significant difference in median flu antibodies detected with the new flu vaccine? 






###############################################################################################
# Japanese car problem.  Is there a difference in median mpg between Japanese and US cars?
library(readr)

# Load the mpg.csv data:
df <- read_csv("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week7\\mpg.csv")
df <- as.data.table(df)
head(df)

# View the structure
str(df)

# View a summary of the data
summary(df)

# Create a US data set from the V1 data and a Japanese data set from the V2 data
names(df)[names(df) == "V1"] <- "US"
names(df)[names(df) == "V2"] <- "JP"

head(df)
summary(df)
str(df)

# Remove the negative -999 outlier
df <- subset(df, US!=-999)
#df <- subset(df, JP!=-999 & US!=-999)

# View box plot of the data
boxplot(df)
title("MPG")







# Use the shapiro-wilks test to see if they are from a normal distribution


# Plot the density ditribution plots too.


 
# Are they from a normal distribution?  
# Are the data paired or unpaired?
# Will you use a one sided or two sided test?
# Which test will you use?  
# Is there a difference in median mpg between Japanese and US cars?



###########################################################################################
### vocab training problem.  Does vocab training significantly improve median test scores?

# Load the vocab.csv data:
dt2 = read_csv("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week7\\vocab.csv")

# View the structure of the data
str(dt2)

# View box plot of the data
boxplot(dt2)
title("Vocabulary")

# Use the shapiro-wilks test to see if they are from a normal distribution
shapiro.test(dt2$before.training)
shapiro.test(dt2$after.training)
#the p values are above 0.05 for each variable so it is normally distributed

# Plot the density ditribution plots too.
hist(dt2$before.training)
hist(dt2$after.training)



#Are they from a normal distribution?  
# Yes, according to the shapiro wilks test, both variables conform to a normal distribution

#Are the data paired or unpaired? 
# I would say the data is paired because they are comparable, before and after training.

# One sided or two sided test? 
# Which test will you use?  
  # I will use the Sign Test to see if the population means are significantly different from each other

# Does vocab training significantly improve median test scores?
SIGN.test(dt2)

#################################################################################################################

# mcdonald's menu problem.  Is there a significant difference in salt in the menu items?

# Load the menu.csv data
dt3 = read_csv("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week7\\menu.csv")
summary(dt3)

# View the structure of the data
str(dt3)

# View box plot of sodium vs the category


# Use the shapiro-wilks test to see if the sodium content in each categories are from a normal distribution.




# Are they from a normal distribution?  
# Do you have more than 2 groups?
# Which test will you use?  
# Is there a significant difference in salt in the menu items? 

