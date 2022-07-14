# load the data.table, ggolot2, and dplyr libraries and the zillow_price.csv file
library('ggplot2')
library('Rmisc')
library('stargazer')
library('dplyr')
library('purrr')

dt <- read.csv("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week2\\zillow_price.csv")

# Convert the file to a data table
dt <- as.data.frame(dt)

head(dt)
# how many observations and columns are there?
# number of observations = number of rows = 90275
nrow(dt)
# number of columns = 60 
ncol(dt)

# use str and summary to see how many missing values we have,
# and what the data looks like
str(dt)
summary(dt)

# first I'm going to just create some boxplots to satisfy my curiosity
small_nums <- c('bathroomcnt',
                'bedroomcnt',
                'roomcnt')

big_nums <- c('taxvaluedollarcnt',
              'landtaxvaluedollarcnt',
              'price')
dt_smallnums <- dt[ , small_nums]
dt_bignums <- dt[ , big_nums]
boxplot(dt_bignums)
title("Price Variables")
boxplot(dt_smallnums)
title("# Room Variables")
boxplot(dt$yearbuilt)
title("Year Built")
boxplot(dt$calculatedfinishedsquarefeet)
title("Square Feet")


# columns that are numeric and don't have lots of missing values
# you can add others if you like
numeric_cols <- c('bathroomcnt',
                  'bedroomcnt',
                  'calculatedfinishedsquarefeet',
                  'roomcnt',
                  'yearbuilt',
                  'taxvaluedollarcnt',
                  'landtaxvaluedollarcnt',
                  'price')

# Simplify your dataset by only selecting the columns of your choosing dt[, numeric_cols, with = FALSE]
dt_num <- dt[ , numeric_cols]

summary(dt_num)


# We want to try to correlate home price with another variable.
# Let's look to see if there are any outliers in the price column we need to remove
# Create a boxplot of the price data
price_data <- c('price')
dt_price <- dt[ , price_data]
boxplot(dt_price, xlab="Price", ylab="Dollars")
title("Home Price")


# Wow there are expensive homes!

# Remove the outliers. dt[!which(dt$price %in% boxplot(dt$price)$out)]
dt_price_wo_outliers = dt_price[!dt_price %in% boxplot.stats(dt_price)$out]

# How many outliers did we drop? And lets plot a new box plot to see the column
# 90275 - 84180 = 6095 entries were dropped!!! that's a lot
boxplot(dt_price_wo_outliers, xlab="Price", ylab="Dollars")
title("Home Price (No Outliers)")

# In our case, we have too many observations.  
# Use sample() to only sample a few hundred (maybe 500) points to plot.
# plot a few of the more interesting pairs together
sample_dt_price_wo_outliers = sample(dt_price_wo_outliers, 500)
boxplot(sample_dt_price_wo_outliers, xlab="Price", ylab="Dollars")
title("Home Price (No Outliers) (n=500)")

# bonus: try to make some nice-looking scatter plots with ggplot2


# create a new data.table by dropping any missing values
# look up 'complete.cases()'
# use dim() to see how many cases we dropped
dt_no_na <- dt_num[complete.cases(dt_num),]
dt_no_na
dim(dt_no_na)

# get the pearson correlation between price and another variable using cor()
#...there are other types of correlations
# try ?cor to see options, and try another correlation 
?cor
cor(dt_no_na$yearbuilt, dt_no_na$price, method="pearson")
cor.test(dt_no_na$yearbuilt, dt_no_na$price, method="pearson")
cor.test(dt_no_na$bathroomcnt, dt_no_na$price, method="pearson")
cor.test(dt_no_na$bedroomcnt, dt_no_na$price, method="pearson")
cor.test(dt_no_na$calculatedfinishedsquarefeet, dt_no_na$price, method="pearson")
cor.test(dt_no_na$roomcnt, dt_no_na$price, method="pearson")
cor.test(dt_no_na$taxvaluedollarcnt, dt_no_na$price, method="pearson")
cor.test(dt_no_na$landtaxvaluedollarcnt, dt_no_na$price, method="pearson")
#from these, it appears that taxvaluedollarcnt is the most correlated with price


# use the lm() command to fit a linear model of price to the 
# one variable you think is most correlated or predictive of price
# lm stands for 'linear model'
m1 <- lm(price ~ taxvaluedollarcnt, data = dt_no_na)




# we just did the correlation with outliers still in
# now let's remove outliers



dim(dt_num)
boxplot(dt_num)

# The method commented out below didn't work, but I wanted to keep it here for posterity
#findOutliers <- function(dataframe){
#  dataframe %>%
#  select_if(is.numeric) %>% 
#    map(~ boxplot.stats(.x)$out)
#}

#outliers <- findOutliers(dt_num)
#temp <- list()
#for (col in names(outliers)) {
#  outlier <- outliers[[col]]
#  if (length(outlier) > 0) {
#    temp[col] <- dt_num[-which(dt_num[[col]] %in% outlier),][col]
#  } else {
#    temp[col] <- dt_num[col]
#  }
#}

#boxplot(temp)
#removing the outliers makes all the row numbers different, hmm

#let's try something different
#find Q1, Q3, and interquartile range for values in column A
Q1 <- quantile(dt_no_na$price, .25)
Q3 <- quantile(dt_no_na$price, .75)
IQR <- IQR(dt_no_na$price)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
no_outliers <- subset(dt_no_na, dt_no_na$price> (Q1 - 1.5*IQR) & dt_no_na$price< (Q3 + 1.5*IQR))

#view row and column count of new data frame before and after
dim(dt_no_na)
dim(no_outliers) 

cor.test(no_outliers$yearbuilt, no_outliers$price, method="pearson")
cor.test(no_outliers$bathroomcnt, no_outliers$price, method="pearson")
cor.test(no_outliers$bedroomcnt, no_outliers$price, method="pearson")
cor.test(no_outliers$calculatedfinishedsquarefeet, no_outliers$price, method="pearson")
cor.test(no_outliers$roomcnt, no_outliers$price, method="pearson")
cor.test(no_outliers$taxvaluedollarcnt, no_outliers$price, method="pearson")
cor.test(no_outliers$landtaxvaluedollarcnt, no_outliers$price, method="pearson")
#from these, it appears that taxvaluedollarcnt is STILL the most correlated with price
m2 <- lm(price ~ taxvaluedollarcnt, data = no_outliers)

# view the model summary 
summary(m1)

# plot a scatter plot of the price and the variable you chose
plot(dt_no_na$taxvaluedollarcnt, dt_no_na$price, main = "Price vs. Tax Value Dollar Cnt", xlab = "Tax Value Dollar Cnt", ylab="Price")

# add the regression line to the current plot using abline()
abline(m1, col = "blue")

# R makes it very easy to plot the diagnostics of a fit
# here's a decent resources explaining the plots: 
# http://data.library.virginia.edu/diagnostic-plots/
# plot the fit diagnostics here
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(m1)

par(mfrow=c(1,1)) # Change back to 1 x 1

# view the model summary 
summary(m2)

# plot a scatter plot of the price and the variable you chose
plot(no_outliers$taxvaluedollarcnt, no_outliers$price, main = "Price vs. Tax Value Dollar Cnt", xlab = "Tax Value Dollar Cnt", ylab="Price")

# add the regression line to the current plot using abline()
abline(m2, col = "blue")

# R makes it very easy to plot the diagnostics of a fit
# here's a decent resources explaining the plots: 
# http://data.library.virginia.edu/diagnostic-plots/
# plot the fit diagnostics here
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(m2)

par(mfrow=c(1,1)) # Change back to 1 x 1

# How does your model with the price outliers removed compare to a model with the outliers still in the data?

# Comment on the model you created. # Should you keep the 'outliers'? 
# Which model do you think is more accurate? Is your model reliable? What does the R^2 and RSE 
# tell you about the accuracy of the model? Look at the diagnostics.  Are there any outliers?
# Is there any evidence that the data is not linear or normally distributed? 