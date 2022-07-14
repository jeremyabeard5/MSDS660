# load the data.table, ggolot2, and dplyr libraries and the zillow_price.csv file



# Convert the file to a data table


# how many observations and columns are there?



# use str and summary to see how many missing values we have,
# and what the data looks like



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



# We want to try to correlate home price with another variable.
# Let's look to see if there are any outliers in the price column we need to remove
# Create a boxplot of the price data



# Wow there are expensive homes!

# Remove the outliers. dt[!which(dt$price %in% boxplot(dt$price)$out)]



# How many outliers did we drop? And lets plot a new box plot to see the column



# In our case, we have too many observations.  
# Use sample() to only sample a few hundred (maybe 500) points to plot.
# plot a few of the more interesting pairs together



# bonus: try to make some nice-looking scatter plots with ggplot2


# create a new data.table by dropping any missing values
# look up 'complete.cases()'
# use dim() to see how many cases we dropped



# get the pearson correlation between price and another variable using cor()
#...there are other types of correlations
# try ?cor to see options, and try another correlation 



# use the lm() command to fit a linear model of price to the 
# one variable you think is most correlated or predictive of price
# lm stands for 'linear model'



# view the model summary 


# plot a scatter plot of the price and the variable you chose


# add the regression line to the current plot using abline()


# R makes it very easy to plot the diagnostics of a fit
# here's a decent resources explaining the plots: 
# http://data.library.virginia.edu/diagnostic-plots/
# plot the fit diagnostics here
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2


par(mfrow=c(1,1)) # Change back to 1 x 1

# How does your model with the price outliers removed compare to a model with the outliers still in the data?



# Comment on the model you created. # Should you keep the 'outliers'? 
# Which model do you think is more accurate? Is your model reliable? What does the R^2 and RSE 
# tell you about the accuracy of the model? Look at the diagnostics.  Are there any outliers?
# Is there any evidence that the data is not linear or normally distributed? 