# Dowload the importance_of_plotting data from the worldclass site under week 2

library('ggplot2')
library('Rmisc')
library('stargazer')

# Import data set and convert to a data frame
dt <- read_csv("Imporatance of plotting.csv")

# Exploratory - how many sets of data?



#Create a linear model for the for the datasets


#View the summary statistics of the 4 linear models
#Stargazer is a wonder package that makes nice table of results
#?Stargazer



#View the correlation statistics of the datasets



# What does this tell us? What conclusions can we confidently make?


# Let's plot the data
# Changes the format of plot output
par(mfrow = c(2,2))

# p1 <- ggplot(<your data>, aes(x = , y = )) + 
#   geom_point(color = '<Pick a color>') + 
#   geom_smooth(formula = y ~ x, method = 'lm' , se = FALSE, data = <your data>) +
#   ggtitle("<choose a title>")


#?multiplot

# Now let's plot the diagnostics of the models - make sure to label them
par(mfrow = c(2,2))



#Anscombe's Quartet https://en.wikipedia.org/wiki/Anscombe%27s_quartet