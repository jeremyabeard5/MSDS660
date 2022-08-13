# load the necessary libraries

library(data.table)
library(dplyr)
library(car)
library(caret)
library(caTools)
library(pROC)
library(MASS)

# And set the seed to
set.seed(1)

# Here i have set the working path and loaded data as data table
setwd("~/Downloads")
dt <- read.csv("breast-cancer-wisconsin.data")
dt <- as.data.table(dt)

# Here after performing the str i see that we are not having any headers.
str(dt)
summary(dt)

#Now we are Naming all the variables
colnames(dt) <- c("id", "clump_thickness", "cell_size", "cell_shape" ,"marginal_adhesion" , "se_cell_size", 
                  "bare_nucleoli" ,"bland_chromatin", "normal_nucleoli", "mitoses", "Class")

# here i am removing the variables we don't want to use: id
dt <- dt[, c("id") := NULL]

#Factor class and relable as benign or malignant
dt$Class[dt$Class == 2] <- "benign"
dt$Class[dt$Class == 4] <- "malignant"
dt$Class <- as.factor(dt$Class)

dt$bare_nucleoli <- as.integer(dt$bare_nucleoli)

# Here we are removing the null values if there are any.
dt <- dt[complete.cases(dt),]

str(dt)

# Now we are splitting the data into a train and test sets.
samp <- sample.split(dt$Class, SplitRatio = 0.75)
train <- subset(dt, samp == TRUE)
test <- subset(dt, samp == FALSE)

# Created a multi linear binomial logisitic regression
model <- glm(Class ~ ., data = train, family = "binomial")


# Now lets look at the model summary
summary(model)

# Now lets check for colinearity
vif(model)

# Are any variables colinear and need to be removed?

# Perform stepAIC to remove high p-values
stepAIC(model, dirrection = 'both')

model <- glm(formula = Class ~ clump_thickness + marginal_adhesion + bare_nucleoli + 
               bland_chromatin + normal_nucleoli + mitoses, family = "binomial", 
             data = train)

#Check model summary again
summary(model)

# predict on the train data            
trainpreds <- predict(model, type = 'response', train)

# Round prediction values at 0.5 cutoff factor and change lables
trainp <- factor(trainpreds >= 0.5, labels = c('benign', 'malignant'))

# Buld a confustion matrix and see results
trainCM <- confusionMatrix(train$Class, trainp)
trainCM

# predict on the test data            
testpreds <- predict(model, type = 'response', test)

# Round prediction values at 0.5 cutoff factor and change lables
testp <- factor(testpreds >= 0.5, labels = c('benign', 'malignant'))

# Buld a confustion matrix and see results
testCM <- confusionMatrix(test$Class, testp)
testCM


# Create a Roc curve and and view ROC results for the Train data
train_roc_curve <- roc(train$Class, trainpreds)
train_roc_curve
plot(train_roc_curve)
train_rocc <- coords(roc=train_roc_curve, x = 'best', best.method = 'closest.topleft')
train_rocc

# Create a Roc curve and view results for the Test data
test_roc_curve <- roc(test$Class, testpreds)
test_roc_curve
plot(test_roc_curve)
test_rocc <- coords(roc=test_roc_curve, x = 'best', best.method = 'closest.topleft')
test_rocc

# predict on the train data using the ROC cuttoff            
# Round prediction values at threshold level and change lables
trainrocp <- factor(trainpreds >= as.numeric(train_rocc[1]), labels = c('benign', 'malignant'))

# Buld a confustion matrix to see results
trainROCCM <- confusionMatrix(train$Class, trainrocp)
trainROCCM


# predict on the test data using the ROC cuttoff         
#  Round prediction values at threshold level and change lables
testp <- factor(testpreds >= as.numeric(test_rocc[1]), labels = c('benign', 'malignant'))

# Buld a confustion matrix to see results
testROCCM <- confusionMatrix(test$Class, testp)
testROCCM

#View all the Confustion matricies
trainCM
trainROCCM

testCM
testROCCM

# Comment on the model
# Conclusion:
# Train & Test Split :  I have splitted data in to 75% of train and 25% to test data. 
# Then created multi linear binomial logisitc regression.

# Next, Calculated the VIF for the model to check for any colinearity and i see that the all the values
# are below 5.

# StepAIC --  Next, performed StepAIC which removes highest p-values.

# Predit Train data -- predicting train data by taking 0.5 cutoff factor. 
# Then built a confusion matrix for it which shows the Accuracy is 96.6%.

# Predict Test Data--  Predicting test data with 0.5 cutoff and constructing a confusion matrix for it.
# The result shows the accuracy is 97.6% which is pretty good.

# ROC CUrve

# The result for roc curve for train data says the area under the curve is 99%  which says the
# performance of the train model is good.


# The result for roc curve for test data says the area under the curve is 99.8% which says the
# performance of the test model is good.

















