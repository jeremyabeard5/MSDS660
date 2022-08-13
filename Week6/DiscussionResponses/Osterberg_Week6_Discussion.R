# load libraries
library(data.table)
library(dplyr)
library(car)
library(caret)
library(caTools)
library(pROC)
library(MASS)

# set the seed
set.seed(42)

#load data as data table
dt <- read.csv('~/Backup/StatMethods/breast-cancer-wisconsin.csv')
dt <- as.data.table(dt)



################################################################################
# Breast cancer data.  Predict if class is benign or malignant from these data #
################################################################################

#Name all the variables
colnames(dt) <- c("id", "clump_thickness", "cell_size", "cell_shape" ,"marginal_adhesion" , "se_cell_size", 
                  "bare_nucleoli" ,"bland_chromatin", "normal_nucleoli", "mitoses", "Class")
str(dt)
summary(dt)
# remove the variables we don't want to use: id
dt <- dt[, !"id"]

#Factor class and relabel as benign or malignant


dt$Class[dt$Class == 2] <- "benign"
dt$Class[dt$Class == 4] <- "malignant"
dt$Class <- as.factor(dt$Class)

#Why is bare_nucleoli a character? Change it to integer

dt$bare_nucleoli <- as.integer(dt$bare_nucleoli)

#remove NAs

dt <- na.omit(dt)

#split the data into a train and test set
samp <- sample.split(dt$Class, SplitRatio = 0.8)
train <- subset(dt, samp == TRUE)
test <- subset(dt, samp == FALSE)

# Create a multi linear binomial logisitc regression

model <- glm(Class ~ ., data = train, family = "binomial")

# Look at the model summary
summary(model)

# Check for colinearity

vif(model)

# Are any variables colinear and need to be removed?

# Nothing above 5, per our previous work on this so it should be fine
# but we could look at cell_size and cell_shape and either combine,
# or drop one if we need to later

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

# Round prediction values at 0.5 cutoff factor and change labels
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

# predict on the train data using the ROC cutoff            
# Round prediction values at threshold level and change labels
trainrocp <- factor(trainpreds >= as.numeric(train_rocc[1]), labels = c('benign', 'malignant'))
# Build a confusion matrix to see results
trainROCCM <- confusionMatrix(train$Class, trainrocp)
trainROCCM


# predict on the test data using the ROC cutoff         
#  Round prediction values at threshold level and change labels
testp <- factor(testpreds >= as.numeric(test_rocc[1]), labels = c('benign', 'malignant'))
# Build a confusion matrix to see results
testROCCM <- confusionMatrix(test$Class, testp)
testROCCM

#View all the confusion matrices
trainCM
trainROCCM

testCM
testROCCM



# Comment on the model

# We ended up making two models, the first was a logistic regression classifier
# which had a test accuracy of 95.62%. Our second which was a ROC model with the
# closest to the top left as our optimal model and it ended up with a 97.08% accuracy
# on the test data






















