# load libraries
##### Libraries #####
library(data.table)
library(dplyr)
library(car)
library(caret)
library(caTools)
library(pROC)
library(MASS)
library(popbio)

#Added
library(popbio)
#install.packages("popbio")

# set the seed
set.seed(1)

#load data as data table
dt <- read.csv('breast-cancer-wisconsin.data')
dt <- as.data.table(dt)
str(dt)
summary(dt)


################################################################################
# Breast cancer data.  Predict if class is benign or malignant from these data #
################################################################################

#Name all the variables
colnames(dt) <- c("id", "clump_thickness", "cell_size", "cell_shape" ,"marginal_adhesion" , "se_cell_size", 
                  "bare_nucleoli" ,"bland_chromatin", "normal_nucleoli", "mitoses", "Class")


# remove the variables we don't want to use: id
dt <- dt[, c("id") := NULL]

#Why is bare_nucleoli a character? Change it to integer
dt$bare_nucleoli <- as.numeric(dt$bare_nucleoli)

#Factor class and relabel as benign or malignant
dt$Class <- factor(dt$Class, labels = c('benign', 'malignant'))

# Remove NAs
dt <- dt[complete.cases(dt),]

#Added visual to view outcome against cell size
Class <- dt$Class
cell_size <- dt$cell_size
Classcode <- ifelse(Class == "malignant", 1, 0)
plot(cell_size, jitter(Classcode, 0.15), pch = 19,
     xlab = "Cell size",
     ylab = "Cancer type (0 - benign, 1 - malignant)")

#Added visual to view outcome against cell size
Class <- dt$Class
marginal_adhesion <- dt$marginal_adhesion
Classcode <- ifelse(Class == "malignant", 1, 0)
plot(cell_size, jitter(Classcode, 0.15), pch = 19,
     xlab = "marginal adhesion",
     ylab = "Cancer type (0 - benign, 1 - malignant)")

# Split the data into a train and test set
samp <- sample.split(dt$Class, SplitRatio = 0.8)
train <- subset(dt, samp == TRUE)
test <- subset(dt, samp == FALSE)

# Create a multi-linear binomial logistic regression on malignant
model <- glm(Class ~ ., data = train, family = "binomial")
# read the help for glm and type of model families
?glm
?family

# Look at the model summary
summary(model)

# Check for colinearity
library(car)
vif(model) # VIF values look good. 

# Are any variables colinear and need to be removed?
model_1 <- glm(
  formula = Class ~ mitoses + normal_nucleoli + cell_shape + bland_chromatin, family = "binomial", data = train)
summary(model_1)
vif(model_1)

#Creating a 2nd model to replace cell_shape with marginal_adhesion
model_2 <- glm(
  formula = Class ~ bare_nucleoli + marginal_adhesion + clump_thickness + mitoses, family = "binomial", data = train)
summary(model_2)
vif(model_2)


# predict on the train data                    
trainpreds <- predict(model_2, type = 'response', train)
# Round prediction values at 0.5 cutoff factor and change labels
trainp <- factor(trainpreds >= 0.5, labels = c('benign', 'malignant'))
# Build a confusion matrix to see results
trainCM <- confusionMatrix(train$Class, trainp)
trainCM


# Prediction --------------------------------------------------------------

# predict on the test data            
testpreds <- predict(model_2, type = 'response', test)

# Round prediction values at 0.5 cutoff factor and change labels
testp <- factor(testpreds >= 0.5, labels = c('benign', 'malignant'))

# Build a confusion matrix to see results
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

# Predict on the train data using the ROC cuttoff            
# Round prediction values at threshold level and change labels
trainrocp <- factor(trainpreds >= as.numeric(train_rocc[1]), labels = c('benign', 'malignant'))

# Buld a confustion matrix to see results
trainROCCM <- confusionMatrix(train$Class, trainrocp)
trainROCCM

# Predict on the test data            
# Round prediction values at 0.5 cutoff factor and change labels
testp <- factor(testpreds >= as.numeric(test_rocc[1]), labels = c('benign', 'malignant'))

# Build a confusion matrix to see results
testROCCM <- confusionMatrix(test$Class, testp)
testROCCM

#View all the Confusion matrices
trainCM
trainROCCM

# Comment on the model

















