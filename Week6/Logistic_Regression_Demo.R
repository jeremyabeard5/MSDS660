#### LOGISTIC REGRESSION DEMO ####

##### Libraries #####
library(data.table)
library(dplyr)
library(car)
library(caret)
library(caTools)
library(pROC)
library(MASS)

# set the seed
set.seed(1)

##### Reading in Data ##### 
# as data table 
setwd("~/Work/Regis/MSDS660_Instructor/Week6")
dt <- read.csv('titanic.csv')
dt <- as.data.table(dt)
str(dt)
summary(dt)

###### Predict if a passenger on the titanic survived or not. #######

### Cleaning ###
# remove the variables we don't want to use: PassangerID, Name, Ticket, Embarked, and Cabin
dt <- dt[, c("PassengerId" ,"Name", "Ticket", "Embarked", "Cabin") := NULL]

# Remove NAs
dt <- dt[complete.cases(dt),]
dt$Survived <- factor(dt$Survived, labels = c('Died', 'Lived'))

# Split the data into a train and test set
samp <- sample.split(dt$Survived, SplitRatio = 0.8)
train <- subset(dt, samp == TRUE)
test <- subset(dt, samp == FALSE)

##### Modeling #####

# Create a multi-linear binomial logistic regression on survived vs sex
model <- glm(Survived ~ ., data = train, family = "binomial")
# read the help for glm and type of model families
?glm
?family

# Look at the model summary
summary(model)

# Check for collinearity
vif(model) # VIF values look good. 

# Perform step AIC and remove high p-values
stepAIC(model, dirrection = 'both')

# Update model based on the step AIC
model <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp, family = "binomial", data = train)

#Check model summary again
summary(model)

# Predict on the train data            
trainpreds <- predict(model, type = 'response', train)
# Round prediction values at 0.5 cutoff factor and change labels
trainp <- factor(trainpreds >= 0.5, labels = c('Died', 'Lived'))
# Build a confusion matrix to see results
trainCM <- confusionMatrix(train$Survived, trainp)
trainCM

### Prediction ### 
# predict on the test data            
testpreds <- predict(model, type = 'response', test)

# Round prediction values at 0.5 cutoff factor and change labels
testp <- factor(testpreds >= 0.5, labels = c('Died', 'Lived'))

# Build a confusion matrix to see results
testCM <- confusionMatrix(test$Survived, testp)
testCM


# Create a ROC curve and results for the Train data
train_roc_curve <- roc(train$Survived, trainpreds)
train_roc_curve
plot(train_roc_curve)
train_rocc <- coords(roc=train_roc_curve, x = 'best', best.method = 'closest.topleft')
train_rocc

# Create a ROC curve and results for the Test data
test_roc_curve <- roc(test$Survived, testpreds)
test_roc_curve
plot(test_roc_curve)
test_rocc <- coords(roc=test_roc_curve, x = 'best', best.method = 'closest.topleft')
test_rocc

# Predict on the train data using the ROC cut-off            
# Round prediction values at 0.5 cutoff factor and change labels
trainrocp <- factor(trainpreds >= as.numeric(train_rocc[1]), labels = c('Died', 'Lived'))

# Build a confusion matrix to see results
trainROCCM <- confusionMatrix(train$Survived, trainrocp)
trainROCCM

# Predict on the test data            
# Round prediction values at 0.5 cutoff factor and change labels
testp <- factor(testpreds >= as.numeric(test_rocc[1]), labels = c('Died', 'Lived'))

# Build a confusion matrix to see results
testROCCM <- confusionMatrix(test$Survived, testp)
testROCCM

#View all the Confusion matrices
trainCM
trainROCCM

testCM
testROCCM
