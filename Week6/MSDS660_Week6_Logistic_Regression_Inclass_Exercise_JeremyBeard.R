# load libraries
library(data.table)
library(dplyr)
library(car)
library(caret)
library(caTools)
library(pROC)
library(MASS)

# set the seed
set.seed(1)

#load data as data table
dt <- read.csv("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week6\\breast-cancer-wisconsin.data", sep = ",")
dt <- as.data.table(dt)


################################################################################
# Breast cancer data.  Predict if class is benign or malignant from these data #
################################################################################

#Name all the variables
colnames(dt) <- c("id", "clump_thickness", "cell_size", "cell_shape" ,"marginal_adhesion" , "se_cell_size", 
                  "bare_nucleoli" ,"bland_chromatin", "normal_nucleoli", "mitoses", "Class")

head(dt)
str(dt)
summary(dt)

# remove the variables we don't want to use: id
non_id <- c("clump_thickness", "cell_size", "cell_shape" ,"marginal_adhesion" , "se_cell_size", 
                  "bare_nucleoli" ,"bland_chromatin", "normal_nucleoli", "mitoses", "Class")
dt <- dt[, !"id"]
str(dt)
summary(dt)

#Factor class and relable as benign or malignant
dt$Class <- factor(dt$Class, labels = c('Benign', 'Malignant'))
str(dt)
summary(dt)

#Why is bare_nucleoli a character? Change it to integer
dt$bare_nucleoli <- as.integer(dt$bare_nucleoli)
str(dt)
summary(dt)

#remove NAs
which(is.na(dt$clump_thickness))
which(is.na(dt$cell_size))
which(is.na(dt$cell_shape))
which(is.na(dt$marginal_adhesion))
which(is.na(dt$se_cell_size))
which(is.na(dt$bare_nucleoli))
which(is.na(dt$bland_chromatin))
which(is.na(dt$normal_nucleoli))
which(is.na(dt$mitoses))
which(is.na(dt$Class))

dt <- dt[complete.cases(dt), ]
which(is.na(dt$bare_nucleoli))

#split the data into a train and test set
samp <- sample.split(dt$Class, SplitRatio = 0.8)
train <- subset(dt, samp == TRUE)
test <- subset(dt, samp == FALSE)

# Create a multi linear binomial logisitc regression on survived vs sex
model <- glm(Class ~ ., data = train, family = "binomial")

# Look at the model summary
summary(model)

# Check for colinearity
vif(model)

# Are any variables colinear and need to be removed?
# No variables need to be removed from the VIF analysis. All values under 5

# Perform stepAIC to remove high p-values
stepAIC(model, direction = 'both')

model <- glm(formula = Class ~ clump_thickness + cell_shape + marginal_adhesion + bare_nucleoli + bland_chromatin + normal_nucleoli + mitoses, family = "binomial", data = train)

#Check model summary again
summary(model)

# predict on the train data            
trainpreds <- predict(model, type = 'response', train)

# Round prediction values at 0.5 cutoff factor and change lables
trainp <- factor(trainpreds >= 0.5, labels = c('Benign', 'Malignant'))

# Buld a confustion matrix and see results
trainCM <- confusionMatrix(train$Class, trainp)
trainCM

# predict on the test data            
testpreds <- predict(model, type = 'response', test)

# Round prediction values at 0.5 cutoff factor and change lables
testp <- factor(testpreds >= 0.5, labels = c('Benign', 'Malignant'))

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
trainrocp <- factor(trainpreds >= as.numeric(train_rocc[1]), labels = c('Benign', 'Malignant'))

# Buld a confustion matrix to see results
trainROCCM <- confusionMatrix(train$Class, trainrocp)
trainROCCM

# predict on the test data using the ROC cuttoff         
#  Round prediction values at threshold level and change lables
testp <- factor(testpreds >= as.numeric(test_rocc[1]), labels = c('Benign', 'Malignant'))

# Buld a confustion matrix to see results
testROCCM <- confusionMatrix(test$Class, testp)
testROCCM

#View all the Confustion matricies
trainCM
trainROCCM

testCM
testROCCM



# Comment on the model
























