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

#load data as data 
dt <- read.csv("C:\\Users\\jerem\\OneDrive\\Documents\\School\\_REGIS\\2022-05_Summer\\MSDS660\\Week6\\churn.csv", sep = ",")
dt <- as.data.table(dt)

str(dt)
summary(dt)

dt <- dt[, !"customerID"]
str(dt)
summary(dt)

unique(dt$gender)
unique(dt$SeniorCitizen)
unique(dt$Partner)
unique(dt$Dependents)
#unique(dt$tenure)
unique(dt$PhoneService)
unique(dt$MultipleLines)
unique(dt$InternetService)
unique(dt$OnlineSecurity)
unique(dt$OnlineBackup)
unique(dt$DeviceProtection)
unique(dt$TechSupport)
unique(dt$StreamingTV)
unique(dt$StreamingMovies)
unique(dt$Contract)
unique(dt$PaperlessBilling)
unique(dt$PaymentMethod)
#unique(dt$MonthlyCharges)
#unique(dt$TotalCharges)
unique(dt$Churn)

#Factor class and relable as benign or malignant
dt$Churn <- factor(dt$Churn, labels = c('No', 'Yes'))
dt$gender <- factor(dt$gender, labels = c('Male', 'Female'))
dt$Partner <- factor(dt$Partner, labels = c('No', 'Yes'))
dt$Dependents <- factor(dt$Dependents, labels = c('No', 'Yes'))
dt$PhoneService <- factor(dt$PhoneService, labels = c('No', 'Yes'))
head(dt$MultipleLines)
dt$MultipleLines <- factor(dt$MultipleLines, labels = c('No', 'Yes', 'No phone service'))
dt$InternetService <- factor(dt$InternetService, labels = c('No', 'DSL', 'Fiber optic'))
dt$OnlineSecurity <- factor(dt$OnlineSecurity, labels = c('No', 'Yes', 'No internet service'))
dt$OnlineBackup <- factor(dt$OnlineBackup, labels = c('No', 'Yes', 'No internet service'))
dt$DeviceProtection <- factor(dt$DeviceProtection, labels = c('No', 'Yes', 'No internet service'))
dt$TechSupport <- factor(dt$TechSupport, labels = c('No', 'Yes', 'No internet service'))
dt$StreamingTV <- factor(dt$StreamingTV, labels = c('No', 'Yes', 'No internet service'))
dt$StreamingMovies <- factor(dt$StreamingMovies, labels = c('No', 'Yes', 'No internet service'))
dt$Contract <- factor(dt$Contract, labels = c('Month-to-month', 'One year', 'Two year'))
dt$PaperlessBilling <- factor(dt$PaperlessBilling, labels = c('No', 'Yes'))
dt$PaymentMethod <- factor(dt$PaymentMethod, labels = c('Electronic check', 'Mailed check', 'Bank transfer (automatic)', 'Credit card (automatic)'))



#Why is bare_nucleoli a character? Change it to integer
#dt$bare_nucleoli <- as.integer(dt$bare_nucleoli)
str(dt)
summary(dt)

#remove NAs
which(is.na(dt$gender))
which(is.na(dt$SeniorCitizen))
which(is.na(dt$Partner))
which(is.na(dt$Dependents))
which(is.na(dt$tenure))
which(is.na(dt$PhoneService))
which(is.na(dt$MultipleLines))
which(is.na(dt$InternetService))
which(is.na(dt$OnlineSecurity))
which(is.na(dt$OnlineBackup))
which(is.na(dt$DeviceProtection))
which(is.na(dt$TechSupport))
which(is.na(dt$StreamingTV))
which(is.na(dt$StreamingMovies))
which(is.na(dt$Contract))
which(is.na(dt$PaperlessBilling))
which(is.na(dt$PaymentMethod))
which(is.na(dt$MonthlyCharges))
which(is.na(dt$TotalCharges))
which(is.na(dt$Churn))

# Start here jeremy
dt <- dt[complete.cases(dt), ]
which(is.na(dt$customerID))
which(is.na(dt$gender))
which(is.na(dt$SeniorCitizen))
which(is.na(dt$Partner))
which(is.na(dt$Dependents))
which(is.na(dt$tenure))
which(is.na(dt$PhoneService))
which(is.na(dt$MultipleLines))
which(is.na(dt$InternetService))
which(is.na(dt$OnlineSecurity))
which(is.na(dt$OnlineBackup))
which(is.na(dt$DeviceProtection))
which(is.na(dt$TechSupport))
which(is.na(dt$StreamingTV))
which(is.na(dt$StreamingMovies))
which(is.na(dt$Contract))
which(is.na(dt$PaperlessBilling))
which(is.na(dt$PaymentMethod))
which(is.na(dt$MonthlyCharges))
which(is.na(dt$TotalCharges))
which(is.na(dt$Churn))
#NA's have been removed from the TotalCharges dataset!





#Now time to split the data into a train and test set

#split the data into a train and test set
samp <- sample.split(dt$Churn, SplitRatio = 0.8)
train <- subset(dt, samp == TRUE)
test <- subset(dt, samp == FALSE)

# Create a multi linear binomial logisitc regression
model <- glm(Churn ~ ., data = train, family = "binomial")

# Look at the model summary
summary(model)

# Check for colinearity
vif(model)
#hmmmm, getting the error: "Error in vif.default(model) : there are aliased coefficients in the model"
#it means 2+ variables are very closely related
#let's plot a correlation matrix to see which ones

library(ggcorrplot)
model.matrix(~0+., data=dt) %>%
  cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

# I will remove features which have a correlation of 1.
# this is InternetServiceFiber optic -- OnlineSecurity Yes
#   InternetServiceFiber optic -- Online Backup Yes
#   InternetServiceFiber optic -- DeviceProtection Yees
#   InternetServiceFiber optic -- TechSupport Yes
#   InternetServiceFiber optic -- StreamingTV Yes
#   InternetServiceFiber optic -- StreamingMovies Yes

# So looks like I just need to keep one of these 7 features. I will keep InternetService

dt <- dt[, !"OnlineSecurity"]
dt <- dt[, !"OnlineBackup"]
dt <- dt[, !"DeviceProtection"]
dt <- dt[, !"TechSupport"]
dt <- dt[, !"StreamingTV"]
dt <- dt[, !"StreamingMovies"]

samp <- sample.split(dt$Churn, SplitRatio = 0.8)
train <- subset(dt, samp == TRUE)
test <- subset(dt, samp == FALSE)

# Create a multi linear binomial logisitc regression
model <- glm(Churn ~ ., data = train, family = "binomial")

# Look at the model summary
summary(model)

# Check for colinearity
vif(model)

str(dt)
model.matrix(~0+., data=dt) %>%
  cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

# Looks like I should remove the -1 correlations too
# This is: 
# PhoneService Yes -- MultipleLines Yes
# genderMale -- genderFemale

# Try #3
dt <- dt[, !"gender"]
dt <- dt[, !"MultipleLines"]

samp <- sample.split(dt$Churn, SplitRatio = 0.8)
train <- subset(dt, samp == TRUE)
test <- subset(dt, samp == FALSE)

# Create a multi linear binomial logisitc regression
model <- glm(Churn ~ ., data = train, family = "binomial")

# Look at the model summary
summary(model)

# Check for colinearity
vif(model)
# Whew okay, looks good


# Looks like tenure, InternetService, MonthlyCharges, and TotalCharges have a GVIF over 5. 
# Let's see the correlation matrix again
model.matrix(~0+., data=dt) %>%
  cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

#Yeah, the 3 variables above have correlations above |0.75|. 
# AND PartnerNo has a correlation of 1 to PartnerYes. I will remove them all
dt <- dt[, !"Partner"]
dt <- dt[, !"InternetService"]
dt <- dt[, !"MonthlyCharges"]
dt <- dt[, !"TotalCharges"]

samp <- sample.split(dt$Churn, SplitRatio = 0.8)
train <- subset(dt, samp == TRUE)
test <- subset(dt, samp == FALSE)

# Create a multi linear binomial logisitc regression
model <- glm(Churn ~ ., data = train, family = "binomial")

# Look at the model summary
summary(model)

# Check for colinearity
vif(model)
# Whew okay, looks even better. No GVIF above 5 (should I be using GVIF^(1/(2*Df))???)

# Perform stepAIC to remove high p-values
stepAIC(model, direction = 'both')

model <- glm(formula = Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + Contract + PaperlessBilling + PaymentMethod, family = "binomial", data = train)

#Check model summary again
summary(model)

# predict on the train data            
trainpreds <- predict(model, type = 'response', train)

# Round prediction values at 0.5 cutoff factor and change lables
trainp <- factor(trainpreds >= 0.5, labels = c('No', 'Yes'))

# Build a confusion matrix and see results
trainCM <- confusionMatrix(train$Churn, trainp)
trainCM

# predict on the test data            
testpreds <- predict(model, type = 'response', test)

# Round prediction values at 0.5 cutoff factor and change labels
testp <- factor(testpreds >= 0.5, labels = c('No', 'Yes'))

# Build a confusion matrix and see results
testCM <- confusionMatrix(test$Churn, testp)
testCM

# Create a Roc curve and and view ROC results for the Train data
train_roc_curve <- roc(train$Churn, trainpreds)
train_roc_curve
plot(train_roc_curve)
train_rocc <- coords(roc=train_roc_curve, x = 'best', best.method = 'closest.topleft')
train_rocc

# Create a Roc curve and view results for the Test data
test_roc_curve <- roc(test$Churn, testpreds)
test_roc_curve
plot(test_roc_curve)
test_rocc <- coords(roc=test_roc_curve, x = 'best', best.method = 'closest.topleft')
test_rocc

# predict on the train data using the ROC cutoff            
# Round prediction values at threshold level and change labels
trainrocp <- factor(trainpreds >= as.numeric(train_rocc[1]), labels = c('No', 'Yes'))

# Build a confusion matrix to see results
trainROCCM <- confusionMatrix(train$Churn, trainrocp)
trainROCCM

# predict on the test data using the ROC cutoff         
#  Round prediction values at threshold level and change labels
testp <- factor(testpreds >= as.numeric(test_rocc[1]), labels = c('No', 'Yes'))

# Buld a confusion matrix to see results
testROCCM <- confusionMatrix(test$Churn, testp)
testROCCM

#View all the Confusion matrices
trainCM
trainROCCM

testCM
testROCCM


# Comment on the model
























