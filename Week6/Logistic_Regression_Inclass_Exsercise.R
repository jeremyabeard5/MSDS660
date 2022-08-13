# load libraries

# set the seed


#load data as data table



################################################################################
# Breast cancer data.  Predict if class is benign or malignant from these data #
################################################################################

#Name all the variables
colnames(dt) <- c("id", "clump_thickness", "cell_size", "cell_shape" ,"marginal_adhesion" , "se_cell_size", 
                  "bare_nucleoli" ,"bland_chromatin", "normal_nucleoli", "mitoses", "Class")

# remove the variables we don't want to use: id


#Factor class and relable as benign or malignant


#Why is bare_nucleoli a character? Change it to integer


#remove NAs


#split the data into a train and test set


# Create a multi linear binomial logisitc regression on survived vs sex


# Look at the model summary


# Check for colinearity

# Are any variables colinear and need to be removed?

# Perform stepAIC to remove high p-values


#Check model summary again


# predict on the train data            

# Round prediction values at 0.5 cutoff factor and change lables

# Buld a confustion matrix and see results


# predict on the test data            

# Round prediction values at 0.5 cutoff factor and change lables

# Buld a confustion matrix and see results



# Create a Roc curve and and view ROC results for the Train data


# Create a Roc curve and view results for the Test data


# predict on the train data using the ROC cuttoff            
# Round prediction values at threshold level and change lables

# Buld a confustion matrix to see results



# predict on the test data using the ROC cuttoff         
#  Round prediction values at threshold level and change lables

# Buld a confustion matrix to see results


#View all the Confustion matricies




# Comment on the model
























