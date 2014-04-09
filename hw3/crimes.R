#!/usr/bin/env Rscript

# Load the libraries.
library(lattice)    # cvTools dep.
library(robustbase) # cvTools dep.
library(cvTools)
library(MASS)
library(FNN)

# Define functions.
printf <- function(...) invisible(print(sprintf(...)))

# Read the data.
data <- read.csv('communitiesH.data', h=T) # Headers = True.

# Remove non-predictive attributes (including state number).
clean_data <- data[, -c(1:5)]

# Remove the attributes with question marks (unknowns) of them.
unkw <- clean_data == '?'
unkw_per_attr <- apply(unkw, 2, sum) # Sum the columns (2).
attr_with_unkw <- which(unkw_per_attr > 0) # Equivalent to find in Matlab.
no_unkw_data <- clean_data[, -attr_with_unkw]


###########################
###  Linear Regression  ###
###########################
printf("Linear Regression:")

# Compute the regression with the data free of unknowns.
linear_regr <- lm(ViolentCrimesPerPop ~ ., data=no_unkw_data)

# Evaluate the regression looking at the mean-squared error on the training data.
residuals <- resid(linear_regr)  # Function to get the residuals.
plot(predict(linear_regr, no_unkw_data), residuals) 

mse_residuals <- sum((residuals - mean(residuals)) ^ 2) / length(residuals)
printf(" - Mean-squared error on the whole data: %.2e", mse_residuals)

# Split off some test data and compute a new regression on the rest of the data
# (train data).
folds <- cvFolds(nrow(no_unkw_data), K=5)
train_data <- no_unkw_data[folds$subsets[folds$which != 1], ]
test_data  <- no_unkw_data[folds$subsets[folds$which == 1], ]

linear_regr_test <- lm(ViolentCrimesPerPop ~ ., data=train_data)

# Evaluate the regression looking at the mean-squared error on that test data.
residuals_test <- test_data$ViolentCrimesPerPop - predict(linear_regr_test, test_data)
plot(test_data$ViolentCrimesPerPop, residuals_test)

mse_residuals_test <- sum((residuals_test - mean(residuals_test)) ^ 2) / length(residuals_test)
printf(" - Mean-squared error on the test data (20%%): %.2e", mse_residuals_test)

# Prepare the data for the Box-Cox tranformation substituting zero values by
# a very small number.
no_unkw_data$ViolentCrimesPerPop[ which(no_unkw_data$ViolentCrimesPerPop == 0) ] <- 1e-100

# Apply Box-Cox and get a list of the lamdbas and their log likelihood and obtain 
# the value with the highest likelihood.
lambdas <- boxcox(linear_regr, plotit=F)
max_lambda <- lambdas$x[ which(lambdas$y == max(lambdas$y)) ]

# Change the very low values to zero again.
no_unkw_data$ViolentCrimesPerPop[which(no_unkw_data$ViolentCrimesPerPop == 1e-100)] <- 0

# Transform the data with the given value of lambda.
if (lambda == 0) {
  no_unkw_data$ViolentCrimesPerPop <- log(no_unkw_data$ViolentCrimesPerPop)
} else {
  no_unkw_data$ViolentCrimesPerPop <- (no_unkw_data$ViolentCrimesPerPop^max_lambda - 1)/max_lambda
}

# Compute the linear regression again and plot it.
boxcox_regr <- lm(ViolentCrimesPerPop ~ ., data=no_unkw_data)
plot(boxcox_regr)


###########################
###  Nearest Neighbours  ###
###########################
printf("Nearest Neighbours:")

# Compute the regression and plot it.
nn_regr <- knn.reg(train_data[!(names(train_data)) %in% c("ViolentCrimesPerPop")],
                   test = NULL,
                   train_data$ViolentCrimesPerPop, k = 1,
                   algorithm = c("kd_tree"))
plot(nn_regr$pred, nn_regr$residuals) 

# Evaluate the regression on the above training data with mean-squared error.
mse_residuals_knn <- sum((nn_regr$residuals - mean(nn_regr$residuals)) ^ 2) /
    length(nn_regr$residuals)
printf(" - Mean-squared error on the training data (80%%): %.2e", mse_residuals_knn)

# Compute the regression on the above test data.
klabels <- knn(train_data[!(names(train_data)) %in% c("ViolentCrimesPerPop")],
               test_data [!(names(test_data))  %in% c("ViolentCrimesPerPop")],
               train_data$ViolentCrimesPerPop, k = 1,
               prob = FALSE, algorithm = c("kd_tree"))
k_test_residuals <- test_data$ViolentCrimesPerPop - as.numeric(levels(klabels))[klabels]
plot(as.numeric(levels(klabels))[klabels], k_test_residuals)

# Evaluate the previous regression with mean-squared error and print the result.
mse_residuals_knn_test <- sum((k_test_residuals - mean(k_test_residuals)) ^ 2) /
    length(k_test_residuals)
printf(" - Mean-squared error on the test data (20%%): %.2e", mse_residuals_knn_test)

# Impute unknown values by covering all the columns with unknowns.
interpolated_data <- clean_data
for(i in attr_with_unkw) {
    # Divide data by rows in two sets: one with the samples that contain a question
    # mark in that column and one with the rest.
    unk_indices <- which(clean_data[,i] == '?')
    i_unk_data <- no_unkw_data[unk_indices, ]
    i_no_unk_data <- no_unkw_data[-unk_indices, ]

    # Compute the nearest neighbours in the set of elements with question mark of 
    # the elements with a question mark in that column.
    i_klabels <- knn(i_no_unk_data,
                     i_unk_data,
                     i_no_unk_data$ViolentCrimesPerPop, k = 1,
                     prob = FALSE, algorithm=c("kd_tree"))
    indices <- attr(i_klabels, "nn.index")
    nn_subs <- clean_data[-unk_indices, ][indices,]

    # Substitute the question mark values by the value of the nearest neighbour.
    interpolated_data[unk_indices, i] <- nn_subs[,i]
}

# Split off new test data. 
folds <- cvFolds(nrow(interpolated_data), K=5)
train_data <- interpolated_data[folds$subsets[folds$which != 1], ]
test_data  <- interpolated_data[folds$subsets[folds$which == 1], ]

# Compute the nearest neighbour regression again and evaluate it.
klabels <- knn(train_data[!(names(train_data)) %in% c("ViolentCrimesPerPop")],
               test_data [!(names(test_data))  %in% c("ViolentCrimesPerPop")],
               train_data$ViolentCrimesPerPop, k = 1,
               prob = FALSE, algorithm = c("kd_tree"))
k_test_residuals <- test_data$ViolentCrimesPerPop - as.numeric(levels(klabels))[klabels]
plot(as.numeric(levels(klabels))[klabels], k_test_residuals)

# Compute the mean-squared error and print the result.
mse_residuals_knn_test <- sum((k_test_residuals - mean(k_test_residuals)) ^ 2) /
    length(k_test_residuals)
printf(" - Mean-squared error on the test data (20%%): %.2e", mse_residuals_knn_test)

# EXTRA:
# Data visualization commands:
#  > View(data)
#  > summary(data)