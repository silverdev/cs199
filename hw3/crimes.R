#!/usr/bin/env Rscript

# Load the libraries.
library(lattice)    # cvTools dep.
library(robustbase) # cvTools dep.
library(cvTools)
library(MASS)

# Define functions.
printf <- function(...) invisible(print(sprintf(...)))

# Read the data.
data <- read.csv('communitiesH.data', h=T) # Headers = True.

# Remove non-predictive attributes (including state number).
clean_data <- data[, -c(1:5)]

# Remove the attributes with question marks (unknowns) on them.
unkw <- clean_data == '?'
unkw_per_attr <- apply(unkw, 2, sum) # Sum the columns (2).
attr_with_unkw <- which(unkw_per_attr > 0) # Equivalent to find in Matlab.
no_unkw_data <- clean_data[, -attr_with_unkw]

# Evaluate the regression looking at the mean-squared error on the training data.
regr <- lm(ViolentCrimesPerPop ~ ., data=no_unkw_data)
residuals <- resid(regr)  # Function to get the residuals.
mse_residuals <- sum((residuals - mean(residuals)) ^ 2) / length(residuals)
printf("Mean-squared error on the training data: %.2e", mse_residuals)

# Evaluate the regression splitting off some test data and looking at the 
# mean-squared error on that test data.
folds <- cvFolds(nrow(no_unkw_data), K=5)
train_data <- no_unkw_data[folds$subsets[folds$which != 1], ]
test_data  <- no_unkw_data[folds$subsets[folds$which == 1], ]

regr_test <- lm(ViolentCrimesPerPop ~ ., data=train_data)

# Standard way to get the residuals.
residuals_test <- test_data$ViolentCrimesPerPop - predict(regr_test, test_data)
mse_residuals_test <- sum((residuals_test - mean(residuals_test)) ^ 2) / length(residuals_test)
printf("Mean-squared error on the test data (20%%): %.2e", mse_residuals_test)

# Box cox.
# Remove zero values.
no_unkw_data$ViolentCrimesPerPop[which(no_unkw_data$ViolentCrimesPerPop == 0)] = 1e-100
lambdas <- boxcox(regr, plotit=F)
max_lambda <- lambdas$x[ which(lambdas$y == max(lambdas$y)) ]
no_unkw_data$ViolentCrimesPerPop[which(no_unkw_data$ViolentCrimesPerPop == 1e-100)] = 0
no_unkw_data$ViolentCrimesPerPop <- (no_unkw_data$ViolentCrimesPerPop^max_lambda - 1)/max_lambda
regr <- lm(ViolentCrimesPerPop ~ ., data=no_unkw_data)

# Cover all the columns with unknowns.
for(i in attr_with_unkw){
    # Divide data by rows in two sets: one with the samples that contain a question
    # mark in that column and one with the rest.
    unk_indices <- which(clean_data[,i] == '?')
    i_unk_data <- no_unkw_data[unk_indices, ]
    i_no_unk_data <- no_unkw_data[-unk_indices, ]


}


# EXTRA:
# Data visualization commands:
#  > View(data)
#  > summary(data)