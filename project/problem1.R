#!/usr/bin/env Rscript

# Install imputation package manually.
# install.packages("imputation_2.0.1.tar.gz", repos = NULL, type="source")

# Load the libraries.
library(lattice)    # cvTools dep.
library(robustbase) # cvTools dep.
library(cvTools)
library(imputation)

# Define the constants.
num_partitions <- 10
num_nn <- 10

# Define functions.
printf <- function(...) invisible(print(sprintf(...)))

# Read the data.
data <- read.csv('gradebook.csv', h=T) # Headers = True.

# Check the amount of students that have completed each assignment.
printf("Total number of students: %d\n", nrow(data))
printf("Number of students that have completed the Orientation Quiz: %d\n", nrow(data[-which(is.na(data$Orientation.Quiz)),]))
printf("Number of students that have completed Week 1 Quiz 1: %d\n", nrow(data[-which(is.na(data$Week.1.Quiz.1)),]))
printf("Number of students that have completed Week 1 Quiz 2: %d\n", nrow(data[-which(is.na(data$Week.1.Quiz.2)),]))
printf("Number of students that have completed Week 2 Quiz 1: %d\n", nrow(data[-which(is.na(data$Week.2.Quiz.1)),]))
printf("Number of students that have completed Week 2 Quiz 2: %d\n", nrow(data[-which(is.na(data$Week.2.Quiz.2)),]))
printf("Number of students that have completed Week 3 Quiz 2: %d\n", nrow(data[-which(is.na(data$Week.3.Quiz.2)),]))
printf("Number of students that have completed Week 3 Quiz 1: %d\n", nrow(data[-which(is.na(data$Week.3.Quiz.1)),]))
printf("Number of students that have completed Week 4 Quiz 1: %d\n", nrow(data[-which(is.na(data$Week.4.Quiz.1)),]))
printf("Number of students that have completed Week 4 Quiz 2: %d\n", nrow(data[-which(is.na(data$Week.4.Quiz.2)),]))
printf("Number of students that have completed Week 5 Quiz 1: %d\n", nrow(data[-which(is.na(data$Week.5.Quiz.1)),]))
printf("Number of students that have completed Week 5 Quiz 2: %d\n", nrow(data[-which(is.na(data$Week.5.Quiz.2)),]))
printf("Number of students that have completed Week 6 Quiz 1: %d\n", nrow(data[-which(is.na(data$Week.6.Quiz.1)),]))
printf("Number of students that have completed Week 6 Quiz 2: %d\n", nrow(data[-which(is.na(data$Week.6.Quiz.2)),]))
printf("Number of students that have completed Week 7 Quiz 1: %d\n", nrow(data[-which(is.na(data$Week.7.Quiz.1)),]))
printf("Number of students that have completed Week 7 Quiz 2: %d\n", nrow(data[-which(is.na(data$Week.7.Quiz.2)),]))
printf("Number of students that have completed Week 8 Quiz 1: %d\n", nrow(data[-which(is.na(data$Week.8.Quiz.1)),]))
printf("Number of students that have completed Week 8 Quiz 2: %d\n", nrow(data[-which(is.na(data$Week.8.Quiz.2)),]))
printf("Number of students that have completed Final Exam: %d\n", nrow(data[-which(is.na(data$Final.Exam)),]))

# Linear regression.
#  - Impute NAs using nearest neighbours.
imputed_data <- kNNImpute(data[, -c(1,2)], num_nn) # Omit User ID and Full Name columns.
imputed_data <- imputed_data$x

#  - Compute several linear regressions using cross validation and test them.
mean_mse_residual_all_imputed <- 0
mean_mse_residual_final_imputed <- 0
mean_mse_residual_final <- 0

final_data            <- data[-which(is.na(data$Final.Exam)), -c(1,2)]
final_imputed_data    <- imputed_data[-which(is.na(data$Final.Exam)), ]
no_final_imputed_data <- imputed_data[ which(is.na(data$Final.Exam)), ]

final_folds    <- cvFolds(nrow(final_imputed_data), K=num_partitions)
no_final_folds <- cvFolds(nrow(no_final_imputed_data), K=num_partitions)
for(i in 1:num_partitions) {
  # Print iteration.
  printf("Iterarion %d", i)

  # Separate train and test data.
  final_train <- final_data[final_folds$subsets[final_folds$which != i], ]
  final_imputed_train <- final_imputed_data[
                         final_folds$subsets[final_folds$which != i], ]
  no_final_imputed_train <- no_final_imputed_data[
                            no_final_folds$subsets[no_final_folds$which != i], ]

  final_test <- final_data[final_folds$subsets[final_folds$which == i], ]
  final_imputed_test <- final_imputed_data[
                        final_folds$subsets[final_folds$which == i], ]
  no_final_imputed_test <- no_final_imputed_data[
                           no_final_folds$subsets[no_final_folds$which == i], ]

  train <- rbind(final_imputed_train, no_final_imputed_train)
  test <- rbind(final_imputed_test, no_final_imputed_test)

  # Compute the regressions.
  all_imputed_linear_regr <- lm(Final.Exam ~ Orientation.Quiz+Week.1.Quiz.1+Week.1.Quiz.2+Week.2.Quiz.1+Week.2.Quiz.2+Week.3.Quiz.1+Week.3.Quiz.2+Week.4.Quiz.1+Week.4.Quiz.2+Week.5.Quiz.1+Week.5.Quiz.2+Week.6.Quiz.1+Week.6.Quiz.2+Week.7.Quiz.1+Week.7.Quiz.2+Week.8.Quiz.1+Week.8.Quiz.2, data=train)
  final_imputed_linear_regr <- lm(Final.Exam ~ Orientation.Quiz+Week.1.Quiz.1+Week.1.Quiz.2+Week.2.Quiz.1+Week.2.Quiz.2+Week.3.Quiz.1+Week.3.Quiz.2+Week.4.Quiz.1+Week.4.Quiz.2+Week.5.Quiz.1+Week.5.Quiz.2+Week.6.Quiz.1+Week.6.Quiz.2+Week.7.Quiz.1+Week.7.Quiz.2+Week.8.Quiz.1+Week.8.Quiz.2, data=final_imputed_train)
  final_linear_regr <- lm(Final.Exam ~ Orientation.Quiz+Week.1.Quiz.1+Week.1.Quiz.2+Week.2.Quiz.1+Week.2.Quiz.2+Week.3.Quiz.1+Week.3.Quiz.2+Week.4.Quiz.1+Week.4.Quiz.2+Week.5.Quiz.1+Week.5.Quiz.2+Week.6.Quiz.1+Week.6.Quiz.2+Week.7.Quiz.1+Week.7.Quiz.2+Week.8.Quiz.1+Week.8.Quiz.2, data=final_train)

  # Compute their error rates.
  residuals_all_imputed <- final_imputed_test$Final.Exam - predict(all_imputed_linear_regr, final_imputed_test)
  mse_residuals_all_imputed <- sum((residuals_all_imputed - mean(residuals_all_imputed)) ^ 2) / length(residuals_all_imputed)
  mean_mse_residual_all_imputed <- mean_mse_residual_all_imputed + mse_residuals_all_imputed

  residuals_final_imputed <- final_imputed_test$Final.Exam - predict(final_imputed_linear_regr, final_imputed_test)
  mse_residuals_final_imputed <- sum((residuals_final_imputed - mean(residuals_final_imputed)) ^ 2) / length(residuals_final_imputed)
  mean_mse_residual_final_imputed <- mean_mse_residual_final_imputed + mse_residuals_final_imputed

  residuals_final <- final_test$Final.Exam - predict(final_linear_regr, final_test)
  residuals_final <- residuals_final[-which(is.na(residuals_final))]
  mse_residuals_final <- sum((residuals_final - mean(residuals_final)) ^ 2) / length(residuals_final)
  mean_mse_residual_final <- mean_mse_residual_final + mse_residuals_final
}
# Print results.
mean_mse_residual_all_imputed <- mean_mse_residual_all_imputed/num_partitions
mean_mse_residual_final_imputed <- mean_mse_residual_final_imputed/num_partitions
mean_mse_residual_final <- mean_mse_residual_final/num_partitions
printf("   %.2f   %.2f   %.2f", mean_mse_residual_all_imputed, mean_mse_residual_final_imputed, mean_mse_residual_final)
#printf("Mean-squared error using linear regression obtained with all imputed data (%d partitions): %f\n", num_partitions, mean_mse_residual_all_imputed)
#printf("Mean-squared error using linear regression obtained with imputed data that originally contained the Final Exam grade (%d partitions): %f\n", num_partitions, mean_mse_residual_final_imputed)
#printf("Mean-squared error using linear regression obtained with data that originally contained the Final Exam grade (%d partitions): %f\n", num_partitions, mean_mse_residual_final)
#plot(all_imputed_linear_regr)
#plot(final_imputed_linear_regr)
#plot(final_linear_regr)

# EXTRA:
# Data visualization commands:
#  > View(data)
#  > summary(data)
#  
# Measure time with R:
#  > system.time( instruction )