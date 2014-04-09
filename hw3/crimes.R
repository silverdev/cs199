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

# Remove the attributes with question marks (unknowns) on them.
unkw <- clean_data == '?'
unkw_per_attr <- apply(unkw, 2, sum) # Sum the columns (2).
attr_with_unkw <- which(unkw_per_attr > 0) # Equivalent to find in Matlab.
no_unkw_data <- clean_data[, -attr_with_unkw]

# Evaluate the regression looking at the mean-squared error on the training data.
regr <- lm(ViolentCrimesPerPop ~ ., data=no_unkw_data)
residuals <- resid(regr)  # Function to get the residuals.
plot(predict(regr, no_unkw_data), residuals) 

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
plot(test_data$ViolentCrimesPerPop, residuals_test)

mse_residuals_test <- sum((residuals_test - mean(residuals_test)) ^ 2) / length(residuals_test)
printf("Mean-squared error on the test data (20%%): %.2e", mse_residuals_test)

#Nearest neighbors regression

kregr <- knn.reg(train_data[!(names(train_data)) %in% c("ViolentCrimesPerPop")],
               test = NULL,
               train_data$ViolentCrimesPerPop, k = 1,
               algorithm=c("kd_tree"))

plot(kregr$pred, kregr$residuals) 

mse_residuals_knn <- sum((kregr$residuals - mean(kregr$residuals)) ^ 2) /
    length(kregr$residuals)
printf("Mean-squared error on the training data: %.2e", mse_residuals_knn)
#Nearest neighbors regression on the split off test data from above
klabels <- knn(train_data[!(names(train_data)) %in% c("ViolentCrimesPerPop")],
               test_data[!(names(test_data)) %in% c("ViolentCrimesPerPop")],
               train_data$ViolentCrimesPerPop, k = 1,
               prob = FALSE, algorithm=c("kd_tree"))

k_test_residuals <- test_data$ViolentCrimesPerPop - as.numeric(levels(klabels))[klabels]
plot(as.numeric(levels(klabels))[klabels], k_test_residuals)


mse_residuals_knn_test <- sum((k_test_residuals - mean(k_test_residuals)) ^ 2) /
    length(k_test_residuals)
printf("Mean-squared error on the test data with knn regresstion (20%%): %.2e", mse_residuals_knn_test)

# Box cox.
# Remove zero values.
no_unkw_data$ViolentCrimesPerPop[which(no_unkw_data$ViolentCrimesPerPop == 0)] = 1e-100
lambdas <- boxcox(regr, plotit=F)
max_lambda <- lambdas$x[ which(lambdas$y == max(lambdas$y)) ]
no_unkw_data$ViolentCrimesPerPop[which(no_unkw_data$ViolentCrimesPerPop == 1e-100)] = 0
no_unkw_data$ViolentCrimesPerPop <- (no_unkw_data$ViolentCrimesPerPop^max_lambda - 1)/max_lambda
regr_box<- lm(ViolentCrimesPerPop ~ ., data=no_unkw_data)
plot(regr_box)

# Cover all the columns with unknowns.
interplatedData <- clean_data

for(i in attr_with_unkw[1:2]){
    # Divide data by rows in two sets: one with the samples that contain a question
    # mark in that column and one with the rest.
    unk_indices <- which(clean_data[,i] == '?')
    i_unk_data <- no_unkw_data[unk_indices, ]
    i_no_unk_data <- no_unkw_data[-unk_indices, ]


    i_klabels <- knn(i_no_unk_data,
                   i_unk_data,
                   i_no_unk_data$ViolentCrimesPerPop, k = 1,
                   prob = FALSE, algorithm=c("kd_tree"))
    indices <- attr(i_klabels, "nn.index")
    nnSubs <- i_no_unk_data[indices,]
    for (j in 1:nrow(nnSubs)){
        #printf("replacing %s with %f \n",
               interplatedData[row.names(i_unk_data)[j], i], nnSubs[j,i])
        interplatedData[row.names(i_unk_data)[j], i] <- nnSubs[j,i]
    }
}

nn_data <- clean_data
for(i in 1:nrow(clean_data)){
  min_distance <- -1
  min_index <- -1
  for(j in 1:nrow(clean_data)){
    if(i != j){
      d <- distance(clean_data[i,], clean_data[j,])
      if(min_distance == -1 | d < min_distance){
        min_distance <- d
        min_index <- j
      }
    }
  }
  nn_data[i,] <- clean_data[min_index,]
  print(i)
}

# Compute the euclidean distance between two vectors that may contain question 
# mark elements.
distance <- function(v1, v2){
  # Change question mark elements of each vector by the same position elements
  # in the other vector, so that they get cancelled when computing the distance.
  unk_indices_1 <- which(v1 == '?')
  unk_indices_2 <- which(v2 == '?')

  v1[unk_indices_1] = v2[unk_indices_1]
  v2[unk_indices_2] = v1[unk_indices_2]

  # Change question marks elements with same position in both vectors by 0.
  unk_indices <- which(v1 == '?')
  v1[unk_indices] <- 0
  v2[unk_indices] <- 0

  # Ensure vectors type is numeric.
  v1 <- as.numeric(v1)
  v2 <- as.numeric(v2)

  # Return the euclidian distance.
  return(sqrt(sum((v1 - v2) ^ 2)))
}


distance <- function(v1, v2){
  s <- 0
  for(i in 1:length(v1)){
    if(v1[i] != '?' & v2[i] != '?'){
      s <- s + ((as.numeric(v1[i]) - as.numeric(v2[i])) ^ 2)
    }
  }
  return(sqrt(s))
}


outer(clean_data, clean_data, FUN = distance)


# EXTRA:
# Data visualization commands:
#  > View(data)
#  > summary(data)
