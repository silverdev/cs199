#!/usr/bin/env Rscript

# Install the random forest, cvTools and FNN packages.
#install.packages('randomForest')
#install.packages('cvTools')
#install.packages('FNN')

# Load the libraries.
library(randomForest)
library(cvTools)
library(FNN)

# Define functions.
printf <- function(...) invisible(print(sprintf(...)))

# Read the data.
spam <- read.csv('spambase.data', h=F)   # h=F (headers=FALSE)

# 10 fold cross validation (data divided in 10 parts randomly sampled)
accuracy <- 0
kaccuracy <- 0
folds <- cvFolds(nrow(spam), K=10)
for(i in 1:10) {
  # Print iteration.
  printf("Iterarion %d", i)

  # Separate train and test data.
  train <- spam[folds$subsets[folds$which != i], ]
  test  <- spam[folds$subsets[folds$which == i], ]
  
  # Random Forest classifier:
  #  - Create a random forest using the training data.
  clf <- randomForest(factor(train$V58) ~ ., data=train[,1:54], ntree=100)
  
  #  - Classify the test data with the random forest classifier.
  labels <- predict(clf, test[,1:54])
  
  #  - Get the number of correctly classified samples.
  hits <- length(labels[labels == test[,58]])
  accuracy <- accuracy + (hits / nrow(test))
  
  # Nearest Neighbours classifier:
  #  - Classify using fields 1 to 54 as it gets far better results than 1 to 57.
  klabels<- knn(train[1:54], test[1:54], train$V58, k = 1, prob = FALSE,
              algorithm=c("kd_tree"))
              
  #  - Get the number of correctly classified samples.
  khits <- length(klabels[klabels == test[,58]])
  kaccuracy <- kaccuracy + (khits /nrow(test))
}

# Print the resulting accuracies.
accuracy <- (accuracy / 10) * 100
kaccuracy <- (kaccuracy / 10) * 100

printf("Random forest accuracy: %.2f %%", accuracy)
printf("KNN accuracy: %.2f %%", kaccuracy)