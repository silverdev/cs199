#!/usr/bin/env Rscript

# Install required packages.
#install.packages('randomForest')
#install.packages('cvTools')
#install.packages('FNN')
#install.packages("tm")
#install.packages("SnowballC")

# Load the libraries.
library(randomForest)
library(cvTools)
library(FNN)
library(tm)
library(SnowballC)

# Define functions.
printf <- function(...) invisible(print(sprintf(...)))

# Read all the lines of the document and store them in a vector of characters.
msgChars <- readLines("Texts/SMSSpamCollection")


# Split by tabs since the labels is separated with a \t from the content.
msgList <- strsplit(msgChars, '\t')

# Transform the list into a matrix (do.class() applies the operation to every element in the list).
msgMatrix <- do.call(rbind, msgList)

# Get the labels and convert them to integer.
trueLabels <- (msgMatrix[,1]=="spam") + 0 # Add 0 to convert from logical to int.

# Remove non-ASCII words.
content <- c(msgMatrix[,2])
Encoding(content) <- "latin1"
content <- iconv(content, "latin1", "ASCII", sub="")

# Create the corpus data only the content of the mails (one document per line).
corpus <- Corpus(VectorSource(content))

# Remove punctuation symbols.
corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes=T)

# Everything to lower case.
corpus <- tm_map(corpus, tolower)

# Remove stop words (case sensitive).
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Stem the document words, i.e., reduce them to the root.
corpus <- tm_map(corpus, stemDocument, language="english")

# Remove extra whitespaces.
corpus <- tm_map(corpus, stripWhitespace)

# Build a document term matrix from the corpus.
dtm <- DocumentTermMatrix(corpus)

termCount = c(".94", ".95",".97",".98",".99")
termCount
spam <- new.env()
accuracy<- new.env()
kaccuracy <- new.env()

for (v in termCount){
    # Remove sparse terms to get a managable number of terms.
    dtmtmp <- removeSparseTerms(dtm, as.numeric(v))
    # Convert the document term matrix to a standard matrix.
    freqMatrix <- as.data.frame( as.matrix(dtmtmp) )
    # Normalize the frequency matrix: 0 if absent, 1 if present.
    spamt <- (freqMatrix > 0) + 0 # Add 0 to convert from logical to int.
    # Delete the header names to avoid collisions with R reserved words.
    spam[[v]] <- matrix(spamt, ncol=ncol(spamt), dimnames=NULL)
    accuracy[[v]] <- 0
    kaccuracy[[v]] <- 0
}

# 10 fold cross validation (data divided in 10 parts randomly sampled)
accuracyOfAssumingHam <- 0
folds <- cvFolds(nrow(spam), K=10)
for(i in 1:10) {
    # Print iteration.
    printf("Iterarion %d", i)
    for (v in termCount){
        # Separate train and test data and labels.
        trainData <- spam[[v]][folds$subsets[folds$which != i], ]
        testData  <- spam[[v]][folds$subsets[folds$which == i], ]
        trainLabels <- trueLabels[folds$subsets[folds$which != i]]
        testLabels  <- trueLabels[folds$subsets[folds$which == i]]

        # Random Forest classifier:
        #  - Create a random forest using the training data.
        clf <- randomForest(factor(trainLabels) ~ ., data=trainData, ntree=30)

        #  - Classify the test data with the random forest classifier.
        labels <- predict(clf, testData)
        
        #  - Get the number of correctly classified samples.
        hits <- length(labels[labels == testLabels])
        accuracy[[v]] <- accuracy[[v]] + (hits / nrow(testData))
        
        # Nearest Neighbours classifier:
        #  - Classify using fields 1 to 54 as it gets far better results than 1 to 57.
        klabels<- knn(trainData, testData, trainLabels, k = 1, prob = FALSE,
                      algorithm=c("kd_tree"))
        
        #  - Get the number of correctly classified samples.
        khits <- length(klabels[klabels == testLabels])
        kaccuracy[[v]] <- kaccuracy[[v]] + (khits /nrow(testData))
    }
        ahits <- nrow(testData) -sum(testLabels)
        accuracyOfAssumingHam <- accuracyOfAssumingHam + (ahits/nrow(testData))
    
}

accuracyOfAssumingHam <- (accuracyOfAssumingHam/ 10) * 100

for (v in termCount){
    # Print the resulting accuracies.
    accuracy[[v]] <- (accuracy[[v]] / 10) * 100
    kaccuracy[[v]] <- (kaccuracy[[v]] / 10) * 100
   
    printf("Random forest accuracy: %.2f %%", accuracy[[v]])
    printf("KNN accuracy: %.2f %%", kaccuracy[[v]])
    printf("Assuming nothing is spam %.2f %%", accuracyOfAssumingHam)
    print(ncol(spam[[v]]))
    print(nrow(spam[[v]]))
}
