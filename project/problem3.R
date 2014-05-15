# Define functions.
printf <- function(...) invisible(print(sprintf(...)))

# Read the data.
#toRm = c("Q4Q07_corr", "Q4Q13_corr")
dataWithNames<- read.csv('combinednodropna.csv', h=T) # Headers = True.

#remove user id's
data <- dataWithNames[-1]

# remove values with std of zero

toRm = c()
#check for std of 0
for (d in names(data)){
    s = sd(data[[d]], na.rm = TRUE)
    if (s == 0){
        print(d)
        toRm = c(toRm, d)
    }
}
data <- data[,!(names(data)) %in% toRm]

#quizData <- data[,grep(sprintf("^Q%dQ[0-9][0-9]_corr$",4),names(data))]
#testData <- data[,-(1:(20 * 8-2))]
#ncol(quizData)
#View(quizData)

units = c(0,1,2,3,4,5,6,7)
#units = c(4,5,6)
#quizData <- data[,1:(20*8-2)]
#testData <- data[,-(1:(20 * 8-2))]

#calulate Correlation matrix

correlationMatrix = cor(data, use = "na.or.complete")


#find how correlations compare.
correlationSums <- (apply(correlationMatrix, 2, sum )) -1
correlationMeans <- correlationSums / (ncol(data) - 1 )
View(correlationMeans)
print(mean(correlationMeans))


#now lets compare the quize to the test
quizCol <- correlationMatrix[,1:(20 * 8-2)]
testRow <- quizCol[-(1:(20 * 8-2)), ]
#View(testRow)
units = c(0,1,2,3,4,5,6,7)
for (i in units){
   questionsInUnit <- testRow[,grep(sprintf("^Q%dQ[0-9][0-9]_corr$",i),names(data))]
   t <- questionsInUnit[(1:5) + 5 * i,]
   f <- questionsInUnit[-((1:5) + 5 * i),]
   corInUnit <- mean(t)
   corOutUnit <- mean(f)
   printf("mean corralation to week %d quiz: %f", i, corInUnit)
   printf("max  corralation to week %d quiz: %f", i, max(t))
   printf("mean corralation to other quizs: %f", corOutUnit)
   printf("max  corralation to other quizs: %f", max(f))
   printf("the diffence of corralation: %f", corInUnit - corOutUnit)
}

#make graphs

