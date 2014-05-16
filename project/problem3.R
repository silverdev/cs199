# Define functions.
printf <- function(...) invisible(print(sprintf(...)))
library(ggplot2)

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
#View(correlationMeans)
#print(mean(correlationMeans))


#now lets compare the quize to the test
quizCol <- correlationMatrix[,1:(20 * 8-2)]
testRow <- quizCol[-(1:(20 * 8-2)), ]
#View(testRow)
df <- data.frame(vector = numeric(), unit = numeric())
units = c(0,1,2,3,4,5,6,7)
for (i in units){
    questionsInUnit <- testRow[,grep(sprintf("^Q%dQ[0-9][0-9]_corr$",i),names(data))]
    t <- questionsInUnit[(1:5) + 5 * i,]
    f <- questionsInUnit[-((1:5) + 5 * i),]
    corInUnit <- mean(t)
    corOutUnit <- mean(f)
    printf("mean correlation of week %d quiz to week %d test questions: %f", i, i, corInUnit)
    printf("max  correlation of week %d quiz to week %d test questions: %f", i, i, max(t))
    printf("mean correlation of week %d quiz to other test questions: %f", i, corOutUnit)
    printf("max  correlation of week %d quiz to other test questions: %f", i, max(f))
    printf("the diffence of correlation: %f", corInUnit - corOutUnit)
    #View(t)
    #Sys.sleep(1200)
    df <- rbind(df, data.frame(vector= as.vector(t), unit=i))
}
ggplot(df, aes(y=vector, x=factor(unit))) + geom_boxplot()
ggsave('quizcorrelationbox.pdf')

#make graphs

