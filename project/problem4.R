# Define functions.
printf <- function(...) invisible(print(sprintf(...)))
library(ggplot2)

rmStd0<- function(df){
toRm = c()    
for (d in names(df)){
    s = sd(df[[d]], na.rm = TRUE)
    if (s == 0){
        print(d)
        toRm = c(toRm, d)
    }
}
df <- df[,!(names(df)) %in% toRm]

return(df)
}

# Read the data and clean it!
#toRm = c("Q4Q07_corr", "Q4Q13_corr")
data <- read.csv('bloomNA0.csv', h=T) # Headers = True.
bloom_level_1 <- rmStd0(data[-1])

data <- read.csv('bloomNA1.csv', h=T) # Headers = True.
bloom_level_2 <- rmStd0(data[-1])

data <- read.csv('bloomNA2.csv', h=T) # Headers = True.
bloom_level_3 <- rmStd0(data[-1])

dataBloom = list(bloom_level_1, bloom_level_2, bloom_level_3)

View(bloom_level_1)

View(dataBloom[1])

cor_norm <- function(d) cor(d, use="na.or.complete")

corBloom = lapply(dataBloom, cor_norm)

View(corBloom[1])

units = c(0,1,2,3,4,5,6,7)


#find how correlations compare.
corMeans <- function(d) {
    correlationSums <- (apply(d, 2, sum )) -1
    correlationMeans <- correlationSums / (ncol(d) - 1 )
    return (correlationMeans)
}

corMeansBloom <- lapply(corBloom, corMeans)

mean(unlist(corMeansBloom, recursive = FALSE))





compareWithTest <- function(data){
    linedata <- data.frame(unit = numeric(), same = numeric(), others = numeric(), diff = numeric())
    df <- data.frame(vector = numeric(), unit = numeric())
    
    correlationMatrix <- data.frame(data)
quizCol <- correlationMatrix[,-grep("^Q8Q[0-9][0-9]_corr$" ,names(correlationMatrix))]
    testRow <- quizCol[grep("^Q8Q[0-9][0-9]_corr$",names(correlationMatrix)),]

    units = c(0,1,2,3,4,5,6,7)
    for (i in units){
        questionsInUnit <- testRow[,grep(sprintf("^Q%dQ[0-9][0-9]_corr$",i),names(testRow))]
        testQuestionsInUnit <- ((1:(nrow(questionsInUnit)/8)) + nrow(questionsInUnit)/8* i)
        t <- questionsInUnit[testQuestionsInUnit,]
        f <- questionsInUnit[-testQuestionsInUnit,]
        corInUnit <- mean(unlist(t))
        corOutUnit <- mean(unlist(f))        
        printf("mean correlation of week %d quiz to week %d test questions: %f", i, i, corInUnit)
        printf("max  correlation of week %d quiz to week %d test questions: %f", i, i, max(t))
        printf("mean correlation of week %d quiz to other test questions: %f", i, corOutUnit)
        printf("max  correlation of week %d quiz to other test questions: %f", i, max(f))

        print(corInUnit - corOutUnit)
        df <- rbind(df, data.frame(vector= as.vector(unlist(t)), unit=i))
        linedata <- rbind(linedata, data.frame(unit = i, same =corInUnit, other = corOutUnit, diff = corInUnit - corOutUnit))
    }
return(list(linedata, df))
}
#make graphs

View(corBloom[1])

results <- lapply(corBloom, compareWithTest)

View(results[[1]][[2]])

itter = c(1,2,3)
df =  data.frame()
linedata =  data.frame()
for (i in itter)
{
    ld = results[[i]][[1]]
    ld = cbind(bloom=i,ld)
    linedata = rbind(linedata, ld)
    df <- rbind(df, results[[i]][[2]])
    ## ggplot(ld, aes(unit)) + 
    ##     geom_line(aes(y = same, colour = "related")) + 
    ##         geom_line(aes(y = other, colour = "unrelated")) +
    ##             ylab("mean") +
    ##                 ggtitle(sprintf("mean correlation between questions at bloom level %d",i))
    ##     ggsave(sprintf('correlationlinesbloom%d.pdf', i))
}


View(linedata)

ggplot(linedata, aes(unit, colour=factor(bloom))) + 
    geom_line(aes(y = same, linetype="related", group=bloom)) + 
    geom_line(aes(y = other, linetype="unrelated", group=bloom)) +
    ylab("mean") +
    ggtitle("mean correlation between questions at bloom diffent")
   ggsave('correlationlinesbloomAll.pdf')
