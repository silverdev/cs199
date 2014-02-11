# Load the library.
library(ggplot2)

# Load the data.
wines <- read.csv2('winequality-red.csv', dec = '.', header=TRUE)

# Histogram - Fixed acidity and quality.
wines$Fixed.Acidity <- cut(wines$fixed.acidity, c(-Inf, 3, 5, 7, 8, 9, 11, 13, 15, Inf))
ggplot(wines, aes(x=Fixed.Acidity)) + geom_histogram(binwidth=0.5,fill='darkgreen')+facet_grid(quality~.,scale='free')
ggsave('acidVSqual.pdf')

# Scatterplot - Alcohol, density and quality.
wines$Quality <- cut(wines$quality,c(3,4,5,6,7,8))
ggplot(wines, aes(x=alcohol, y=density, color=Quality)) + geom_point(position=position_jitter(w=0.4,h=0.4))
ggsave('alcoholVSdensity.pdf')

# Histogram - Alcohol and quality.
wines$Alcohol <- cut(wines$alcohol,10)
ggplot(wines, aes(x=Alcohol)) + geom_histogram(binwidth=0.2,fill='darkblue',color='white')+facet_grid(quality~., scale='free')
ggsave('alcoholVSqual(blue).pdf')