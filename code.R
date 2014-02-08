setwd('/users/daf/Current/courses/BigData/Examples')
data1<-read.csv('/users/daf/Current/courses/BigData/doing_data_science-master/dds_datasets/dds_ch2_nyt/nyt1.csv')
data1$agecat<-cut(data1$Age, c(-Inf, 0, 18, 24, 34, 44, 54, 64, 74, 84, Inf))
# This breaks the Age column into categories
data1$impcat<-cut(data1$Impressions, c(-Inf, 0, 1, 2, 3, 4, 5, Inf))
# This breaks the impression column into categories
summary(data1)


#demographic histogram of NYT readers
install.packages("ggplot2")
# you only need to install this once
#
#

#but each session, you need to load the library, as below
library(ggplot2)
#
# this gives a histogram of ages
ggplot(data1, aes(x=agecat))+geom_histogram(binwidth=1)
ggsave('nytagehist.pdf')
#faceted conditional histogram of impressions, by age
#tolerable, shaky pix
ggplot(data1, aes(x=Impressions))+geom_histogram(binwidth=1,fill='white',color='black')+facet_grid(agecat~.,scales='free')
ggsave('nytimpressionsbyage.pdf')
#faceted conditional histogram of impressions, by age
#tolerable, shaky pix
ggplot(data1, aes(x=Clicks))+geom_histogram(binwidth=0.5,fill='white',color='black')+facet_grid(agecat~.,scales='free')
ggsave('nytclicksbyage.pdf')

ggplot(subset(data1, Impressions>0), aes(x=Clicks/Impressions, color=agecat))+geom_histogram()+facet_grid(agecat~.,scales='free')
ggsave('nytclicksperimpbyage.pdf')

# now a scatter plot to show what's going on
# notice the geom_point replacing geom_histogram - that's the crucial
# thing
# r is nice, and jitters the positions for us
ggplot(subset(subset(data1, Impressions>0), Clicks>0), aes(y=Clicks, x=Impressions))+geom_point(position=position_jitter(w = 0.4, h = 0.4))+facet_grid(agecat~.)
ggsave('nytclicksperimpbyagescatter.pdf')

ggplot(subset(subset(data1, Impressions>0), Clicks>0), aes(y=(Clicks-mean(Clicks))/sd(Clicks), x=(Impressions-mean(Impressions))/sd(Impressions)))+geom_point(position=position_jitter(w = 0.4, h = 0.4))+facet_grid(agecat~.)
ggsave('nytclickimpcorr.pdf')
# horrid - how do we do this?

ggplot(subset(subset(data1, Impressions>0), Clicks>0), aes(y=Clicks/Impressions, x=Age))+geom_point(position=position_jitter(w = 0.1, h = 0.1))
#+facet_grid(agecat~.)
ggsave('nytclickimpage.pdf')

ggplot(subset(subset(data1, Impressions>0), Clicks>0), aes(y=Clicks, x=Age))+geom_point(position=position_jitter(w = 0.1, h = 0.1))
#+facet_grid(agecat~.)
ggsave('nytclickage.pdf')
ggplot(subset(subset(data1, Impressions>0), Clicks>0), aes(y=Impressions, x=Age))+geom_point(position=position_jitter(w = 0.1, h = 0.1))
#+facet_grid(agecat~.)
ggsave('nytimpage.pdf')

# do this if it isn't installed
install.packages('vcd')
#then
library(vcd)
# a simple mosaic plot to practice on
data1$agecattight<-cut(data1$Age, c(-Inf, 44,  Inf))
# This breaks the Age column into categories
data1$impcattight<-cut(data1$Impressions, c(-Inf, 10,  Inf))
tab <- with(subset(subset(subset(data1, Impressions>0), Clicks>0), Age>0), table(impcattight, Clicks, agecattight))
setEPS()
postscript("mosaic2.eps")
mosaic(tab)
dev.off()
# this makes a three way count table
data1$agecattight<-cut(data1$Age, c(-Inf, 34,  64, Inf))
# This breaks the Age column into categories
data1$impcattight<-cut(data1$Impressions, c(-Inf, 1, 5, 10, 15, Inf))
tab <- with(subset(subset(subset(data1, Impressions>0), Clicks>0), Age>0), table(impcattight, Clicks, agecattight))
setEPS()
postscript("mosaic3.eps")
mosaic(tab)
dev.off()
