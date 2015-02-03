#test to look at how grain affects our perception of what is going on
#analyses for paul to get the time to collapse

library(ggplot2)
library(lme4)
library(MuMIn)

#########################################################
#exploratory analysis to look importance of grain########
#########################################################

BA<-read.csv("Data/BA_gradient.csv")

#first the unedited data
ggplot(BA,aes(x=Year,y=BAM,group=Block))+geom_point()+geom_line()+facet_wrap(~Block)

#now take average of two plots next to each other
is.even <- function(x) x %% 2 == 0
Block_merge2<-data.frame(Block1=c(seq(1,24,2),seq(27,50,2),seq(51,59,2),seq(61,68,2)),
Block2=c(seq(2,24,2),seq(28,50,2),seq(52,59,2),seq(60,68,2)))


Block_means_2<-NULL
for(i in 1:nrow(Block_merge2)){
 Block_sub<-subset(BA,Block>=Block_merge2[1,1])  
 Block_sub<-subset(BA,Block<=Block_merge2[1,2])
 Block_sub<-Block_sub[with(Block_sub, order(Year)), ]
for (y in 1:nrow(Block_sub)){
 Block_sum<-data.frame()
 Block_sub$Same<-
}
}
