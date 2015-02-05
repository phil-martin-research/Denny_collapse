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
Block_merge2<-data.frame(Block1=c(seq(1,23,2),seq(27,41,2),seq(46,48,2),seq(52,56,2),seq(60,66,2)),
Block2=c(seq(2,24,2),seq(28,42,2),seq(47,49,2),seq(53,57,2),seq(61,67,2)))

unique(BA$Block)

Block_means_2<-NULL
for(i in 1:nrow(Block_merge2)){
 Block_sub<-subset(BA,Block>=Block_merge2[i,1])  
 Block_sub<-subset(Block_sub,Block<=Block_merge2[i,2])
 Block_sub<-Block_sub[with(Block_sub, order(Year)), ]
 Year_unique<-unique(Block_sub$Year)
for (y in 1:length(Year_unique)){
 Block_sub_sub<-subset(Block_sub,Year==Year_unique[y])
 Block_sum_Block<-paste(Block_sub_sub$Block[1],"-",Block_sub_sub$Block[2],sep = "")
 Block_sum_BA<-mean(Block_sub_sub$BAM[1],Block_sub_sub$BAM[2])
 Block_sum_North<-mean(Block_sub_sub$Northing[1],Block_sub_sub$Northing[2])
 Block_sum_East<-mean(Block_sub_sub$Easting[1],Block_sub_sub$Easting[2])
 Block_sum<-data.frame(Year=Year_unique[y],Block=Block_sum_Block,BA=Block_sum_BA,Northing=Block_sum_North,Easting=Block_sum_East)
 Block_means_2<-rbind(Block_sum,Block_means_2)
}
}


ggplot(Block_means_2,aes(x=Easting,y=Northing,colour=BA))+geom_point(size=3)+facet_wrap(~Year)

ggplot(Block_means_2,aes(x=Year,y=BA,group=Block))+geom_point(size=3)+geom_line()
