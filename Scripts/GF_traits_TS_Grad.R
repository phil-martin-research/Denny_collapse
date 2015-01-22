#script to do analysis of ground flora change over time for collapsed and uncollapsed plots

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)

#load in data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
GF<-read.csv("BA_GF_ALL.csv")

GF$Collapse<-NA
for (i in 1:nrow(GF)){
  GF$Collapse[i]<-ifelse(GF$BAPERCM[i]<=-0.25,1,0)
}
#classify plots to identify those that have *at some point* been classed as collapsed
GF$Collapse2<-NA
GF2<-NULL
Block_unique<-unique(GF$Block)
for (i in 1:length(Block_unique)){
  Block_sub<-subset(GF,Block==Block_unique[i])
  Block_sub$Collapse2<-ifelse(sum(Block_sub$Collapse)>0,1,0)
  GF2<-rbind(Block_sub,GF2)
}

#exploratory plots of change in different traits over time
#light
ggplot(GF2,aes(x=Year,y=Light,group=Block))+geom_point()+geom_line()+facet_wrap(~Collapse2)+geom_smooth(se=F,method="lm",colour="blue",size=2,aes(group=NULL))
#Nitrogen
ggplot(GF2,aes(x=Year,y=Nit,group=Block))+geom_point()+geom_line()+facet_wrap(~Collapse2)+geom_smooth(se=F,method="lm",colour="blue",size=2,aes(group=NULL))
#Moisture
ggplot(GF2,aes(x=Year,y=Moist,group=Block))+geom_point()+geom_line()+facet_wrap(~Collapse2)+geom_smooth(se=F,method="lm",colour="blue",size=2,aes(group=NULL))

#exploratory plots of change in different traits over the gradient
#light
ggplot(GF2,aes(x=BAPERCM,y=Light_Change,group=Block))+geom_point()+geom_smooth(se=F,method="lm",colour="blue",size=2,aes(group=NULL))
#Nitrogen
ggplot(GF2,aes(x=BAPERCM,y=Nit_Change,group=Block))+geom_point()+geom_smooth(se=F,method="lm",colour="blue",size=2,aes(group=NULL))
#Moisture
ggplot(GF2,aes(x=BAPERCM,y=Moist_change,group=Block))+geom_point()+geom_smooth(se=F,method="lm",colour="blue",size=2,aes(group=NULL))
