#script to do analysis to see whether any plots recovered following loss

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)

#load in data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("BA_Gradient.csv")

#classify plots by collapse status - declined, or not
BA$Collapse<-NA
for (i in 1:nrow(BA)){
  BA$Collapse[i]<-ifelse(BA$BAPERCM[i]<0,1,0)
}


#classify plots to identify those that have *at some point* declined in BA
BA$Collapse2<-NA
Plots2<-NULL
Block_unique<-unique(BA$Block)
for (i in 1:length(Block_unique)){
  Block_sub<-subset(BA,Block==Block_unique[i])
  Block_sub$Collapse4<-NA
  Block_sub$Collapse4[1]<-0
  Block_sub$Time_Coll<-NA
  Block_sub$Coll_Sever<-NA
  for (y in 2:nrow(Block_sub)){  
    if (Block_sub$Collapse4[y-1]==0&Block_sub$Collapse[y]==0) {
    Block_sub$Collapse4[y]<-0
    }else{Block_sub$Collapse4[y]<-1    
    }
  }
  Block_sub$Collapse2<-ifelse(sum(Block_sub$Collapse)>0,1,0)
  Block_sub$Collapse3<-ifelse(sum(Block_sub$Collapse2)>0&tail(Block_sub$Collapse,1)==0,2,Block_sub$Collapse2)
  Plots2<-rbind(Block_sub,Plots2)
}


ggplot(Plots2,aes(x=Year,y=BAPERCM,group=Block))+geom_point()+geom_line()+facet_wrap(~Collapse3)+geom_smooth(se=F,method="lm",colour="blue",size=3,aes(group=NULL))


#now subset to give plots post decline
Plots3<-subset(Plots2,Collapse3==1)
