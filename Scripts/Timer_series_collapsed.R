#script to show time series of collapsed vs non-collapsed plots

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)

#load data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Plots<-read.csv("Denny_plots.csv")

head(Plots)

#add column for tanner index
Plots$Tanner<-(Plots$Sor_BA+Plots$Sorensen.y)/2

#classify plots by collapse status - collapsed (1) or not (0)
Plots$Collapse<-NA
for (i in 1:nrow(Plots)){
  Plots$Collapse[i]<-ifelse(Plots$BAPERCM[i]<=-0.25,1,0)
}
#classify plots to identify those that have *at some point* been classed as collapsed
Plots$Collapse2<-NA
Plots2<-NULL
Block_unique<-unique(Plots$Block)
for (i in 1:length(Block_unique)){
  Block_sub<-subset(Plots,Block==Block_unique[i])
  Block_sub$Collapse2<-ifelse(sum(Block_sub$Collapse)>0,1,0)
  Plots2<-rbind(Block_sub,Plots2)
}

keeps<-c("Block","Year","Tanner","BAPERCM","Collapse2")
Plots3<-Plots2[keeps]
Plots4<-rbind(data.frame(Block=7,Year=2014,Tanner=0,BAPERCM=0,Collapse2=1),Plots3)

#plot basal area time series
theme_set(theme_bw(base_size=12))
BA_Coll1<-ggplot(Plots4,aes(x=Year,y=BAPERCM,group=Block))+geom_point(shape=1,size=2)+geom_line()+facet_wrap(~Collapse2)
BA_Coll2<-BA_Coll1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
BA_Coll2+geom_smooth(se=F,method="lm",size=2,aes(group=NULL))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Collapse_BA_TS.png",width = 8,height=6,units = "in",dpi=300)

#plot tanner time series
theme_set(theme_bw(base_size=12))
Tanner_Coll1<-ggplot(Plots4,aes(x=Year,y=Tanner,group=Block))+geom_point(shape=1,size=2)+geom_line()+facet_wrap(~Collapse2)
Tanner_Coll2<-Tanner_Coll1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Tanner_Coll2+geom_smooth(se=F,method="glm",size=2,aes(group=NULL))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Collapse_Tanner_TS.png",width = 8,height=6,units = "in",dpi=300)

#plot trait time series
theme_set(theme_bw(base_size=12))
BA_Coll1<-ggplot(Plots2,aes(x=Year,y=LightPercM,group=Block))+geom_point(shape=1,size=2)+geom_line()+facet_wrap(~Collapse2)
BA_Coll2<-BA_Coll1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
BA_Coll2+geom_smooth(se=F,method="lm",size=2,aes(group=NULL))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Collapse_BA_TS.png",width = 8,height=6,units = "in",dpi=300)

