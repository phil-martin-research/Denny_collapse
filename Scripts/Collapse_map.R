#script to show location of collapsed plots over time

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)

#load data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Plots<-read.csv("Denny_plots.csv")

head(Plots)

#classify plots by collapse status
Plots$Collapse<-NA
for (i in 1:nrow(Plots)){
  Plots$Collapse[i]<-ifelse(Plots$BAPERCM[i]<=-0.25,1,0)
}

#change 1996 to 1999

for (i in 1:nrow(Plots)){
  Plots$Year[i]<-ifelse(Plots$Year[i]==1996,1999,Plots$Year[i])
}

#plot location of plots that have collapsed
theme_set(theme_bw(base_size=12))
Collapse_map1<-ggplot(Plots,aes(x=Easting,Northing,colour=as.factor(Collapse)))+geom_point(shape=15,size=1)+facet_wrap(~Year)
Collapse_map2<-Collapse_map1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Collapse_map2+scale_colour_manual(values=c("dark green","red"),"Collapse status")+ theme(axis.text.x=element_blank(),
       axis.text.y=element_blank(),axis.ticks=element_blank(),
       axis.title.x=element_blank(),
       axis.title.y=element_blank())

setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Collapse_map.png",width = 8,height=6,units = "in",dpi=300)
