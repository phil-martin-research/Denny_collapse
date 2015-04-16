#script to show location of collapsed plots over time

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)

#load data
Plots<-read.csv("Data/BA_tree_ab.csv")

#classify plots by the degree to which they have collapsed
#1- stable or increase, 2 - 0-25% decrease, 3 - 25-50% decrease, 
#4 - 50-75% decrease, 5 75-100% decrease
Groups<-data.frame(max=c(-0.75,-0.50,-0.25,0,3),min=c(-1.00,-0.75,-0.50,-0.25,0),group=as.character(c("75-100% loss","50-75% loss","25-50% loss","0-25% loss","Stable or increase")))
Plots$Collapse_group<-"Empty"
Plots2<-NULL
for (i in 1:nrow(Groups)){
  Plot_sub<-subset(Plots,BAPERCM<Groups[i,1]&BAPERCM>=Groups[i,2])
  Plot_sub$Collapse_group<-as.character(Groups$group[i])
  Plots2<-rbind(Plot_sub,Plots2)
}

#change 1996 to 1999
Plots2$Year2<-NA
for (i in 1:nrow(Plots2)){
  Plots2$Year2[i]<-ifelse(Plots2$Year[i]==1996|Plots2$Year[i]==1999,"1996/9",Plots2$Year[i])
}

#reorder factor for collapse groups to plot from stable to most collapsed
Plots2$Collapse_group <- factor(Plots2$Collapse_group, c("Stable or increase","0-25% loss","25-50% loss","50-75% loss","75-100% loss"))

#add labels for text annotations
labs<-data.frame(x=min(Plots2$Easting)+10,y=max(Plots2$Northing),Year2=c("1964","1984","1988","1996/9","2014"),labs=c("(a)","(b)","(c)","(d)","(e)"))
  
#add scale bar
scale<-data.frame(x=max(Plots2$Easting)-150,y=max(Plots2$Northing)-250,Year2=c("1964"))


#plot location of plots grouped by collapse status
theme_set(theme_bw(base_size=12))
Collapse_map1<-ggplot(Plots2,aes(x=Easting,Northing,colour=as.factor(Collapse_group)))+geom_point(shape=15,size=1.5)+facet_wrap(~Year2)
Collapse_map2<-Collapse_map1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Collapse_map3<-Collapse_map2+scale_colour_grey("Collapse group",end=0.2,start=0.8)+
  theme(axis.text.x=element_blank(),
  axis.text.y=element_blank(),axis.ticks=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y=element_blank())
Collapse_map4<-Collapse_map3+coord_fixed(ratio = 1)+xlim(c(432930,433450))+ geom_text(data=labs,aes(x, y, label=labs, group=NULL),colour="black")
Collapse_map4+geom_segment(data=scale,aes(x = x, y = y, xend = x+200, yend = y),colour="black")+geom_text(data=scale,aes(x+90, y+50, label="200m", group=NULL),colour="black")
ggsave("Figures/Collapse_map_groups.png",width = 10,height=10,units = "in",dpi=300)

Collapse_map4+ 
  theme(strip.background = element_blank(), strip.text = element_blank())+geom_segment(data=scale,aes(x = x, y = y, xend = x+200, yend = y),colour="black")+geom_text(data=scale,aes(x+90, y+50, label="200m", group=NULL),colour="black")
ggsave("Figures/Collapse_map_nolab.png",width = 10,height=12,units = "in",dpi=300)

