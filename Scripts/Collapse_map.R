#script to show location of collapsed plots over time

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)

#load data
Plots<-read.csv("Data/Denny_plots.csv")

head(Plots)

#classify plots by collapse status - collapsed (1) or not (0)
Plots$Collapse<-NA
for (i in 1:nrow(Plots)){
  Plots$Collapse[i]<-ifelse(Plots$BAPERCM[i]<=-0.25,1,0)
}

#classify plots by the degree to which they have collapsed
#1- stable or increase, 2 - 0-25% decrease, 3 - 25-50% decrease, 
#4 - 50-75% decrease, 5 75-100% decrease
Groups<-data.frame(max=c(-0.75,-0.50,-0.25,0,3),min=c(-1.00,-0.75,-0.50,-0.25,0),group=as.character(c("Stable or increase","0-25% loss","25-50% loss","50-75% loss","75-100% loss")))
Plots$Collapse_group<-"Empty"
for (i in 1:nrow(Groups)){
  for (y in 1:nrow(Plots)){
    Plots$Collapse_group[y]<-as.character(ifelse(Plots$Year[y]==1964,"Stable or increase",Plots$Collapse_group[y]))
    Plots$Collapse_group[y]<-as.character(ifelse(Plots$BAPERCM[y]<Groups[i,1]&Plots$BAPERCM[y]>Groups[i,2],as.character(Groups$group[i]),Plots$Collapse_group[y]))
  }
}

plot(Plots$Collapse_group,Plots$BAPERCM)

#change 1996 to 1999
for (i in 1:nrow(Plots)){
  Plots$Year[i]<-ifelse(Plots$Year[i]==1996,1999,Plots$Year[i])
}

#change 1996 to 1999
for (i in 1:nrow(Plots)){
  Plots$Year[i]<-ifelse(Plots$Year[i]==1999,"1996/9",Plots$Year[i])
}

#reorder factor for collapse groups to plot from stable to most collapsed
Plots$Collapse_group <- factor(Plots$Collapse_group, c("Stable or increase","0-25% loss","25-50% loss","50-75% loss","75-100% loss"))

#add labels for text annotations
labels<-data.frame(x=min(Plots$Easting)+10,y=max(Plots$Northing),Year=c("1964","1984","1988","1996/9","2014"),labs=c("(a)","(b)","(c)","(d)","(e)"))
  
#add scale bar
scale<-data.frame(x=max(Plots$Easting)-150,y=max(Plots$Northing)-250,Year=c("1964"))



#plot location of plots that have collapsed
theme_set(theme_bw(base_size=12))
Collapse_map1<-ggplot(Plots,aes(x=Easting,Northing,colour=as.factor(Collapse)))+geom_point(shape=15,size=1)+facet_wrap(~Year)
Collapse_map2<-Collapse_map1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Collapse_map2+scale_colour_grey("Collapse status")+ theme(axis.text.x=element_blank(),
       axis.text.y=element_blank(),axis.ticks=element_blank(),
       axis.title.x=element_blank(),
       axis.title.y=element_blank())

setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Collapse_map.png",width = 8,height=6,units = "in",dpi=300)

#plot location of plots grouped by collapse status
theme_set(theme_bw(base_size=12))
Collapse_map1<-ggplot(Plots,aes(x=Easting,Northing,colour=as.factor(Collapse_group)))+geom_point(shape=15,size=1.5)+facet_wrap(~Year)
Collapse_map2<-Collapse_map1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Collapse_map3<-Collapse_map2+scale_colour_grey("Collapse group",start=0.8,end=0.2)+
  theme(axis.text.x=element_blank(),
  axis.text.y=element_blank(),axis.ticks=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y=element_blank())
Collapse_map4<-Collapse_map3+coord_fixed(ratio = 1)+xlim(c(432930,433450))+ geom_text(data=labels,aes(x, y, label=labs, group=NULL),colour="black")
Collapse_map4+geom_segment(data=scale,aes(x = x, y = y, xend = x+200, yend = y),colour="black")+geom_text(data=scale,aes(x+90, y+50, label="200m", group=NULL),colour="black")
ggsave("Figures/Collapse_map_groups.png",width = 8,height=6,units = "in",dpi=300)
Collapse_map3+coord_fixed(ratio = 1)+xlim(c(432930,433450))+ geom_text(data=labels,aes(x, y, label=labs, group=NULL),colour="black")+ 
  theme(strip.background = element_blank(), strip.text = element_blank())+geom_segment(data=scale,aes(x = x, y = y, xend = x+200, yend = y),colour="black")+geom_text(data=scale,aes(x+90, y+50, label="200m", group=NULL),colour="black")
ggsave("Figures/Collapse_map_nolab.png",width = 8,height=6,units = "in",dpi=300)

#plot location of plots using continous variable
theme_set(theme_bw(base_size=12))
Collapse_map1<-ggplot(Plots,aes(x=Easting,Northing,colour=BAPERCM))+geom_point(shape=15,size=1)+facet_wrap(~Year)
Collapse_map2<-Collapse_map1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Collapse_map3<-Collapse_map2+scale_colour_gradient2(low="red",mid="grey",high="blue","Percentage change in Basal area")+ theme(axis.text.x=element_blank(),
                                                                                    axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                    axis.title.x=element_blank(),
                                                                                    axis.title.y=element_blank())
Collapse_map3+coord_fixed(ratio = 1)
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Collapse_map_continuous.png",width = 8,height=6,units = "in",dpi=300)
