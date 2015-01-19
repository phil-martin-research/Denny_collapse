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

#classify plots by collapse status - collapsed (1) or not (0)
Plots$Collapse<-NA
for (i in 1:nrow(Plots)){
  Plots$Collapse[i]<-ifelse(Plots$BAPERCM[i]<=-0.25,1,0)
}

#classify plots by the degree to which they have collapsed
#1- stable or increase, 2 - 0-25% decrease, 3 - 25-50% decrease, 
#4 - 50-75% decrease, 5 75-100% decrease
Groups<-data.frame(max=c(-0.75,-0.50,-0.25,0,3),min=c(-1.00,-0.75,-0.50,-0.25,0),group=c(5,4,3,2,1))
Plots$Collapse_group<-NA
for (i in 1:nrow(Groups)){
  for (y in 1:nrow(Plots)){
  Plots$Collapse_group[y]<-ifelse(Plots$BAPERCM[y]<Groups[i,1]&Plots$BAPERCM[y]>Groups[i,2],Groups[i,3],Plots$Collapse_group[y])
  Plots$Collapse_group[y]<-ifelse(Plots$Year[y]==1964,1,Plots$Collapse_group[y])
  }
}

plot(Plots$Collapse_group,Plots$BAPERCM)

#change 1996 to 1999
for (i in 1:nrow(Plots)){
  Plots$Year[i]<-ifelse(Plots$Year[i]==1996,1999,Plots$Year[i])
}


#set all plots in 1964 as uncollapsed (1)



#plot location of plots that have collapsed
theme_set(theme_bw(base_size=12))
Collapse_map1<-ggplot(Plots,aes(x=Easting,Northing,colour=as.factor(Collapse)))+geom_point(shape=15,size=1)+facet_wrap(~Year)
Collapse_map2<-Collapse_map1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Collapse_map2+scale_colour_manual(values=c("dark grey","light red","red", "dark red","very dark red"),"Collapse status")+ theme(axis.text.x=element_blank(),
       axis.text.y=element_blank(),axis.ticks=element_blank(),
       axis.title.x=element_blank(),
       axis.title.y=element_blank())

setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Collapse_map.png",width = 8,height=6,units = "in",dpi=300)

#plot location of plots grouped by collapse status
theme_set(theme_bw(base_size=12))
Collapse_map1<-ggplot(Plots,aes(x=Easting,Northing,colour=as.factor(Collapse_group)))+geom_point(shape=15,size=1.5)+facet_wrap(~Year)
Collapse_map2<-Collapse_map1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Collapse_map3<-Collapse_map2+scale_colour_manual(values=c("grey","orange","orangered2","red","red3"),"Collapse group")+
  theme(axis.text.x=element_blank(),
  axis.text.y=element_blank(),axis.ticks=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y=element_blank())
Collapse_map3+coord_fixed(ratio = 1)
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Collapse_map_groups.png",width = 8,height=6,units = "in",dpi=300)

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
