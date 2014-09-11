#create plots of tree species in 2014

#open packages neeeded for analysis
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(nlme)
library(MuMIn)
library(gridExtra)
library(MASS)


#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
DBH<-read.csv("Denny_trees_cleaned.csv")
#subset trees to give only those inside plots that are alive
DBH<-subset(DBH,In_out=="In")
DBH<-subset(DBH,Status==1)
DBH<-subset(DBH,Year==1964|Year==2014)
DBH$Species<-as.factor(DBH$Species)
DBH$Year<-ifelse(DBH$Year==1996,1999,DBH$Year)
DBH<-subset(DBH,Species!="Fr")
DBH<-subset(DBH,Species!="PM")
DBH<-subset(DBH,Species!="Sc")
DBH<-subset(DBH,Species!="Sc")


theme_set(theme_bw(base_size=30))
Transects<-ggplot(DBH,aes(x=Easting,y=Northing,colour=Species,size=DBH))+geom_point(alpha=0.8,shape=1)+scale_colour_brewer(palette="Greens")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+theme(axis.ticks=element_blank(),axis.text.y=element_blank(),axis.text.x=element_blank(),axis.title=element_blank(),legend.position="none")+facet_wrap(~Year)
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures/Forest_collapse")
ggsave("2014_block.png",width = 8,height = 8,units = "in",dpi = 300)
