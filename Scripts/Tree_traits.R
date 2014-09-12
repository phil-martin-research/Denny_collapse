#script to look at changes in species traits

rm(list=ls(all=TRUE))

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
library(survival)
library(GGally)
library(lme4)
library(fields)
library(ROCR)

#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("Denny_plots.csv")


#####################################
#exploration of trait change#########
#####################################
str(BA)

#first light requirements
theme_set(theme_bw(base_size=12))
ggplot(BA,aes(x=BAPERCM*100,y=LightM))+geom_point()+facet_wrap(~Year2)+xlab("Percentage change in basal area since 1964")+ylab("Community weighted mean \nlight dependancy")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures/Forest_collapse")
ggsave("Light_BA_loss.png",width = 12,height = 8,units = "in",dpi = 300)

#then nitrogen
theme_set(theme_bw(base_size=12))
ggplot(BA,aes(x=BAPERCM*100,y=NitM))+geom_point()+facet_wrap(~Year2)+xlab("Percentage change in basal area since 1964")+ylab("Community weighted mean \nnitrogen dependancy")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures/Forest_collapse")
ggsave("Nit_BA_loss.png",width = 12,height = 8,units = "in",dpi = 300)

#then nitrogen
theme_set(theme_bw(base_size=12))
ggplot(BA,aes(x=BAPERCM*100,y=MoistM))+geom_point()+facet_wrap(~Year2)+xlab("Percentage change in basal area since 1964")+ylab("Community weighted mean \nmoisture dependancy")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures/Forest_collapse")
ggsave("Moist_BA_loss.png",width = 12,height = 8,units = "in",dpi = 300)
