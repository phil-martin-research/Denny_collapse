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
M0.1<-lme(LightM~1,random=~1|Block,data=BA,na.action=na.omit)
M0.2<-lme(LightM~1,random=~BAPERCM|Block,data=BA,na.action=na.omit)
AICc(M0.1,M0.2)

#random slopes is best
M1<-lme(LightM~BAPERCM,random=~BAPERCM|Block,data=BA,na.action=na.omit)
AICc(M0.2,M1)# the null model is better


theme_set(theme_bw(base_size=12))
ggplot(BA,aes(x=BAPERCM*100,y=LightM))+geom_point()+xlab("Percentage change in basal area since 1964")+facet_wrap(~Year2)+ylab("Community weighted mean light dependancy")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures/Forest_collapse")
ggsave("Light_BA_loss.png",width = 12,height = 8,units = "in",dpi = 300)

#then nitrogen
M0.1<-lme(NitM~1,random=~1|Block,data=BA,na.action=na.omit)
M0.2<-lme(NitM~1,random=~BAPERCM|Block,data=BA,na.action=na.omit)
AICc(M0.1,M0.2)

#random slopes is best
M1<-lme(NitM~BAPERCM,random=~BAPERCM|Block,data=BA,na.action=na.omit)
M2<-lme(NitM~BAPERCM+Year2,random=~BAPERCM|Block,data=BA,na.action=na.omit)
M3<-lme(NitM~BAPERCM*Year2,random=~BAPERCM|Block,data=BA,na.action=na.omit)
AICc(M0.2,M1,M2,M3)# the null model is better


theme_set(theme_bw(base_size=12))
ggplot(BA,aes(x=BAPERCM*100,y=NitM))+geom_point()+facet_wrap(~Year2)+xlab("Percentage change in basal area since 1964")+ylab("Community weighted mean nitrogen dependancy")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures/Forest_collapse")
ggsave("Nit_BA_loss.png",width = 12,height = 8,units = "in",dpi = 300)

#then moisture
theme_set(theme_bw(base_size=12))
ggplot(BA,aes(x=BAPERCM*100,y=MoistM))+geom_point()+facet_wrap(~Year2)+xlab("Percentage change in basal area since 1964")+ylab("Community weighted mean moisture dependancy")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures/Forest_collapse")
ggsave("Moist_BA_loss.png",width = 12,height = 8,units = "in",dpi = 300)

