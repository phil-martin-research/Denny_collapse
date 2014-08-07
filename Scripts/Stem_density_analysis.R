#script to look at changes in stem density in Denny Wood

#needs some tidying

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

#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Location<-read.csv("Plot_coords.csv")
Location<-unique(Location[,c(3,5:6)])
DBH<-read.csv("Denny_trees_cleaned.csv")
head(DBH)
head(Location)
#subset trees to give only those inside plots
DBH<-subset(DBH,In_out=="In")

#############################################
#analyses of stem density change#############
#############################################

#calculate stem density per plot per time period
Stem_density<-(with(DBH, tapply(Tree_ID,list(Block,Year), function(x) length(unique(na.omit(x))))))
SD_melt<-melt(Stem_density)
colnames(SD_melt)<-c("Block","Year","SD")
SD_melt$Transect<-ifelse(SD_melt$Block>=51,"Unenclosed","Enclosed")
SD_melt$Block<-as.factor(SD_melt$Block)
SD_melt_CC<-SD_melt[complete.cases(SD_melt),]

#plot time series of stem density change per plot for both transects
ggplot(SD_melt_CC,aes(x=Year,y=SD,group=Block,colour=Transect))+geom_line()+geom_point()+facet_wrap(~Block)




#models of stem density change
SD_melt_CC$Year2<-SD_melt_CC$Year-mean(SD_melt_CC$Year)
M0<-lme(SD~1,random=~1|Block,data=SD_melt_CC,method="REML")
M1<-lme(SD~Year2,random=~1|Block,data=SD_melt_CC,method="REML")

#perform model averaging for null and yearly change models
Models<-dredge(M1,rank = AICc,trace = T,REML=F)
Model_sel<-mod.sel(Models)
Model_sel$R_squared<-c(r.squaredGLMM(M1)[1],r.squaredGLMM(M0)[1])
#model of change per year is much better than null model

Time<-data.frame(Year2=seq(1959,2014,1)-(mean(SD_melt_CC$Year)))
SD_pred<-predict(M1,newdata=Time,se.fit=T,backtransform=T,level=0,)
SD_pred$Time<-Time+(mean(SD_melt_CC$Year))
SD_pred<-data.frame(SD_pred)

head(SD_pred)

#plot graph of change
theme_set(theme_bw(base_size=12))

SD_plot1<-ggplot(SD_melt_CC,aes(x=Year,y=SD,group=Block))+geom_point(alpha=0.5,shape=1)+geom_line(alpha=0.2)
SD_plot2<-SD_plot1+geom_line(data=SD_pred,size=3,aes(x=Year2,y=fit,group=NULL,colour=NULL),colour="blue")
SD_plot3<-SD_plot2+geom_line(data=SD_pred,size=2,lty=2,aes(x=Year2,y=fit+(1.96*se.fit),group=NULL,colour=NULL),colour="blue")+geom_line(data=SD_pred,size=2,lty=2,aes(x=Year2,y=fit-(1.96*se.fit),group=NULL,colour=NULL),colour="blue")
SD_plot4<-SD_plot3+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010,2020))+ theme(legend.position="none")
SD_plot4+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+xlab("Year")+ylab("Stem density per hectare")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("SD_change.png",width = 8,height=6,units = "in",dpi=300)
