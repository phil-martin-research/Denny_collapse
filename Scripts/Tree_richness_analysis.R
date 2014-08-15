#script to calculate changes in ctree species richness in Denny wood over time

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(vegan)
library(reshape2)
library(nlme)
library(MuMIn)
library(lme4)


#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Trees<-read.csv("Denny_trees_cleaned.csv")
head(Trees)

#subset trees to give only those inside plots
Trees<-subset(Trees,In_out=="In")

#calculate species richness per plot per time period
Sp_richness<-(with(Trees, tapply(Species,list(Block,Year), function(x) length(unique(na.omit(x))))))
Sp_rich_melt<-melt(Sp_richness)
colnames(Sp_rich_melt)<-c("Block","Year","Sp_R")
Sp_rich_melt$Transect<-ifelse(Sp_rich_melt$Block>=51,"Unenclosed","Enclosed")
Sp_Rich_melt_CC<-Sp_rich_melt[complete.cases(Sp_rich_melt),]

#plot of trends per plot, looks like there is little systematic change
ggplot(Sp_Rich_melt_CC,aes(x=Year,y=Sp_R,group=Block))+geom_line(alpha=0.1)+geom_point()+facet_wrap(~Block)

head(Sp_Rich_melt_CC)

#analyse this in a mixed model
#first add new varibale to reduce correlation of fixed effects
Sp_Rich_melt_CC$Year2<-Sp_Rich_melt_CC$Year-mean(Sp_Rich_melt_CC$Year)

#now run models
options(na.action = "na.fail")
M0<-glmer(Sp_R~1+(1|Block),data=Sp_Rich_melt_CC,family="poisson")
M0.1<-glmer(Sp_R~1+(Year|Block),data=Sp_Rich_melt_CC,family="poisson")
AICc(M0,M0.1)
#AICc of M0 is lower so we go with random intercepts but not random slopes
M1<-glmer(Sp_R~Year2+(1|Block),data=Sp_Rich_melt_CC,family="poisson")
plot(M1)

#model avaeraging
Models<-dredge(M1,rank = AICc,trace = T,REML=F)
Model_sel<-mod.sel(Models)
Model_sel$R_squared<-c(r.squaredGLMM(M1)[1],r.squaredGLMM(M0)[1])
Model_average<-model.avg(Models,fit=T)

#predicts change in species richness
Time<-data.frame(Year2=seq(1959,2014,1)-(mean(Sp_Rich_melt_CC$Year)))
R_pred<-predict(Model_average,newdata=Time,se.fit=T,backtransform=T,level=0)
R_pred$Time<-Time+(mean(Sp_Rich_melt_CC$Year))
R_pred<-data.frame(R_pred)

#plot changes in richness
theme_set(theme_bw(base_size=12))
R_plot1<-ggplot(Sp_Rich_melt_CC,aes(x=Year,y=Sp_R,group=Block))+geom_point(alpha=0.5,shape=1)+geom_line(alpha=0.2)
R_plot2<-R_plot1+geom_line(data=R_pred,size=3,aes(x=Year2,y=fit,group=NULL,colour=NULL),colour="blue")
R_plot3<-R_plot2+geom_line(data=R_pred,size=2,lty=2,aes(x=Year2,y=fit+(1.96*se.fit),group=NULL,colour=NULL),colour="blue")+geom_line(data=R_pred,size=2,lty=2,aes(x=Year2,y=fit-(1.96*se.fit),group=NULL,colour=NULL),colour="blue")
R_plot4<-R_plot3+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010,2020))+ theme(legend.position="none")
R_plot4+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+xlab("Year")+ylab("Species richness per plot")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Richness_change.png",width = 8,height=6,units = "in",dpi=300)

#alternative plot without lines linking plot changes
R_plot1<-ggplot(Sp_Rich_melt_CC,aes(x=Year,y=Sp_R,group=Block))+geom_jitter(alpha=0.5,shape=1)
R_plot2<-R_plot1+geom_line(data=R_pred,size=3,aes(x=Year2,y=fit,group=NULL,colour=NULL),colour="blue")
R_plot3<-R_plot2+geom_line(data=R_pred,size=2,lty=2,aes(x=Year2,y=fit+(1.96*se.fit),group=NULL,colour=NULL),colour="blue")+geom_line(data=R_pred,size=2,lty=2,aes(x=Year2,y=fit-(1.96*se.fit),group=NULL,colour=NULL),colour="blue")
R_plot4<-R_plot3+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010,2020))+ theme(legend.position="none")
R_plot4+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+xlab("Year")+ylab("Species richness per plot")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Richness_change2.png",width = 8,height=6,units = "in",dpi=300)

