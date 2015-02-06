#script to look at whether collapse is driven by neighbouring plots
rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(vegan)
library(reshape2)
library(lme4)
library(nlme)
library(MuMIn)
library(quantreg)
library(car)
library(arm)

#save all this as a csv
Plots<-read.csv("Data/BA_gradient_spatial.csv")

head(Plots2)

Plots$Transect<-ifelse(Plots$Block>=51,"Undenclosed","Enclosed")
Plots$Collapsed_adj3<-ifelse(Plots$Collapsed_adj==1,"Collapsed","Stable")
Plots$Collapsed_adj4<-ifelse(Plots$Collapsed_adj2==1,"Collapsed","Stable")
for (i in 1:nrow(Plots)){
  Plots$Year[i]<-ifelse(Plots$Year[i]==1996,1999,Plots$Year[i])
}


ggplot(Plots,aes(x=as.factor(Collapsed_adj),y=Collapsed))+geom_boxplot()+facet_grid(Year~Transect)



Plots2<-subset(Plots,Transect=="Enclosed")
Plots2<-subset(Plots2,Year>1984)
Plots2<-subset(Plots2, !is.na(Collapsed_adj))




ggplot(Plots2,aes(x=as.factor(Collapsed_adj),y=Collapsed))+geom_boxplot()+facet_wrap(~Year)
ggplot(Plots2,aes(x=as.factor(Collapsed_adj2),y=Collapsed))+geom_boxplot()+facet_wrap(~Year)+xlab("Status of adjacent plots")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+scale_y_continuous(breaks=1:0)
ggsave("Figures/Adjacent_collapse.png",width = 8,height=6,units = "in",dpi=300)

#produce a different way of plotting this data


Plots3<-ddply(Plots2,.(Collapsed,Collapsed_adj3,Year),summarise,sum=sum(Collapsed),mean=mean(Collapsed),median=median(Collapsed),freq=length(Collapsed))
Plots4<-ddply(Plots2,.(Collapsed_adj3,Year),summarise,sum=sum(Collapsed),mean=mean(Collapsed),median=median(Collapsed))


Adj_plot1<-ggplot(Plots3,aes(x=Collapsed_adj3,y=Collapsed))+geom_point(aes(size=freq),shape=15)+facet_wrap(~Year)
Adj_plot1+geom_point(data=Plots4,aes(y=mean),shape=15,size=4,colour="red")+xlab("Status of adjacent plots")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+scale_y_continuous(breaks=1:0)+ theme(legend.position="none")
ggsave("Figures/Adjacent_collapse2.png",width = 8,height=6,units = "in",dpi=300)


#and now are plots that are collapsed more likley to be next to other plots that have collapsed
Plots3<-ddply(Plots,.(Collapsed,Collapsed_adj4,Year),summarise,sum=sum(Collapsed),mean=mean(Collapsed),median=median(Collapsed),freq=length(Collapsed))
Plots4<-ddply(Plots,.(Collapsed_adj4,Year),summarise,sum=sum(Collapsed),mean=mean(Collapsed),median=median(Collapsed))


Adj_plot1<-ggplot(Plots3,aes(x=Collapsed_adj4,y=Collapsed))+geom_point(aes(size=freq),shape=15)+facet_wrap(~Year)
Adj_plot1+geom_point(data=Plots4,aes(y=mean),shape=15,size=4,colour="red")+xlab("Status of adjacent plots")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+scale_y_continuous(breaks=1:0)+ theme(legend.position="none")
ggsave("Figures/Adjacent_collapse3.png",width = 8,height=6,units = "in",dpi=300)

#create model to look at whether plots that have not collapsed are more likely 
#to have collapsed by the next time point when they are next to a collapsed plot

#first a null model
M0.1<-glmer(Collapsed~1+(1|Block),data=Plots2,family="binomial")
M0.2<-glmer(Collapsed~1+(Block|Year),data=Plots2,family="binomial")
AICc(M0.1,M0.2)

#use first set of random effects
M1<-glmer(Collapsed~Collapsed_adj3+(1|Block),data=Plots2,family="binomial")
M2<-glmer(Collapsed~Collapsed_adj3+as.factor(Year)+(1|Block),data=Plots2,family="binomial")
M3<-glmer(Collapsed~Collapsed_adj3*as.factor(Year)+(1|Block),data=Plots2,family="binomial")
AICc(M0.1,M1,M2,M3)

#the null model is best


#create model to look at whether collapsed plots are more likley to be found next 
#to other collapsed plots
M0.1<-glmer(Collapsed~1+(1|Block),data=Plots,family="binomial")
M0.2<-glmer(Collapsed~1+(Block|Year),data=Plots,family="binomial")
AICc(M0.1,M0.2)

M1<-glmer(Collapsed~Collapsed_adj4+(1|Block),data=Plots,family="binomial")
M2<-glmer(Collapsed~Collapsed_adj4+as.factor(Year)+(1|Block),data=Plots,family="binomial")
M3<-glmer(Collapsed~Collapsed_adj4*as.factor(Year)+(1|Block),data=Plots,family="binomial")
AICc(M0.1,M1,M2,M3)

r.squaredGLMM(M2)

summary(M2)

