#script to show time series of collapsed vs non-collapsed plots

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(lme4)
library(MuMIn)

#load data
Plots<-read.csv("Data/Denny_plots.csv")

head(Plots)

#add column for tanner index
Plots$Tanner<-(Plots$Sor_BA+Plots$SorM)/2

#classify plots by collapse status - collapsed (1) or not (0)
Plots$Collapse<-NA
for (i in 1:nrow(Plots)){
  Plots$Collapse[i]<-ifelse(Plots$BAPERCM[i]<=-0.25,1,0)
}
#classify plots to identify those that have *at some point* been classed as collapsed
Plots$Collapse2<-NA
Plots2<-NULL
Block_unique<-unique(Plots$Block)
for (i in 1:length(Block_unique)){
  Block_sub<-subset(Plots,Block==Block_unique[i])
  Block_sub$Collapse2<-ifelse(sum(Block_sub$Collapse)>0,1,0)
  Plots2<-rbind(Block_sub,Plots2)
}

keeps<-c("Block","Year","Tanner","BAPERCM","Collapse2","BAM")
Plots3<-Plots2[keeps]
Plots4<-rbind(data.frame(Block=c(7,7),Year=c(1996,2014),Tanner=c(0,0),BAPERCM=c(-1,-1),Collapse2=c(1,1),BAM=c(0,0)),Plots3)

#now do analysis of this
#create variable for BAPERCM that is bounded above -1
Plots4$BAPERCM2<-Plots4$BAPERCM*(-1)
summary(Plots4$BAPERCM2)
Plots4$BAPERCM3<-qlogis((Plots4$BAPERCM2+2.2)/3.3)

#create a variable of number of years since 1964
Plots4$Year2<-Plots4$Year-1964

#now a null model
M0.1<-lmer(BAM~1+(1|Block),data=Plots4)
M0.2<-lmer(BAM~1+(Block|Year),data=Plots4)
AICc(M0.1,M0.2)
#use first random effect specification
M1<-lmer(BAM~Year2+(Block|Year2),data=Plots4)
M2<-lmer(BAM~Collapse2+(Block|Year2),data=Plots4)
M3<-lmer(BAM~Year2+Collapse2+(Block|Year2),data=Plots4)
M4<-lmer(BAM~Year2*Collapse2+(Block|Year2),data=Plots4)
AICc(M1,M2,M3,M4,M0.2)
plot(M4)

r.squaredGLMM(M4)

#put in predictions
Plots4$Preds<-predict(M4,re.form=NA)
ddply(Plots4,.(Year,Collapse2),summarize,BA=mean(Preds))

(47.48-22.43)/47.48

(46.30403/39.33224)-1


#put in different marker of collapse
Plots4$Collapse3<-ifelse(Plots4$Collapse2==1,"Collapsed","Stable")


#plot basal area time series
theme_set(theme_bw(base_size=12))
BA_Coll1<-ggplot(Plots4,aes(x=Year,y=BAM,group=Block))+geom_point(shape=1,size=2,alpha=0.3)+geom_line(alpha=0.3)+facet_wrap(~Collapse3)
BA_Coll2<-BA_Coll1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
BA_Coll2+geom_line(data=Plots4,aes(x=Year,y=Preds,group=NULL),size=2)+ylab("Basal area")+scale_x_continuous(breaks=c(1965,1990,2015))
ggsave("Figures/Collapse_BA_TS2.png",width = 8,height=6,units = "in",dpi=300)

nrow(Plots4)

