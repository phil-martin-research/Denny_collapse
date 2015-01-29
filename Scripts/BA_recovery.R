#script to do analysis to see whether any plots recovered following loss

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)
library(lme4)
library(MuMIn)

#load in data
BA<-read.csv("Data/BA_Gradient.csv")

#classify plots by collapse status - declined, or not
BA$Collapse<-NA
for (i in 1:nrow(BA)){
  BA$Collapse[i]<-ifelse(BA$BAPERCM[i]<0,1,0)
}


#classify plots to identify those that have *at some point* declined in BA
BA$Collapse2<-NA
Plots2<-NULL
Block_unique<-unique(BA$Block)
for (i in 1:length(Block_unique)){
  Block_sub<-subset(BA,Block==Block_unique[i])
  Block_sub$Collapse2<-NA
  Block_sub$Collapse2[1]<-0
  for (y in 2:nrow(Block_sub)){  
    if (Block_sub$Collapse2[y-1]==0&Block_sub$Collapse[y]==0) {
    Block_sub$Collapse2[y]<-0
    }else{Block_sub$Collapse2[y]<-1    
    }
  }
  Block_sub2<-subset(Block_sub,Collapse2==1)
  if(nrow(Block_sub2)>0){
  Block_sub2$Time_Coll<-Block_sub2$Year-Block_sub2$Year[1]
  Block_sub2$Coll_Sever<-Block_sub2$BAPERCM[1]
  Plots2<-rbind(Block_sub2,Plots2)
  }
}


head(Plots2)

ggplot(Plots2,aes(x=Coll_Sever,y=BAPERCM,group=Block))+geom_point()


#now subset to give plots post decline
Plots3<-subset(Plots2,Collapse2==1)
Plots3<-subset(Plots2,Time_Coll>0)

#create new variable for percentage change so that it is bound above -100%
summary(Plots3$BAPERCM)

Plots3$BAPERCM2<-Plots3$BAPERCM+1
Plots3$BAPERCM2<-ifelse(Plots3$BAPERCM2==0,0.0001,Plots3$BAPERCM2)
Plots3$BAPERCM2<-qlogis(Plots3$BAPERCM2/2)

#model of recovery as a function of collapse
M0.1<-lmer(BAPERCM2~(1|Block),data=Plots3)
M0.2<-lmer(BAPERCM2~(0+Block|Time_Coll),data=Plots3)
M0.3<-lmer(BAPERCM2~(1|Block)+(0+Block|Time_Coll),data=Plots3)
M0.4<-lmer(BAPERCM2~(1|Block)+(0+Block|Coll_Sever),data=Plots3)

AICc(M0.1,M0.2,M0.3,M0.4)

#go with M0.1 random effects
M1<-lmer(BAPERCM2~Time_Coll*Coll_Sever+(1|Block),data=Plots3)
M2<-lmer(BAPERCM2~Time_Coll+Coll_Sever+(1|Block),data=Plots3)
M3<-lmer(BAPERCM2~Time_Coll+(1|Block),data=Plots3)
M4<-lmer(BAPERCM2~Coll_Sever+(1|Block),data=Plots3)
M5<-lmer(BAPERCM2~Coll_Sever+I(Coll_Sever^2)+(1|Block),data=Plots3)


AICc(M1,M2,M3,M4,M5)

plot(M5)
qqnorm(resid(M5))
qqline(resid(M5))

r.squaredGLMM(M5)
r.squaredGLMM(M3)

#put predictions for M4 into dataframe
Plots3$PredsR<-predict(M5)
Plots3$Preds<-((plogis(predict(M5,re.form=NA)))*2)-1

#create predictions for M5 in new dataframe
Predictions<-data.frame(Coll_Sever=seq(min(Plots3$Coll_Sever),max(Plots3$Coll_Sever),by=0.001))
Predictions$Preds<-((plogis(predict(M5,re.form=NA,newdata=Predictions)))*2)-1

#plot change in BA relative to intial change in BA
theme_set(theme_bw(base_size=12))
Recovery_plot<-ggplot(Plots3,aes(x=Coll_Sever*100,y=BAPERCM*100,colour=as.factor(Year)))+geom_point()+geom_hline(y=0,lty=2)
Recovery_plot2<-Recovery_plot+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Recovery_plot3<-Recovery_plot2+scale_colour_discrete("Year")+ylab("Basal area percentage change relative to 1964")+xlab("Initial percentage change in basal area relative to 1964")
Recovery_plot4<-Recovery_plot3+geom_line(data=Predictions,size=2,aes(x=Coll_Sever*100,y=Preds*100,colour=NULL))+ theme(legend.position="none")

#plot dynamics of change in BA - time since collapse
New_coll<-data.frame(Coll_Sever=mean(Plots3$Coll_Sever))
predict(M4,re.form=NA,newdata=New_coll)
Dynamics_plot<-ggplot(Plots3,aes(x=Time_Coll,y=BAPERCM*100,group=Block))+geom_point(alpha=0.5)+geom_line(alpha=0.5)+geom_hline(y=0,lty=2)
Dynamics_plot2<-Dynamics_plot+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Dynamics_plot3<-Dynamics_plot2+scale_colour_discrete("Year")+ylab("Basal area percentage change relative to 1964")+xlab("Years since initial collapse")+geom_line(y=-36.7,size=2)+xlim(c(0,30))

png(filename="Figures/Recovery.png",height=6,width=12,units="in",res=300)
grid.arrange(Recovery_plot4,Dynamics_plot3, ncol=2)
dev.off()
