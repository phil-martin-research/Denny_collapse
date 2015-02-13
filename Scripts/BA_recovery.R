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
M6<-lmer(BAPERCM2~Coll_Sever*Time_Coll+I(Coll_Sever^2)+(1|Block),data=Plots3)


AICc(M1,M2,M3,M4,M5,M6)

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
Recovery_plot<-ggplot(Plots3,aes(x=Coll_Sever*100,y=BAPERCM*100,group=Block))+geom_point()+geom_line()+geom_hline(y=0,lty=2)
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



#table for adrian
#create a table in which only plots that have declined are included and 
#then subset these into plots that showed recovery and those that didn't
#then group them by the amount of decline they showed prior to increase
#then calculate the percentage recovery for each of these
Blocks<-unique(Plots3$Block)
Recovery_table<-NULL
for (i in 1:length(Blocks)){
  Block_subset<-subset(Plots3,Block==Blocks[i])
  Block_subset$Recov<-Block_subset$BAPERCM-Block_subset$BAPERCM[1]
  Block_subset$Decline<-min(Block_subset$BAPERCM)
  Recovery_table<-rbind(Recovery_table,Block_subset)
}

Recovery_table2<-subset(Recovery_table,Time_Coll>0)

for (i in 1:nrow(Recovery_table2)){
  Recovery_table2$Recovered[i]<-ifelse(Recovery_table2$Recov[i]>0,1,0)
}


Recovery_table_2014<-subset(Recovery_table2,Year==2014)


Recovery_table_2014$Decline

#put into different groups
Reclass<-data.frame(from=c(0,-0.25,-0.5,-0.75),to=c(-0.25,-0.5,-0.75,-1),gives=c("0-25%","25-50%","50-75%","75-100%"))
Reclass_recovery<-NULL
Recovery_table_2014$Group<-NA
for (i in 1:nrow(Reclass)){
  Reclass_sub<-subset(Recovery_table_2014,Decline<=Reclass$from[i])
  Reclass_sub<-subset(Reclass_sub,Decline>=Reclass$to[i])
  Reclass_sub$Group<-Reclass$gives[i]
  Reclass_recovery<-rbind(Reclass_sub,Reclass_recovery)
}
Reclass_recovery$Count<-1


Recovery_group<-ddply(Reclass_recovery, .(Group, Recovered), summarize,Mean_BA = round(mean(BAPERCM*100), 2),SD_BA=sd(BAPERCM*100),No_group = sum(Count))
Recovery_group$BA_SE<-Recovery_group$SD_BA/sqrt(Recovery_group$No_group)
write.csv(Recovery_group,"Figures/Recovery_group.csv")

