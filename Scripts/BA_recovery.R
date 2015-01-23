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
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("BA_Gradient.csv")

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


head(Plots3)

ggplot(Plots3,aes(x=Coll_Sever,y=BAPERCM,group=Block))+geom_point()+geom_line()+facet_wrap(~Block)


#now subset to give plots post decline
Plots3<-subset(Plots2,Collapse3==1)
Plots3<-subset(Plots2,Time_Coll>0)

#model of recovery as a function of collapse
M0.1<-lmer(BAPERCM~(1|Block),data=Plots3)
M0.2<-lmer(BAPERCM~(0+Block|Time_Coll),data=Plots3)
M0.3<-lmer(BAPERCM~(1|Block)+(0+Block|Time_Coll),data=Plots3)
M0.3<-lmer(BAPERCM~(1|Block)+(0+Block|Coll_Sever),data=Plots3)

AICc(M0.1,M0.2)

#go with M0.1 random effects
M1<-lmer(BAPERCM~Time_Coll*Coll_Sever+(1|Block),data=Plots3)
M2<-lmer(BAPERCM~Time_Coll+Coll_Sever+(1|Block),data=Plots3)
M3<-lmer(BAPERCM~Time_Coll+(1|Block),data=Plots3)
M4<-lmer(BAPERCM~Coll_Sever+(1|Block),data=Plots3)

AICc(M1,M2,M3,M4)

plot(Plots3$Coll_Sever,Plots3$BAPERCM)
points(Plots3$Coll_Sever,predict(M2,re.form = NA),col="red")
abline(a=0,b=1)
plot(Plots3$Time_Coll,Plots3$BAPERCM)
points(Plots3$Time_Coll,predict(M2,re.form = NA),col="red")
