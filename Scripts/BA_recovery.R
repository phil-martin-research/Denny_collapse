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

ggplot(Plots2,aes(x=Time_Coll,y=BAPERCM,group=Block))+geom_point()+facet_wrap(~Block)+geom_smooth(method="lm",se=F)

#get a figure of the percentage change per year
#following BA decline

Blocks<-unique(Plots2$Block)
Changes<-NULL
for (i in 1:length(Blocks)){
  Plot_Sub<-subset(Plots2,Block==Blocks[i])
  Change<-(tail(Plot_Sub$BAPERCM,1)-Plot_Sub$BAPERCM[1])/(tail(Plot_Sub$Year,1)-Plot_Sub$Year[1])
  Change_sub<-data.frame(Block=Blocks[i],Coll_sever=Plot_Sub$Coll_Sever[1],Change=Change)
  Changes<-rbind(Changes,Change_sub)
}


ggplot(Changes,aes(x=Coll_sever,y=Change))+geom_point()

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
summary(M1)

AICc(M1,M0.1)

r.squaredGLMM(M1)

coefs <- data.frame(coef(summary(M1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

#create predictions for M1 in new dataframe
Pred1<-ddply(Plots3,.(Block),summarize,Coll_Sever2=seq(min(Coll_Sever),max(Coll_Sever),0.01))
Pred2<-ddply(Plots3,.(Block),summarise,Time_Coll=seq(min(Time_Coll),max(Time_Coll),1))
Predictions<-merge(Pred1,Pred2,by="Block")

newdat<-expand.grid(Coll_Sever=c(-0.75,-0.5,-0.25,-0.1),
                         Time_Coll=seq(min(Plots3$Time_Coll),max(Plots3$Time_Coll),0.1),BAPERCM2=0)

mm <- model.matrix(terms(M1),newdat)
newdat$BAPERCM2 <- predict(M1,newdat,re.form=NA)
## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(M1),mm))
tvar1 <- pvar1+VarCorr(M1)$Block[1]  ## must be adapted for more complex models 
  newdat <- data.frame(
    newdat
    , plo = newdat$BAPERCM2-2*sqrt(pvar1)
    , phi = newdat$BAPERCM2+2*sqrt(pvar1)
    , tlo = newdat$BAPERCM2-2*sqrt(tvar1)
    , thi = newdat$BAPERCM2+2*sqrt(tvar1)
  )

newdat$Preds<-((plogis(newdat$BAPERCM2))*2)-1
newdat$UCI<-((plogis(newdat$phi))*2)-1
newdat$LCI<-((plogis(newdat$plo))*2)-1

ggplot(Predictions,aes(x=Coll_Sever,y=Time_Coll,fill=Preds))+geom_raster()+scale_fill_gradient(low="black",high="light grey")

#plot dynamics of change in BA - time since collapse
theme_set(theme_bw(base_size=12))
Dynamics_plot<-ggplot(Plots3,aes(x=Time_Coll,y=BAPERCM*100,group=Block))+geom_point(shape=1)+geom_hline(y=0,lty=2)
Dynamics_plot2<-Dynamics_plot+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Dynamics_plot3<-Dynamics_plot2+scale_colour_discrete("% intitial decline\n in basal area")+ylab("Basal area percentage change relative to 1964")+xlab("Years since initial collapse")+geom_line(data=newdat,aes(y=Preds*100,group=as.factor(Coll_Sever),colour=as.factor(Coll_Sever*100)),size=1)+xlim(c(0,30))
Dynamics_plot3+geom_ribbon(data=newdat)
ggsave("Figures/BA_recovery.png",height=4,width=6,units="in",dpi=1200)
