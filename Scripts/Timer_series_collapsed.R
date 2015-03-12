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
M1<-lmer(BAM~Year2+(Year2|Block),data=Plots4)
M2<-lmer(BAM~Collapse2+(Year2|Block),data=Plots4)
M3<-lmer(BAM~Year2+Collapse2+(Year2|Block),data=Plots4)
M4<-lmer(BAM~Year2*Collapse2+(Year2|Block),data=Plots4)
M5<-lmer(BAM~Year2*Collapse2+I(Year2^2)+(Year2|Block),data=Plots4)
M6<-lmer(BAM~Year2*Collapse2+I(Year2^2)*Collapse2+(Year2|Block),data=Plots4)

Models<-list(M1,M2,M3,M4,M5,M6)

#produce tables to give summary statistics
Model_table<-mod.sel(Models)
Model_table$R_sq<-c(r.squaredGLMM(M4)[1],r.squaredGLMM(M2)[1],r.squaredGLMM(M3)[1],r.squaredGLMM(M1)[1])

write.csv(Model_table,"Figures/BA_TS_table.csv",row.names=F)

coefs <- data.frame(coef(summary(M4)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

write.csv(coefs,"Figures/BA_TS_coefs.csv")

plot(M4)

head(Plots4)

#put in predictions
Plots4$Preds<-predict(M4,re.form=NA)
newdat<-expand.grid(Year=seq(1964,2014,0.1),Collapse2=c(0,1))
newdat$Year2<-newdat$Year-1964                          
newdat$BAM<-0
mm <- model.matrix(terms(M4),newdat)
newdat$BAM <- predict(M4,newdat,re.form=NA)
## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(M4),mm))
tvar1 <- pvar1+VarCorr(M4)$Block[1]  ## must be adapted for more complex models
  newdat <- data.frame(
    newdat
    , plo = newdat$BAM-2*sqrt(pvar1)
    , phi = newdat$BAM+2*sqrt(pvar1)
    , tlo = newdat$BAM-2*sqrt(tvar1)
    , thi = newdat$BAM+2*sqrt(tvar1)
  )


newdat$Collapse3<-ifelse(newdat$Collapse2==1,"Collapsed","Stable")

ddply(newdat,.(Year,Collapse2),summarize,BA=mean(BAM),plo=mean(plo),phi=mean(phi))

(47.48-22.43)/47.48

((47.48-17.54)/47.48)
((47.48-27.32)/47.48)


(46.30403/39.33224)-1
(40.65400/39.33224)-1
(51.95405/39.33224)-1

(47.48032-42.44737)/1.96
(22.43374-17.54483)/1.96

(39.57-33.57)/1.96
(40.65400-46.30403)/1.96
labels<-data.frame(x=min(Plots4$Year)+1,y=max(Plots$BAM)-1,Collapse3=c("Collapsed","Stable"),labs=c("(a)","(b)"))

head(Plots4)


#put in different marker of collapse
Plots4$Collapse3<-ifelse(Plots4$Collapse2==1,"Collapsed","Stable")

Plots4$Transect<-ifelse(Plots4$Block>50,"Open","Fenced")


#plot basal area time series
theme_set(theme_bw(base_size=12))
BA_Coll1<-ggplot(data=Plots4,size=2,alpha=0.3,aes(group=Block,shape=Transect,x=Year,y=BAM))+geom_point(alpha=0.3)+geom_line(alpha=0.3)+facet_wrap(~Collapse3)
BA_Coll2<-BA_Coll1+geom_ribbon(data=newdat,aes(ymax=phi,ymin=plo,group=NULL,shape=NULL),alpha=0.4)+geom_line(data=newdat,aes(group=NULL,shape=NULL),size=2)
BA_Coll3<-BA_Coll2+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
BA_Coll4<-BA_Coll3+ylab(expression(paste("Basal area (", m^bold("2"),ha^bold("-1"),")")))
BA_Coll4+theme(strip.background = element_blank(),strip.text.x = element_blank())+
  geom_text(data=labels,aes(shape=NULL,x=x,y=y,label=labs, group=NULL),colour="black")
ggsave("Figures/Collapse_BA_TS2.png",width = 8,height=6,units = "in",dpi=300)

nrow(Plots4)


Plots4$Pred<-predict(M4)


ggplot(Plots4,aes(x=Year,y=BAM,group=Block,shape=Transect))+geom_point()+geom_line(aes(y=Pred))+facet_wrap(~Block)


head(Plots4)

Plots4$Year3<-ifelse(Plots4$Year==1999,1996,Plots4$Year)
Plots4$Decline<-ifelse(Plots4$BAPERCM2>0,1,0)
ddply(Plots4,.(Year3,Decline),summarise,Decline2=length(Decline),mean=mean(BAPERCM),se=sd(BAPERCM)/sqrt(length(BAPERCM)))
ddply(Plots4,.(Year3,Collapse2),summarise,Decline2=length(Decline))
ddply(Plots4,.(Year3,Collapse2,Transect),summarise,Decline2=length(Decline))

26/46
13/15
