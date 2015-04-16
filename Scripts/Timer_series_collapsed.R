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
#first looking at the collapsed plots
Plots4$Year2<-Plots4$Year-1964
Plots_coll<-subset(Plots4,Collapse2==1)
Plots_stable<-subset(Plots4,Collapse2==0)


#first run a null model
M0_coll<-lmer(BAM~1+(1|Block),data=Plots_coll)
#use first random effect specification
M1_coll<-lmer(BAM~Year2+(1|Block),data=Plots_coll)
M2_coll<-lmer(BAM~Year2+I(Year2^2)+(1|Block),data=Plots_coll)
M3_coll<-lmer(BAM~Year2+I(Year2^2)+I(Year2^3)+(1|Block),data=Plots_coll)
Models<-list(M0_coll,M1_coll,M2_coll,M3_coll)

#produce tables to give summary statistics
Model_table<-model.sel(Models)
Model_table$R_sq<-c(r.squaredGLMM(M1_coll)[1],r.squaredGLMM(M2_coll)[1],r.squaredGLMM(M3_coll)[1],r.squaredGLMM(M0_coll)[1])

write.csv(Model_table,"Figures/BA_TS_table_coll.csv",row.names=F)

Model_average<-model.avg(Models,fit=T)
summary(Model_average)

#put in predictions
newdat<-expand.grid(Year2=(seq(0,50,0.1)))
newdat$BAM<-0

mm <- model.matrix(terms(M3_coll),newdat)
newdat$BAM <- predict(Model_average,newdat,re.form=NA)
pvar1 <- diag(mm %*% tcrossprod(vcov(M3_coll),mm))
tvar1 <- pvar1+VarCorr(M3_coll)$Block[1]  ## must be adapted for more complex models
newdat <- data.frame(
  newdat
  , plo = newdat$BAM-2*sqrt(pvar1)
  , phi = newdat$BAM+2*sqrt(pvar1)
  , tlo = newdat$BAM-2*sqrt(tvar1)
  , thi = newdat$BAM+2*sqrt(tvar1)
)


newdat$Year<-newdat$Year2+1964
newdat$Collapse2<-1
newdat$Collapse3<-"Collapsed"

#now do the same for stable plots
#first run a null model
M0_stab<-lmer(BAM~1+(1|Block),data=Plots_stable)
#use first random effect specification
M1_stab<-lmer(BAM~Year2+(1|Block),data=Plots_stable)
M2_stab<-lmer(BAM~Year2+I(Year2^2)+(1|Block),data=Plots_stable)
M3_stab<-lmer(BAM~Year2+I(Year2^2)+I(Year2^3)+(1|Block),data=Plots_stable)
Models<-list(M0_stab,M1_stab,M2_stab,M3_stab)

#produce tables to give summary statistics
Model_table<-model.sel(Models)
Model_table$R_sq<-c(r.squaredGLMM(M1_stab)[1],r.squaredGLMM(M2_stab)[1],r.squaredGLMM(M3_stab)[1],r.squaredGLMM(M0_stab)[1])

write.csv(Model_table,"Figures/BA_TS_table_stable.csv",row.names=F)

Model_average_stab<-model.avg(Models,fit=T)
summary(Model_average_stab)

#now produce predictions
newdat2<-expand.grid(Year2=(seq(0,50,0.1)))
newdat2$BAM<-0

mm <- model.matrix(terms(M3_stab),newdat2)
newdat2$BAM <- predict(Model_average_stab,newdat2,re.form=NA)
pvar1 <- diag(mm %*% tcrossprod(vcov(M3_stab),mm))
tvar1 <- pvar1+VarCorr(M3_stab)$Block[1]  ## must be adapted for more complex models
newdat2 <- data.frame(
  newdat2
  , plo = newdat2$BAM-2*sqrt(pvar1)
  , phi = newdat2$BAM+2*sqrt(pvar1)
  , tlo = newdat2$BAM-2*sqrt(tvar1)
  , thi = newdat2$BAM+2*sqrt(tvar1)
)


newdat2$Year<-newdat2$Year2+1964
newdat2$Collapse2<-0
newdat2$Collapse3<-"Stable"

Predictions<-rbind(newdat,newdat2)

#now plot these predictions
Plots4$Collapse3<-ifelse(Plots4$Collapse2==1,"Collapsed","Stable")
Plots4$Transect<-ifelse(Plots4$Block>50,"Open","Fenced")

labels<-data.frame(x=min(Plots4$Year)+1,y=max(Plots$BAM)-1,Collapse3=c("Collapsed","Stable"),labs=c("(a)","(b)"))
theme_set(theme_bw(base_size=12))
BA_Coll1<-ggplot(data=Plots4,size=2,alpha=0.1,aes(group=Block,shape=Transect,x=Year,y=BAM))+geom_point(alpha=0.1)+geom_line(alpha=0.1)+facet_wrap(~Collapse3)
BA_Coll2<-BA_Coll1+geom_ribbon(data=Predictions,aes(ymax=phi,ymin=plo,group=NULL,shape=NULL),alpha=0.4)+geom_line(data=Predictions,aes(group=Collapse3,shape=NULL,y=BAM),size=2)
BA_Coll3<-BA_Coll2+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
BA_Coll4<-BA_Coll3+ylab(expression(paste("Basal area (", m^bold("2"),ha^bold("-1"),")")))
BA_Coll4+theme(strip.background = element_blank(),strip.text.x = element_blank(),legend.position="none")+
geom_text(data=labels,aes(shape=NULL,x=x,y=y,label=labs, group=NULL),colour="black")
ggsave("Figures/Collapse_BA_TS.png",width = 8,height=4,units = "in",dpi=800)
