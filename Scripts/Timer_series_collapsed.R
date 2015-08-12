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
Plots$Year2<-Plots$Year-1964
head(Plots)

#first run a null model
M0_coll<-lmer(BAM~1+(1|Block),data=Plots)
#use first random effect specification
M1_coll<-lmer(BAM~Year2+(Year2|Block),data=Plots)
M2_coll<-lmer(BAM~Year2+I(Year2^2)+(Year2|Block),data=Plots)
M3_coll<-lmer(BAM~Year2+I(Year2^2)+I(Year2^3)+(Year2|Block),data=Plots)
Models<-list(M0_coll,M1_coll,M2_coll,M3_coll)

#produce tables to give summary statistics
Model_table<-model.sel(Models,extra =r.squaredGLMM)

write.csv(Model_table,"Figures/BA_TS_table_coll.csv",row.names=F)

Model_average<-model.avg(Models,fit=T)
summary(Model_average)

#put in predictions
newdat<-expand.grid(Year2=(seq(0,50,0.1)),Block=unique(Plots$Block))
newdat$BAM<-0

mm <- model.matrix(terms(M3_coll),newdat)
newdat$BAM <- predict(Model_average,newdat,re.form=NA)
newdat$BAM_R <- predict(Model_average,newdat)
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

#now plot these predictions

theme_set(theme_bw(base_size=12))
BA_Coll1<-ggplot(data=Plots,size=2,alpha=0.1,aes(group=Block,x=Year,y=BAM))#+geom_point(alpha=0.1)
BA_Coll2<-BA_Coll1+geom_line(data=newdat,aes(x=Year,y=BAM_R,group=Block),alpha=0.1,size=0.3)
BA_Coll3<-BA_Coll2+geom_ribbon(data=newdat,aes(ymax=phi,ymin=plo,group=NULL,shape=NULL),alpha=0.4)+geom_line(data=newdat,aes(group=NULL,shape=NULL,y=BAM),size=3)
BA_Coll4<-BA_Coll3+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
BA_Coll5<-BA_Coll4+ylab(expression(paste("Basal area (", m^bold("2"),ha^bold("-1"),")")))
ggsave("Figures/BA_TS.png",width = 6,height=6,units = "in",dpi=800)
