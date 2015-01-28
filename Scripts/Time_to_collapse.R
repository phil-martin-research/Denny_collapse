#analyses for paul to get the time to collapse

library(ggplot2)
library(ape)
library(geoR)
library(ncf)
library(lme4)
library(MuMIn)

#########################################################
#exploratory analysis to look at spatial autocorrelation#
#########################################################
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("BA_gradient.csv")

#set year 2 as the number of years since 1964
BA$Year2<-BA$Year-1964

#now create loop to identify the plots that have declined by >= 75% at some point
BA_coll<-NULL
Block_unique<-unique(BA$Block)
for (i in 1:length(Block)){
  BA_sub<-subset(BA,Block==Block_unique[i])
  BA_sub$Collapse<-ifelse(min(BA_sub$BAPERCM)<=(-0.75),1,0)
  BA_coll<-rbind(BA_sub,BA_coll)
}

ggplot(BA_coll,aes(x=Year2,y=BAPERCM,group=Block))+geom_point()+geom_line()+facet_wrap(~Collapse)

#making a subset to only include plots that collapsed by > 75% gives 8 plots
BA_coll2<-subset(BA_coll,Collapse==1)

#model this collapse
#null models
M0.1<-lmer(BAPERCM~1+(1|Block),data=BA_coll2)
M0.2<-lmer(BAPERCM~1+(Block|Year2),data=BA_coll2)
AICc(M0.1,M0.2) #the first form is better

#now fixed effects are added
BA_coll2$Year_log<-log(BA_coll2$Year2+1)

M1<-lmer(BAPERCM~Year_log+(1|Block),data=BA_coll2)
M1<-lmer(BAPERCM~Year2-1+(1|Block),data=BA_coll2)
M2<-lmer(BAPERCM~Year2+I(Year2^2)-1+(1|Block),data=BA_coll2)

plot(M2)
r.squaredGLMM(M2)


plot(BA_coll2$Year2,BA_coll2$BAPERCM)
points(BA_coll2$Year2,predict(M1,re.form=NA),col="red")

Preds<-data.frame(Year2=seq(0,50,.1))
Preds$Year_log<-log(Preds$Year+1)

Preds$Pred<-predict(M2,re.form=NA,newdata=Preds)

#plot model predictions
theme_set(theme_bw(base_size=12))
Collapse_time1<-ggplot(BA_coll2,aes(x=Year2,y=BAPERCM,group=Block))+geom_point()+geom_line()
Collapse_time2<-Collapse_time1+geom_line(data=Preds,aes(x=Year2,y=Pred,group=NULL),size=3,colour="blue")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Collapse_time2+ylab("Change in BA since 1964")+xlab("Years since 1964")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Space_for_time.png",width = 8,height=6,units = "in",dpi=300)

#predicted time to  reach 0=0 years, to reach -0.25=7 years, to reach -0.5=15.7, -0.75=28, ~-.87=49 years

