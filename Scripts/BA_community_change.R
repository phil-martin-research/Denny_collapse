#tree community change as a function of basal area loss

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(vegan)
library(reshape2)
library(nlme)
library(MuMIn)
library(gridExtra)


#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("Denny_plots.csv")
head(BA)


#model of percentage change in basal area vs  change in  beta  diversity
M1<-lme(qlogis(SorM)~BAPERCM,random=~BAPERCM|Block,data=BA)
M1.2<-lme(qlogis(SorM)~I(BAPERCM-mean(BAPERCM)),random=~BAPERCM|Block,data=BA)
M2<-lme(qlogis(SorM)~BAPERCM+I(BAPERCM^2),random=~BAPERCM|Block,data=BA)


plot(M1)
plot(M2)

AICc(M1,M1.2)

Perc_BA<-data.frame(BAPERCM=seq(-1,1,0.01))

Perc_BA$Pred<-plogis(predict(M2,level=0,Perc_BA))

#exploratory plots of percentage change in basal area and change in similarity for individuals >10cm dbh
theme_set(theme_bw(base_size=12))
BA_comm<-ggplot(BA,aes(x=BAPERCM*100,y=SorM,group=Block))+geom_point(alpha=0.5)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
BA_comm+xlab("Percentage change in basal area since 1964")+ylab("Sorensen similarity index")+geom_line(data=Perc_BA,aes(x=BAPERCM*100,y=Pred,group=NULL))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures/Forest_collapse")
ggsave("Comm_BA_pres.png",width = 12,height = 8,units = "in",dpi = 300)

#exploratory plots of percentage change in basal area and change in similarity for individuals <10cm dbh
theme_set(theme_bw(base_size=12))
head(Sor_sap_BA2)
BA_comm<-ggplot(BA,aes(x=BAPERCM*100,y=SorS,group=Block))+geom_point(alpha=0.5)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
BA_comm+xlab("Percentage change in basal area since 1964")+ylab("Sorensen similarity index")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures/Forest_collapse")
ggsave("Comm_BA_sap.png",width = 12,height = 8,units = "in",dpi = 300)



#basal area change vs proportional basal area represented by holly
theme_set(theme_bw(base_size=12))

Holly<-ggplot(BA,aes(x=BAPERCM*100,y=IM,group=Block))+geom_point(alpha=0.5)+geom_line(alpha=0.2)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Holly+xlab("Percentage change in basal area since 1964")+ylab("Proportion of basal \narea represented by holly")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures/Forest_collapse")
ggsave("Holly_BA_pres.png",width = 12,height = 8,units = "in",dpi = 300)


#basal area change vs species richness
ggplot(Sor_BA3,aes(x=Perc,y=Sp_Rich,group=Block))+geom_point(alpha=0.5)+geom_line(alpha=0.2)+geom_smooth(se=F,method="lm",aes(group=NULL))



ggplot(Sor_BA2,aes(x=Change,y=Sorensen,group=Block))+geom_point(alpha=0.8)
ggplot(Sor_BA2,aes(x=RR,y=Sorensen,group=Block))+geom_point(alpha=0.8)



#test different models against each other using different metrics for BA change, could also include SD? Any other structural stuff?

#work out which random effects I should be using first
M0.1<-lme(qlogis(Sorensen)~1,random = ~1|Block,data=Sor_BA3)
M0.2<-lme(qlogis(Sorensen)~1,random = ~Year|Block,data=Sor_BA3)
M0.3<-lme(qlogis(Sorensen)~1,random = ~Perc|Block,data=Sor_BA3)

AICc(M0.1,M0.2,M0.3)

#the model with random slopes and intercept is the most appropriate - so use these random effects
M1<-lme(qlogis(Sorensen)~Perc,random = ~Perc|Block,data=Sor_BA2,control=ctrl)
M2<-lme(qlogis(Sorensen)~Perc+I(Perc^2),random = ~Perc|Block,data=Sor_BA2,control=ctrl)


#diagnostic plots
grid.arrange(plot(M1), plot(M2),qqnorm(M1),qqnorm(M2))

#model predictions against actual values
par(mfrow=c(1,2))
plot(Sor_BA2$Sorensen,plogis(predict(M1)))
abline(0,1)
plot(Sor_BA2$Sorensen,plogis(predict(M2)))
abline(0,1)
#now look at variance structure of models over the predictors
par(mfrow=c(1,2))
plot(Sor_BA2$Perc,resid(M1))
plot(Sor_BA2$Perc,resid(M2))


#look at what happens if  you remove the outliers of percentage >100% increase

Sor_BA4<-subset(Sor_BA3,Perc<1)
#the model with random slopes and intercept is the most appropriate - so use these random effects
ctrl<-lmeControl(opt='optim')

M1<-lme(qlogis(Sorensen)~Perc,random = ~Perc|Block,data=Sor_BA4,control=ctrl)
M2<-lme(qlogis(Sorensen)~Perc+I(Perc^2),random = ~Perc|Block,data=Sor_BA4,control=ctrl)
M3<-lme(qlogis(Sorensen)~Perc+I(Perc^2)+I(Perc^3),random = ~Perc|Block,data=Sor_BA4,control=ctrl)
M4<-lme(qlogis(Sorensen)~Perc*Year,random = ~Perc|Block,data=Sor_BA4,control=ctrl)
M5<-lme(qlogis(Sorensen)~Perc*Year+I(Perc^2)*Year,random = ~Perc|Block,data=Sor_BA4,control=ctrl)


#diagnostic plots
grid.arrange(plot(M1), plot(M2),plot(M3),plot(M4),plot(M5),qqnorm(M1),qqnorm(M2),qqnorm(M3),qqnorm(M4),qqnorm(M5),ncol=5)

#model predictions against actual values
par(mfrow=c(1,5))
plot(Sor_BA4$Sorensen,plogis(predict(M1)))
abline(0,1)
plot(Sor_BA4$Sorensen,plogis(predict(M2)))
abline(0,1)
plot(Sor_BA4$Sorensen,plogis(predict(M3)))
abline(0,1)
plot(Sor_BA4$Sorensen,plogis(predict(M4)))
abline(0,1)
plot(Sor_BA4$Sorensen,plogis(predict(M5)))
abline(0,1)

#now look at variance structure of models over the predictors
par(mfrow=c(1,2))
plot(Sor_BA4$Perc,resid(M1))
plot(Sor_BA4$Perc,resid(M2))


dredge(M5,rank = AICc,REML=F,subset=dc(Perc,Perc^2))


r.squaredGLMM(M2)
par(mfrow=c(1,1))
plot(Sor_BA2$Perc,Sor_BA2$Sorensen)
points(Sor_BA4$Perc,plogis(predict(M2,level=0)),col="red")

Preds_var<-data.frame(Perc=seq(-0.95,1,0.001))

#create predictions for model 2

Preds<-data.frame(predict(M2,level=0,newdata = Preds_var,se.fit=T))
head(Preds)
Preds$Perc<-seq(-0.95,1,0.001)

theme_set(theme_bw(base_size=12))
a<-ggplot(Sor_BA4,aes(x=Perc*100,y=Sorensen,group=Block))+geom_point(size=3,alpha=0.5)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ylab("Sorensen similarity")
a+xlab("Percentage basal area change from 1964")+geom_line(data=Preds,aes(x=Perc*100,y=plogis(fit),group=NULL))+geom_line(data=Preds,aes(x=Perc*100,y=plogis(fit+(1.96*se.fit)),group=NULL),lty=2)+geom_line(data=Preds,aes(x=Perc*100,y=plogis(fit-(1.96*se.fit)),group=NULL),lty=2)
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Sor_BA_trees.png",width = 8,height = 4,units = "in",dpi = 500)



#need to add (before presentation) analyses on changes in functional traits (light, nitrogen, moisture), 
#basal area of certain species (holly etc) in collapsed plots, and ground flora from this year & their traits
#also look at spatial distribution of collapse over the transects by year - possible future directions

#model of basal area change vs proportion of basal area of holly
head(Sor_BA4)

ctrl<-lmeControl(opt='optim')

M0.1<-lme(I/BA~1,random=~1|Block,data=Sor_BA4,control=ctrl)
M0.2<-lme(I/BA~1,random=~Perc|Block,data=Sor_BA4,control=ctrl)

#null model with random slopes is best

M1<-lme(asin(sqrt(I/BA))~Perc,random=~Perc|Block,data=Sor_BA4,control=ctrl)
M2<-lme(asin(sqrt(I/BA))~Perc+I(Perc^2),random=~Perc|Block,data=Sor_BA4,control=ctrl)
M3<-lme(asin(sqrt(I/BA))~log(Perc+1),random=~Perc|Block,data=Sor_BA4,control=ctrl)



plot(Sor_BA4$Perc,Sor_BA4$I/Sor_BA4$BA)
points(Sor_BA4$Perc,predict(M2,level=0),col="red")
