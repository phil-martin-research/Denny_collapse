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
Trees<-read.csv("Denny_trees_cleaned.csv")
head(Trees)
#subset trees to give only those inside plots that are alive
Trees<-subset(Trees,In_out=="In")
Trees<-subset(Trees,Year>=1964)
Trees<-subset(Trees,Status==1)
Trees<-subset(Trees,Block!=25)
Trees<-subset(Trees,Block!=26)

#produce sum of basal area per species per block
head(Trees)
#calculate basal area per plot per time period
#for all species
for (i in 1:nrow(Trees)){
  Trees$BA[i]<-ifelse(Trees$DBH[i]>10,(Trees$DBH[i]^2*(pi/4))/400,0)
}

for (i in 1:nrow(Trees)){
  Trees$BA_sa[i]<-ifelse(Trees$DBH[i]<10,(Trees$DBH[i]^2*(pi/4))/400,0)
}



head(Trees)
BA_Sp<-ddply(Trees, .(Block,Year,Species),summarize,Count=sum(Status))

Trees2<-subset(Trees,DBH<10)
BA_Sp_sap<-ddply(Trees2, .(Block,Year,Species),summarize,Count_sa=sum(Status))

#put species as columns and rows as blocks for each year
BA_Sp2<-dcast(BA_Sp,Block+Year~Species)

#and for saplings
BA_Sp_sap2<-dcast(BA_Sp_sap,Block+Year~Species)

#now set up loop to carry out similarity analysis comparing each block to itself in 1959
Blocks<-unique(BA_Sp2$Block)
Sor_similarity<-NULL
for (i in 1:length(Blocks)){
  Block_subset<-subset(BA_Sp2,Block==Blocks[i])
  Block_subset[is.na(Block_subset)]<-0
  Block_subset2<-Block_subset[-c(1:2)]
  Block_subset2[is.na(Block_subset2)]<-0
  Block_subset$Sorensen<-c(1,1-vegdist(Block_subset2)[1:nrow(Block_subset2)-1])
  Sor_similarity<-rbind(Sor_similarity,Block_subset)
}

#now set up loop to carry out similarity analysis for saplings comparing each block to itself in 1964
Blocks<-unique(BA_Sp_sap2$Block)
Sor_similarity_sap<-NULL
for (i in 1:length(Blocks)){
  Block_subset<-subset(BA_Sp_sap2,Block==Blocks[i])
  Block_subset[is.na(Block_subset)]<-0
  Block_subset2<-Block_subset[-c(1:2)]
  Block_subset2[is.na(Block_subset2)]<-0
  Block_subset$Sorensen<-c(1,1-vegdist(Block_subset2)[1:nrow(Block_subset2)-1])
  Sor_similarity_sap<-rbind(Sor_similarity_sap,Block_subset)
}


plot(Sor_similarity_sap$Sorensen)


#calculate basal area per plot per time period
#for all species
for (i in 1:nrow(Trees)){
Trees$BA[i]<-ifelse(Trees$DBH[i]>10,Trees$DBH[i]^2*(pi/4),0)
}

#for beech only
Trees$Beech_BA<-ifelse(Trees$DBH>10&Trees$Species=="F",(Trees$DBH^2*(pi/4)),0)

#for saplings

for (i in 1:nrow(Trees)){
  Trees$BA_sap[i]<-ifelse(Trees$DBH[i]<10,Trees$DBH[i]^2*(pi/4),0)
}

#total basal area
BA_change<-melt(with(Trees, tapply(BA,list(Block,Year), function(x) sum(na.omit(x)/400))))
colnames(BA_change)<-c("Block","Year","BA")
BA_change$Transect<-ifelse(BA_change$Block>=52,"Unenclosed","Enclosed")
BA_change_CC<-BA_change[complete.cases(BA_change),]

#total basal area - for beech
BA_change_F<-melt(with(Trees, tapply(Beech_BA,list(Block,Year), function(x) sum(na.omit(x)/400))))
colnames(BA_change_F)<-c("Block","Year","BA")
BA_change_F$Transect<-ifelse(BA_change$Block>=52,"Unenclosed","Enclosed")
BA_change_F2<-BA_change_F[complete.cases(BA_change_F),]

#total basal area for saplings
BA_sap_change<-melt(with(Trees, tapply(BA_sap,list(Block,Year), function(x) sum(na.omit(x)/400))))
colnames(BA_sap_change)<-c("Block","Year","BA")
BA_sap_change$Transect<-ifelse(BA_sap_change$Block>=52,"Unenclosed","Enclosed")
BA_sap_change_CC<-BA_sap_change[complete.cases(BA_sap_change),]

#merge BA sapling change and BA total change
BA_comb<-merge(BA_sap_change_CC,BA_change_CC,by=c("Block","Year"))
head(BA_comb)

ggplot(BA_comb,aes(x=BA.y,y=BA.x,group=Block))+geom_point()+geom_line(alpha=0.2)+facet_wrap(~Year)

BA_change_1964<-NULL
Blocks<-unique(BA_change_CC$Block)

head(Block_subset)

#now calculate BA change for each year relative to 1964
for (i in 1:length(Blocks)){
  Block_subset<-subset(BA_change_CC,Block==Blocks[i])
  Block_subset$Perc<-NA
  Block_subset$Change<-NA
  Block_subset$RR<-NA
  for( v in 1:nrow(Block_subset)){
    Block_subset$Perc[v]<-(Block_subset$BA[v]-Block_subset$BA[1])/Block_subset$BA[1]
    Block_subset$RR[v]<-log(Block_subset$BA[v])-log(Block_subset$BA[1])
    Block_subset$Change[v]<-(Block_subset$BA[v]-Block_subset$BA[1])
  }
  BA_change_1964<-rbind(BA_change_1964,Block_subset) 
}

#now calculate BA change for saplings for each year relative to 1964
BA_sap_change_1964<-NULL
Blocks<-unique(BA_sap_change_CC$Block)

for (i in 1:length(Blocks)){
  Block_subset<-subset(BA_sap_change_CC,Block==Blocks[i])
  Block_subset$Perc<-NA
  Block_subset$Change<-NA
  Block_subset$RR<-NA
  for( v in 1:nrow(Block_subset)){
    Block_subset$Perc[v]<-(Block_subset$BA[v]-Block_subset$BA[1])/Block_subset$BA[1]
    Block_subset$RR[v]<-log(Block_subset$BA[v])-log(Block_subset$BA[1])
    Block_subset$Change[v]<-(Block_subset$BA[v]-Block_subset$BA[1])
  }
  BA_sap_change_1964<-rbind(BA_sap_change_1964,Block_subset) 
}




Sor_BA<-merge(Sor_similarity,BA_change_1964,by=c("Block","Year"))
Sor_sap_BA<-merge(Sor_similarity_sap,BA_change_1964,by=c("Block","Year"))


Sor_BA2<-subset(Sor_BA,Year>1964)
Sor_sap_BA2<-subset(Sor_sap_BA,Year>1964)


#calculate species richness per plot per time period
Sp_richness<-(with(Trees, tapply(Species,list(Block,Year), function(x) length(unique(na.omit(x))))))
Sp_rich_melt<-melt(Sp_richness)
colnames(Sp_rich_melt)<-c("Block","Year","Sp_Rich")
Sor_BA3<-merge(Sor_BA2,Sp_rich_melt,by=c("Block","Year"))


head(Sor_BA3)

#model of percentage change in basal area vs  change in  beta  diversity
M1<-lme(qlogis(Sorensen)~Perc,random=~Perc|Block,data=Sor_BA3)
M2<-lme(qlogis(Sorensen)~Perc+I(Perc^2),random=~Perc|Block,data=Sor_BA3)

plot(M1)
plot(M2)

AICc(M1,M2)

Perc_BA<-data.frame(Perc=seq(-1,1,0.01))

Perc_BA$Pred<-plogis(predict(M2,level=0,Perc_BA))

#exploratory plots of percentage change in basal area and change in similarity for individuals >10cm dbh
theme_set(theme_bw(base_size=30))
BA_comm<-ggplot(Sor_BA3,aes(x=Perc*100,y=Sorensen,group=Block))+geom_point(alpha=0.5)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
BA_comm+xlab("Percentage change in basal area since 1964")+ylab("Sorensen similarity index")+geom_line(data=Perc_BA,aes(x=Perc*100,y=Pred,group=NULL))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures/Forest_collapse")
ggsave("Comm_BA_pres.png",width = 12,height = 8,units = "in",dpi = 300)

#exploratory plots of percentage change in basal area and change in similarity for individuals <10cm dbh
theme_set(theme_bw(base_size=30))
head(Sor_sap_BA2)
BA_comm<-ggplot(Sor_sap_BA2,aes(x=Perc*100,y=Sorensen,group=Block))+geom_point(alpha=0.5)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
BA_comm+xlab("Percentage change in basal area since 1964")+ylab("Sorensen similarity index")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures/Forest_collapse")
ggsave("Comm_BA_sap.png",width = 12,height = 8,units = "in",dpi = 300)



#basal area change vs proportional basal area represented by holly
theme_set(theme_bw(base_size=30))
Holly<-ggplot(Sor_BA3,aes(x=Perc*100,y=I/BA,group=Block))+geom_point(alpha=0.5)+geom_line(alpha=0.2)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
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
