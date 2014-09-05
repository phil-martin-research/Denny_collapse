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

#produce counts of species per block per year
Sp_counts<-count(Trees,vars = c("Species","Block","Year"))
head(Sp_counts)
#and put in form that is usable by vegan
Sp_counts2<-dcast(Sp_counts,Block + Year ~Species)

#now set up loop to carry out similarity analysis comparing each block to itself in 1959
Blocks<-unique(Sp_counts2$Block)
Sor_similarity<-NULL
for (i in 1:length(Blocks)){
  Block_subset<-subset(Sp_counts2,Block==Blocks[i])
  Block_subset[is.na(Block_subset)]<-0
  Block_subset2<-Block_subset[-c(1:2)]
  Block_subset2[is.na(Block_subset2)]<-0
  Block_subset$Sorensen<-c(1,1-vegdist(Block_subset2)[1:nrow(Block_subset2)-1])
  Sor_similarity<-rbind(Sor_similarity,Block_subset)
}


head(Trees)
#calculate basal area per plot per time period
#for all species
for (i in 1:nrow(Trees)){
Trees$BA[i]<-ifelse(Trees$DBH[i]>10,Trees$DBH[i]^2*(pi/4),0)
}

#for beech only
Trees$Beech_BA<-ifelse(Trees$DBH>10&Trees$Species=="F",(Trees$DBH^2*(pi/4)),0)

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

BA_change_1964<-NULL
Blocks<-unique(BA_change_CC$Block)

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


Sor_BA<-merge(Sor_similarity,BA_change_1964,by=c("Block","Year"))

Sor_BA2<-subset(Sor_BA,Year>1964)


#exploratory plots of percentage change in basal area and change in similarity

ggplot(Sor_BA2,aes(x=Perc,y=Sorensen,group=Block))+geom_point(alpha=0.5)+geom_line(alpha=0.2)

ggplot(Sor_BA2,aes(x=Change,y=Sorensen,group=Block))+geom_point(alpha=0.8)
ggplot(Sor_BA2,aes(x=RR,y=Sorensen,group=Block))+geom_point(alpha=0.8)


#test different models against each other using different metrics for BA change, could also include SD? Any other structural stuff?

#work out which random effects I should be using first
M0.1<-lme(qlogis(Sorensen)~1,random = ~1|Block,data=Sor_BA2)
M0.2<-lme(qlogis(Sorensen)~1,random = ~Year|Block,data=Sor_BA2)
M0.3<-lme(qlogis(Sorensen)~1,random = ~Perc|Block,data=Sor_BA2)

AICc(M0.1,M0.2,M0.3)

#the model with random slopes and intercept is the most appropriate - so use these random effects
M1<-lme(qlogis(Sorensen)~Perc,random = ~Perc|Block,data=Sor_BA2)
M2<-lme(qlogis(Sorensen)~Perc+I(Perc^2),random = ~Perc|Block,data=Sor_BA2)

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


dredge(M2,rank = AICc,REML=F)

r.squaredGLMM(M2)
par(mfrow=c(1,1))
plot(Sor_BA2$Perc,Sor_BA2$Sorensen)
points(Sor_BA2$Perc,plogis(predict(M2,level=0)),col="red")
