#script to look at the probability of a tree dying

rm(list=ls(all=TRUE))

#open packages neeeded for analysis
library(ggplot2)
library(plyr)
library(reshape2)1
library(ape)
library(geoR)
library(nlme)
library(MuMIn)
library(gridExtra)
library(MASS)
library(survival)
library(GGally)
library(lme4)
library(fields)
library(ROCR)

#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Location<-read.csv("Plot_coords.csv")
Location<-unique(Location[,c(3,5:6)])
DBH<-read.csv("Denny_trees_cleaned.csv")
head(DBH)
unique(DBH$Year)
head(Location)
#subset trees to give only those inside plots
DBH<-subset(DBH,In_out=="In")

#first some exploratory analysis to look at variation in tree deaths by year
#subset the data to only include data from 1964
DBH_64<-subset(DBH,Year<1989&Year>1960)
head(DBH_64)
ggplot(DBH,aes(x=as.factor(Status),y=DBH))+geom_boxplot()+facet_wrap(~Year)


#toy example for data from 1964 to 1984/88
Alive_64<-subset(DBH_64,Status==1&Year==1964)[,3]
#create a loop to only give those trees which were alive in 1964 and their status in 1984/88
Mort_88<-NULL
for (i in 1:length(Alive_64)){
Mort<-subset(DBH_64,Tree_ID==Alive_64[i]&Year==1988)
Mort_88<-rbind(Mort_88,Mort)
}
head(Mort_88,n = 20)

#change status to 1=dead, 0=alive
Mort_88$Dead<-ifelse(Mort_88$Status==1,0,1)
Mort_88_2<-Mort_88[complete.cases(Mort_88[,12]),]
summary(Mort_88_2)
#work out distance to nearest dead tree
Distances<-rdist(Mort_88_2[9:10])
Mort_88_2$Tree_ID
arrayInd(4, dim(Distances))

#produce function to remove distances for live trees
for (i in 1:nrow(Distances)){
  for (y in 1:ncol(Distances)){
    Distances[i,y]<-ifelse(Mort_88_2$Dead[i]==1,Distances[i,y],NA)
  }
}
Distances[Distances==0]<-NA

#now work out minimum distance to a dead tree
for (i in 1:ncol(Distances)){
  Mort_88_2$Dead_Dist[i]<-min(Distances[,i],na.rm = T)
}
summary(Mort_88_2$Dead_Dist)

#now work out DBH of nearest dead tree
for (i in 1:ncol(Distances)){
  Index<-match(min(Distances[,i],na.rm = T),Distances[,i])
  Mort_88_2$Dead_DBH[i]<-Mort_88_2$DBH[Index]
}

#model of mortality from 1964-1988
M0<-glm(Dead~1,data=Mort_88_2,family=binomial(link = "logit"))
M1<-glm(Dead~DBH,data=Mort_88_2,family=binomial(link = "logit"))
M2<-glm(Dead~DBH+I(DBH^2),data=Mort_88_2,family=binomial(link = "logit"))
M3<-glm(Dead~DBH+Dead_Dist,data=Mort_88_2,family=binomial(link = "logit"))
M4<-glm(Dead~Dead_Dist,data=Mort_88_2,family=binomial(link = "logit"))
M5<-glm(Dead~Dead_Dist+Dead_DBH,data=Mort_88_2,family=binomial(link = "logit"))
M6<-glm(Dead~Dead_Dist*Dead_DBH,data=Mort_88_2,family=binomial(link = "logit"))
M7<-glm(Dead~DBH+Species,data=Mort_88_2,family=binomial(link = "logit"))
M8<-glm(Dead~DBH*Species,data=Mort_88_2,family=binomial(link = "logit"))



plot(Mort_88_2$Dead_Dist,(plogis(predict(M4)))/24)
plot(Mort_88_2$DBH,(plogis(predict(M1)))/24)
plot(Mort_88_2$DBH,Mort_88_2$Dead)

auc.tmp <- performance(predict(M1),"auc")
auc <- as.numeric(auc.tmp@y.values)
plot(M1)

################################################################################
#below this point is the survival analysis code, not sure that this is much use#
################################################################################


#include only complete cases - those with data on whether stems are alive or dead and not nas
DBH<-DBH[complete.cases(DBH[,6]),]
#recode deatch as 1 and alive as 0
DBH$Death<-ifelse(DBH$Status==1,0,1)

#create function to work out time since first measurment for each tree
Tree_ID2<-unique(DBH$Tree_ID)
Tree_surv<-NULL
for (i in 1:length(Tree_ID)){
  Tree_sub<-subset(DBH,Tree_ID==Tree_ID2[i])
  Tree_sub$TSFM<-max(Tree_sub$Year)-min(Tree_sub$Year)
  Tree_sub2<-data.frame(start=min(Tree_sub$Year),stop=max(Tree_sub$Year),min_dbh=min(Tree_sub$DBH),max_dbh=max(Tree_sub$DBH),
             gr=(max(Tree_sub$DBH)-min(Tree_sub$DBH))/max(Tree_sub$TSFM),death=max(Tree_sub$Death),Sp=Tree_sub$Species[1])
  Tree_surv<-rbind(Tree_surv,Tree_sub2)
}

head(Tree_surv)

#subset to include only trees that have been measured at two time periods or more
Tree_surv<-subset(Tree_surv,stop>start)
Tree_surv<-subset(Tree_surv,max_dbh>min_dbh)
selected<-c("F","I","Q")
Tree_surv<-Tree_surv[Tree_surv$Sp %in% selected,]
Tree_surv$Sp<-factor(Tree_surv$Sp)

#create sruvival object
Tree_surv$S<-Surv(Tree_surv$min_dbh,Tree_surv$max_dbh,Tree_surv$death)
summary(Tree_surv$Sp)

(xtabs( ~ S+S, data=Tree_surv))

#create model using  coph model type
M1<- coxph(S~Sp, data = Tree_surv)

#plot base survival over dbh
ggsurv(survfit(M1))

#plot species specific survival
tree_surv2<- survfit(Surv(Tree_surv$min_dbh,Tree_surv$max_dbh,Tree_surv$death)~Sp, data = Tree_surv)
ggsurv(tree_surv2,plot.cens = F)




#plot base survival by speceis


