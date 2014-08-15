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
DBH_64<-subset(DBH,Year==1964|1988|1984)
ggplot(DBH,aes(x=as.factor(Status),y=DBH))+geom_boxplot()+facet_wrap(~Year)


#use function to work out the base rate proabability of mortality for each period
head(DBH_64)
Tree_ID<-unique(DBH_64$Tree_ID)
Mort<-NULL
for (i in 1:unique(Tree_ID)){
  for (y in 1:nrow(DBH_64)){
 Stat<-ifelse(DBH_64$Tree_ID[y]==Tree_ID[i]&DBH_64$Year[y]==1988|1984,DBH_64$Status[y],NA)
 Mort<-rbind(Mort,Stat)
}
}

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


