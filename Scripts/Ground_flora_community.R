#script to calculate changes in Denny wood ground flora over time#
rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(vegan)
library(reshape2)

#load in data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
DBH<-read.csv("Denny_plots.csv")
GF<-read.csv("GF_ab.csv")

head(GF)
str(GF)

#replace zeros in dataframe
GF[is.na(GF)] <- 0

#remove data from plots 26, 44 and 45 which have incomplete records
GF<-subset(GF,Block!=26)
GF<-subset(GF,Block!=44)
GF<-subset(GF,Block!=45)
GF[GF==2001]<-1996

#produce counts of species per block per year
GF_melt<-melt(GF,id =c("Block","Year") )
head(GF_melt)
GF_melt2<-subset(GF_melt,value>0)
GF_Sp_R<-count(GF_melt2,vars = c("Block","Year"))


#plot this over time
ggplot(GF_Sp_R,aes(x=Year,y=freq,group=Block))+geom_line()+facet_wrap(~Block)

#merge species richness data to plot data


unique(DBH$Year)

GF_Sp_BA<-merge(GF_Sp_R,DBH,by=c("Block","Year"))

ggplot(GF_Sp_BA,aes(x=BAPERCS,y=freq))+geom_point()+facet_wrap(~Year)


#do a community similarity analysis for ground flora
Blocks<-unique(GF$Block)
Sor_similarity<-NULL
for (i in 1:length(Blocks)){
  Block_subset<-subset(GF,Block==Blocks[i])
  Block_subset[is.na(Block_subset)]<-0
  Block_subset2<-Block_subset[-c(1:2)]
  Block_subset$Sorensen<-c(1,1-vegdist(Block_subset)[1:nrow(Block_subset)-1])
  Sor_similarity<-rbind(Sor_similarity,Block_subset)
}

str(Sor_similarity)

#merge community composition data to plot data

GF_BA_2<-merge(GF_Sp_BA,Sor_similarity,by=c("Block","Year"))

GF_BA_2$Sorensen

ncol(GF_BA_2)

subset(GF_BA_2, select=c(BAPERCS,Sorensen))

ggplot(GF_BA_2,aes(x=BAPERCS,y=Sorensen))+geom_point()

