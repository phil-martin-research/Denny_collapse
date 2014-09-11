#script to produce csv of all variables of interest at the plot scale from Denny data
#using 1964 as a baseline

rm(list=ls(all=TRUE))

#open packages neeeded for analysis
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(nlme)
library(MuMIn)
library(gridExtra)
library(MASS)
library(vegan)

#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Location<-read.csv("Plot_coords.csv")
Location<-unique(Location[,c(3,5:6)])
Trees<-read.csv("Denny_trees_cleaned.csv")
#subset trees to give only those inside plots that are alive and plots that have been surveyed in 1964
Trees<-subset(Trees,In_out=="In")
Trees<-subset(Trees,Year>=1964)
Trees<-subset(Trees,Status==1)
Trees<-subset(Trees,Block!=25)
Trees<-subset(Trees,Block!=26)
#replace all species codes Qr with Q as they represent the same species
levels(Trees$Species)[which(levels(Trees$Species)=="Qr")] <- "Q"

#produce sum of basal area per species per block
head(Trees)
#calculate basal area per plot per time period
#for all species >10cm DBH
Trees$BA<NULL
Trees$BA_Q<-NULL
Trees$BA_F<-NULL
for (i in 1:nrow(Trees)){
  Trees$BA[i]<-ifelse(Trees$DBH[i]>10,(Trees$DBH[i]^2*(pi/4))/400,0)
  Trees$BA_F[i]<-ifelse(Trees$DBH[i]>10&Trees$Species[i]=="F",(Trees$DBH[i]^2*(pi/4))/400,0)
  Trees$BA_Q[i]<-ifelse(Trees$DBH[i]>10&Trees$Species[i]=="Q",(Trees$DBH[i]^2*(pi/4))/400,0)
}


#organise data for community analysis
Sp<-ddply(Trees, .(Block,Year,Species),summarize,Count=sum(Status))
#and for analysis of basal area, stem density and speceis richness
BA<-ddply(Trees, .(Block,Year),summarize,SD=sum(Status),BA=sum(BA),BA_F=sum(BA_F),BA_Q=sum(BA_Q),Sp_R=length(unique(Species)))


#put species as columns and rows as blocks for each year
Sp2<-dcast(Sp,Block+Year~Species,value.var="Count")

#do the same for saplings - all species <10cm DBH
Trees2<-subset(Trees,DBH<10)

for (i in 1:nrow(Trees2)){
  Trees2$BA_sa[i]<-ifelse(Trees2$DBH[i]<10,(Trees2$DBH[i]^2*(pi/4))/400,0)
  Trees2$BA_F[i]<-ifelse(Trees2$DBH[i]<10&Trees2$Species[i]=="F",(Trees2$DBH[i]^2*(pi/4))/400,0)
  Trees2$BA_Q[i]<-ifelse(Trees2$DBH[i]<10&Trees2$Species[i]=="Q",(Trees2$DBH[i]^2*(pi/4))/400,0)
}
#community composition
Sap_Sp<-ddply(Trees2, .(Block,Year,Species),summarize,Count=sum(Status))

#and for analysis of basal area and stem density
Sap_BA<-ddply(Trees2, .(Block,Year),summarize,SD=sum(Status),BA=sum(BA_sa),BA_F=sum(BA_F),BA_Q=sum(BA_Q),Sp_R=length(unique(Species)))

#put species as columns and rows as blocks for each year
Sap_Sp2<-dcast(Sap_Sp,Block+Year~Species,value.var="Count")

#now set up loop to carry out similarity analysis comparing each block to itself in 1964
Blocks<-unique(Sp2$Block)
Sor_similarity<-NULL
for (i in 1:length(Blocks)){
  Block_subset<-subset(Sp2,Block==Blocks[i])
  Block_subset[is.na(Block_subset)]<-0
  Block_subset2<-Block_subset[-c(1:2)]
  Block_subset2[is.na(Block_subset2)]<-0
  Block_subset$Sorensen<-c(1,1-vegdist(Block_subset2)[1:nrow(Block_subset2)-1])
  Sor_similarity<-rbind(Sor_similarity,Block_subset)
}

#now set up loop to carry out similarity analysis for saplings comparing each block to itself in 1964
Blocks<-unique(Sap_Sp2$Block)
Sor_similarity_sap<-NULL
for (i in 1:length(Blocks)){
  Block_subset<-subset(Sap_Sp2,Block==Blocks[i])
  Block_subset[is.na(Block_subset)]<-0
  Block_subset2<-Block_subset[-c(1:2)]
  Block_subset2[is.na(Block_subset2)]<-0
  Block_subset$Sorensen<-c(1,1-vegdist(Block_subset2)[1:nrow(Block_subset2)-1])
  Sor_similarity_sap<-rbind(Sor_similarity_sap,Block_subset)
}


#merge BA sapling change and BA total change
BA_comb<-merge(BA,Sap_BA,by=c("Block","Year"),all=T)
head(BA_comb)
colnames(BA_comb)<-c("Block","Year","SDM","BAM","BAFM","BAQM","SPRM","SDS","BAS","BAFS","BAQS","SPRS")

#now calculate BA and SD change for each year relative to 1964
#for total SD, total BA, beech BA, and oak BA, for mature and saplings
BA_change<-NULL
Blocks<-unique(BA_comb$Block)

for (i in 1:length(Blocks)){
  Block_subset<-subset(BA_comb,Block==Blocks[i])
  for( v in 1:nrow(Block_subset)){
    Block_subset$BAPERCM[v]<-(Block_subset$BAM[v]-Block_subset$BAM[1])/Block_subset$BAM[1]
    Block_subset$BARAWM[v]<-(Block_subset$BAM[v]-Block_subset$BAM[1])
    Block_subset$BAFPERCM[v]<-(Block_subset$BAFM[v]-Block_subset$BAFM[1])/Block_subset$BAFM[1]
    Block_subset$BAFRAWM[v]<-(Block_subset$BAFM[v]-Block_subset$BAFM[1])
    Block_subset$BAQPERCM[v]<-(Block_subset$BAQM[v]-Block_subset$BAQM[1])/Block_subset$BAQM[1]
    Block_subset$BAQRAWM[v]<-(Block_subset$BAQM[v]-Block_subset$BAQM[1])
    Block_subset$SDPERCM[v]<-(Block_subset$SDM[v]-Block_subset$SDM[1])/Block_subset$SDM[1]
    Block_subset$SDRAWM[v]<-(Block_subset$SDM[v]-Block_subset$SDM[1])
    Block_subset$SPRPERCM[v]<-(Block_subset$SPRM[v]-Block_subset$SPRM[1])/Block_subset$SPRM[1]
    Block_subset$SPRRAWM[v]<-(Block_subset$SPRM[v]-Block_subset$SPRM[1])
    Block_subset$BAPERCS[v]<-(Block_subset$BAS[v]-Block_subset$BAS[1])/Block_subset$BAS[1]
    Block_subset$BARAWS[v]<-(Block_subset$BAS[v]-Block_subset$BAS[1])
    Block_subset$BAFPERCS[v]<-(Block_subset$BAFS[v]-Block_subset$BAFS[1])/Block_subset$BAFS[1]
    Block_subset$BAFRAWS[v]<-(Block_subset$BAFS[v]-Block_subset$BAFS[1])
    Block_subset$BAQPERCS[v]<-(Block_subset$BAQS[v]-Block_subset$BAQS[1])/Block_subset$BAQS[1]
    Block_subset$BAQRAWS[v]<-(Block_subset$BAQS[v]-Block_subset$BAQS[1])
    Block_subset$SDPERCS[v]<-(Block_subset$SDS[v]-Block_subset$SDS[1])/Block_subset$SDS[1]
    Block_subset$SDRAWS[v]<-(Block_subset$SDS[v]-Block_subset$SDS[1])
    Block_subset$SPRPERCS[v]<-(Block_subset$SPRS[v]-Block_subset$SPRS[1])/Block_subset$SPRS[1]
    Block_subset$SPRRAWS[v]<-(Block_subset$SPRS[v]-Block_subset$SPRS[1])
    
    
  }
  BA_change<-rbind(BA_change,Block_subset) 
}

#remove all NaN values to be zero instead
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

BA_change[is.nan(BA_change)] <- 0

#merge sorensen similarity data onto this
Sor_BA<-merge(Sor_similarity,BA_change,by=c("Block","Year"),all=T)
head(Sor_BA)
Sor_BA2<-merge(Sor_similarity_sap,Sor_BA,by=c("Block","Year"),all=T)
head(Sor_BA2)

Sor_BA3<-subset(Sor_BA2,Year>1964)
str(Sor_BA3)
colnames(Sor_BA3)[3:23]<-c("AS","BS","CrS","FS","IS","PMS","QS","ScS","TS","SorS","AM","BM","CrM","FM","FrM","IM","PMM","QM","ScM","TM","SorM")

#reclass 1996 plots as 1999 for grouping
Sor_BA3$Year2<-ifelse(Sor_BA3$Year==1996,1999,Sor_BA3$Year)
#add column to identify transect
Sor_BA3$Transect<-ifelse(Sor_BA3$Block>51,"Unenclosed","Enclosed")

ggplot(Sor_BA3,aes(x=BAPERCM,y=SorM,colour=Transect))+geom_point()+facet_wrap(~Year2)


#save all this as a csv
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
write.csv(Sor_BA3,"Denny_plots.csv",row.names=F)
