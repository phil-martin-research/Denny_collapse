#script to produce csv of all variables of interest at the plot scale from Denny data
#using 1964 as a baseline

#######need to include trait data & locations!!!

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
library(TR8)

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
#add species names as a column
Tree_sp<-read.csv("Tree_sp.csv")
Trees<-merge(Trees,Tree_sp,by.x="Species",by.y="Code")
colnames(Trees)[12]<-"Sp_name"



#produce sum of basal area per species per block
head(Trees)
#calculate basal area per plot per time period
#for all species >10cm DBH
Trees$BA<-NULL
Trees$BA_Q<-NULL
Trees$BA_F<-NULL
for (i in 1:nrow(Trees)){
  Trees$BA[i]<-ifelse(Trees$DBH[i]>10,(Trees$DBH[i]^2*(pi/4))/400,0)
  Trees$BA_F[i]<-ifelse(Trees$DBH[i]>10&Trees$Species[i]=="F",(Trees$DBH[i]^2*(pi/4))/400,0)
  Trees$BA_Q[i]<-ifelse(Trees$DBH[i]>10&Trees$Species[i]=="Q",(Trees$DBH[i]^2*(pi/4))/400,0)
}

#get trait values for light, moisture and nitrogen from databases
my_traits<-tr8(unique(as.character(Trees$Sp_name)))
traits_df<-extract_traits(my_traits)

#put trait values into tree df
Trees$Light<-NA
Trees$Moist<-NA
Trees$Nit<-NA
for (i in 1:nrow(traits_df)){
  Trees$Light<-ifelse(Trees$Sp_name==row.names(traits_df)[i],traits_df$ell_light_uk[i],Trees$Light)
  Trees$Moist<-ifelse(Trees$Sp_name==row.names(traits_df)[i],traits_df$ell_moist_uk[i],Trees$Moist)
  Trees$Nit<-ifelse(Trees$Sp_name==row.names(traits_df)[i],traits_df$ell_N[i],Trees$Nit)
}

#multiply by BA to allow weighting in community weighted mean calculations
Trees$Light2<-Trees$Light*Trees$BA
Trees$Moist2<-Trees$Moist*Trees$BA
Trees$Nit2<-Trees$Nit*Trees$BA
  

#organise data for community analysis
#for stem density
Sp<-ddply(Trees, .(Block,Year,Species),summarize,Count=sum(Status))
#and basal area
Sp_BA<-ddply(Trees, .(Block,Year,Species),summarize,Sum_BA=sum(BA))


#and for analysis of basal area, stem density and species richness
BA<-ddply(Trees, .(Block,Year),summarize,SD=sum(Status),BA=sum(BA),BA_F=sum(BA_F),BA_Q=sum(BA_Q),Sp_R=length(unique(Species)),
          Light=sum(Light2),Moist=sum(Moist2),Nit=sum(Nit2))

BA$Light<-BA$Light/BA$BA
BA$Moist<-BA$Moist/BA$BA
BA$Nit<-BA$Nit/BA$BA

#put species as columns and rows as blocks for each year
Sp2<-dcast(Sp,Block+Year~Species,value.var="Count")
Sp_BA2<-dcast(Sp_BA,Block+Year~Species,value.var="Sum_BA")

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
Trees2$Light2<-Trees2$Light*Trees2$BA_sa
Trees2$Moist2<-Trees2$Moist*Trees2$BA_sa
Trees2$Nit2<-Trees2$Nit*Trees2$BA_sa

Sap_BA<-ddply(Trees2, .(Block,Year),summarize,SD=sum(Status),BA=sum(BA_sa),BA_F=sum(BA_F),BA_Q=sum(BA_Q),Sp_R=length(unique(Species)),
              Light=sum(Light2,na.rm = T),Moist=sum(Moist2,Light2,na.rm = T),Nit=sum(Nit2,Light2,na.rm = T))

Sap_BA$Light<-Sap_BA$Light/Sap_BA$BA
Sap_BA$Moist<-Sap_BA$Moist/Sap_BA$BA
Sap_BA$Nit<-Sap_BA$Nit/Sap_BA$BA

#put species as columns and rows as blocks for each year
Sap_Sp2<-dcast(Sap_Sp,Block+Year~Species,value.var="Count")

#now set up loop to carry out similarity analysis comparing each block to itself in 1964
#first using stem density

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
#next using basal area
Sp_BA2
Blocks_BA<-unique(Sp_BA2$Block)
Sor_similarity_BA<-NULL
for (i in 1:length(Blocks_BA)){
  Block_subset<-subset(Sp_BA2,Block==Blocks_BA[i])
  Block_subset[is.na(Block_subset)]<-0
  Block_subset2<-Block_subset[-c(1:2)]
  Block_subset2[is.na(Block_subset2)]<-0
  Block_subset$Sorensen<-c(1,1-vegdist(Block_subset2)[1:nrow(Block_subset2)-1])
  Sor_similarity_BA<-rbind(Sor_similarity_BA,Block_subset)
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
colnames(BA_comb)<-c("Block","Year","SDM","BAM","BAFM","BAQM","SPRM","LightM","MoistM","NitM","SDS","BAS","BAFS","BAQS","SPRS",
                     "LightS","MoistS","NitS")

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
    Block_subset$BAQFPERCM[v]<-((Block_subset$BAQM[v]+Block_subset$BAFM[v])-(Block_subset$BAQM[1]+Block_subset$BAFM[1]))/(Block_subset$BAQM[1]+Block_subset$BAFM[1])
    Block_subset$BAQFRAWM[v]<-((Block_subset$BAQM[v]+Block_subset$BAFM[v])-(Block_subset$BAQM[1]+Block_subset$BAFM[1]))
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
    Block_subset$BAQFPERCS[v]<-((Block_subset$BAQS[v]+Block_subset$BAFS[v])-(Block_subset$BAQS[1]+Block_subset$BAFS[1]))/(Block_subset$BAQS[1]+Block_subset$BAFS[1])
    Block_subset$BAQFRAWS[v]<-((Block_subset$BAQS[v]+Block_subset$BAFS[v])-(Block_subset$BAQS[1]+Block_subset$BAFS[1]))
    Block_subset$SDPERCS[v]<-(Block_subset$SDS[v]-Block_subset$SDS[1])/Block_subset$SDS[1]
    Block_subset$SDRAWS[v]<-(Block_subset$SDS[v]-Block_subset$SDS[1])
    Block_subset$SPRPERCS[v]<-(Block_subset$SPRS[v]-Block_subset$SPRS[1])/Block_subset$SPRS[1]
    Block_subset$SPRRAWS[v]<-(Block_subset$SPRS[v]-Block_subset$SPRS[1])
    Block_subset$LightPercM[v]<-(Block_subset$LightM[v]-Block_subset$LightM[1])/Block_subset$LightM[1]
    Block_subset$LightRawM[v]<-(Block_subset$LightM[v]-Block_subset$LightM[1])
    Block_subset$MoistPercM[v]<-(Block_subset$MoistM[v]-Block_subset$MoistM[1])/Block_subset$MoistM[1]
    Block_subset$MoistRawM[v]<-(Block_subset$MoistM[v]-Block_subset$MoistM[1])
    Block_subset$NitPercM[v]<-(Block_subset$NitM[v]-Block_subset$NitM[1])/Block_subset$NitM[1]
    Block_subset$NitRawM[v]<-(Block_subset$NitM[v]-Block_subset$NitM[1])
    Block_subset$LightPercS[v]<-(Block_subset$LightS[v]-Block_subset$LightS[1])/Block_subset$LightS[1]
    Block_subset$LightRawS[v]<-(Block_subset$LightS[v]-Block_subset$LightS[1])
    Block_subset$MoistPercS[v]<-(Block_subset$MoistS[v]-Block_subset$MoistS[1])/Block_subset$MoistS[1]
    Block_subset$MoistRawS[v]<-(Block_subset$MoistS[v]-Block_subset$MoistS[1])
    Block_subset$NitPercS[v]<-(Block_subset$NitS[v]-Block_subset$NitS[1])/Block_subset$NitS[1]
    Block_subset$NitRawS[v]<-(Block_subset$NitS[v]-Block_subset$NitS[1])
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
Sor_BA3<-merge(Sor_similarity_BA,Sor_BA2,by=c("Block","Year"),all=T)
head(Sor_BA3)
Sor_BA4<-subset(Sor_BA3,Year>=1964)
str(Sor_BA4)
colnames(Sor_BA3)[3:34]<-c("A_BA","B_BA","Cr_BA","F_BA","Fr_BA","I_BA","PM_BA","Q_BA","Sc_BA","T_BA","Sor_BA","AS","BS","CrS","FS","IS","PMS","QS","ScS","TS","SorS","AM","BM","CrM","FM","FrM","IM","PMM","QM","ScM","TM","SorM")


#reclass 1996 plots as 1999 for grouping
Sor_BA4$Year2<-ifelse(Sor_BA3$Year==1996,1999,Sor_BA3$Year)
#add column to identify transect
Sor_BA4$Transect<-ifelse(Sor_BA3$Block>51,"Unenclosed","Enclosed")

#add location data to this
Sor_BA4<-merge(Sor_BA3,Location,by.x="Block",by.y="Plot_number")

head(Sor_BA4)

#save all this as a csv
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
write.csv(Sor_BA4,"Denny_plots.csv",row.names=F)

#create a subset of this containing just data on the collapse gradient
keeps1<-c("Block","Year","BAM","BAPERCM","Northing","Easting")
BA_grad<-Sor_BA4[keeps1]
Block_7<-data.frame(Block=c(7,7),Year=c(1996,2014),BAM=c(0,0),BAPERCM=c(-1,-1),Northing=c(106786,106786),Easting=c(433045,433045))
BA_grad<-rbind(BA_grad,Block_7)

setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
write.csv(BA_grad,"BA_gradient.csv",row.names=F)

#create a subset of this containing just data on the collapse gradient and mature tree basal area
keeps2<-c("Block","Year","BAM","BAPERCM","A_BA","B_BA","Cr_BA","F_BA","Fr_BA","I_BA","PM_BA","Q_BA","Sc_BA","T_BA","Northing","Easting")
BA_ab<-Sor_BA4[keeps2]
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
write.csv(BA_ab,"BA_tree_ab.csv",row.names=F)

#create a subset of this containing just data on the collapse gradient and mature tree traits
keeps3<-c("Block","Year","BAM","BAPERCM","LightM","MoistM","NitM","LightPercM","MoistPercM","NitPercM","Northing","Easting")
BA_traits<-Sor_BA4[keeps3]
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
write.csv(BA_traits,"BA_tree_traits.csv",row.names=F)

#create a subset of this containing just data on the collapse gradient and mature tree community
keeps3<-c("Block","Year","BAM","BAPERCM","Sor_BA","SorM","SPRM","Northing","Easting")
BA_comm<-Sor_BA4[keeps3]
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
write.csv(BA_comm,"BA_tree_comm.csv",row.names=F)


