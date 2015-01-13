#script for organising ground flora traits

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(vegan)
library(reshape2)
library(lme4)
library(nlme)
library(MuMIn)
library(quantreg)
library(car)
library(TR8)
library(Taxonstand)

#load in data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
GF<-read.csv("GF_ab_nw.csv")

#downoad local ecoflora db
EF<-ECOFLORA_df
head(EF)

#replace na with zeros in dataframe
GF[is.na(GF)] <- 0
#remove data from plots 26, 44 and 45 which have incomplete records
GF<-subset(GF,Block!=26)
GF<-subset(GF,Block!=44)
GF<-subset(GF,Block!=45)
GF[GF==2001]<-199
#produce counts of species per block per year
GF_melt<-melt(GF,id =c("Block","Year") )
head(GF_melt)

#now download traits
#first clean data to get correct species names
GF_melt2<-subset(GF_melt,variable!="Agrostis.spp.")
GF_melt2<-subset(GF_melt,variable!="Ground_cover")
GF_melt2<-subset(GF_melt,variable!="Glyceria sp")
Other_sp<-gsub("[.]"," ",as.vector(unique(GF_melt$variable)))
Other_sp<-as.vector(Other_sp)
Other_sp<-replace(Other_sp,Other_sp=="Taraxacum officinalis agg ","Taraxacum officinalis")
Other_sp<-replace(Other_sp,Other_sp=="Athyrium filix femina ","Athyrium filix-femina")
Other_sp<-replace(Other_sp,Other_sp=="Dryopteris filix mas ","Dryopteris filix")
Other_sp<-replace(Other_sp,Other_sp=="Juncus buffonius","Juncus bufonius")
Other_sp<-replace(Other_sp,Other_sp=="Montia fontanum","Montia fontana")
Other_sp<-replace(Other_sp,Other_sp=="Ranunculus sarduous","Ranunculus sardous")
Other_sp<-replace(Other_sp,Other_sp=="Senecio sylvatica","Senecio sylvaticus")
Other_sp<-replace(Other_sp,Other_sp=="Taraxacum officinalis","Taraxacum officinale agg")

#however two  species is not identified to species level only to genus
#and as such we averaged accross all species in the genus found in the UK
Agrostis<-as.vector(EF$species[grep("Agrostis",EF$species)])
Trait_Ag<-tr8(Agrostis)
Trait_Ag<-data.frame(Trait_Ag@results)

#similarly Glyceria sp are not identified to species level
Glyceria<-data.frame(EF$species[grep("Glyceria",EF$species)])
Glyceria<-as.vector(Glyceria[-(3:4),])
Trait_Gly<-tr8(Glyceria)
Trait_Gly<-data.frame(Trait_Gly@results)



#now download
other_traits<-tr8(Other_sp)
other_traits<-data.frame(other_traits@results)
other_traits$species<-row.names(other_traits)
row.names(other_traits)<-NULL

#now merge trait and species data
#first for data where only genus data were available
#produce a mean of this and substitute it in for the values

#Agrostis
Trait_Ag$species<-"Agrostis.spp."
Trait_Ag$ell_light_uk<-as.numeric(as.character(Trait_Ag$ell_light_uk))
Trait_Ag$ell_moist_uk<-as.numeric(as.character(Trait_Ag$ell_moist_uk))
Trait_Ag$ell_N<-as.numeric(as.character(Trait_Ag$ell_N))
Ag_means<-colMeans(Trait_Ag, na.rm = T, dims = 1)
str(Ag_means)

#Glyceria
Trait_Gly$species<-"Glyceria sp"
Trait_Gly$ell_light_uk<-as.numeric(as.character(Trait_Gly$ell_light_uk))
Trait_Gly$ell_moist_uk<-as.numeric(as.character(Trait_Gly$ell_moist_uk))
Trait_Gly$ell_N<-as.numeric(as.character(Trait_Gly$ell_N))
Gly_means<-colMeans(Trait_Gly, na.rm = T, dims = 1)
str(Gly_means)

#now substitute in values
#bind all df first
All_Sp_traits<-rbind(Trait_Ag,Trait_Gly,other_traits)
All_Sp_traits$ell_light_uk<-as.numeric(as.character(All_Sp_traits$ell_light_uk))
All_Sp_traits$ ell_moist_uk<-as.numeric(as.character(All_Sp_traits$ ell_moist_uk))
All_Sp_traits$ell_N<-as.numeric(as.character(All_Sp_traits$ell_N))

plot(All_Sp_traits$ell_light_uk,All_Sp_traits$ell_N)

str(All_Sp_traits)
aggregate(All_Sp_traits$ell_light_uk, list(as.factor(All_Sp_traits$species)), mean,na.rm=TRUE)


All_Sp_traits<-replace(All_Sp_traits,All_Sp_traits$species=="Taraxacum officinalis","Taraxacum officinalis agg")
All_Sp_traits<-replace(All_Sp_traits,All_Sp_traits$species=="Athyrium filix-femina","Athyrium filix femina")
All_Sp_traits<-replace(All_Sp_traits,All_Sp_traits$species=="Dryopteris filix","Dryopteris filix mas")
All_Sp_traits<-replace(All_Sp_traits,All_Sp_traits$species=="Juncus bufonius","Juncus buffonius")
All_Sp_traits<-replace(All_Sp_traits,All_Sp_traits$species=="Montia fontana","Montia fontanum")
All_Sp_traits<-replace(All_Sp_traits,All_Sp_traits$species=="Ranunculus sardous","Ranunculus sarduous")
All_Sp_traits<-replace(All_Sp_traits,All_Sp_traits$species=="Senecio sylvaticus","Senecio sylvatica")
All_Sp_traits<-replace(All_Sp_traits,All_Sp_traits$species=="Taraxacum officinale agg","Taraxacum officinalis")

#
GF_melt$Light<-NA
GF_melt$Moist<-NA
GF_melt$N<-NA





for (i in 1:nrow(GF_melt)){
  if GF_melt$variable[i]==
}
