#script to clean ground flora data and output in various useful forms
#!!!! - WARNING THIS SCRIPT TAKES A LONG TIME TO RUN, SO ONLY DO SO IF ABSOLUTELY NECESSARY

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)
library(TR8)
library(Taxonstand)

#load in data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("BA_gradient.csv")
GF<-read.csv("GF_ab_nw.csv")
Grass<-read.csv("Reclass_grass.csv")

#replace na with zeros in dataframe
GF[is.na(GF)] <- 0

#remove data from plots 26, 44 and 45 which have incomplete records
GF<-subset(GF,Block!=26)
GF<-subset(GF,Block!=44)
GF<-subset(GF,Block!=45)
GF[GF==2001]<-1999

#produce counts of species per block per year
GF_melt<-melt(GF,id =c("Block","Year") )
head(GF_melt)
#remove rows with NAs
GF_melt2<-GF_melt[complete.cases(GF_melt),]

#and species richness
GF_melt3<-subset(GF_melt2,value>0)
GF_Sp_R<-count(GF_melt3,vars = c("Block","Year"))

#####################################################
# ground flora abundance#############################
#####################################################

#remove the sum of all ground cover
GF_melt2<-subset(GF_melt2,variable!="Ground_cover")

#remove space from species names for Grass table
Grass$Species<-gsub(" ", "", Grass$Species, fixed = TRUE)
Grass$Species<-as.factor(Grass$Species)
Grass$Species2<-as.numeric(Grass$Species)

GF_melt2$variable<- factor(GF_melt2$variable, levels=levels(Grass$Species))
GF_melt2$variable2<-as.numeric(GF_melt2$variable)

#add column to identify species as grass
GF_melt2$Grass<-"No"
for (i in 1:nrow(Grass)){
  for (y in 1:nrow(GF_melt2)){
    GF_melt2$Grass[y]<-ifelse(GF_melt2$variable[y]==Grass$Species[i],as.character(Grass$FF[i]),as.character(GF_melt2$Grass[y]))
  }
}

GF_melt2$Grass<-as.factor(GF_melt2$Grass)

#subset to only include grass species
Grass_species<-subset(GF_melt2,Grass=="Grass")
#sum the cover of grassy species
Grass_cover<-aggregate(Grass_species$value, list(Grass_species$Block,Grass_species$Year), sum)
colnames(Grass_cover)<-c("Block","Year","Perc_C")

#calculate changes in grass abundance relative to first survey
#do this using a loop and calculating the raw percentage difference
head(Grass_cover)
Grass_cover<-Grass_cover[with(Grass_cover, order(Year)), ]

Blocks<-unique(Grass_cover$Block)
Rel_Ab<-NULL
for (i in 1:length(Blocks)){
  Cov_block<-subset(Grass_cover,Block==Blocks[i])  
  Cov_block$PCC<-Cov_block$Perc_C-Cov_block$Perc_C[1]
  Rel_Ab<-rbind(Rel_Ab,Cov_block)
}

#merge data on abundances to data on BA change
BA2<-subset(BA,select=c("Year","Block","BAPERCM","BAM"))
BA_ab<-merge(Rel_Ab,BA2,by=c("Block","Year"))


#####################################################
#species richness calculations#######################
#####################################################
GF_Sp_R<-GF_Sp_R[with(GF_Sp_R, order(Year)), ]

Block_SpR<-unique(GF_Sp_R$Block)
Rel_SpR<-NULL
for (i in 1:length(Block_SpR)){
  SpR_block<-subset(GF_Sp_R,Block==Block_SpR[i])  
  SpR_block$PSpR<-log(SpR_block$freq)-log(SpR_block$freq[1])
  Rel_SpR<-rbind(Rel_SpR,SpR_block)
}

#############################################################
#Community similarity for ground flora#######################
#############################################################

Blocks<-unique(GF$Block)
Sor_similarity<-NULL
for (i in 1:length(Blocks)){
  Block_subset<-subset(GF,Block==Blocks[i])
  Block_subset<-Block_subset[with(Block_subset, order(Year)), ]
  Block_subset[is.na(Block_subset)]<-0
  Block_subset2<-Block_subset[-c(1:2)]
  Block_subset2<-Block_subset2[-c(71)]
  Block_subset$Sorensen<-c(1,1-vegdist(Block_subset2)[1:nrow(Block_subset2)-1])
  Sor_similarity<-rbind(Sor_similarity,Block_subset)
}

Sor_similarity2<-Sor_similarity[c("Block","Year","Sorensen")]

#merge these data together
BA_SP_R<-merge(BA_ab,Rel_SpR,by=c("Block","Year"))
BA_Comm<-merge(BA_SP_R,Sor_similarity2,by=c("Block","Year"))

#save this ouput
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
write.csv(BA_Comm,"BA_GF.csv",row.names=F)

#downoad local ecoflora db
EF<-ECOFLORA_df
head(EF)

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



#however two  species are not identified to species level only to genus
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
Ag_means<-data.frame(mean(Trait_Ag$ell_light_uk,na.rm = T),mean(Trait_Ag$ell_moist_uk,na.rm = T),mean(Trait_Ag$ell_N,na.rm = T),species="Agrostis.spp.")
colnames(Ag_means)<-c("ell_light_uk","ell_moist_uk","ell_N","species")
str(Ag_means)

#Glyceria
Trait_Gly$species<-"Glyceria sp"
Trait_Gly$ell_light_uk<-as.numeric(as.character(Trait_Gly$ell_light_uk))
Trait_Gly$ell_moist_uk<-as.numeric(as.character(Trait_Gly$ell_moist_uk))
Trait_Gly$ell_N<-as.numeric(as.character(Trait_Gly$ell_N))
Gly_means<-data.frame(mean(Trait_Gly$ell_light_uk,na.rm = T),mean(Trait_Gly$ell_moist_uk,na.rm = T),mean(Trait_Gly$ell_N,na.rm = T),species="Glyceria sp")
colnames(Gly_means)<-c("ell_light_uk","ell_moist_uk","ell_N","species")
str(Gly_means)

#now substitute in values
#bind all df first
All_Sp_traits<-rbind(Ag_means,Gly_means,other_traits)
All_Sp_traits$ell_light_uk<-as.numeric(as.character(All_Sp_traits$ell_light_uk))
All_Sp_traits$ ell_moist_uk<-as.numeric(as.character(All_Sp_traits$ ell_moist_uk))
All_Sp_traits$ell_N<-as.numeric(as.character(All_Sp_traits$ell_N))

#remove rows with ground cover as a species label
All_Sp_traits2<-subset(All_Sp_traits,species!="Ground_cover")

#add loop to change names back to those in dataframe
All_Sp_traits2[]<-lapply(All_Sp_traits2,as.character)

for (i in 1:nrow(All_Sp_traits2)){
  All_Sp_traits2$species[i]<-ifelse(All_Sp_traits2$species[i]=="Taraxacum officinalis","Taraxacum officinalis agg",All_Sp_traits2$species[i])
  All_Sp_traits2$species[i]<-ifelse(All_Sp_traits2$species[i]=="Athyrium filix-femina","Athyrium filix femina",All_Sp_traits2$species[i])
  All_Sp_traits2$species[i]<-ifelse(All_Sp_traits2$species[i]=="Dryopteris filix","Dryopteris filix mas",All_Sp_traits2$species[i])
  All_Sp_traits2$species[i]<-ifelse(All_Sp_traits2$species[i]=="Juncus bufonius","Juncus bufonius",All_Sp_traits2$species[i])
  All_Sp_traits2$species[i]<-ifelse(All_Sp_traits2$species[i]=="Montia fontana","Montia fontanum",All_Sp_traits2$species[i])
  All_Sp_traits2$species[i]<-ifelse(All_Sp_traits2$species[i]=="Ranunculus sardous","Ranunculus sarduous",All_Sp_traits2$species[i])
  All_Sp_traits2$species[i]<-ifelse(All_Sp_traits2$species[i]=="Senecio sylvaticus","Senecio sylvatica",All_Sp_traits2$species[i])
  All_Sp_traits2$species[i]<-ifelse(All_Sp_traits2$species[i]=="Taraxacum officinalis","Taraxacum officinalis agg",All_Sp_traits2$species[i])
}

All_Sp_traits2$species<-gsub(" ",".",All_Sp_traits2$species)
All_Sp_traits2$species<-as.factor(All_Sp_traits2$species)
All_Sp_traits2$ell_light_uk<-as.numeric(All_Sp_traits2$ell_light_uk)
All_Sp_traits2$ell_moist_uk<-as.numeric(All_Sp_traits2$ell_moist_uk)
All_Sp_traits2$ell_N<-as.numeric(All_Sp_traits2$ell_N)


#now match traits to species in plots
GF_melt$Light<-NA
GF_melt$Moist<-NA
GF_melt$N<-NA
GF_melt<-subset(GF_melt,variable!="ground_cover")

test<-data.frame(sort(levels(GF_melt$variable)),sort(levels(All_Sp_traits2$species)))
test$same<-NA
head(test)
for (i in 1:nrow(test)){
  test$same<-test[1,1]==test[1,2]
}


#make sure species levels are the same
GF_melt$variable <- factor(GF_melt$variable, levels=levels(All_Sp_traits2$species))

for (i in 1:nrow(All_Sp_traits2)){
  for (y in 1:nrow(GF_melt)){
    GF_melt$Light[y]<-ifelse(GF_melt$variable[y]==All_Sp_traits2$species[i],All_Sp_traits2$ell_light_uk[i],GF_melt$Light[y])
    GF_melt$Moist[y]<-ifelse(GF_melt$variable[y]==All_Sp_traits2$species[i],All_Sp_traits2$ell_moist_uk[i],GF_melt$Moist[y])
    GF_melt$N[y]<-ifelse(GF_melt$variable[y]==All_Sp_traits2$species[i],All_Sp_traits2$ell_N[i],GF_melt$N[y])  
  }
}

head(GF_melt)

