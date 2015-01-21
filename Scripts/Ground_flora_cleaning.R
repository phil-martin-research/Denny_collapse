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
library(vegan)

#load in data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("BA_gradient.csv")
GF<-read.csv("GF_ab_nw.csv")
Grass<-read.csv("Reclass_grass.csv")
Traits<-read.csv("Reclass_traits.csv")

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

#######################################################################
#add data on traits####################################################
######################################################################

GF_melt$variable2<-gsub("\\."," ",GF_melt$variable)

#remove rows with ground cover as a species label
GF_melt<-subset(GF_melt,variable!="Ground_cover")

#now match traits to species in plots

#make sure species levels are the same
head(Traits)

GF_melt$variable2<-factor(GF_melt$variable2, levels=levels(Traits$Name))


GF_melt2<-merge(GF_melt,Traits,by.x="variable2",by.y="Name")

GF_melt3<-GF_melt2[rownames(unique(GF_melt2[,1:3])),]

head(GF_melt3)

#now multiply trait value by abundance of species
GF_melt3$Light<-GF_melt3$L*GF_melt3$value
GF_melt3$Moist<-GF_melt3$F*GF_melt3$value
GF_melt3$Nit<-GF_melt3$N*GF_melt3$value
GF_melt3$Hab_B<-GF_melt3$Hab_breadth*GF_melt3$value


#now sum all these values for each block by year and divide by total cover in block
Comm_means<-NULL
Block_year<-unique(GF_melt[,1:2])
for (i in 1:nrow(Block_year)){
  Comm_sub<-subset(GF_melt3,Block==Block_year$Block[2])
  Comm_sub<-subset(Comm_sub,Year==Block_year$Year[2])
  head(Comm_sub)
  Comm_sub_means<-data.frame(unique(Comm_sub[c("Block", "Year")]))
  if sum(Comm_sub$Value,na.rm = T)==0 
  Comm_sub_means$Cover<-0
  Comm_sub_means$Hab_B<-0
  Comm_sub_means$Moist<-0
  Comm_sub_means$Nit<-0
  Comm_sub_means$Light<-0
  else
    Comm_sub_means$Cover<-sum(Comm_sub$Cover)
    Comm_sub_means$Hab_B<-sum(Comm_sub$Hab_B)/sum(Comm_sub$Cover)
    Comm_sub_means$Moist<-sum(Comm_sub$Moist)/sum(Comm_sub$Cover)
    Comm_sub_means$Nit<-sum(Comm_sub$Nit)/sum(Comm_sub$Cover)
    Comm_sub_means$Light<-sum(Comm_sub$Light)/sum(Comm_sub$Cover)

}

