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
BA_SP_R<-merge(BA_ab,Rel_SpR,by=c("Block","Year"),all=T)
BA_Comm<-merge(BA_SP_R,Sor_similarity2,by=c("Block","Year"),all=T)

#save this ouput
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
write.csv(BA_Comm,"BA_GF.csv",row.names=F)

#######################################################################
#add data on traits####################################################
######################################################################

GF_melt$variable2<-gsub("\\."," ",GF_melt$variable)
GF_melt$variable2<-ifelse(GF_melt$variable2=="Agrostis spp ","Agrostis spp",GF_melt$variable2)

#remove rows with ground cover as a species label
GF_melt<-subset(GF_melt,variable!="Ground_cover")

#now match traits to species in plots

#make sure species levels are the same

GF_melt$variable2<-factor(GF_melt$variable2, levels=levels(Traits$Name))

GF_melt_Traits<-merge(GF_melt,Traits,by.x="variable2",by.y="Name")

GF_melt_Traits2<-GF_melt_Traits[rownames(unique(GF_melt_Traits[,1:3])),]

#now multiply trait value by abundance of species
GF_melt_Traits2$Light<-GF_melt_Traits2$L*GF_melt_Traits2$value
GF_melt_Traits2$Moist<-GF_melt_Traits2$F*GF_melt_Traits2$value
GF_melt_Traits2$Nit<-GF_melt_Traits2$N*GF_melt_Traits2$value
GF_melt_Traits2$Hab_B<-GF_melt_Traits2$Hab_breadth*GF_melt_Traits2$value


#now sum all these values for each block by year and divide by total cover in block
Comm_means<-NULL
Block_year<-unique(GF_melt_Traits2[,2:3])
Block_year<-Block_year[with(Block_year, order(Year)), ]
for (i in 1:nrow(Block_year)){
  Comm_sub<-subset(GF_melt_Traits2,Block==Block_year$Block[i])
  Comm_sub<-subset(Comm_sub,Year==Block_year$Year[i])
  Comm_sub_means<-data.frame(unique(Comm_sub[c("Block", "Year")]))
  Sum_cov<-sum(Comm_sub$value,na.rm = T)
  Comm_sub_means$Cover<-ifelse(Sum_cov==0,0,Sum_cov)
  Comm_sub_means$Hab_B<-ifelse(Sum_cov==0,0,sum(Comm_sub$Hab_B,na.rm = T)/Sum_cov)
  Comm_sub_means$Moist<-ifelse(Sum_cov==0,0,sum(Comm_sub$Moist,na.rm = T)/Sum_cov)
  Comm_sub_means$Nit<-ifelse(Sum_cov==0,0,sum(Comm_sub$Nit,na.rm = T)/Sum_cov)
  Comm_sub_means$Light<-ifelse(Sum_cov==0,0,sum(Comm_sub$Light,na.rm = T)/Sum_cov)
  Comm_means<-rbind(Comm_sub_means,Comm_means)
}

#now compare community mean trait values to what they were in 1964
Blocks<-unique(Comm_means$Block)
Comm_means2<-NULL
for (i in 1:length(Blocks)){
  Block_subset<-subset(Comm_means,Block==Blocks[i])
  Block_subset<-Block_subset[with(Block_subset, order(Year)), ]
  Block_subset[is.na(Block_subset)]<-0
  Block_subset$Hab_B_Change<-Block_subset$Hab_B-Block_subset$Hab_B[1]
  Block_subset$Moist_Change<-Block_subset$Moist-Block_subset$Moist[1]
  Block_subset$Nit_Change<-Block_subset$Nit-Block_subset$Nit[1]
  Block_subset$Light_Change<-Block_subset$Light-Block_subset$Light[1]
  Comm_means2<-rbind(Comm_means2,Block_subset)
}

#now bring together all of this data into one table
BA_Comm_traits<-merge(BA_Comm,Comm_means2,by=c("Block","Year"),all=T)


#classify into groups
Groups<-data.frame(max=c(-0.75,-0.50,-0.25,0,3),min=c(-1.00,-0.75,-0.50,-0.25,0),group=c(5,4,3,2,1))
head(BA_Comm_traits)
BA_Comm_traits_groups<-NULL
for(i in 1:nrow(Groups)){
  BA_Comm_traits2<-subset(BA_Comm_traits,BA_Comm_traits$BAPERCM>Groups[i,2]&BA_Comm_traits$BAPERCM<Groups[i,1])
  BA_Comm_traits2$Group<-Groups[i,3]
  BA_Comm_traits_groups<-rbind(BA_Comm_traits_groups,BA_Comm_traits2)
}

colnames(BA_Comm_traits_groups)<-c("Block","Year","Perc_Cov","Perc_Cov_change","BAPERCM","BAM","Sp_R","Sp_R_Ch","Sorensen","Cover","Hab_B","Moist","Nit","Light","Hab_B_Change","Moist_change","Nit_Change","Light_Change","Coll_Group")

#save this ouput
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
write.csv(BA_Comm_traits_groups,"BA_GF_ALL.csv",row.names=F)

