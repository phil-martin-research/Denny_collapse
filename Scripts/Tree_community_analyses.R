#script to calculate changes in community similarity in Denny wood over time#

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(vegan)
library(reshape2)


#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Denny<-read.csv("Denny_cleaned.csv")
Denny<-subset(Denny,Block_new!=24&25&40)
sort(unique(Denny$Block_new))
Denny$Status<-ifelse(is.na(Denny$Status2),0,Denny$Status2)
Tree_ID<-read.csv("Tree_ID.csv")
Tree_ID2<-unique(Tree_ID[c("Tree_ID", "In.Out")])

#merge tree_ID data and denny data to find trees inside and outside of the transect
Denny<-merge(Denny,Tree_ID2,by="Tree_ID")
Denny<-subset(Denny,In.Out=="In")
head(Denny)
Denny_species<-Denny[,c(1,3,6,8:11,13)]
head(Denny_species)
Denny_species<-subset(Denny_species,En.Un=="Enclosed")



#produce counts of species per block per year
Sp_counts<-count(Denny_species,vars = c("Sp","Block_new","Year"))
head(Sp_counts)
#and put in form that is usable by vegan
Sp_counts2<-dcast(Sp_counts,Block_new + Year ~Sp)

#now set up loop to carry out similarity analysis comparing each block to itself in 1959
Blocks<-unique(Sp_counts2$Block_new)
Sor_similarity<-NULL

for (i in 1:length(Blocks)){
  Block_subset<-subset(Sp_counts2,Block_new==Blocks[i])
  Block_subset[is.na(Block_subset)]<-0
  Block_subset2<-Block_subset[-c(1:2)]
  Block_subset2[is.na(Block_subset2)]<-0
  Block_subset$Sorensen<-c(1,1-vegdist(Block_subset2)[1:nrow(Block_subset2)-1])
  Sor_similarity<-rbind(Sor_similarity,Block_subset)
}


#plot of similarity change over time
head(Sor_similarity)
ggplot(Sor_similarity,aes(x=Year,y=Sorensen,group=Block_new))+geom_point()+geom_line()+facet_wrap(~Block_new)

#add ndms plot here
Sp_counts2[is.na(Sp_counts2)]<-0
#remove block and year
Sp_counts3<-Sp_counts2[,-c(1:2)]

#compute distance matricies
vare.dis<-vegdist(Sp_counts3)
vare.mds0<-metaMDS(vare.dis)

#create dataframe for nmds
NMDS<-data.frame(MDS1 = vare.mds0$points[,1], MDS2 = vare.mds0$points[,2],Year=Sp_counts2$Year,Plot=Sp_counts2$Block_new)
ggplot(NMDS,aes(x=MDS1,y=MDS2,colour=as.factor(Year),group=Plot))+geom_point()+facet_wrap(~Year)

#add elipses around points
ord<-ordiellipse(vare.mds0, NMDS$Year, display = "sites", kind = "se", conf = 0.95, label = T)
df_ell <- data.frame()
for(y in unique(NMDS$Year)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$Year==y,],
            veganCovEllipse(ord[[y]]$cov,ord[[y]]$center,ord[[y]]$scale))),group=y))
}

#create figure with ellipses
ggplot(NMDS,aes(x=MDS1,y=MDS2,colour=as.factor(Year),group=Plot))+geom_point(size=4,alpha=0.8)+facet_wrap(~Year)
