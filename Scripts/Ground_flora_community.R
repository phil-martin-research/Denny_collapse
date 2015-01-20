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
library(lme4)
library(nlme)
library(MuMIn)
library(quantreg)
library(car)

#load in data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("Denny_plots.csv")
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

#add column to identify species as grass - this needs fixing
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

############################################################
#analysis of change in grass abundance######################
############################################################
Grass_ab<-subset(BA_ab,Year>1964)
Grass_ab$PCC<-ifelse(Grass_ab$PCC>100,100,Grass_ab$PCC)

#null model
M0.1_G<-lmer(qlogis((PCC+4)/200)~1+(1|Block),data=Grass_ab)
M0.2_G<-lmer(qlogis((PCC+4)/200)~1+(Block|Year),data=Grass_ab)
AICc(M0.1_G,M0.2_G)#the more simple model seems fine so we go with that

M1_G<-lmer(qlogis((PCC+4)/200)~BAPERCM+(Block|Year),data=Grass_ab)
M2_G<-lmer(qlogis((PCC+4)/200)~BAPERCM+I(BAPERCM^2)+(Block|Year),data=Grass_ab)
M3_G<-lmer(qlogis((PCC+4)/200)~BAPERCM+I(BAPERCM^2)+I(BAPERCM^3)+(Block|Year),data=Grass_ab)
M4_G<-lmer(qlogis((PCC+4)/200)~log(BAPERCM+1)+(Block|Year),data=Grass_ab)
plot(M1_G)
plot(M2_G)
plot(M3_G)
plot(M4_G)


AICc(M0.2_G,M1_G,M2_G,M3_G,M4_G)

plot(Grass_ab$BAPERCM,Grass_ab$PCC)
points(Grass_ab$BAPERCM,(((plogis(predict(M3_G,re.form=NA)))*200)-4),col="red")





######################################################
#ground flora species richness########################
######################################################

#plot this over time
ggplot(GF_Sp_R,aes(x=Year,y=freq,group=Block))+geom_line()

#do a comparison of species richness in 1964 to that now
head(GF_Sp_R)


Block_SpR<-unique(GF_Sp_R$Block)
Rel_SpR<-NULL
for (i in 1:nrow(Species)){
  SpR_block<-subset(GF_Sp_R,Block==Block_SpR[i])  
  SpR_block$PSpR<-log(SpR_block$freq)-log(SpR_block$freq[1])
  Rel_SpR<-rbind(Rel_SpR,SpR_block)
}



#merge species richness data to plot data
GF_Sp_BA<-merge(Rel_SpR,BA,by=c("Block","Year"))
str(GF_Sp_BA)

#plot of relationship between species richness against collapse gradient
#null model
Rich_M0.1<-lmer(PSpR~1+(1|Block),data=Rel_SpR)
Rich_M0.2<-lmer(PSpR~1+(Block|Year),data=Rel_SpR)

#linear relationship - this is really the only logical relationship I can think of
Rich_M1<-lmer(PSpR~BAPERCM+(1|Block),data=GF_Sp_BA)
Rich_M2<-lmer(PSpR~BAPERCM+I(BAPERCM^2)+(1|Block),data=GF_Sp_BA)

plot(Rich_M1)
plot(Rich_M2)

AICc(Rich_M0.1,Rich_M1,Rich_M2)
r.squaredGLMM(Rich_M1)
#the model is the best we have but doesn't say too much - the descriptive power is low
#r squared <0.1

#produce predictions for this model
#first produce variable to use for prediction
summary(GF_Sp_BA$BAPERCM)

BAPERCM<-data.frame(seq(from=min(GF_Sp_BA$BAPERCM),to=max(GF_Sp_BA$BAPERCM),length.out = 500))
colnames(BAPERCM)<-"BAPERCM"
PSpR<-(exp(predict(Rich_M1,newdata=BAPERCM,re.form=NA)))-1
Rich_pred<-cbind(PSpR,BAPERCM)


#plot richness against BA
theme_set(theme_bw(base_size=12))
Rich_BA<-ggplot(GF_Sp_BA,aes(x=BAPERCM*100,y=(exp(PSpR)-1)*100))+geom_point(shape=1,size=3,aes(colour=as.factor(Year)))
Rich_BA2<-Rich_BA+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ylab("Percentage change in species richness of ground flora since 1964")+xlab("Percentage change in basal area since 1964")
Rich_BA2+geom_line(data=Rich_pred,aes(x=BAPERCM*100,y=PSpR*100))+scale_colour_discrete(name="Year of measurements")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("GF_SPR_change.png",width = 8,height=6,units = "in",dpi=300)



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
Sor_similarity2<-subset(Sor_similarity,Year>1964)

str(Sor_similarity2)

#merge community composition data to plot data
BA3<-subset(BA,select=c(BAPERCM,Year,Block))
GF_BA_2<-merge(BA,Sor_similarity2,by=c("Block","Year"))
GF_BA_3<-subset(GF_BA_2, select=c(BAM,BAPERCM,Sorensen,Year,Block))
ggplot(GF_BA_3,aes(x=BAPERCM,y=Sorensen,group=Block,colour=as.factor(Year)))+geom_point()


#these plots seem to show a reduction in similarity with increasing basal area loss, but let's do this properly
#with statistics!

#first a null model
M0.1<-lmer(plogis(Sorensen)~1+(1|Block),data=GF_BA_3)
M0.2<-lmer(plogis(Sorensen)~1+(Block|Year),data=GF_BA_3)
AICc(M0.1,M0.2)
plot(M0.1)
plot(M0.2)

#we go with formation of M0.2 becuase AICc is lowest
#now add fixed terms
M1<-lmer(plogis(Sorensen)~BAPERCM+(1|Block),data=GF_BA_3)
plot(M1)#seems a bit shit, need to work out box cox transformation for this
summary(M1) #random effects are also highly correlated so I will centre them
GF_BA_3$BAPERCM2<-GF_BA_3$BAPERCM-mean(GF_BA_3$BAPERCM)
GF_BA_3$Block2<-GF_BA_3$Block-mean(GF_BA_3$Block)
#try again
M1.2<-lmer(plogis(Sorensen)~BAPERCM2+(1|Block),data=GF_BA_3)
M2.2<-lmer(plogis(Sorensen)~BAPERCM2+I(BAPERCM2^2)+(1|Block),data=GF_BA_3)

plot(M1.2)
plot(M2.2)
AICc(M0.2,M1.2,M2.2)
r.squaredGLMM(M2.2)

#plot this non-relationship
GF_sor1<-ggplot(GF_BA_3,aes(x=BAPERCM*100,y=Sorensen,group=Block,colour=as.factor(Year)))+geom_point(size=3,shape=1)
GF_sor2<-GF_sor1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ylab("Sorenson similarity to community present in 1964")+xlab("Percentage change in basal area since 1964")
GF_sor2+scale_colour_discrete(name="Year of measurements")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("GF_Sorenson.png",width = 8,height=6,units = "in",dpi=300)
