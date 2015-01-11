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

head(GF)
str(GF)

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
#remove rows with NAs
GF_melt2<-GF_melt[complete.cases(GF_melt),]
GF_melt3<-subset(GF_melt2,value>0)
GF_Sp_R<-count(GF_melt3,vars = c("Block","Year"))


#####################################################
# ground flora abundance#############################
#####################################################

#plot changes in GF species abundances over disturbance gradient
ggplot(GF_melt2,aes(x=Year,y=value,group=Block))+geom_point()+geom_line()+facet_wrap(~variable)

#subset data to only include species with abundances >10%
Ab_max<-aggregate(GF_melt2$value,list(GF_melt2$variable),max)
colnames(Ab_max)<-c("Species","Ab")
Ab_max2<-subset(Ab_max,Ab>10)
#run a loop to remove species with abundance <10%
GF_melt4<-NULL
for (i in 1:nrow(Ab_max2)){
  GF_melt3<-GF_melt2[GF_melt2$variable == Ab_max2[i,1], ]
  GF_melt4<-rbind(GF_melt4,GF_melt3)
}


#plot changes in GF species abundances over time
theme_set(theme_bw(base_size=12))
Ab_time1<-ggplot(GF_melt4,aes(x=Year,y=value,group=Block))+geom_point(alpha=0.2)+geom_line(alpha=0.2)+facet_wrap(~variable)+geom_smooth(aes(group=NULL),se=F,method=lm,size=3)
Ab_time1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ylab("Species richness of ground flora")

#calculate changes in species abundance relative to first survey
#do this using a loop and calculating the log response ratio log(x)-log(y)
head(GF_melt4)
GF_melt5<-GF_melt4[,-c(2,4)]
Species<-unique(GF_melt5)
Rel_Ab<-NULL
for (i in 1:nrow(Species)){
Cov_block<-subset(GF_melt4,variable==Species[i,2]&Block==Species[i,1])  
Cov_block$PCC<-Cov_block$value-Cov_block$value[1]
Rel_Ab<-rbind(Rel_Ab,Cov_block)
}

#merge data on abundances to data on BA change
BA2<-subset(BA,select=c("Year","Block","BAPERCM","BAM"))
BA_ab<-merge(Rel_Ab,BA2,by=c("Block","Year"))

#plot abundances of species of against BA gradient
ggplot(BA_ab,aes(x=BAPERCM,y=value))+geom_point(size=3,shape=1,aes(colour=as.factor(Year)))+facet_wrap(~variable,scales = "free_y")+geom_smooth(method=glm,family="poisson",se=F,size=3,alpha=0.1,aes(colour=NULL))
ggplot(BA_ab,aes(x=BAM,y=value))+geom_point(size=3,shape=1,aes(colour=as.factor(Year)))+facet_wrap(~variable,scales = "free_y")+geom_smooth(method=glm,family="poisson",se=F,size=3,alpha=0.1,aes(colour=NULL))

#plot changes in abundance vs BA gradient
ggplot(BA_ab,aes(x=BAPERCM,y=PCC))+geom_point(size=3,shape=1,aes(colour=as.factor(Year)))+facet_wrap(~variable,scales = "free_y")+geom_smooth(method=glm,se=F,size=3,alpha=0.1,aes(colour=NULL))
ggplot(BA_ab,aes(x=BAM,y=PCC))+geom_point(size=3,shape=1,aes(colour=as.factor(Year)))+facet_wrap(~variable,scales = "free_y")+geom_smooth(method=glm,se=F,size=3,alpha=0.1,aes(colour=NULL))


#analyse this change in abundance
#first - agrostis spp.
Ag_ab<-subset(BA_ab,variable=="Agrostis.spp.")

#null model
M0.1_Ag<-lmer(qlogis((PCC+60)/200)~1+(1|Block),data=Ag_ab)
M0.2_Ag<-lmer(qlogis((PCC+60)/200)~1+(Block|Year),data=Ag_ab)

M1_Ag<-lmer(qlogis((PCC+60)/200)~BAPERCM+(1|Block),data=Ag_ab)
M2_Ag<-lmer(qlogis((PCC+60)/200)~BAPERCM+I(BAPERCM^2)+(1|Block),data=Ag_ab)
M3_Ag<-lmer(qlogis((PCC+60)/200)~BAPERCM+I(BAPERCM^2)+I(BAPERCM^3)+(1|Block),data=Ag_ab)
M4_Ag<-lmer(qlogis((PCC+60)/200)~log(BAPERCM+1)+(1|Block),data=Ag_ab)
plot(M1_Ag)
plot(M2_Ag)
AICc(M0.1_Ag,M1_Ag,M2_Ag,M3_Ag,M4_Ag)

plot(Ag_ab$BAPERCM,Ag_ab$PCC)
points(Ag_ab$BAPERCM,(((plogis(predict(M1_Ag,re.form=NA)))*200)-60),col="red")


ggplot(Ag_ab,aes(x=BAPERCM,y=PCC))+geom_point()+facet_wrap(~Year)+geom_smooth(se=F)

#next juncus effusus
JU_ab<-subset(BA_ab,variable=="Juncus.effusus")
#null model
M0.1_JU<-lmer(qlogis((PCC+60)/200)~1+(1|Block),data=JU_ab)
M0.2_JU<-lmer(qlogis((PCC+60)/200)~1+(Block|Year),data=JU_ab)
AICc(M0.1_JU,M0.2_JU)

M1_JU<-lmer(qlogis((PCC+60)/200)~BAPERCM+(Block|Year),data=JU_ab)
plot(M1_JU)
AICc(M0.2_JU,M1_JU)
r.squaredGLMM(M1_JU)

plot(JU_ab$BAPERCM,JU_ab$PCC)
points(JU_ab$BAPERCM,(((plogis(predict(M1_JU,re.form=NA)))*200)-60),col="red")

#next Pteridium aquilinum
PA_ab<-subset(BA_ab,variable=="Pteridium.aquilinum")
#null model
M0.1_PA<-lmer(qlogis((PCC+60)/200)~1+(1|Block),data=PA_ab)
M0.2_PA<-lmer(qlogis((PCC+60)/200)~1+(Block|Year),data=PA_ab)
AICc(M0.1_PA,M0.2_PA)

M1_PA<-lmer(qlogis((PCC+60)/200)~BAPERCM+(1|Block),data=PA_ab)
M2_PA<-lmer(qlogis((PCC+60)/200)~BAPERCM+I(BAPERCM^2)+(1|Block),data=PA_ab)

plot(M1_PA)
plot(M2_PA)
AICc(M0.1_PA,M1_PA,M2_PA)
#the null model is best in this case


#now ground cover
GC_ab<-subset(BA_ab,variable=="Ground_cover")
#null model
M0.1_GC<-lmer(qlogis((PCC+60)/200)~1+(1|Block),data=GC_ab)
M0.2_GC<-lmer(qlogis((PCC+60)/200)~1+(Block|Year),data=GC_ab)
AICc(M0.1_GC,M0.2_GC)

M1_GC<-lmer(qlogis((PCC+60)/200)~BAPERCM+(1|Block),data=GC_ab)
M2_GC<-lmer(qlogis((PCC+60)/200)~BAPERCM+I(BAPERCM^2)+(1|Block),data=GC_ab)

r.squaredGLMM(M1_GC)

plot(M1_GC)
plot(M2_GC)
AICc(M0.1_GC,M1_GC,M2_GC)

plot(GC_ab$BAPERCM,GC_ab$PCC)
points(GC_ab$BAPERCM,(((plogis(predict(M1_GC,re.form=NA)))*200)-60),col="red")

plot(seq(1,100),-log(seq(1,100)))


#now produce a summary figure of all of this
#first produce a dataframe with predictions from all the best models along with species names
BA_PERC1<-data.frame(BAPERCM=seq(min(BA_ab$BAPERCM),max(BA_ab$BAPERCM),length.out = 200))
BA_PERC2<-data.frame(BAPERCM=BA_PERC1$BAPERCM+1)
Ag.pred<-data.frame(BAPERCM=BA_PERC1$BAPERCM,PCC=(((plogis(predict(M2_Ag,newdata=BA_PERC1,re.form=NA)))*200)-60),variable="Agrostis.spp.")
Gc.pred<-data.frame(BAPERCM=BA_PERC1$BAPERCM,PCC=(((plogis(predict(M1_GC,newdata=BA_PERC1,re.form=NA)))*200)-60),variable="Ground_cover")
JU.pred<-data.frame(BAPERCM=BA_PERC1$BAPERCM,PCC=(((plogis(predict(M1_JU,newdata=BA_PERC1,re.form=NA)))*200)-60),variable="Juncus.effusus")
All_ab_pred<-rbind(Ag.pred,Gc.pred,JU.pred)

BA_AB1<-ggplot(BA_ab,aes(x=BAPERCM,y=PCC,colour=as.factor(Year)))+geom_point(shape=1,size=3)+facet_wrap(~variable,scales = "free_y")
BA_AB2<-BA_AB1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ylab("Change in percentage cover since 1964")+xlab("Percentage change in mature tree basal area since 1964")
BA_AB2+geom_line(data=All_ab_pred,aes(colour=NULL),size=2,alpha=0.8)+scale_colour_discrete(name="Year of measurements")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("GF_Ab_change.png",width = 8,height=6,units = "in",dpi=300)



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
