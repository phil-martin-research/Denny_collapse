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


#merge data on abundances to data on BA change
BA2<-subset(BA,select=c("Year","Block","BAPERCM","BAM"))
BA_ab<-merge(GF_melt4,BA2,by=c("Block","Year"))

#plot abundances of species of against BA gradient
ggplot(BA_ab,aes(x=BAPERCM,y=value))+geom_point(size=3,shape=1,aes(colour=as.factor(Year)))+facet_wrap(~variable,scales = "free_y")+geom_smooth(method=glm,family="poisson",se=F,size=3,alpha=0.1,aes(colour=NULL))
ggplot(BA_ab,aes(x=BAM,y=value))+geom_point(size=3,shape=1,aes(colour=as.factor(Year)))+facet_wrap(~variable,scales = "free_y")+geom_smooth(method=glm,family="poisson",se=F,size=3,alpha=0.1,aes(colour=NULL))


######################################################
#ground flora species richness########################
######################################################

#plot this over time
ggplot(GF_Sp_R,aes(x=Year,y=freq,group=Block))+geom_line()

#merge species richness data to plot data
GF_Sp_BA<-merge(GF_Sp_R,BA,by=c("Block","Year"))
str(GF_Sp_BA)
#plot of relationship between species richness against collapse gradient
#null model
Rich_M0.1<-lmer(sqrt(freq)~1+(Block|Year),data=GF_Sp_BA)

#linear relationship - this is really the only logical relationship I can think of
Rich_M1<-glmer(freq~BAPERCM+(1|Block),data=GF_Sp_BA,family=poisson)
Rich_M1<-lmer(sqrt(freq)~BAPERCM+(Block|Year),data=GF_Sp_BA)

plot(Rich_M1)

AICc(Rich_M1,Rich_M0.1)
r.squaredGLMM(Rich_M1)
#the model is the best we have but doesn't say too much - the descriptive power is low
#r squared <0.1

#produce predictions for this model
#first produce variable to use for prediction
summary(GF_Sp_BA$BAPERCM)

BAPERCM<-data.frame(seq(from=min(GF_Sp_BA$BAPERCM),to=max(GF_Sp_BA$BAPERCM),length.out = 500))
colnames(BAPERCM)<-"BAPERCM"
freq<-predict(Rich_M1,newdata=BAPERCM,re.form=NA)^2
Rich_pred<-cbind(freq,BAPERCM)


#plot richness against BA
theme_set(theme_bw(base_size=12))
Rich_BA<-ggplot(GF_Sp_BA,aes(x=BAPERCM,y=freq))+geom_point(aes(colour=as.factor(Year)))
Rich_BA2<-Rich_BA+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ylab("Species richness of ground flora")
Rich_BA2+geom_line(data=Rich_pred,aes(x=BAPERCM,y=freq))



#############################################################
#community similarity for ground flora#######################
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
plot(M1.2)
summary(M1.2) 

r.squaredGLMM(M1.2)






