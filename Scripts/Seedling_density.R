#seedling analysis
rm(list=ls(all=TRUE))
Seedlings<-read.csv("Data/Seedlings_denny.csv",header = T,stringsAsFactors=F)
BA<-read.csv("Data/BA_gradient.csv",header = T,stringsAsFactors=F)
BA<-subset(BA,Year==2014)


#load packages
library(ggplot2)
library(lme4)
library(MuMIn)
library(plyr)
library(reshape2)
library(tidyr)
library(dplyr)


#tidy data
Seedlings$Plot<-sapply(strsplit(Seedlings$Plot2,"-"),'[',2)
drop<-"Plot2"
Seedlings<-Seedlings[,!(names(Seedlings) %in% drop)]
head(Seedlings)
Seedlings2<-subset(Seedlings,Species=="Fagus sylvatica"|Species=="Quercus spp"|Species=="Ilex aquifolium")
Seed_melt<-melt(Seedlings2,id.vars = c("Plot","Count"))
Seed_melt2<-ddply(Seed_melt,.(Plot,value),summarize,Count=sum(Count)*100)
Seed_melt3<-spread(Seed_melt2, value, Count)
Seed_melt3[is.na(Seed_melt3)] <- 0
colnames(Seed_melt3)<-c("Block","Beech","Holly","Oak")
Seed_melt4<-merge(Seed_melt3,BA,by="Block",all=T)

Seed_melt4$Collapse<-NA
for (i in 1:nrow(Seed_melt4)){
  Seed_melt4$Collapse[i]<-ifelse(Seed_melt4$BAPERCM[i]<=-0.25,1,0)
}

Seed_melt5<-subset(Seed_melt4,!is.na(Collapse))

#plot seedling density against basal area change
ggplot(Seed_melt5,aes(x=BAPERCM*-1,y=Beech))+geom_point()+geom_smooth(se=F,method=glm,family=poisson)
ggplot(Seed_melt5,aes(x=BAPERCM*-1,y=Holly))+geom_point()+geom_smooth(se=F,method=glm,family=poisson)
ggplot(Seed_melt5,aes(x=BAPERCM*-1,y=Oak))+geom_point()+geom_smooth(se=F,method=glm,family=poisson)

#plot seedling density for collapsed and stable plots
ggplot(Seed_melt5,aes(x=as.factor(Collapse),y=Beech*100))+geom_violin()+geom_jitter()
ggplot(Seed_melt5,aes(x=as.factor(Collapse),y=Holly*100))+geom_violin()+geom_jitter()
ggplot(Seed_melt5,aes(x=as.factor(Collapse),y=Oak*100))+geom_violin()+geom_jitter()

#now produce model of the seedling densities for beech, oak and holly
#comparing collapsed and uncollapsed plots
#first, beech
M0<-glm(Beech~1,family=poisson,data=Seed_melt5)
M1<-glm(Beech~as.factor(Collapse),family=poisson,data=Seed_melt5)
model_list<-list(M0,M1)
Null_dev<-deviance(M0)
deviance(M1)
Model_sel<-model.sel(model_list)
Model_sel$R_2<-c(1-(deviance(M1)/Null_dev),0)
Model_avg<-model.avg(Model_sel)
summary(Model_avg)


#now oak
M0<-glm(Oak~1,family=poisson,data=Seed_melt5)
M1<-glm(Oak~as.factor(Collapse),family=poisson,data=Seed_melt5)
model_list<-list(M0,M1)
Null_dev<-deviance(M0)
deviance(M1)
Model_sel<-model.sel(model_list)
Model_sel$R_2<-c(1-(deviance(M1)/Null_dev),0)
Model_avg<-model.avg(Model_sel)
summary(Model_avg)

exp(4.80584)
exp(4.80584+0.01741)-exp(4.80584)
exp(4.80584-0.23144)
exp(4.80584-0.23144+0.02481)-exp(4.80584-0.23144)

exp(7.551557+0.595012+0.005314)-exp(7.551557+0.595012)


#now holly
M0<-glm(Holly~1,family=poisson,data=Seed_melt5)
M1<-glm(Holly~as.factor(Collapse),family=poisson,data=Seed_melt5)
model_list<-list(M0,M1)
Null_dev<-deviance(M0)
deviance(M1)
Model_sel<-model.sel(model_list)
Model_sel$R_2<-c(1-(deviance(M1)/Null_dev),0)
Model_avg<-model.avg(Model_sel)
summary(Model_avg)

exp(7.551557+0.004411)-exp(7.551557)
exp(7.551557+0.595012+0.005314)-exp(7.551557+0.595012)
