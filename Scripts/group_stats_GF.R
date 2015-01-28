#script to produce group statistics for ground flora 

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)
library(lme4)
library(MuMIn)

#load in data
GF<-read.csv("Data/BA_GF_ALL.csv")

head(GF)

GF$Sorensen2<-ifelse(GF$Sorensen==0,0.001,GF$Sorensen)
GF$Sorensen2<-ifelse(GF$Sorensen==1,0.9999,GF$Sorensen2)
GF$Perc_Cov2<-GF$Perc_Cov/100
GF$Perc_Cov2<-ifelse(GF$Perc_Cov2==0,0.001,GF$Perc_Cov2)
GF$Perc_Cov2<-ifelse(GF$Perc_Cov2>1,.9999,GF$Perc_Cov2)


#look at Sp_R for each group
M0_Spr<-lmer(Sp_R~1+(1|Block),data=GF)
M1_Spr<-lmer(Sp_R~as.factor(Coll_Group)+(1|Block),data=GF)
M2_Spr<-lmer(Sp_R~as.factor(Coll_Group)+Year+(1|Block),data=GF)
M3_Spr<-lmer(Sp_R~as.factor(Coll_Group)*Year+(1|Block),data=GF)
AICc(M0_Spr,M1_Spr,M2_Spr,M3_Spr)
r.squaredGLMM(M1_Spr)

summary(M1_Spr)

#Look at sorensen for each group
M0_Sor<-lmer(qlogis(Sorensen2)~1+(1|Block),data=GF)
M1_Sor<-lmer(qlogis(Sorensen2)~as.factor(Coll_Group)+(1|Block),data=GF)
M2_Sor<-lmer(qlogis(Sorensen2)~as.factor(Coll_Group)+Year+(1|Block),data=GF)
M3_Sor<-lmer(qlogis(Sorensen2)~as.factor(Coll_Group)*Year+(1|Block),data=GF)
AICc(M0_Sor,M1_Sor,M2_Sor,M3_Sor)
r.squaredGLMM(M3_Sor)

summary(M3_Sor)

#Look at grass cover for each group
M0_Grass<-lmer(qlogis(Perc_Cov2)~1+(1|Block),data=GF)
M1_Grass<-lmer(qlogis(Perc_Cov2)~as.factor(Coll_Group)+(1|Block),data=GF)
M2_Grass<-lmer(qlogis(Perc_Cov2)~as.factor(Coll_Group)+Year+(1|Block),data=GF)
M3_Grass<-lmer(qlogis(Perc_Cov2)~as.factor(Coll_Group)*Year+(1|Block),data=GF)
AICc(M0_Grass,M1_Grass,M2_Grass,M3_Grass)
r.squaredGLMM(M3_Grass)

plot(M3_Grass)


#look at trait values for each group - light

M0_Light<-lmer(Light~1+(1|Block),data=GF)
M1_Light<-lmer(Light~as.factor(Coll_Group)+(1|Block),data=GF)
M2_Light<-lmer(Light~as.factor(Coll_Group)+Year+(1|Block),data=GF)
M3_Light<-lmer(Light~as.factor(Coll_Group)*Year+(1|Block),data=GF)
AICc(M0_Light,M1_Light,M2_Light,M3_Light)
r.squaredGLMM(M2_Light)

plot(M3_Light)

#look at trait values for each group - Nitrogen

M0_Nit<-lmer(Nit~1+(1|Block),data=GF)
M1_Nit<-lmer(Nit~as.factor(Coll_Group)+(1|Block),data=GF)
M2_Nit<-lmer(Nit~as.factor(Coll_Group)+Year+(1|Block),data=GF)
M3_Nit<-lmer(Nit~as.factor(Coll_Group)*Year+(1|Block),data=GF)
AICc(M0_Nit,M1_Nit,M2_Nit,M3_Nit)
r.squaredGLMM(M1_Nit)

plot(M1_Nit)

#look at trait values for each group - Moist

M0_Moist<-lmer(Moist~1+(1|Block),data=GF)
M1_Moist<-lmer(Moist~as.factor(Coll_Group)+(1|Block),data=GF)
M2_Moist<-lmer(Moist~as.factor(Coll_Group)+Year+(1|Block),data=GF)
M3_Moist<-lmer(Moist~as.factor(Coll_Group)*Year+(1|Block),data=GF)
AICc(M0_Moist,M1_Moist,M2_Moist,M3_Moist)
r.squaredGLMM(M1_Moist)

plot(M1_Moist)

