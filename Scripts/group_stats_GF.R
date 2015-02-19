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
AICc(M0_Spr,M1_Spr,M2_Spr,M3_Spr)
r.squaredGLMM(M2_Spr)

summary(M2_Spr)

coefs <- data.frame(coef(summary(M1_Spr)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M1_Spr)[1]
coefs$Estimate[2:nrow(coefs)]<-coefs$Estimate[2:nrow(coefs)]+coefs$Estimate[1]
coefs$Estimate<-round(coefs$Estimate,2)
coefs$Std..Error<-round(coefs$Std..Error,2)
coefs$t.value<-round(coefs$t.value,2)
coefs$p.z<-round(coefs$p.z,2)
coefs$Level<-c("Stable","0-25%","25-50%","50-75%","75-100%")
colnames(coefs)<-c("Estimate","SE","T.Value","P.value","R_squared","Level")
head(coefs)
write.csv(coefs,"Figures/Spr_sum_GF.csv")

#Look at sorensen for each group
M0_Sor<-lmer(qlogis(Sorensen2)~1+(1|Block),data=GF)
M1_Sor<-lmer(qlogis(Sorensen2)~as.factor(Coll_Group)+(1|Block),data=GF)
AICc(M0_Sor,M1_Sor,M2_Sor,M3_Sor)
r.squaredGLMM(M3_Sor)

summary(M3_Sor)

coefs <- data.frame(coef(summary(M1_Sor)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M1_Sor)[1]
coefs$Estimate[2:nrow(coefs)]<-coefs$Estimate[2:nrow(coefs)]+coefs$Estimate[1]
coefs$Estimate<-round(coefs$Estimate,2)
coefs$Std..Error<-round(coefs$Std..Error,2)
coefs$t.value<-round(coefs$t.value,2)
coefs$p.z<-round(coefs$p.z,2)
coefs$Level<-c("Stable","0-25%","25-50%","50-75%","75-100%")
colnames(coefs)<-c("Estimate","SE","T.Value","P.value","R_squared","Level")
head(coefs)
coefs$Estimate<-plogis(coefs$Estimate)
write.csv(coefs,"Figures/Sor_sum_GF.csv")

#Look at grass cover for each group
M0_Grass<-lmer(qlogis(Perc_Cov2)~1+(1|Block),data=GF)
M1_Grass<-lmer(qlogis(Perc_Cov2)~as.factor(Coll_Group)+(1|Block),data=GF)

AICc(M0_Grass,M1_Grass,M2_Grass,M3_Grass)
r.squaredGLMM(M3_Grass)

plot(M3_Grass)

coefs <- data.frame(coef(summary(M1_Grass)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M1_Grass)[1]
coefs$Estimate[2:nrow(coefs)]<-coefs$Estimate[2:nrow(coefs)]+coefs$Estimate[1]
coefs$Estimate<-round(coefs$Estimate,2)
coefs$Std..Error<-round(coefs$Std..Error,2)
coefs$t.value<-round(coefs$t.value,2)
coefs$p.z<-round(coefs$p.z,2)
coefs$Level<-c("Stable","0-25%","25-50%","50-75%","75-100%")
colnames(coefs)<-c("Estimate","SE","T.Value","P.value","R_squared","Level")
head(coefs)
coefs$Estimate<-plogis(coefs$Estimate)*100
head(coefs)
write.csv(coefs,"Figures/Grass_sum.csv")


#look at trait values for each group - light

M0_Light<-lmer(Light~1+(1|Block),data=GF)
M1_Light<-lmer(Light~as.factor(Coll_Group)+(1|Block),data=GF)
AICc(M0_Light,M1_Light,M2_Light,M3_Light)
r.squaredGLMM(M1_Light)

plot(M2_Light)

coefs <- data.frame(coef(summary(M1_Light)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M1_Light)[1]
coefs$Estimate[2:nrow(coefs)]<-coefs$Estimate[2:nrow(coefs)]+coefs$Estimate[1]
coefs$Estimate<-round(coefs$Estimate,2)
coefs$Std..Error<-round(coefs$Std..Error,2)
coefs$t.value<-round(coefs$t.value,2)
coefs$p.z<-round(coefs$p.z,2)
coefs$Level<-c("Stable","0-25%","25-50%","50-75%","75-100%")
colnames(coefs)<-c("Estimate","SE","T.Value","P.value","R_squared","Level")
head(coefs)
write.csv(coefs,"Figures/Light_sum_F.csv")


#look at trait values for each group - Nitrogen

M0_Nit<-lmer(Nit~1+(1|Block),data=GF)
M1_Nit<-lmer(Nit~as.factor(Coll_Group)+(1|Block),data=GF)

AICc(M0_Nit,M1_Nit,M2_Nit,M3_Nit)
r.squaredGLMM(M2_Nit)

plot(M2_Nit)

coefs <- data.frame(coef(summary(M1_Nit)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M1_Nit)[1]
coefs$Estimate[2:nrow(coefs)]<-coefs$Estimate[2:nrow(coefs)]+coefs$Estimate[1]
coefs$Estimate<-round(coefs$Estimate,2)
coefs$Std..Error<-round(coefs$Std..Error,2)
coefs$t.value<-round(coefs$t.value,2)
coefs$p.z<-round(coefs$p.z,2)
coefs$Level<-c("Stable","0-25%","25-50%","50-75%","75-100%")
colnames(coefs)<-c("Estimate","SE","T.Value","P.value","R_squared","Level")
head(coefs)
write.csv(coefs,"Figures/Nit_sum_GF.csv")

