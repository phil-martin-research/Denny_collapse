#script to produce group statistics for woody species

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)

#load in data
BA<-read.csv("Data/Denny_plots.csv")

head(BA)

#calculate Tanner index the mean of Sorensen weighted by BA and SD
BA$Tanner<-(BA$Sor_BA+BA$SorM)/2

#classify into groups
Groups<-data.frame(max=c(-0.75,-0.50,-0.25,0,3),min=c(-1.00,-0.75,-0.50,-0.25,0),group=c(5,4,3,2,1))
head(BA)
BA_groups<-NULL
for(i in 1:nrow(Groups)){
  BA2<-subset(BA,BA$BAPERCM>Groups[i,2]&BA$BAPERCM<Groups[i,1])
  BA2$Group<-Groups[i,3]
  BA_groups<-rbind(BA_groups,BA2)
}

head(BA_groups)

#look at Sp_R for each group
M0_Spr<-lmer(SPRM~1+(1|Block),data=BA_groups)
M1_Spr<-lmer(SPRM~as.factor(Group)+(1|Block),data=BA_groups)
M2_Spr<-lmer(SPRM~as.factor(Group)+Year+(1|Block),data=BA_groups)
M3_Spr<-lmer(SPRM~as.factor(Group)*Year+(1|Block),data=BA_groups)
AICc(M0_Spr,M1_Spr,M2_Spr,M3_Spr)
r.squaredGLMM(M1_Spr)

coefs <- data.frame(coef(summary(M1_Spr)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M1_Spr)[1]

write.csv(coefs,"Figures/SpR_W_sum.csv")


#Look at sorensen for each group - BA
M0_Sor<-lmer(qlogis(Sor_BA)~1+(1|Block),data=BA_groups)
M1_Sor<-lmer(qlogis(Sor_BA)~as.factor(Group)+(1|Block),data=BA_groups)
M2_Sor<-lmer(qlogis(Sor_BA)~as.factor(Group)+as.factor(Year)+(1|Block),data=BA_groups)
M3_Sor<-lmer(qlogis(Sor_BA)~as.factor(Group)*as.factor(Year)+(1|Block),data=BA_groups)
AICc(M0_Sor,M1_Sor,M2_Sor,M3_Sor)
r.squaredGLMM(M2_Sor)

summary(M2_Sor)

coefs <- data.frame(coef(summary(M2_Sor)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M2_Sor)[1]
write.csv(coefs,"Figures/Sor_W_sum.csv")

#Look at sorensen for each group - SD
M0_Sor_SD<-lmer(qlogis(SorM)~1+(1|Block),data=BA_groups)
M1_Sor_SD<-lmer(qlogis(SorM)~as.factor(Group)+(1|Block),data=BA_groups)
M2_Sor_SD<-lmer(qlogis(SorM)~as.factor(Group)+as.factor(Year)+(1|Block),data=BA_groups)
M3_Sor_SD<-lmer(qlogis(SorM)~as.factor(Group)*as.factor(Year)+(1|Block),data=BA_groups)
AICc(M0_Sor_SD,M1_Sor_SD,M2_Sor_SD,M3_Sor_SD)
r.squaredGLMM(M2_Sor)

summary(M2_Sor_SD)

coefs <- data.frame(coef(summary(M2_Sor_SD)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M2_Sor_SD)[1]
write.csv(coefs,"Figures/SorSD_W_sum.csv")

#Look at Tanner index for each group
M0_Tanner<-lmer(qlogis(Tanner)~1+(1|Block),data=BA_groups)
M1_Tanner<-lmer(qlogis(Tanner)~as.factor(Group)+(1|Block),data=BA_groups)
M2_Tanner<-lmer(qlogis(Tanner)~as.factor(Group)+as.factor(Year)+(1|Block),data=BA_groups)
M3_Tanner<-lmer(qlogis(Tanner)~as.factor(Group)*as.factor(Year)+(1|Block),data=BA_groups)
AICc(M0_Tanner,M1_Tanner,M2_Tanner,M3_Tanner)
r.squaredGLMM(M2_Tanner)

coefs <- data.frame(coef(summary(M2_Tanner)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M2_Tanner)[1]
write.csv(coefs,"Figures/Tanner_sum_W.csv")

#Look at stem density for each group
M0_SD<-lmer(SDM~1+(1|Block),data=BA_groups)
M1_SD<-lmer(SDM~as.factor(Group)+(1|Block),data=BA_groups)
M2_SD<-lmer(SDM~as.factor(Group)+as.factor(Year)+(1|Block),data=BA_groups)
M3_SD<-lmer(SDM~as.factor(Group)*as.factor(Year)+(1|Block),data=BA_groups)
AICc(M0_SD,M1_SD,M2_SD,M3_SD)
r.squaredGLMM(M3_SD)
plot(M3_SD)

coefs <- data.frame(coef(summary(M3_SD)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M3_SD)[1]
write.csv(coefs,"Figures/SD_sum_W.csv")


#look at trait values for each group - light

M0_LightM<-lmer(LightM~1+(1|Block),data=BA_groups)
M1_LightM<-lmer(LightM~as.factor(Group)+(1|Block),data=BA_groups)
M2_LightM<-lmer(LightM~as.factor(Group)+as.factor(Year)+(1|Block),data=BA_groups)
M3_LightM<-lmer(LightM~as.factor(Group)*as.factor(Year)+(1|Block),data=BA_groups)
AICc(M0_LightM,M1_LightM,M2_LightM,M3_LightM)
r.squaredGLMM(M1_LightM)
plot(M1_LightM)
summary(M1_LightM)

coefs <- data.frame(coef(summary(M1_LightM)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M1_LightM)[1]
write.csv(coefs,"Figures/Light_sum_W.csv")


#look at trait values for each group - Nitrogen
M0_NitM<-lmer(NitM~1+(1|Block),data=BA_groups)
M1_NitM<-lmer(NitM~as.factor(Group)+(1|Block),data=BA_groups)
M2_NitM<-lmer(NitM~as.factor(Group)+as.factor(Year)+(1|Block),data=BA_groups)
M3_NitM<-lmer(NitM~as.factor(Group)*as.factor(Year)+(1|Block),data=BA_groups)
AICc(M0_NitM,M1_NitM,M2_NitM,M3_NitM)
r.squaredGLMM(M1_NitM)
plot(M1_NitM)
summary(M1_NitM)

plot(M1_Nit)

coefs <- data.frame(coef(summary(M1_NitM)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M1_NitM)[1]
write.csv(coefs,"Figures/Nit_sum_W.csv")
