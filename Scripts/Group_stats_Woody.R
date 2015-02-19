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
AICc(M0_Spr,M1_Spr)
r.squaredGLMM(M1_Spr)

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

write.csv(coefs,"Figures/SpR_W_sum.csv",row.names = F)


#Look at Tanner index for each group
M0_Tanner<-lmer(qlogis(Tanner)~1+(1|Block),data=BA_groups)
M1_Tanner<-lmer(qlogis(Tanner)~as.factor(Group)+(1|Block),data=BA_groups)

AICc(M0_Tanner,M1_Tanner)
r.squaredGLMM(M1_Tanner)

coefs <- data.frame(coef(summary(M1_Tanner)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M1_Tanner)[1]
coefs$Estimate[2:nrow(coefs)]<-coefs$Estimate[2:nrow(coefs)]+coefs$Estimate[1]
coefs$Estimate<-round(coefs$Estimate,2)
coefs$Std..Error<-round(coefs$Std..Error,2)
coefs$t.value<-round(coefs$t.value,2)
coefs$p.z<-round(coefs$p.z,2)
coefs$Level<-c("Stable","0-25%","25-50%","50-75%","75-100%")
colnames(coefs)<-c("Estimate","SE","T.Value","P.value","R_squared","Level")
head(coefs)
coefs$Estimate<-plogis(coefs$Estimate)

write.csv(coefs,"Figures/Tanner_sum_W.csv",row.names = F)

#Look at stem density for each group
M0_SD<-lmer(SDM~1+(1|Block),data=BA_groups)
M1_SD<-lmer(SDM~as.factor(Group)+(1|Block),data=BA_groups)
AICc(M0_SD,M1_SD)
r.squaredGLMM(M1_SD)
plot(M1_SD)

coefs <- data.frame(coef(summary(M1_SD)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M1_SD)[1]
coefs$Estimate[2:nrow(coefs)]<-coefs$Estimate[2:nrow(coefs)]+coefs$Estimate[1]
coefs$Estimate<-round(coefs$Estimate,2)
coefs$Std..Error<-round(coefs$Std..Error,2)
coefs$t.value<-round(coefs$t.value,2)
coefs$p.z<-round(coefs$p.z,2)
coefs$Level<-c("Stable","0-25%","25-50%","50-75%","75-100%")
colnames(coefs)<-c("Estimate","SE","T.Value","P.value","R_squared","Level")
head(coefs)
write.csv(coefs,"Figures/SD_sum_W.csv")


#look at trait values for each group - light

M0_LightM<-lmer(LightM~1+(1|Block),data=BA_groups)
M1_LightM<-lmer(LightM~as.factor(Group)+(1|Block),data=BA_groups)
AICc(M0_LightM,M1_LightM,M2_LightM,M3_LightM)
r.squaredGLMM(M1_LightM)
plot(M1_LightM)
summary(M1_LightM)

coefs <- data.frame(coef(summary(M1_LightM)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M1_LightM)[1]
coefs$Estimate[2:nrow(coefs)]<-coefs$Estimate[2:nrow(coefs)]+coefs$Estimate[1]
coefs$Estimate<-round(coefs$Estimate,2)
coefs$Std..Error<-round(coefs$Std..Error,2)
coefs$t.value<-round(coefs$t.value,2)
coefs$p.z<-round(coefs$p.z,2)
coefs$Level<-c("Stable","0-25%","25-50%","50-75%","75-100%")
colnames(coefs)<-c("Estimate","SE","T.Value","P.value","R_squared","Level")
head(coefs)
write.csv(coefs,"Figures/Light_sum_W.csv")


#look at trait values for each group - Nitrogen
M0_NitM<-lmer(NitM~1+(1|Block),data=BA_groups)
M1_NitM<-lmer(NitM~as.factor(Group)+(1|Block),data=BA_groups)

AICc(M0_NitM,M1_NitM)
r.squaredGLMM(M1_NitM)
plot(M1_NitM)
summary(M1_NitM)

plot(M1_Nit)

coefs <- data.frame(coef(summary(M1_NitM)))
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs$R2<-r.squaredGLMM(M1_NitM)[1]
coefs$Estimate[2:nrow(coefs)]<-coefs$Estimate[2:nrow(coefs)]+coefs$Estimate[1]
coefs$Estimate<-round(coefs$Estimate,2)
coefs$Std..Error<-round(coefs$Std..Error,2)
coefs$t.value<-round(coefs$t.value,2)
coefs$p.z<-round(coefs$p.z,2)
coefs$Level<-c("Stable","0-25%","25-50%","50-75%","75-100%")
colnames(coefs)<-c("Estimate","SE","T.Value","P.value","R_squared","Level")
head(coefs)
write.csv(coefs,"Figures/Nit_sum_W.csv")
