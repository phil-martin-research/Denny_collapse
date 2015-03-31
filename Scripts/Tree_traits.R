#script to look at changes in species traits

rm(list=ls(all=TRUE))

#open packages neeeded for analysis
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(nlme)
library(MuMIn)
library(gridExtra)
library(MASS)
library(survival)
library(GGally)
library(lme4)
library(fields)
library(ROCR)

#import data
BA<-read.csv("Data/Denny_plots.csv")
BA<-subset(BA,Year>1964)

#####################################
#exploration of trait change#########
#####################################
str(BA)
BA$LightM<-ifelse(BA$LightM==0,NA,BA$LightM)
BA$NitM<-ifelse(BA$NitM==0,NA,BA$NitM)
BA$MoistM<-ifelse(BA$MoistM==0,NA,BA$MoistM)

####################################
#test for changes in traits over####
#the gradient#######################
BA$BAPERCM2<-BA$BAPERCM*-1

#first light requirements
M0<-lmer(LightM~1+(1|Block),data=BA,na.action=na.omit)

#random slopes is best
M1<-lmer(LightM~BAPERCM2+(1|Block),data=BA,na.action=na.omit)
M2<-lmer(LightM~BAPERCM2+I(BAPERCM2^2)+(1|Block),data=BA,na.action=na.omit)
M3<-lmer(LightM~BAPERCM2+I(BAPERCM2^2)+I(BAPERCM2^3)+(1|Block),data=BA,na.action=na.omit)

AICc(M0.1,M1,M2,M3)# the null model is better

modlist<-list(M0,M1,M2,M3)

Mod_sel_light_Tree<-model.sel(modlist)
Mod_sel_light_Tree
Mod_sel_light_Tree$R_sq<-c(r.squaredGLMM(M3)[1],r.squaredGLMM(M2)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M0)[1])

Mod.avg_Light<-model.avg(Mod_sel_light_Tree,subset = delta<7)
summary(Mod.avg_Light)

write.csv(Mod_sel_light_Tree,"Figures/Mod_sel_light_Tree.csv")


#then nitrogen
BA$NitM2<-qlogis((BA$NitM/max(BA$NitM,na.rm = T))-0.01)
M0<-lmer(NitM2~1+(1|Block),data=BA,na.action=na.omit)


#random slopes is best
M1<-lmer(NitM2~BAPERCM2+(1|Block),data=BA,na.action=na.omit)
M2<-lmer(NitM2~BAPERCM2+I(BAPERCM2^2)+(1|Block),data=BA,na.action=na.omit)
M3<-lmer(NitM2~BAPERCM2+I(BAPERCM2^2)+I(BAPERCM2^3)+(1|Block),data=BA,na.action=na.omit)
AICc(M0,M1,M2,M3)# the null model is better
plot(subset(BA$BAPERCM2,!is.na(BA$LightM)),subset(BA$NitM2,!is.na(BA$LightM)))

length(subset(BA$BAPERCM2,!is.na(BA$LightM)))


modlist<-list(M0,M1,M2,M3)

Mod_sel_Nit_Tree<-model.sel(modlist)
Mod_sel_Nit_Tree
Mod_sel_Nit_Tree$R_sq<-c(r.squaredGLMM(M3)[1],r.squaredGLMM(M2)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M0)[1])

coef(summary(M3))

Mod.avg_Light<-model.avg(Mod_sel_Nit_Tree,subset = delta<7)
summary(Mod.avg_Light)

plot(M1)

min(BA$BAPERCM2)
length(subset(BA$BAPERCM2,!is.na(BA$LightM)))
length(predict(M3,re.form=NA))
write.csv(Mod_sel_light_Tree,"Figures/Mod_sel_light_Tree.csv")



#then moisture
#first rescale Ellenburg value
BA$MoistM2<-qlogis((BA$MoistM/max(BA$MoistM,na.rm = T))-0.01)

M0<-lmer(MoistM2~1+(1|Block),data=BA,na.action=na.omit)
#random slopes is best
M1<-lmer(MoistM2~BAPERCM2+(1|Block),data=BA,na.action=na.omit)
M2<-lmer(MoistM2~BAPERCM2+I(BAPERCM2^2)+(1|Block),data=BA,na.action=na.omit)
M3<-lmer(MoistM2~BAPERCM2+I(BAPERCM2^2)+I(BAPERCM2^3)+(1|Block),data=BA,na.action=na.omit)
AICc(M0,M1,M2,M3)# the null model is better

plot(BA$BAPERCM2,BA$MoistM)
