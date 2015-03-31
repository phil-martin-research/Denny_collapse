#script to calculate changes in tree species richness in Denny wood over time

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(vegan)
library(reshape2)
library(nlme)
library(MuMIn)
library(lme4)


#import data
Spr<-read.csv("Data/BA_tree_comm.csv")
head(Spr)

#change BAPERCM sign
Spr$BAPERCM2<-Spr$BAPERCM*-1

#subset to remove data from 1964
Spr2<-subset(Spr,Year>1964)

M0<-glmer(SPRM~1+(1|Block),data=Spr2,family=poisson)
M1<-glmer(SPRM~BAPERCM2+(1|Block),data=Spr2,family=poisson)
M2<-glmer(SPRM~BAPERCM2+I(BAPERCM2^2)+(1|Block),data=Spr2,family=poisson)

modlist<-list(M0,M1,M2)

Mod_sel_Spr<-model.sel(modlist)
Mod_sel_Spr
Mod_sel_Spr$R_sq<-c(r.squaredGLMM(M1)[1],r.squaredGLMM(M2)[1],r.squaredGLMM(M0)[1])

Mod.avg_Spr<-model.avg(Mod_sel_Spr,subset=delta<7)
summary(Mod.avg_Spr)

write.csv(Mod_sel_Spr,"Figures/Mod_sel_Spr_tree.csv")
