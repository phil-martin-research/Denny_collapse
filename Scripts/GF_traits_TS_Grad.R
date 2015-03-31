#script to do analysis of ground flora change over time for collapsed and uncollapsed plots

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

GF$Collapse<-NA
for (i in 1:nrow(GF)){
  GF$Collapse[i]<-ifelse(GF$BAPERCM[i]<=-0.25,1,0)
}
#classify plots to identify those that have *at some point* been classed as collapsed
GF$Collapse2<-NA
GF2<-NULL
Block_unique<-unique(GF$Block)
for (i in 1:length(Block_unique)){
  Block_sub<-subset(GF,Block==Block_unique[i])
  Block_sub$Collapse2<-ifelse(sum(Block_sub$Collapse)>0,1,0)
  GF2<-rbind(Block_sub,GF2)
}

#exploratory plots of change in different traits over time
#light
Light_plot<-ggplot(GF2,aes(x=as.factor(Year),y=Light))+geom_boxplot()+facet_wrap(~Collapse2)
Light_plot2<-Light_plot+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ylim(c(4,7))
Light_plot2+ylab("Community weighted mean plant light requirements")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Collapse_Light_TS.png",width = 8,height=6,units = "in",dpi=300)

#Nitrogen
Nitrogen_plot<-ggplot(GF2,aes(x=as.factor(Year),y=Nit))+geom_boxplot()+facet_wrap(~Collapse2)
Nitrogen_plot2<-Nitrogen_plot+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ylim(c(2,7))
Nitrogen_plot2+ylab("Community weighted mean plant nitrogen requirements")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Collapse_Nitrogen_TS.png",width = 8,height=6,units = "in",dpi=300)

#Moisture
Moisture_plot<-ggplot(GF2,aes(x=as.factor(Year),y=Moist))+geom_boxplot()+facet_wrap(~Collapse2)
Moisture_plot2<-Moisture_plot+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ylim(c(4,8))
Moisture_plot2+ylab("Community weighted mean plant water requirements")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Collapse_Moisture_TS.png",width = 8,height=6,units = "in",dpi=300)

#exploratory plots of change in different traits over the gradient
#light

GF3<-subset(GF2,Year>1964)
ggplot(GF3,aes(x=BAPERCM*-1,y=Light,group=Block))+geom_point()+geom_smooth(se=F,method="lm",colour="blue",size=2,aes(group=NULL))
#Nitrogen
ggplot(GF3,aes(x=BAPERCM*-1,y=Nit,group=Block))+geom_point()+geom_smooth(se=F,method="lm",colour="blue",size=2,aes(group=NULL))
#Moisture
ggplot(GF3,aes(x=BAPERCM*-1,y=Moist,group=Block))+geom_point()+geom_smooth(se=F,method="lm",colour="blue",size=2,aes(group=NULL))

####################################
#test for changes in traits over####
#the gradient#######################
GF3$BAPERCM2<-GF3$BAPERCM*-1


#first ellenburg values for light
GF3$Light<-ifelse(GF3$Light==0,NA,GF3$Light)

M0<-lmer(Light~1+(1|Block),data = GF3)
M1<-lmer(Light~BAPERCM2+(1|Block),data = GF3)
M2<-lmer(Light~BAPERCM2+I(BAPERCM2^2)+(1|Block),data = GF3)
M3<-lmer(Light~BAPERCM2+I(BAPERCM2^2)+I(BAPERCM2^3)+(1|Block),data = GF3)

modlist<-list(M0,M1,M2,M3)

Mod_sel_light_GF<-model.sel(modlist)
Mod_sel_light_GF$R_sq<-c(r.squaredGLMM(M1)[1],r.squaredGLMM(M2)[1],r.squaredGLMM(M0)[1],r.squaredGLMM(M3)[1])

Mod.avg_Light<-model.avg(Mod_sel_light_GF)
summary(Mod.avg_Light)

write.csv(Mod_sel_light_GF,"Figures/Mod_sel_light_GF.csv")

#next ellenburg values for nitrogen
M0<-lmer(Nit~1+(1|Block),data = GF3)
M1<-lmer(Nit~BAPERCM2+(1|Block),data = GF3)
M2<-lmer(Nit~BAPERCM2+I(BAPERCM2^2)+(1|Block),data = GF3)
M3<-lmer(Nit~BAPERCM2+I(BAPERCM2^2)+I(BAPERCM2^3)+(1|Block),data = GF3)

modlist<-list(M0,M1,M2,M3)

Mod_sel_nit_GF<-model.sel(modlist)
Mod_sel_nit_GF$R_sq<-c(r.squaredGLMM(M1)[1],r.squaredGLMM(M2)[1],r.squaredGLMM(M0)[1],r.squaredGLMM(M3)[1])

Mod.avg_nit<-model.avg(Mod_sel_nit_GF)
summary(Mod.avg_nit)

write.csv(Mod_sel_nit_GF,"Figures/Mod_sel_nit_GF.csv")

#next ellenburg values for moisture
M0<-lmer(Moist~1+(1|Block),data = GF3)
M1<-lmer(Moist~BAPERCM2+(1|Block),data = GF3)
M2<-lmer(Moist~BAPERCM2+I(BAPERCM2^2)+(1|Block),data = GF3)
M3<-lmer(Moist~BAPERCM2+I(BAPERCM2^2)+I(BAPERCM2^3)+(1|Block),data = GF3)

modlist<-list(M0,M1,M2,M3)

Mod_sel_moist_GF<-model.sel(modlist)
Mod_sel_moist_GF$R_sq<-c(r.squaredGLMM(M1)[1],r.squaredGLMM(M2)[1],r.squaredGLMM(M0)[1],r.squaredGLMM(M3)[1])

Mod.avg_moist<-model.avg(Mod_sel_moist_GF)
summary(Mod.avg_moist)

write.csv(Mod_sel_moist_GF,"Figures/Mod_sel_moist_GF.csv")




