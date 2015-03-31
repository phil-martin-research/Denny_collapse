#script for analyses of changes in grass cover from 1964-2014

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(lme4)
library(MuMIn)

Grass_ab<-read.csv("Data/Grass_ab_BA.csv")

############################################################
#analysis of change in grass abundance######################
############################################################
#classify plots by collapse status - collapsed (1) or not (0)
Grass_ab$Collapse<-NA
for (i in 1:nrow(Grass_ab)){
  Grass_ab$Collapse[i]<-ifelse(Grass_ab$BAPERCM[i]<=-0.25,1,0)
}
#classify plots to identify those that have *at some point* been classed as collapsed
Grass_ab$Collapse2<-NA
Grass_ab2<-NULL
Block_unique<-unique(Grass_ab$Block)
for (i in 1:length(Block_unique)){
  Block_sub<-subset(Grass_ab,Block==Block_unique[i])
  Block_sub$Collapse2<-ifelse(sum(Block_sub$Collapse)>0,1,0)
  Grass_ab2<-rbind(Block_sub,Grass_ab2)
}
head(Grass_ab2)


#now make sure values are constrained between 0 and 1
Grass_ab2$Year2<-as.factor(Grass_ab2$Year)
Grass_ab2$Perc_C<-ifelse(Grass_ab2$Perc_C>=99,99,Grass_ab2$Perc_C)
Grass_ab2$Perc_C_trans<-qlogis((Grass_ab2$Perc_C/100)+0.001)

#now model this
#first null models
M0.1<-lmer(Perc_C_trans~1+(1|Block),data=Grass_ab2)


#now models with fixed effects
M1<-lmer(Perc_C_trans~Year2*Collapse2+(1|Block),data=Grass_ab2)
M2<-lmer(Perc_C_trans~Year2+Collapse2+(1|Block),data=Grass_ab2)
M3<-lmer(Perc_C_trans~Year2+(1|Block),data=Grass_ab2)
M4<-lmer(Perc_C_trans~Collapse2+(1|Block),data=Grass_ab2)
AICc(M0.1,M1,M2,M3,M4)

#create model selection output for these models
Mod_list<-list(M0.1,M1,M2,M3,M4)
Model_selection<-model.sel(Mod_list)
Model_selection$R2<-c(r.squaredGLMM(M1)[1],r.squaredGLMM(M2)[1],r.squaredGLMM(M3)[1],r.squaredGLMM(M4)[1],r.squaredGLMM(M0.1)[1])
write.csv(Model_selection,"Figures/Mod_sel_Grass_change.csv")

#get coefficient of top model
coefs <- data.frame(coef(summary(M1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
write.csv(coefs,"Figures/Grass_TS_coefs.csv")

#now create plots of this
newdat<-expand.grid(Year2=unique(Grass_ab2$Year2),Collapse2=unique(Grass_ab2$Collapse2))
newdat$Perc_C_trans<-0

mm <- model.matrix(terms(M1),newdat)
newdat$Perc_C_trans <- predict(M1,newdat,re.form=NA)
## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(M1),mm))
tvar1 <- pvar1+VarCorr(M1)$Block[1]  ## must be adapted for more complex models
tvar1 <- 
  newdat <- data.frame(
    newdat
    , plo = newdat$Perc_C_trans-2*sqrt(pvar1)
    , phi = newdat$Perc_C_trans+2*sqrt(pvar1)
    , tlo = newdat$Perc_C_trans-2*sqrt(tvar1)
    , thi = newdat$Perc_C_trans+2*sqrt(tvar1)
  )

newdat$Collapse2<-as.numeric(as.character(newdat$Collapse2))
newdat$Collapse3<-ifelse(newdat$Collapse2==1,"Collapsed at some point","Stable/Increasing for entire period")



plogis((newdat$plo+newdat$Perc_C_trans))*100

(plogis(newdat$plo+newdat$Perc_C_trans))/2


(plogis(newdat$phi)-plogis(newdat$Perc_C_trans))/2


#create plots of these
dodge <- position_dodge(width=0.9)
Grass_TS1<-ggplot(newdat,aes(x=Year2,y=(plogis(Perc_C_trans))*100,ymin=(plogis(plo))*100,ymax=(plogis(phi))*100,fill=Collapse3))+geom_bar(stat="identity",position =dodge)+geom_errorbar(position =dodge,width=0.25)
Grass_TS2<-Grass_TS1+ylab("Percentage ground cover of grass species")+xlab("Year")+scale_fill_brewer("Collapse status",palette="Set1")
Grass_TS2+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))                                                                                                                                          
ggsave("Figures/Collapse_Grass_TS.png",width = 8,height=6,units = "in",dpi=300)
