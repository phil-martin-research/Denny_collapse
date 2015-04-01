#script to show time series of collapsed vs non-collapsed plots

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(lme4)
library(MuMIn)

#load data
Plots<-read.csv("Data/Denny_plots.csv")

head(Plots)

#add column for tanner index
Plots$Tanner<-(Plots$Sor_BA+Plots$SorM)/2

#classify plots by collapse status - collapsed (1) or not (0)
Plots$Collapse<-NA
for (i in 1:nrow(Plots)){
  Plots$Collapse[i]<-ifelse(Plots$BAPERCM[i]<=-0.25,1,0)
}
#classify plots to identify those that have *at some point* been classed as collapsed
Plots$Collapse2<-NA
Plots2<-NULL
Block_unique<-unique(Plots$Block)
for (i in 1:length(Block_unique)){
  Block_sub<-subset(Plots,Block==Block_unique[i])
  Block_sub$Collapse2<-ifelse(sum(Block_sub$Collapse)>0,1,0)
  Plots2<-rbind(Block_sub,Plots2)
}

keeps<-c("Block","Year","Tanner","BAPERCM","Collapse2","BAM")
Plots3<-Plots2[keeps]
Plots4<-rbind(data.frame(Block=c(7,7),Year=c(1996,2014),Tanner=c(0,0),BAPERCM=c(-1,-1),Collapse2=c(1,1),BAM=c(0,0)),Plots3)

#now do analysis of this
#create variable for BAPERCM that is bounded above -1
Plots4<-subset(Plots4,Year==1964|Year==2014)
#create a variable of number of years since 1964
Plots4$Year2<-as.factor(Plots4$Year)
Plots4$Collapse2<-as.factor(Plots4$Collapse2)

#now a null model
M0<-lmer(BAM~1+(1|Block),data=Plots4)
#use first random effect specification
M1<-lmer(BAM~Year2+(1|Block),data=Plots4)
M2<-lmer(BAM~Collapse2+(1|Block),data=Plots4)
M3<-lmer(BAM~Year2+Collapse2+(1|Block),data=Plots4)
M4<-lmer(BAM~Year2*Collapse2+(1|Block),data=Plots4)

Models<-list(M0,M1,M2,M3,M4)

#produce tables to give summary statistics
Model_table<-model.sel(Models)
Model_table$R_sq<-c(r.squaredGLMM(M4)[1],r.squaredGLMM(M3)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M2)[1],r.squaredGLMM(M0)[1])

write.csv(Model_table,"Figures/BA_TS_table.csv",row.names=F)

coefs <- data.frame(coef(summary(M4)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

write.csv(coefs,"Figures/BA_TS_coefs.csv")


#put in predictions
newdat<-expand.grid(Year2=as.factor(c(1964,2014)),Collapse2=as.factor(c(0,1)))
newdat$BAM<-0

mm <- model.matrix(terms(M4),newdat)
newdat$BAM <- predict(M4,newdat,re.form=NA)
## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(M4),mm))
tvar1 <- pvar1+VarCorr(M4)$Block[1]  ## must be adapted for more complex models
  newdat <- data.frame(
    newdat
    , plo = newdat$BAM-2*sqrt(pvar1)
    , phi = newdat$BAM+2*sqrt(pvar1)
    , tlo = newdat$BAM-2*sqrt(tvar1)
    , thi = newdat$BAM+2*sqrt(tvar1)
  )


newdat$Collapse3<-ifelse(newdat$Collapse2==1,"Collapsed","Stable")


labels<-data.frame(x=min(Plots4$Year)+1,y=max(Plots$BAM)-1,Collapse3=c("Collapsed","Stable"),labs=c("(a)","(b)"))
theme_set(theme_bw(base_size=12))
dodge <- position_dodge(width=0.9)
BA_plot<-ggplot(newdat,aes(x=Year2,y=BAM,ymax=phi,ymin=plo,fill=Collapse2))+geom_bar(stat="identity",position =dodge)+geom_errorbar(position =dodge,width=0.25)
BA_plot2<-BA_plot+xlab("Year")+ theme(legend.position = "none")+ylab(expression(paste("Basal area (", m^bold("2"),ha^bold("-1"),")")))
BA_plot2+scale_fill_brewer(palette="Set1")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))                                                                                                                                          
ggsave("Figures/Collapse_BA_TS2.png",width = 8,height=6,units = "in",dpi=300)



