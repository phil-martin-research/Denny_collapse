#community composition for different measures - trees

library(ggplot2)
library(lme4)
library(MuMIn)

#save all this as a csv
Plots<-read.csv("Data/Denny_plots.csv")

head(Plots)

Plots$Tanner<-(Plots$Sor_BA+Plots$SorM)/2

keeps<-c("BAPERCM","Tanner","Block","Year")

Plots2<-Plots[keeps]
#remove plots from 1964
Plots2<-subset(Plots2,Year>1964)

#create variable for tanner that is bounded above 0
Plots2$Tanner2<-qlogis(Plots2$Tanner)

M0.1<-lmer(Tanner2~1+(1|Block),data=Plots2)
M0.2<-lmer(Tanner2~1+(Block|Year),data=Plots2)
AICc(M0.1,M0.2)
#use first random effect specification
M1<-lmer(Tanner2~BAPERCM+(Block|Year),data=Plots2)
M2<-lmer(Tanner2~BAPERCM+I(BAPERCM^2)+(Block|Year),data=Plots2)
AICc(M1,M2,M0.1)

plot(plogis(fitted(M2)),resid(M2))
r.squaredGLMM(M2)

#put prediction into dataframe
Plots2$Pred<-plogis(predict(M2,re.form=NA))
Plots3<-subset(Plots2,BAPERCM<1)

newdat<-expand.grid(BAPERCM=seq(min(Plots2$BAPERCM),0.5,0.001))
newdat$Tanner2<-predict(M2,re.form=NA,newdata=newdat)
mm <- model.matrix(terms(M2),newdat)
newdat$distance <- mm %*% fixef(M2)
pvar1 <- diag(mm %*% tcrossprod(vcov(M2),mm))
tvar1 <- pvar1+VarCorr(M2)$Year[1]  ## must be adapted for more complex models
newdat <- data.frame(
  newdat
  , plo = newdat$distance-2*sqrt(pvar1)
  , phi = newdat$distance+2*sqrt(pvar1)
  , tlo = newdat$distance-2*sqrt(tvar1)
  , thi = newdat$distance+2*sqrt(tvar1)
)


newdat$Pred<-plogis(predict(M2,newdata = newdat,re.form=NA))
newdat$plo2<-plogis(newdat$plo)
newdat$phi2<-plogis(newdat$phi)

#identify plots on unenclosed and enclosed transects
Plots3$Transect<-ifelse(Plots3$Block>=51,"Unenclosed","Enclosed")

nrow(Plots3)

#plot this
theme_set(theme_bw(base_size=12))
Coll_Tanner1<-ggplot(Plots3,aes(x=BAPERCM*100*(-1),y=Tanner,colour=as.factor(Year),shape=Transect))+geom_point(size=3)
Coll_Tanner2<-Coll_Tanner1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Coll_Tanner3<-Coll_Tanner2+geom_line(data=newdat,aes(x=BAPERCM*100*(-1),y=Pred,shape=NULL),colour="black")+ylab("Tanner index")+xlab("Percentage basal area loss since 1964")
Coll_Tanner4<-Coll_Tanner3+ scale_colour_grey("Year of survey")+geom_line(data=newdat,aes(x=BAPERCM*100*(-1),y=plo2,shape=NULL),colour="black",lty=2)+geom_line(data=newdat,aes(x=BAPERCM*100*(-1),y=phi2,shape=NULL),colour="black",lty=2)
Coll_Tanner4+scale_shape_manual(values=c(16,1))
ggsave("Figures/BA_Tanner.png",width = 8,height=6,units = "in",dpi=300)
Coll_Tanner4<-Coll_Tanner3+ scale_colour_brewer(palette = "Set1","Year of survey")+geom_line(data=newdat,aes(x=BAPERCM*100*(-1),y=plo2,shape=NULL),colour="black",lty=2)+geom_line(data=newdat,aes(x=BAPERCM*100*(-1),y=phi2,shape=NULL),colour="black",lty=2)
Coll_Tanner4+scale_shape_manual(values=c(16,1))
ggsave("Figures/BA_Tanner_colour.png",width = 8,height=6,units = "in",dpi=300)
