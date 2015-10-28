#community composition for different measures - trees

library(ggplot2)
library(lme4)
library(MuMIn)

#import .csv containing plot data
Plots<-read.csv("Data/Denny_plots.csv")
Plots$Tanner<-(Plots$Sor_BA+Plots$SorM)/2
#drop all columns apart from the ones I need
keeps<-c("BAPERCM","Tanner","Block","Year")
Plots2<-Plots[keeps]
#remove plots from 1964
Plots2<-subset(Plots2,Year>1965)
#create variable for tanner that is bounded above 0
Plots2$Tanner<-ifelse(Plots2$Tanner==1,Plots2$Tanner-0.001,Plots2$Tanner+0.001)
Plots2$Tanner2<-qlogis(Plots2$Tanner)
Plots2$Year<-ifelse(Plots2$Year==1999,1996,Plots2$Year)
Plots2$Year2<-ifelse(Plots2$Year==1996,"1996/9",Plots2$Year)

#create plots of changes in tanner index over gradient for different years
ggplot(Plots2,aes(x=BAPERCM,y=Tanner))+geom_point()+facet_wrap(~Year)+geom_smooth(method="lm")

M0.1<-lmer(Tanner2~1+(1|Block),data=Plots2)
M0.2<-lmer(Tanner2~1+(1|Block)+(1|Year),data=Plots2)
M0.3<-lmer(Tanner2~1+(1|Block)+(BAPERCM|Year),data=Plots2)
AICc(M0.1,M0.2,M0.3)
##use third random effect specification
M1<-lmer(Tanner2~BAPERCM+(1|Block)+(BAPERCM|Year),data=Plots2)
M2<-lmer(Tanner2~BAPERCM+I(BAPERCM^2)+(1|Block)+(BAPERCM|Year),data=Plots2)
AICc(M1,M2,M0.3)

#check residuals - these look fine
plot(M2)
qqnorm(resid(M2))

#create model selection output for these models
Mod_list<-list(M0.3,M1,M2)
Model_selection<-model.sel(Mod_list)
Model_selection$R2<-c(r.squaredGLMM(M2)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M0.1)[1])
write.csv(Model_selection,"Tables/Mod_sel_Grad_Tanner.csv")

#get coefficient of top model
coefs <- data.frame(coef(summary(M2)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
write.csv(coefs,"Tables/Tanner_Grad_coefs.csv")

#put prediction into dataframe
Plots2$Pred<-plogis(predict(M2))
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

#plot this
theme_set(theme_bw(base_size=12))
Coll_Tanner1<-ggplot(Plots3,aes(x=BAPERCM*100*(-1),y=Tanner,colour=as.factor(Year2),shape=Transect))+geom_point(size=3,alpha=0.5,guide=F)
Coll_Tanner2<-Coll_Tanner1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Coll_Tanner3<-Coll_Tanner2+geom_line(data=newdat,aes(x=BAPERCM*100*(-1),y=Pred,shape=NULL),colour="black")+ylab("Tanner Index")+xlab("Percentage basal area loss since 1964")
Coll_Tanner4<-Coll_Tanner3+ scale_colour_grey("Year of survey")+geom_line(data=newdat,aes(x=BAPERCM*100*(-1),y=plo2,shape=NULL),colour="black",lty=2)+geom_line(data=newdat,aes(x=BAPERCM*100*(-1),y=phi2,shape=NULL),colour="black",lty=2)
Coll_Tanner4+guides(shape=FALSE)
ggsave("Figures/For_Paper/BA_Tanner.png",width = 8,height=6,units = "in",dpi=300)
Coll_Tanner4<-Coll_Tanner3+ scale_colour_brewer(palette = "Set1","Year of survey")+geom_line(data=newdat,aes(x=BAPERCM*100*(-1),y=plo2,shape=NULL),colour="black",lty=2)+geom_line(data=newdat,aes(x=BAPERCM*100*(-1),y=phi2,shape=NULL),colour="black",lty=2)
Coll_Tanner4+guides(shape=FALSE) 
ggsave("Figures/For_Paper/BA_Tanner_colour.png",width = 8,height=6,units = "in",dpi=800)

#plot for ICCB poster
theme_set(theme_grey(base_size=20))
Coll_Tanner1<-ggplot(Plots3,aes(x=BAPERCM*100*(-1),y=Tanner,colour=as.factor(Year2)))+geom_point(size=5,alpha=0.5,guide=F)
Coll_Tanner2<-Coll_Tanner1+scale_shape_manual(values = c(1,2))
Coll_Tanner3<-Coll_Tanner2+geom_line(data=newdat,aes(x=BAPERCM*100*(-1),y=Pred,shape=NULL),colour="black")+ylab("Change in community composition (Tanner Index)")+xlab("Percentage basal area loss since 1964")
Coll_Tanner4<-Coll_Tanner3+ scale_colour_brewer(palette = "Set1","Year of survey")+geom_line(data=newdat,aes(x=BAPERCM*100*(-1),y=plo2,shape=NULL),colour="black",lty=2)+geom_line(data=newdat,aes(x=BAPERCM*100*(-1),y=phi2,shape=NULL),colour="black",lty=2)
Coll_Tanner4+guides(shape=F)
ggsave("Figures/For_Presentations/BA_Tanner_colour_ICCB.png",width = 20,height=20,units = "cm",dpi=800)
