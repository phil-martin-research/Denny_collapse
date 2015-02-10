#script to look at changes in fungi richness over gradient
rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(vegan)
library(reshape2)
library(lme4)
library(nlme)
library(MuMIn)
library(quantreg)
library(car)

#load in data
BA<-read.csv("Data/BA_gradient.csv")
Fungi<-read.csv("Data/Fungi_data.csv")

BA_2014<-subset(BA,Year==2014)


BA_fungi<-merge(BA_2014,Fungi,by.x="Block",by.y="Plot")

#plot the species richness of fungi over the gradient of collapse


ggplot(BA_fungi,aes(x=BAPERCM,y=Sp_rich))+geom_point()+xlim(c(-1,0.5))+ylim(c(0,15))+geom_smooth(se=F)

#remove plot with large increase in BA as it is screwing up model fitting
BA_fungi2<-subset(BA_fungi,BAPERCM<0.5)


#now test the relationship
M0<-lm(Sp_rich~1,data=BA_fungi2)
M1<-lm(Sp_rich~BAPERCM,data=BA_fungi2)
M2<-lm(Sp_rich~BAPERCM+I(BAPERCM^2),data=BA_fungi2)

par(mfrow=c(2,2))

plot(M0)
plot(M1)
plot(M2)

AICc(M0,M1,M2)

par(mfrow=c(1,1))
plot(BA_fungi2$BAPERCM,BA_fungi2$Sp_rich)
points(BA_fungi2$BAPERCM,predict(M2))



#produce proper model averaging of this

Models<-list(M0,M1,M2)

M_sel<-model.sel(Models)
M_sel$R2<-c(0.08324,0,0.001796)

M_Av<-model.avg(Models)

BA_fungi2$Preds<-predict(M_Av,se.fit=T)$fit
BA_fungi2$SE<-predict(M_Av,se.fit=T)$se.fit
BA_fungi2$UCI<-BA_fungi2$Pred+(1.96*BA_fungi2$SE)
BA_fungi2$LCI<-BA_fungi2$Pred-(1.96*BA_fungi2$SE)

#now plot this relationship
theme_set(theme_bw(base_size=12))
Fungi_plot1<-ggplot(BA_fungi2,aes(x=BAPERCM,y=Sp_rich))+geom_point(shape=1,size=3)+geom_line(data=BA_fungi2,aes(y=Preds))+geom_ribbon(data=BA_fungi2,aes(ymax=UCI,ymin=LCI,y=NULL),fill="blue",alpha=0.2)
Fungi_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ylab("Fungi species richness")+xlab("Percentage loss of basal area since 1964")
ggsave("Figures/Fungi_rich.png",width = 8,height=6,units = "in",dpi=300)

#now do it using categorical methods

Groups<-data.frame(max=c(-0.75,-0.50,-0.25,0,3),min=c(-1.00,-0.75,-0.50,-0.25,0),group=c("75-100%","50-75%","25-50%","0-25%","Stable/Increasing"),numbers=c(5,4,3,2,1))
head(BA_fungi)
BA_groups<-NULL
for(i in 1:nrow(Groups)){
  BA2<-subset(BA_fungi,BA_fungi$BAPERCM>Groups[i,2]&BA_fungi$BAPERCM<Groups[i,1])
  BA2$Group<-Groups[i,3]
  BA2$Numbers<-Groups[i,4]
  BA_groups<-rbind(BA_groups,BA2)
}

factor(d$Team1, c("Cowboys", "Giants", "Eagles", "Redskins"))

BA_groups$Group2<-factor(BA_groups$Group, c("Stable/Increasing", "0-25%", "25-50%", "50-75%","75-100%"))


theme_set(theme_bw(base_size=12))
Fungi_plot1<-ggplot(BA_groups,aes(x=Group2,y=Sp_rich))+geom_boxplot()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Fungi_plot1+ylab("Fungi species richness")+xlab("Collapse group")
ggsave("Figures/Fungi_rich_cat.png",width = 8,height=6,units = "in",dpi=300)

#model this
#now test the relationship
M0<-lm(Sp_rich~1,data=BA_groups)
M1<-lm(Sp_rich~Group2,data=BA_groups)
summary(M1)

par(mfrow=c(2,2))

plot(M0)
plot(M1)

AICc(M0,M1)

par(mfrow=c(1,1))
plot(BA_fungi2$BAPERCM,BA_fungi2$Sp_rich)
points(BA_fungi2$BAPERCM,predict(M2))


