#seedling analysis
rm(list=ls(all=TRUE))
Browsing<-read.csv("Data/Browsing_Denny.csv",header = T,stringsAsFactors=F)
BA<-read.csv("Data/BA_gradient.csv",header = T,stringsAsFactors=F)
BA<-subset(BA,Year==2014)


#load packages
library(ggplot2)
library(lme4)
library(MuMIn)
library(plyr)
library(reshape2)
library(tidyr)
library(dplyr)


#tidy data
Browsing$Plot<-sapply(strsplit(Browsing$Plot,"-"),'[',2)
head(Browsing)
Browsing_BA<-merge(Browsing,BA,by.x="Plot",by.y="Block",all=T)
Browsing_BA<-Browsing_BA[complete.cases(Browsing_BA),]
head(Browsing_BA)

#explore data to look at grazing pressure agains the collapse gradient

ggplot(data=Browsing_BA,aes(x=BAPERCM,y=Bramble_browsed))+geom_point()+geom_smooth(method=lm)
ggplot(data=Browsing_BA,aes(x=BAPERCM,y=Holly_browsed))+geom_point()+geom_smooth(method=lm)
ggplot(data=Browsing_BA,aes(x=BAPERCM,y=Sward_h))+geom_point()+geom_smooth(method=lm)
ggplot(data=Browsing_BA,aes(x=BAPERCM,y=Deer_dung))+geom_point()+geom_smooth(method=lm)
ggplot(data=Browsing_BA,aes(x=BAPERCM,y=Horse_dung))+geom_point()+geom_smooth(method=lm)
ggplot(data=Browsing_BA,aes(x=BAPERCM,y=Canopy_open))+geom_point()+geom_smooth(method=lm)




#the only relationship that looks any good is
#that between deer dung anf basal area change
Browsing_BA$Deer_dung2<-round(Browsing_BA$Deer_dung,0)

M0<-glm(Deer_dung2~1,data=Browsing_BA,family=poisson)
M1<-glm(Deer_dung2~BAPERCM,data=Browsing_BA,family=poisson)
M2<-glm(Deer_dung2~BAPERCM+I(BAPERCM^2),data=Browsing_BA,family=poisson)
par(mfrow=c(1,1))

1-(2211/2300)


plot(Browsing_BA$BAPERCM,Browsing_BA$Deer_dung2)
points(Browsing_BA$BAPERCM,exp(predict(M2)),col="red")

new.data<-data.frame(BAPERCM=seq(-1,0.5,0.01))
new.data$Deer_dung2<-exp(predict(M2,newdata = new.data))

#plot the relationship between collapse and dung counts
theme_set(theme_bw(base_size=12))
Dung_plot1<-ggplot(data=Browsing_BA,aes(x=BAPERCM*100,y=Deer_dung2))+geom_point(shape=1)
Dung_plot2<-Dung_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))                                                                                                                                          
Dung_plot2+geom_line(data=new.data)+xlab("Percentage change in BA since 1964")+ylab("Total deer dung found in plot")
ggsave("Figures/Deer_Collapse.png",width = 8,height=6,units = "in",dpi=600)
