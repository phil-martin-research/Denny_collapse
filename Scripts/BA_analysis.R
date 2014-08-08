#script to analyse changes in basal area in Denny Wood


#needs some tidying

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


#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Location<-read.csv("Plot_coords.csv")
Location<-unique(Location[,c(3,5:6)])
DBH<-read.csv("Denny_trees_cleaned.csv")
head(DBH)
head(Location)
#subset trees to give only those inside plots
DBH<-subset(DBH,In_out=="In")


#####################################
#analyses of basal area change#######
#####################################
head(DBH)
#calculate basal area per plot per time period
#for all species
DBH$BA<-ifelse(DBH$DBH>10,DBH$DBH^2*(pi/4),0)
#for beech only
DBH$Beech_BA<-ifelse(DBH$DBH>10&DBH$Species=="F",DBH$DBH^2*(pi/4),0)

#total basal area
BA_change<-melt(with(DBH, tapply(BA,list(Block,Year), function(x) sum(na.omit(x)/400))))
colnames(BA_change)<-c("Block","Year","BA")
BA_change$Transect<-ifelse(BA_change$Block>=52,"Unenclosed","Enclosed")
BA_change_CC<-BA_change[complete.cases(BA_change),]

#total basal area - for beech
BA_change_F<-melt(with(DBH, tapply(Beech_BA,list(Block,Year), function(x) sum(na.omit(x)/400))))
colnames(BA_change_F)<-c("Block","Year","BA")
BA_change_F$Transect<-ifelse(BA_change$Block>=52,"Unenclosed","Enclosed")
BA_change_F2<-BA_change_F[complete.cases(BA_change_F),]

#plot time series of BA for both transects
ggplot(BA_change_CC,aes(x=Year,y=BA,group=Block,colour=Transect))+geom_line()+geom_point()+facet_wrap(~Block)


#test change in basal area as a function of area sampled
Plots_2014<-subset(BA_change_CC,Year==2014)
BA_area<-NULL
BA_area$Plots<-1:nrow(Plots_2014)
BA_area$Area<-(BA_area$Plots)*(20*20)
BA_area<-data.frame(BA_area)
BA_results<-NULL

#loop to add one plot at a time and calculate mean iterated 1000 times
for (i in 1:1000){
  R_sample<-sample(Plots_2014$BA,nrow(Plots_2014))
  Sample_area<-cbind(BA_area,R_sample)
  nrow(BA_area)
for (y in 1:length(R_sample)){
  Sample_area$Mean_BA[y]<-mean(R_sample[1:y])
}
Sample_area$Run<-i
BA_results<-rbind(BA_results,Sample_area)
}

#plot
ggplot(BA_results,aes(Area,Mean_BA,group=Run))+geom_line(alpha=0.2)
#this result shows that between plot variation is large up until about 0.5 hectares have been sampled

#merge Northing and Easting data onto Basal Area data
head(BA_change_CC)
BA_change2<-((merge(y=Location,x=BA_change_CC,by.x=c("Block"),by.y=c("Plot_number"),all.x=F)))

#now for beech basal area as well
BA_change_F3<-((merge(y=Location,x=BA_change_F2,by.x=c("Block"),by.y=c("Plot_number"),all.x=F)))

#exploratory plot of basal area
ggplot(BA_change2,aes(x=Easting,y=Northing,colour=BA))+geom_point(shape=15)+facet_wrap(~Year)

############################################
#explore changes in spatial autocorrelation#
###########################################
head(BA_change2)
BA_change2$Transect<-as.factor(BA_change2$Transect)

#now do comparison for each years worth of data
Years<-unique(BA_change2$Year)
Semi_var<-NULL
for (i in 1:length(Years)){
  Plots_year<-subset(BA_change2,Year==Years[i])
  dists<-dist(Plots_year[,5:6])
  breaks<-seq(0,2000,20)
  v1 <- variog(coords = Plots_year[,5:6], data = Plots_year[,3], breaks = breaks)
  v1.summary <- cbind(c(1:length(v1$v)), v1$v, v1$n,Years[i])
  colnames(v1.summary) <- c("lag", "semi", "No_pairs","Year")
  v1.summary<-data.frame(v1.summary)
  v1.summary$Year<-Years[i]
  v1.summary$Distance<-v1.summary$lag*20
  Semi_var<-rbind(Semi_var,v1.summary)
}

#plot semi-variogram by year
theme_set(theme_bw(base_size=12))
BA_semi<-ggplot(Semi_var,aes(Distance,semi,group=Year))+geom_point(shape=1)+geom_line()+facet_wrap(~Year)
BA_semi+ylab("semi-variance")+xlab("Distance (m)")+geom_smooth(se=F,method="lm",size=2)
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("BA_semi.png",width = 8,height=6,units = "in",dpi=300)


#models of change of total BA with time
#create second variable for year so that it is centred
BA_change2$Year2<-BA_change2$Year-mean(BA_change2$Year)
#run models
M0<-lme(BA~1,random=~Year|Block,data=BA_change2,method="REML")
M1<-lme(BA~Year2,random=~Year|Block,data=BA_change2,method="REML")
M2<-lme(log(BA+1)~Year2,random=~Year|Block,data=BA_change2,method="REML")

M3<-glmmPQL(BA~Year2,random=~Year|Block,correlation=corSpher(form=~Easting+Northing|Year),data=BA_change2,family="gaussian")

plot(M3)

?glmmPQL

#plot residuals and qqplots
grid.arrange(plot(M0),plot(M1),plot(M2),qqnorm(M0),qqnorm(M1),qqnorm(M2),nrow=2)
#the untransformed data looks best

#perform model averaging for null and yearly change models
Models<-dredge(B2A,rank = AICc,trace = T,REML=F)
Model_sel<-mod.sel(Models)
Model_sel$R_squared<-c(r.squaredGLMM(M1)[1],r.squaredGLMM(M0)[1])
Model_average<-model.avg(Models,fit=T)
#model of change per year is much better than null model

Time<-data.frame(Year2=seq(1959,2014,1)-(mean(BA_change2$Year)))
BA_pred<-predict(B2A,newdata=Time,se.fit=T,backtransform=T,level=0,)
BA_pred$Time<-Time+(mean(BA_change2$Year))
BA_pred<-data.frame(BA_pred)

theme_set(theme_bw(base_size=12))
BA_plot1<-ggplot(BA_change2,aes(x=Year,y=BA,group=Block))+geom_point(alpha=0.5,shape=1)+geom_line(alpha=0.2)
BA_plot2<-BA_plot1+geom_line(data=BA_pred,size=3,aes(x=Year2,y=fit,group=NULL,colour=NULL),colour="blue")
BA_plot3<-BA_plot2+geom_line(data=BA_pred,size=2,lty=2,aes(x=Year2,y=fit+(1.96*se.fit),group=NULL,colour=NULL),colour="blue")+geom_line(data=BA_pred,size=2,lty=2,aes(x=Year2,y=fit-(1.96*se.fit),group=NULL,colour=NULL),colour="blue")
BA_plot4<-BA_plot3+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010,2020))+ theme(legend.position="none")
BA_plot4+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+xlab("Year")+ylab("Basal area per hectare")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("BA_change.png",width = 8,height=6,units = "in",dpi=300)


#################################
#analysis of beech BA change#####
#################################

plot(BA_change2$BA,BA_change_F3$BA)

ggplot(BA_change_F3,aes(x=Year,y=BA,group=Block))+geom_point()+geom_line()+facet_wrap(~Block)


head(BA_change2)
BA_change_F3$Transect<-as.factor(BA_change_F3$Transect)

#now do comparison for each years worth of data
Years<-unique(BA_change_F3$Year)
Semi_var<-NULL
for (i in 1:length(Years)){
  Plots_year<-subset(BA_change_F3,Year==Years[i])
  dists<-dist(Plots_year[,5:6])
  breaks<-seq(0,2000,20)
  v1 <- variog(coords = Plots_year[,5:6], data = Plots_year[,3], breaks = breaks)
  v1.summary <- cbind(c(1:length(v1$v)), v1$v, v1$n,Years[i])
  colnames(v1.summary) <- c("lag", "semi", "No_pairs","Year")
  v1.summary<-data.frame(v1.summary)
  v1.summary$Year<-Years[i]
  v1.summary$Distance<-v1.summary$lag*20
  Semi_var<-rbind(Semi_var,v1.summary)
}

#plot semi-variogram by year
theme_set(theme_bw(base_size=12))
BA_semi<-ggplot(Semi_var,aes(Distance,semi,group=Year))+geom_point(shape=1)+geom_line()+facet_wrap(~Year)
BA_semi+ylab("semi-variance")+xlab("Distance (m)")+geom_smooth(se=F,method="lm",size=2)
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("BA_F_semi.png",width = 8,height=6,units = "in",dpi=300)


#models of change of total BA with time
#create second variable for year so that it is centred
BA_change_F3$Year2<-BA_change_F3$Year-mean(BA_change_F3$Year)
#run models
M0<-lme(BA~1,random=~Year|Block,data=BA_change_F3,method="REML")
M1<-lme(BA~Year2,random=~Year|Block,data=BA_change_F3,method="REML")
M2<-lme(log(BA+1)~Year2,random=~Year|Block,data=BA_change_F3,method="REML")

#plot residuals and qqplots
grid.arrange(plot(M0),plot(M1),plot(M2),qqnorm(M0),qqnorm(M1),qqnorm(M2),nrow=2)
#the untransformed data looks best

#perform model averaging for null and yearly change models
Models<-dredge(M1,rank = AICc,trace = T,REML=F)
Model_sel<-mod.sel(Models)
Model_sel$R_squared<-c(r.squaredGLMM(M1)[1],r.squaredGLMM(M0)[1])
Model_average<-model.avg(Models,fit=T)
#model of change per year is much better than null model

Time<-data.frame(Year2=seq(1959,2014,1)-(mean(BA_change_F3$Year)))
BA_pred<-predict(M1,newdata=Time,se.fit=T,backtransform=T,level=0,)
BA_pred$Time<-Time+(mean(BA_change_F3$Year))
BA_pred<-data.frame(BA_pred)

theme_set(theme_bw(base_size=12))
BA_plot1<-ggplot(BA_change_F3,aes(x=Year,y=BA,group=Block))+geom_point(alpha=0.5,shape=1)+geom_line(alpha=0.2)
BA_plot2<-BA_plot1+geom_line(data=BA_pred,size=3,aes(x=Year2,y=fit,group=NULL,colour=NULL),colour="blue")
BA_plot3<-BA_plot2+geom_line(data=BA_pred,size=2,lty=2,aes(x=Year2,y=fit+(1.96*se.fit),group=NULL,colour=NULL),colour="blue")+geom_line(data=BA_pred,size=2,lty=2,aes(x=Year2,y=fit-(1.96*se.fit),group=NULL,colour=NULL),colour="blue")
BA_plot4<-BA_plot3+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010,2020))+ theme(legend.position="none")
BA_plot4+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+xlab("Year")+ylab("Basal area of Beech trees per hectare")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("BA_change_Beech.png",width = 8,height=6,units = "in",dpi=300)


#calculate change in beech BA as percentage relative to 1959

BA_change_prop<-merge(BA_change_F3,subset(BA_change_F3,Year==1959)[,c(1,3)],by="Block")
head(BA_change_prop)
BA_change_prop$lnRR<-log(BA_change_prop$BA.x)-log(BA_change_prop$BA.y)

ggplot(BA_change_prop,aes(x=Year,y=exp(lnRR)-1,group=Block))+geom_point(shape=1,alpha=0.5)+geom_line(alpha=0.2)


ggplot(BA_change_CC,aes(x=BA))+geom_histogram()+facet_grid(Transect~Year)

