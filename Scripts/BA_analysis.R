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
BA<-read.csv("Denny_plots.csv")


##############################################################
#test change in basal area as a function of area sampled#####
#############################################################

Plots_2014<-subset(BA,Year==2014)
BA_area<-NULL
BA_area$Plots<-1:nrow(Plots_2014)
BA_area$Area<-(BA_area$Plots)*(20*20)
BA_area<-data.frame(BA_area)
BA_results<-NULL

#loop to add one plot at a time and calculate mean iterated 1000 times
for (i in 1:1000){
  R_sample<-sample(Plots_2014$BAM,nrow(Plots_2014))
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


############################################
#explore changes in spatial autocorrelation#
###########################################

#do this comparison for each year's worth of data
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



########################################################################################################
#model change in BA over time###########################################################################
########################################################################################################

#models of change of total BA with time
#create second variable for year so that it is centred
BA_change2$Year2<-BA_change2$Year-mean(BA_change2$Year)
#run models
M0<-lme(BA~1,random=~Year|Block,data=BA_change2,method="REML")
M1<-lme(BA~Year2,random=~1|Block,data=BA_change2,method="REML")
M2<-lme(log(BA+1)~Year2,random=~Year|Block,data=BA_change2,method="REML")

ggplot(data.frame(BA_change2,res=resid(M2)),aes(Easting,Northing,size=abs(res),colour=res))+geom_point()+facet_wrap(~Year)+scale_colour_gradient2(mid="grey")



#plot residuals and qqplots
grid.arrange(plot(M0),plot(M1),plot(M2),qqnorm(M0),qqnorm(M1),qqnorm(M2),nrow=2)
#the untransformed data looks best

#perform model averaging for null and yearly change models
Models<-dredge(M1,rank = AICc,trace = T,REML=F)
Model_sel<-mod.sel(Models)
Model_sel$R_squared<-c(r.squaredGLMM(M1)[1],r.squaredGLMM(M0)[1])
Model_average<-model.avg(Models,fit=T)
#model of change per year is much better than null model

Time<-data.frame(Year2=seq(1964,2014,1)-(mean(BA_change2$Year)))
BA_pred<-predict(M1,newdata=Time,se.fit=T,backtransform=T,level=0,)
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

Time<-data.frame(Year2=seq(1964,2014,1)-(mean(BA_change_F3$Year)))
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


##################################################################################
#histograms to look at possible bimodal distribution of basal area################
#################################################################################

BA_change_CC2<-BA_change_CC
BA_change_CC2[BA_change_CC2==1996]<-1999
BA_change_CC2<-BA_change_CC2[!(BA_change_CC2$Year %in% c(1988,1984)),]

BA_hist<-ggplot(BA_change_CC2,aes(x=BA))+geom_histogram(binwidth=10)+facet_wrap(~Year)+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ylab("number of plots")
BA_hist+xlab("Basal area per hectare")+ylab("Number of plots")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("BA_hist.png",width = 8,height = 4,units = "in",dpi = 300)


#################################################################################
#basal area analyses for Adrian##################################################
#################################################################################

#plots histogram of BA change by 2014
BA2014<-subset(BA,Year==2014)
keeps <- c("Block","BAPERCM","BAFPERCM","BAQPERCM")
BA2014_2<-BA2014[keeps]
BA_melt<-melt(BA2014_2,id.vars = "Block")
head(BA_melt)

#create labels for facets
tree_names <- list(
  'BAPERCM'="Total basal area",
  'BAFPERCM#2'="Beech basal area",
  'BAQPERCM#3'="Oak basal area"
)
tree_labeller <- function(variable,value){
  return(tree_names[value])
}

theme_set(theme_bw(base_size=12))
BA_hist<-ggplot(BA_melt,aes(x=value*100))+geom_histogram()+facet_grid(.~variable,labeller=tree_labeller)+xlab("Percentage change in basal area relative to 1964")
BA_hist+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+coord_cartesian(ylim=c(0,25))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("BA_hists.png",width = 8,height = 4,units = "in",dpi = 300)

