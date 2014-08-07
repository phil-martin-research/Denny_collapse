#script to analyse changes in basal area in Denny Wood

rm(list=ls(all=TRUE))

#open packages neeeded for analysis
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(nlme)
library(MuMIn)


#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Location<-read.csv("Plot_coords.csv")
Location<-unique(Location[,c(3,5:6)])
DBH<-read.csv("Denny_trees_cleaned.csv")
head(DBH)
head(Location)


#####################################
#analyses of basal area change#######
#####################################
head(DBH)
#calculate basal area per plot per time period
#for all species
DBH$BA<-ifelse(DBH$DBH>10,DBH$DBH^2*(pi/4),0)
#for beech only
DBH$Beech_BA<-DBH$BA<-ifelse(DBH$DBH>10&DBH$Species=="F",DBH$DBH^2*(pi/4),0)

#total basal area
BA_change<-melt(with(DBH, tapply(BA,list(Block,Year), function(x) sum(na.omit(x)/400))))
colnames(BA_change)<-c("Block","Year","BA")
BA_change$Transect<-ifelse(BA_change$Block>=52,"Unenclosed","Enclosed")
BA_change_CC<-BA_change[complete.cases(BA_change),]

#plot time series of BA for both transects
ggplot(BA_change_CC,aes(x=Year,y=BA,group=Block,colour=Transect))+geom_line()+geom_point()+facet_wrap(~Block)


Plots_2014<-subset(BA_change_CC,Year==2014)
#test change in basal area as a function of area sampled
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
head(DBH)
BA_change2<-((merge(y=Location,x=BA_change_CC,by.x=c("Block"),by.y=c("Plot_number"),all.x=F)))
head(BA_change2)


#exploratory plot of basal area
ggplot(BA_change2,aes(x=Easting,y=Northing,colour=BA))+geom_point(shape=15)+facet_wrap(~Year)

############################################
#explore changes in spatial autocorrelation#
###########################################
head(BA_change2)
BA_change2$Transect<-as.factor(BA_change2$Transect)
  
BA_change_en<-subset(BA_change2,Transect.x=="Enclosed")



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

head(Semi_var)

#plot semi-variogram by year
theme_set(theme_bw(base_size=12))
BA_semi<-ggplot(Semi_var,aes(Distance,semi,group=Year))+geom_point(shape=1)+geom_line()+facet_wrap(~Year)
BA_semi+ylab("semi-variance")+xlab("Distance (m)")+geom_smooth(se=F,method="lm",size=2)
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("BA_semi.png",width = 8,height=6,units = "in",dpi=300)


#time-series model for change in BA in Denny wood
f1<-formula(BA.x~Year)
B1.gls<-gls(f1,data=BA_change_en)
Vario.gls<-Variogram(B1.gls,form=~ Easting + Northing,robust=TRUE,maxdist=1000,resType="pearson")
plot(Vario.gls,smooth=T)
#now attempt to account for this spatial autorcorrelation
#create second variable for year so that it is centred
BA_change2$Year2<-BA_change2$Year-mean(BA_change2$Year)

head(BA_change2)

B0<-gls(BA~1,correlation=corSpher(form=~ Easting + Northing|Year, nugget=T),data=BA_change2,method="ML")
B1A<-gls(BA~Year2,correlation=corSpher(form=~ Easting + Northing|Block|Year, nugget=T),data=BA_change2,method="ML")
B2A<-lme(log(BA+1)~Year2,random=~1|Block,data=BA_change2,method="REML")
B2B<-lme(log(BA+1)~Year2,random=~Year|Block,correlation=corSpher(form=~ Easting + Northing|Year),data=BA_change2,method="REML")

plot(B2B)
qqnorm(B2A)

Models<-dredge(B1A,rank = AICc,trace = T)
Model_sel<-mod.sel(Models)
Model_average<-model.avg(Models,fit=T)

Time<-data.frame(Year2=seq(1959,2014,1)-(mean(BA_change2$Year)))

predict(B0,Time)

predict(Model_average,newdata=Time,se.fit=T,backtransform=T,level=0)

ggplot(BA_change2,aes(x=Year,y=BA,group=Block,colour=Transect))+geom_point(alpha=0.7)+geom_line(alpha=0.8)+geom_smooth(se=F,method="lm",size=4,aes(group=NULL,colour=NULL))
