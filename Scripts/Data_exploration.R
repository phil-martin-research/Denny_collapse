#script to produce exploratory plots for Denny Wood data

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)


#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Denny<-read.csv("Denny_cleaned.csv")
head(Denny)
summary(Denny)


#############################################
#analyses of stem density change#############
#############################################


#calculate stem density per plot per time period
Stem_density<-(with(Denny, tapply(Tree_ID,list(Block_new,Year), function(x) length(unique(na.omit(x))))))
SD_melt<-melt(Stem_density)
head(SD_melt)
colnames(SD_melt)<-c("Block","Year","SD")
SD_melt$Transect<-ifelse(SD_melt$Block>=51,"Unenclosed","Enclosed")
SD_melt_CC<-SD_melt[complete.cases(SD_melt),]

#plot time series of stem density change per plot for both transects
ggplot(SD_melt_CC,aes(x=Year,y=SD,group=Block,colour=Transect))+geom_line()+geom_smooth(size=2,se=F,method="lm",aes(group=NULL))+facet_wrap(~Transect)

#calculate percentage change in stem density per plot
SD_perc_change<-merge(x=SD_melt_CC,y=subset(SD_melt_CC,Year==1959),by ="Block")
SD_perc_change$Perc_change<-((SD_perc_change$SD.x/SD_perc_change$SD.y)*100)-100

#plot graph of change
ggplot(SD_perc_change,aes(x=Perc_change))+geom_histogram()+facet_grid(Transect.x~Year.x)+geom_vline(x=0,lty=2)
ggplot(SD_perc_change,aes(x=Year.x,y=Perc_change,group=Block,colour=Transect.x))+geom_line(aplha=0.5)+geom_smooth(size=4,se=F,method="loess",aes(group=NULL),)



#####################################
#analyses of basal area change#######
#####################################


#calculate basal area per plot per time period
Denny$BA<-Denny$DBH_mean^2*(pi/4)
BA_change<-melt(with(Denny, tapply(BA,list(Block_new,Year), function(x) sum(na.omit(x))/400)))
colnames(BA_change)<-c("Block","Year","BA")
BA_change$Transect<-ifelse(BA_change$Block>=51,"Unenclosed","Enclosed")
BA_change_CC<-BA_change[complete.cases(BA_change),]

#plot time series of BA for both transects
ggplot(BA_change_CC,aes(x=Year,y=BA,group=Block,colour=Transect))+geom_line()+geom_smooth(size=2,se=F,method="lm",aes(group=NULL))+facet_wrap(~Transect)

#calculate percentage change since first survey
BA_perc_change<-merge(x=BA_change_CC,y=subset(BA_change_CC,Year==1959),by ="Block")
BA_perc_change$Perc_change<-((BA_perc_change$BA.x/BA_perc_change$BA.y)*100)-100


#plots of percentage change
ggplot(BA_perc_change,aes(x=Perc_change))+geom_histogram()+facet_grid(Transect.x~Year.x)
ggplot(BA_perc_change,aes(x=Year.x,y=Perc_change,group=Block,colour=Transect.x))+geom_line(aplha=0.5)+geom_smooth(size=4,se=F,method="loess",aes(group=NULL),)

#quick plots to check correlation between changes in stem density and basal area
Perc_SD_BA<-merge(BA_perc_change,SD_perc_change,by=c("Block","Year.x"))
head(Perc_SD_BA)
ggplot(Perc_SD_BA,aes(x=Perc_change.y,y=Perc_change.x,group=Block,colour=as.factor(Year.x)))+geom_point()+facet_wrap(~Year.x)
ggplot(Perc_SD_BA,aes(x=SD.y,y=BA.x,group=Block,colour=as.factor(Year.x)))+geom_point()+facet_wrap(~Year.x)

#####################################
#analysis of mean transect SD and BA#
#####################################

#function to calculate SE
std <- function(x) sd(x)/sqrt(length(x))

#work out mean and SE of basal area change
#first basal area
Transect_BA<-cbind(melt(with(BA_change_CC, tapply(BA,list(Transect,Year), function(x) mean(na.omit(x))))),
melt(with(BA_change_CC, tapply(BA,list(Transect,Year), function(x) (sd(x)/sqrt(length(x)))*1.96))))
colnames(Transect_BA)<-c("Transect","Year","BA","Transect_1","Year_1","CI")
#plot results
ggplot(Transect_BA,aes(x=Year,y=BA,ymax=BA+CI,ymin=BA-CI,colour=Transect))+geom_pointrange()+facet_wrap(~Transect)

#now stem density

Transect_SD<-cbind(melt(with(SD_melt_CC, tapply(SD,list(Transect,Year), function(x) mean(na.omit(x))))),
                   melt(with(SD_melt_CC, tapply(SD,list(Transect,Year), function(x) (sd(x)/sqrt(length(x)))*1.96))))
colnames(Transect_SD)<-c("Transect","Year","SD","Transect_1","Year_1","CI")
#plot results
ggplot(Transect_SD,aes(x=Year,y=SD,ymax=SD+CI,ymin=SD-CI,colour=Transect))+geom_pointrange()+facet_wrap(~Transect)

#########################################################
#exploratory analysis to look at spatial autocorrelation#
#########################################################

plot(Plots_1959[,10:11])

#first at the level of individual tree's DBH
#loop through different year's data to give an idea of spatial autocorrelation over time
Years<-unique(Denny$Year)
for (i in 1:length(Years)){
  Plots_year<-subset(Denny,Year==Years[i])
  dists<-dist(Plots_year[,10:11])
  breaks<-seq(0,0.01,0.0001)
  v1 <- variog(coords = Plots_year[,10:11], data = Plots_year[,14], breaks = breaks)
  v1.summary <- cbind(c(1:length(v1$v)), v1$v, v1$n,Years[i])
  colnames(v1.summary) <- c("lag", "semi", "No_pairs","Year")
  setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data/Processing")
  write.csv(v1.summary,paste("V1_DBH",Years[i],".csv"))
}

#rbind all years data together
files  <- list.files(pattern = '\\.csv')
tables <- lapply(files, read.csv, header = TRUE)
combined.df <- do.call(rbind , tables)

#plot semi-variogram for all years
ggplot(data=combined.df,aes(x=lag,y=semi,size=No_pairs))+geom_line(size=0.5)+geom_point(alpha=0.5)+facet_wrap(~Year)

#next look at plot level - stem density

#next look at plot level - BA

