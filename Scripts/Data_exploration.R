#script to produce exploratory plots for Denny Wood data

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)


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



###############################################
#below this point analysis may need changing###
###############################################

#produce plots of change in size structure
DBH_count<-count(Plots3,c("Year","Class"))
Count_1959<-subset(DBH_count,Year==1959)
DBH_count$LnRR<-log(DBH_count$freq)-log(Count_1959$freq)
DBH_count$Change<-(DBH_count$freq)-(Count_1959$freq)
DBH_count2<-subset(DBH_count,Year!=1959)

ggplot(DBH_count2,aes(x=Class,y=Change))+geom_point()+facet_wrap(~Year)+geom_hline(y=0,lty=2)
ggplot(DBH_count2,aes(x=as.factor(Class),y=exp(LnRR)-1))+geom_bar(stat="identity")+facet_wrap(~Year)+geom_hline(y=0,lty=2)+xlab("Upper limit for DBH size class")+ylab("Change in stem density relative to 1958/9")

ggplot(DBH_count,aes(x=Year-1959,y=exp(LnRR)-1,colour=as.factor(Class)))+geom_smooth(se=F,method="lm")+geom_hline(y=0,lty=2)


#look at variation in DBH in space
Plots_1996<-subset(Plots2,Year==1996)
head(Plots_1996)
dists<-dist(Plots_1996[,3:4])

summary(dists)
breaks<-seq(0,1000,20)
v1 <- variog(coords = Plots_1996[,3:4], data = Plots_1996[,8], breaks = breaks)
v1.summary <- cbind(c(1:51), v1$v, v1$n)
colnames(v1.summary) <- c("lag", "semi", "No_pairs")

plot(v1, type = "b", main = "Variogram: Av8top") 

VG<-data.frame(v1.summary)
VG$distance<-seq(0,1000,20)
head(VG)

ggplot(data=VG,aes(x=distance,y=semi,size=No_pairs))+geom_line(size=0.5)+geom_point(alpha=0.5)

#now produce morans I statistic
dists2<-as.matrix(dist(cbind(Plots_1996$Dist_south, Plots_1996$Dist_west)))
inv.dists<-1/dists2
diag(inv.dists) <- 0
inv.dists[1:5, 1:5]
Moran.I(Plots_1996$DBH, inv.dists)

#produce basal area per plot
Plots2$BA<-(((Plots2$DBH^2)*(pi/4))/10000)
Plots2$BA2<-(Plots2$DBH^2)*0.00007854

tapply(Plots2$BA,Plots2$Block,sum)*4

head(Plots2)

BA_change<-data.frame(with(Plots2, tapply(BA2,list(Block,Year), sum)))
BA_change$Block<-as.factor(rownames(BA_change))

BA_change2<-melt(BA_change,id.vars = "Block")
head(BA_change2)
subset(BA_change2)
?gsub
BA_change2$Year<-as.numeric(gsub("X","",BA_change2$variable))
BA_change3<-subset(BA_change2,grepl("out",BA_change2$Block)==F&Year>1958)

a<-ggplot(BA_change3,aes(x=Year,y=(value)*25,group=Block))+geom_line()
a+geom_smooth(aes(group=NA),method="lm",se=F,size=4)+ylab("Basal area")
