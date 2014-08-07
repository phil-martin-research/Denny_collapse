#script to tidy up data from Denny wood


rm(list=ls(all=TRUE))

library(ggplot2)
library(plyr)
library(reshape2)
library(geoR)
library(ape)
library(ggmap)
library(sp)
library(raster)  
library(rgdal)  
library(rgeos)  
library(spdep)
library(BRCmap)


#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
#transect Latitude and longitude
Coord<-read.csv("Transect_coords.csv")
#data on trees
Plots<-read.csv("Denny_plots_edit4.csv")

#first organise data to give plot level information
summary(Plots)
head(Coord)

#give block names to coordinates
Coord$Block<-as.numeric(substring(Coord$name, 2))+1
Coord<-Coord[with(Coord, order(Block)), ]
#give block names to coordinates
Plots$Block_new<-Plots$Block_new+1
Plots<-Plots[with(Plots, order(Block_new)), ]
#convert distances south and west to numeric
Plots$Dist_west<-as.numeric(as.character(Plots$Dist_west))
Plots$Dist_south<-as.numeric(as.character(Plots$Dist_south))
#define areas as in or out of transect based on west and south distances, if they are negative they are out
Plots$In_Out<-as.factor(ifelse(Plots$Dist_west<0|Plots$Dist_south<0,"Out","In"))

#convert coordinates into OS grid
Coord2<-cbind(Coord,(gps_latlon2gr(latitude=Coord$lat,longitude=Coord$lon)[,2:3]))
#turn into spatial dataframe
coordinates(Coord2) = ~EASTING + NORTHING

#subset to include enclosed transect only
Coord_E<-subset(Coord2,Transect=="Enclosed")
Plots_E<-subset(Plots,En.Un=="Enclosed")
#subset for unenclosed transect
Coord_U<-subset(Coord2,Transect=="Unenclosed")
Plots_U<-subset(Plots,En.Un=="Unenclosed")



#work out distance for each plot for enclosed data
Block_dist<-data.frame(Block=unique(Plots_E$Block_new),Distance=(unique(Plots_E$Block_new)*20)-20)
Plots_E$Dist_South2<-NA
Plots_E$Dist_West2<-Plots_E$Dist_west-10
#calculate distance south
for (i in 1:nrow(Block_dist)){
  Plots_E$Dist_South2<-ifelse(Plots_E$Block_new==Block_dist$Block[i],Plots_E$Dist_south-Block_dist$Distance[i],Plots_E$Dist_South2)
}
Plots2<-merge(Plots_E,Coord_E,by.x="Block_new",by.y="Block")
head(Plots2)
#calculate distance east and north
for (i in 1:nrow(Plots2)){
  Plots2$Easting1[i]<-Plots2$EASTING[i]-Plots2$Dist_West2[i]
  Plots2$Northing1[i]<-Plots2$NORTHING[i]-Plots2$Dist_South2[i]
}
str(Plots2)

plot(Coord_E)
points(Plots2$Easting1,Plots2$Northing1)

ggplot(Plots2,aes(x=Easting1,y=Northing1))+geom_point(shape=1)+facet_wrap(~Year)

#################################################################
#now do the same for the unenclosed transect
#work out distance for each plot for enclosed data
################################################################

#work out national grid coordinates for each plot
Block_dist_U<-data.frame(Block=unique(Plots_U$Block_new),Distance=(unique(Plots_U$Block_new-51)*20))
Block_dist_U<-Block_dist_U[complete.cases(Block_dist_U),]
Plots_U$Dist_South2<-Plots_U$Dist_west-10
Plots_U$Dist_West2<-NA
#calculate distance south for each tree
for (i in 1:nrow(Block_dist_U)){
  Plots_U$Dist_West2<-ifelse(Plots_U$Block_new==Block_dist_U$Block[i],Plots_U$Dist_south-Block_dist_U$Distance[i],Plots_U$Dist_West2)
}

Plots2U<-merge(Plots_U,Coord_U,by.x="Block_new",by.y="Block")
subset(Plots2U,Year==2014)
#calculate distance east
for (i in 1:nrow(Plots2U)){
  Plots2U$Easting1[i]<-Plots2U$EASTING[i]-Plots2U$Dist_West2[i]
  Plots2U$Northing1[i]<-Plots2U$NORTHING[i]-Plots2U$Dist_South2[i]
}

ggplot(Plots2U,aes(x=Easting1,y=Northing1,colour=DBH))+geom_point(shape=16,size=4)+facet_wrap(~Year)


#put data back together again
Plots_comb<-rbind(Plots2,Plots2U)

#calculate DBH where needed
for (i in 1:nrow(Plots_comb)){
  Plots_comb$DBH[i]<-ifelse(is.na(Plots_comb$DBH[i])&Plots_comb$DBH2[i]!=0,Plots_comb$DBH2[i],Plots_comb$DBH[i])
}

hist(Plots_comb$DBH)

ggplot(Plots_comb,aes(x=Easting1,y=Northing1,colour=DBH,alpha=DBH))+geom_point(shape=16)+facet_wrap(~Year)+scale_colour_gradient(low="light grey",high="dark green",limits=c(10,240))

#create column to give size classes of 10 cm intervals
Class<-c(seq(0,100,10),seq(120,140,20))
Plots_comb$Class<-NA
for (i in 1:12) {
  Plots_comb$Class<-ifelse(Plots_comb$DBH>=Class[i]&Plots_comb$DBH<=Class[i+1],Class[i+1],Plots_comb$Class)
}

#change status column only be live or dead
levels(Plots_comb$Status)

Status<-data.frame(seq(1:4),c(1,0,1,1))
Plots_comb$Status2<-NA
for (i in 1:length(Status)){
  Plots_comb$Status2<-ifelse(Plots_comb$Status==Status[i,1],Status[i,2],Plots_comb$Status2)
}

#merge data from 1950s where data for years is not available
Plots_pre60<-subset(Plots_comb,Year<=1959) #subset data to be pre 1960
#produce mean DBH for this period for each tree
Pre_mean<-data.frame(tapply(Plots_pre60$DBH,Plots_pre60$Tree_ID,mean, na.rm=TRUE)) 
#give each tree an ID and a mean DBH value
colnames(Pre_mean)<-c("DBH")
Pre_mean$ID<-as.factor(row.names(Pre_mean))

#join the two dataframes 
Plots_pre60_2<-merge(Plots_pre60,Pre_mean,by.x="Tree_ID",by.y="ID")

#remove na values and duplicates trees
Plots_pre60_3<-Plots_pre60_2[complete.cases(Plots_pre60_2[,27]),]
Dup_rm<-subset(Plots_pre60_3, !duplicated(Plots_pre60_3[,1])) 
head(Dup_rm)
Dup_rm2<-data.frame(Tree_ID=Dup_rm$Tree_ID,DBH=Dup_rm$DBH.y,Year=Dup_rm$Year)

#merge datasets together
Plots_comb2<-merge(x=Plots_comb,y=Dup_rm2,by=c("Tree_ID","Year"),all.x = T)

#for everything pre 1960 get the mean pre 1960 mean
for (i in 1:nrow(Plots_comb2)){
  Plots_comb2$DBH_mean[i]<-ifelse(Plots_comb2$Year[i]<=1959,Plots_comb2$DBH.y[i],Plots_comb2$DBH.x[i])
}
#set year as 1959 for all data pre 1960
for (i in 1:nrow(Plots_comb2)){
  Plots_comb2$Year[i]<-ifelse(Plots_comb2$Year[i]<=1959,1959,Plots_comb2$Year[i])
}


str(Plots_comb2)

Plots_2014<-subset(Plots_comb,Year==2014)


ggplot(Plots_2014,aes(x=Transect,y=DBH))+geom_point()

#remove trees with no dbh measurements
Plots3<-Plots_comb2[complete.cases(Plots_comb2$DBH_mean),]
head(Plots3)
ggplot(Plots3,aes(x=Year,y=DBH_mean))+geom_point()+facet_wrap(~Transect)

#set as snag or not
Plots3$Snag<-as.numeric(as.character(Plots3$Snag))
Plots3$Snag<-ifelse(is.na(Plots3$Snag),"No","Yes")


ggplot(Plots3,aes(Easting1,Northing1,colour=as.factor(Snag)))+geom_point(shape=1)+facet_wrap(~Year)

#tidy data so that only important bits are kept
summary(Plots3)

Plots_final<-data.frame(Block=Plots3$Block_new,Year=Plots3$Year,Tree_ID=Plots3$Tree_ID,GBH=Plots3$GBH,DBH=Plots3$DBH_mean,
                        Status=Plots3$Status2,Snag=as.factor(Plots3$Snag),Species=as.factor(Plots3$Sp),Easting=Plots3$Easting1,Northing=Plots3$Northing1)


head(Plots_final)

#save edited csv
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
write.csv(x=Plots_final,"Denny_trees_cleaned.csv",row.names=FALSE)

