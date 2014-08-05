#script to tidy up data from Denny wood

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
Plots<-read.csv("Denny_plots_edit3.csv")

#first organise data to give plot level information
head(Plots)
head(Coord)

#give block names to coordinates
Coord$Block<-as.numeric(substring(Coord$name, 2))+1
Coord<-Coord[with(Coord, order(Block)), ]
#give block names to coordinates
Plots$Block_new<-Plots$Block_new+1
Plots<-Plots[with(Plots, order(Block_new)), ]

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
#calculate distance south
for (i in 1:nrow(Block_dist)){
  Plots_E$Dist_South2<-ifelse(Plots_E$Block_new==Block_dist$Block[i],Plots_E$Dist_south-Block_dist$Distance[i],Plots_E$Dist_South2)
}
Plots2<-merge(Plots_E,Coord_E,by.x="Block_new",by.y="Block")
head(Plots2)
#calculate distance east
for (i in 1:nrow(Plots2)){
  Plots2$Easting1[i]<-Plots2$EASTING[i]-Plots2$Dist_west2[i]
  Plots2$Northing1[i]<-Plots2$NORTHING[i]-Plots2$Dist_South2[i]
}

#################################################################
#now do the same for the unenclosed transect
#work out distance for each plot for enclosed data
################################################################

head(Plots_U)


plot(Plots_U$Dist_west,Plots_U$Dist_south)
plot(Coord_U)

#work out national grid coordinated for each plot
Block_dist_U<-data.frame(Block=unique(Plots_U$Block_new),Distance=(unique(Plots_U$Block_new-51)*20)-20)
Plots_U$Dist_South2<-Plots_U$Dist_west-10
Plots_U$Dist_West2<-NA
#calculate distance south for each tree
for (i in 1:nrow(Block_dist_U)){
  Plots_U$Dist_West2<-ifelse(Plots_U$Block_new==Block_dist_U$Block[i],Plots_U$Dist_west-Block_dist_U$Distance[i],Plots_U$Dist_West2)
}

Plots2U<-merge(Plots_U,Coord_U,by.x="Block_new",by.y="Block")
head(Plots2U)
#calculate distance east
for (i in 1:nrow(Plots2U)){
  Plots2U$Easting1[i]<-Plots2U$EASTING[i]-Plots2U$Dist_west[i]
  Plots2U$Northing1[i]<-Plots2U$NORTHING[i]-Plots2U$Dist_South2[i]
}


plot(Plots2U$Easting1,Plots2U$Northing1)


ggplot(Plots2U,aes(x=Easting1,y=Northing1,colour=Status))+geom_point(shape=1)

#first subset so that only trees on the enclosed plots are included
Plots_E<-subset(Plots,En.Un=="Enclosed")


#look at plot location
df<-data.frame(Coord$lon,Coord$lat)
map<-
ggmap(get_map(maptype = 'satellite',location = c(mean(Coord$lon),mean(Coord$lat)),zoom=15))+geom_point(data=Coord,aes(x=lon,y=lat,label=Block),size=10,shape=0)
?get_map

#calculate DBH where needed
for (i in 1:nrow(Plots)){
  Plots$DBH[i]<-ifelse(is.na(Plots$DBH[i])&Plots$DBH2[i]!=0,Plots$DBH2[i],Plots$DBH[i])
}



#create column to give size classes of 10 cm intervals
Class<-c(seq(0,100,10),seq(120,140,20))
Plots$Class<-NA
for (i in 1:12) {
  Plots$Class<-ifelse(Plots$DBH>=Class[i]&Plots$DBH<=Class[i+1],Class[i+1],Plots$Class)
}

#change status column only be live or dead
Status<-data.frame(seq(1:4),c(1,0,1,1))
Plots$Status2<-NA
for (i in 1:length(Status)){
  Plots$Status2<-ifelse(Plots$Status==Status[i,1],Status[i,2],Plots$Status2)
}

#merge data from 1950s where data for years is not available
Plots_pre60<-subset(Plots,Year<=1959) #subset data to be pre 1960
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
Plots2<-merge(x=Plots,y=Dup_rm2,by=c("Tree_ID","Year"),all.x = T)

#for everything pre 1960 get the mean pre 1960 mean
for (i in 1:nrow(Plots2)){
  Plots2$DBH_mean[i]<-ifelse(Plots2$Year[i]<=1959,Plots2$DBH.y[i],Plots2$DBH.x[i])
}
#set year as 1959 for all data pre 1960
for (i in 1:nrow(Plots2)){
  Plots2$Year[i]<-ifelse(Plots2$Year[i]<=1959,1959,Plots2$Year[i])
}

#remove trees with no dbh measurements
Plots3<-Plots2[complete.cases(Plots2[,29]),]
head(Plots3)
#set location as numeric
Plots3$Dist_west<-as.numeric(as.character(Plots3$Dist_west))
Plots3$Dist_south<-as.numeric(as.character(Plots3$Dist_south))

summary(Plots3)

#tidy data so that only important bits are kept
Plots_final<-Plots3[ -c(3,8:19,24,25) ]
summary(Plots_final)


#save edited csv
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
write.csv(x=Plots_final,"Denny_cleaned.csv")

