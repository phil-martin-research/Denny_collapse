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

#make points into a spatial dataframe
coordinates(Coord)<-~lon+lat

#calculate angle of first transect
#subset to include enclosed transect only
Coord_E<-subset(Coord,Transect=="Enclosed")
#create loop to work out angle between each plot to determine the heading needed 

plot(Coord_E)
lines(Coord_E$lon,Coord_E$lat)
abs(Coord_E$lon[1]-Coord_E$lon[1+1])

Angles<-NULL
for (i in 1:nrow(Coord_E)-1){
  Lon<-abs(Coord_E$lon[i+1]-Coord_E$lon[i])
  Lat<-abs(Coord_E$lat[i+1]-Coord_E$lat[i])
  A<-ifelse(Lon<Lat,asin(Lon/Lat),asin(Lat/Lon))*57.2957795
  Angles<-rbind(Angles,data.frame(Block=Coord_E$Block[i],Angle=A))
}


x<-(Coord_E$lon[2+1]-Coord_E$lon[2])/(Coord_E$lat[2+1]-Coord_E$lat[2])
x2<-(10/(R*cos(pi*Coord_E$lat[2]/180)))
lon0<-Coord_E$lon[2] + (x*(x2 * 180/pi))



lat1<-Coord_E$lat[1]
lon1<-Coord_E$lon[1]

#Earth’s radius, sphere
R<-6378137

#offsets in meters
dn<-20
de<-20

#Coordinate offsets in radians
dLat<-dn/R
dLon<-de/(R*cos(pi*lat1/180))

#OffsetPosition, decimal degrees
lat0<-lat1 - dLat * 180/pi
lon0<-lon1 + dLon * 180/pi 


lat0



#so to work out tree distances I need to use geometry
head(Plots)
Plots_E<-subset(Plots,En.Un=="Enclosed")

#run a for loop to calculate the position of individual trees
Plots_E2<-merge(Plots_E,Angles,by.x="Block_new",by.y="Block")
for (i in 1:nrow(Plots_E2)){
  sin(Plots_E2$Angle[1])*(as.numeric(as.character(Plots_E2$Dist_south[1]))/111111)
}
Plots_E2$Dist_south[1]
Plots_E2$Dist_west[1]
Plots_E2$Angle[1]
Coord_E$lat[1]
Coord_E$lon[1]
Coord_E$lat[2]
Coord_E$lon[2]

plot(c(Coord_E$lon[1:2],Coord_E$lon[1]),c(Coord_E$lat[1:2],Coord_E$lat[1]-20/100000))

Plot1<-data.frame(Lon=c(Coord_E$lon[1:2],Coord_E$lon[1],Coord_E$lon[2]),Lat=c(Coord_E$lat[1:2],Coord_E$lat[2],Coord_E$lat[1]))
coordinates(Plot1)<-~Lon+Lat

plot(Plot1)


20/111111


summary(as.numeric(as.character(Plots_E2$Dist_south)))
summary(as.numeric(as.character(Plots_E2$Dist_west)))-10



?rbind()

(atan((max(Coord_E$lon)-min(Coord_E$lon))/(max(Coord_E$lat)-min(Coord_E$lat))))*57.2957795

atan(0.75)


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

