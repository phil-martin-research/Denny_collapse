#script to clean data from Denny wood plots and put into .csv format for use in database

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
Plots<-read.csv("Denny_plots_edit3.csv")

#give block names to coordinates
Coord$Block<-as.numeric(substring(Coord$name, 2))+1
Coord<-Coord[with(Coord, order(Block)), ]
#give block names to coordinates
Plots$Block_new<-Plots$Block_new+1
Plots<-Plots[with(Plots, order(Block_new)), ]
#convert distances south and west to numeric
Plots$Dist_west<-as.numeric(as.character(Plots$Dist_west))
Plots$Dist_south<-as.numeric(as.character(Plots$Dist_south))


#convert coordinates into OS grid
Coord2<-cbind(Coord,(gps_latlon2gr(latitude=Coord$lat,longitude=Coord$lon)[,2:3]))
#turn into spatial dataframe
coordinates(Coord2) = ~EASTING + NORTHING

#merge data on years with coordinates
Plots$Block<-Plots$Block_new
Lookup<-data.frame(unique(Plots[,c('Block','Year')]))
Blocks<-join(Coord2, Lookup, by = "Block")


#only keep useful stuff
Block_final<-data.frame(Block=Blocks$Block,Year=Blocks$Year,Easting=Blocks$EASTING,Northing=Blocks$NORTHING,Transect=Blocks$Transect)


#save edited csv
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
write.csv(x=Block_final,"Denny_plots_cleaned.csv",row.names=FALSE)
