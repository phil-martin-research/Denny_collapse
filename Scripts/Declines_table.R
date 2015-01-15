#script to produce table with plots classified by collapse status for each year
#and then show how many plots are in each group and their movements

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)

#load in data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("Denny_plots.csv")

keeps<-c("Year","BAPERCM")
BA2<-BA[keeps]


#classify based on declines, here we consider anything with a decline >=25% in basal area to be collapsed
for (i in 1:nrow(BA2)){
  BA2$Collapse[i]<-ifelse(BA2$BAPERCM[i]<=-0.25,1,0)
}
BA2$Year2<-ifelse(BA2$Year==1999,1996,BA2$Year)

Count_plots1<-count(BA2,"Year2")

Count_plots<-count(BA2,c("Collapse","Year2"))

#work out the proportion of plots in each group for survey period
Count_plots$Prop[y]<-NA
for (i in 1:nrow(Count_plots1)){
  for (y in 1:nrow(Count_plots)){
    Count_plots$Prop[y]<-ifelse(Count_plots$Year2[y]==Count_plots1$Year2[i],Count_plots$freq[y]/Count_plots1$freq[i],Count_plots$Prop[y])
  }
}


