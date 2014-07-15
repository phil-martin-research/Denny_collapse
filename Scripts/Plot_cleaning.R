#script to tidy up data from Denny wood

library(ggplot2)
library(plyr)
library(reshape2)
library(geoR)
library(ape)

#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
#transect Latitude and longitude
Coord<-read.csv("Transect_coords.csv")
#data on trees
Plots<-read.csv("Denny_plots_edit2.csv")


#create column to give size classes of 10 cm intervals
Class<-c(seq(0,100,10),seq(120,140,20))
Plots$Class<-NA
for (i in 1:12) {
  Plots$Class<-ifelse(Plots$DBH>=Class[i]&Plots$DBH<=Class[i+1],Class[i+1],Plots$Class)
}

#change status column only be live or dead
Status<-data.frame(seq(1:4),c(1,2,1,1))
Plots$Status2<-NA
for (i in 1:length(Status)){
  Plots$Status2<-ifelse(Plots$Status==Status[i,1],Status[i,2],Plots$Status2)
}

#merge data from 1950s where data for years in not available
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
Plots3<-Plots2[complete.cases(Plots2[,28]),]
head(Plots3)
#set location as numeric
Plots3$Dist_west<-as.numeric(as.character(Plots3$Dist_west))
Plots3$Dist_south<-as.numeric(as.character(Plots3$Dist_south))

#tidy data so that only important bits are kept
Plots_final<-Plots3[ -c(3,8:19,23,24,27) ]

#save edited csv
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
write.csv(x=Plots_final,"Denny_cleaned.csv")

