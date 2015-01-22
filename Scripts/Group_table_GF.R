#script to produce table with plots classified by collapse status for each year for ground flora

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)

#load in data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
GF<-read.csv("BA_GF_ALL.csv")

head(GF)

#load functions needed for work
std <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

#now do calculations of means for year and group
#first drop the colums I don't need
#I want to keep BA, species richness,sorenson, etc
keeps<-c("Year","Coll_Group","BAM","BAPERCM","Perc_Cov","Sorensen","Sp_R","Light","Nit","Moist")
GF2<-GF[keeps]
df_melt <- melt(GF2, id = c("Year", "Coll_Group")) # then melt by year and group
df_melt$Year[df_melt$Year==1999]<-1996#change 1999 to 1996 becuase only one transect was surveyed in this period

#and then cast the data to produce means, SE and the number of plots in each group
Groups2_Mean<-dcast(df_melt, Year + Coll_Group ~ variable, mean,na.rm = TRUE)
Groups2_SE<-dcast(df_melt, Year + Coll_Group ~ variable, std)
#number of blocks in each group
Count_plots<-count(GF2,c("Year","Coll_Group"))
#merge the dataframes together
Group_summ<-merge(Groups2_Mean,Groups2_SE,by=c("Year","Coll_Group"))
Group_summ2<-merge(Group_summ,Count_plots,by=c("Year","Coll_Group"))

#rename columns
head(Group_summ2)
colnames(Group_summ2)<-c("Year","Group","BA_mean","BA_perc_mean","Grass_Cover_mean","Sorensen_mean","Spr_Mean","Light_mean","Nit_mean","Moist_mean",
                         "BA_SE","BA_perc_SE","Grass_Cover_SE","Sorensen_SE","Spr_SE","Light_SE","Nit_SE","Moist_SE","no_of_plots")
#run a loop to work out the proportion of plots in each period found in each group 
Plot_count<-data.frame(tapply(Group_summ2$no_of_plots,Group_summ2$Year,sum))
colnames(Plot_count)<-"Count"
Plot_count$Year<-as.numeric(rownames(Plot_count))
Group_summ3<-NULL
for (i in 1:nrow(Plot_count)){
  Group_plot<-subset(Group_summ2,Year==Plot_count[i,2])
  Group_plot$Prop_plot<-Group_plot$no_of_plots/Plot_count[i,1]
  Group_summ3<-rbind(Group_summ3,Group_plot)
}
#multiply proportions to give percentage
Group_summ3$BA_perc_mean<-Group_summ3$BA_perc_mean*100
Group_summ3$BA_perc_SE<-Group_summ3$BA_perc_SE*100

#round numeric columns to two decimal places
is.num <- sapply(Group_summ3, is.numeric)
Group_summ3[is.num] <- lapply(Group_summ3[is.num], round, 2)
head(Group_summ3)

#now create columns with means and standard errors in brackets

Group_summ3$BA<-paste(Group_summ3$BA_mean,"(",Group_summ3$BA_SE,")",sep="")
Group_summ3$BA_perc<-paste(Group_summ3$BA_perc_mean,"(",Group_summ3$BA_perc_SE,")",sep="")
Group_summ3$Grass_Cover<-paste(Group_summ3$Grass_Cover_mean,"(",Group_summ3$Grass_Cover_SE,")",sep="")
Group_summ3$Sorensen<-paste(Group_summ3$Sorensen_mean,"(",Group_summ3$Sorensen_SE,")",sep="")
Group_summ3$Spr<-paste(Group_summ3$Spr_mean,"(",Group_summ3$Spr_SE,")",sep="")
Group_summ3$Light<-paste(Group_summ3$Light_mean,"(",Group_summ3$Light_SE,")",sep="")
Group_summ3$Nit<-paste(Group_summ3$Nit_mean,"(",Group_summ3$Nit_SE,")",sep="")
Group_summ3$Moist<-paste(Group_summ3$Moist_mean,"(",Group_summ3$Moist_SE,")",sep="")


keeps<-c("Year","Group","no_of_plots","Prop_plot","BA","BA_perc","Grass_Cover","Sorensen","Spr","Light","Nit","Moist")
Group_summ4<-Group_summ3[keeps]
#write this csv 
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
write.csv(Group_summ4,file="Group_table_GF.csv",row.names=F)


