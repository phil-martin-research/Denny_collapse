#script to produce table with plots classified by collapse status for each year

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)

#load in data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("Denny_plots.csv")

#calculate Tanner index the mean of Sorensen weighted by BA and SD
BA$Tanner<-(BA$Sorensen_BA+BA$Sorensen.SD)/2


#load functions needed for work
std <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

#classify into groups
Groups<-data.frame(max=c(-0.75,-0.50,-0.25,0,3),min=c(-1.00,-0.75,-0.50,-0.25,0),group=c("100-75%","75-50%","50-25%","25-0%","0-216% increase"))
head(BA)
BA_groups<-NULL
for(i in 1:nrow(Groups)){
  BA2<-subset(BA,BA$BAPERCM>Groups[i,2]&BA$BAPERCM<Groups[i,1])
  BA2$Group<-Groups[i,3]
  BA_groups<-rbind(BA_groups,BA2)
}

head(BA_groups)

#now do calculations of means for year and group
#first drop the colums I don't need
#I want to keep BA, species richness,sorenson, etc
keeps<-c("Year","Group","BAM","BAPERCM","SDM","Sorensen_BA","Sorensen.SD","Tanner","SPRM","LightM","NitM","MoistM")
BA_groups2<-BA_groups[keeps]
df_melt <- melt(BA_groups2, id = c("Year", "Group")) # then melt by year and group
df_melt$Year[df_melt$Year==1999]<-1996#change 1999 to 1996 becuase only one transect was surveyed in this period

#and then cast the data to produce means, SE and the number of plots in each group
Groups2_Mean<-dcast(df_melt, Year + Group ~ variable, mean,na.rm = TRUE)
Groups2_SE<-dcast(df_melt, Year + Group ~ variable, std)
#number of blocks in each group
Count_plots<-count(BA_groups,c("Year","Group"))
#merge the dataframes together
Group_summ<-merge(Groups2_Mean,Groups2_SE,by=c("Year","Group"))
Group_summ2<-merge(Group_summ,Count_plots,by=c("Year","Group"))

#rename columns
colnames(Group_summ2)<-c("Year","Group","BA_mean","BA_perc_mean","SD_mean","Sorensen_BA_mean","Sorensen_SD_mean","Tanner_mean","Spr_Mean","Light_mean","Nit_mean","Moist_mean",
                         "BA_SE","BA_perc_SE","SD_SE","Sorensen_BA_SE","Sorensen_SD_SE","Tanner_SE","Spr_SE","Light_SE","Nit_SE","Moist_SE","no_of_plots")
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

#write this csv 
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
write.csv(Group_summ3,file="Group_table.csv",row.names=F)

###########################################################################
#transitions table#########################################################
###########################################################################

#now create a table showing the transitions between one group and another for all blocks
keeps2<-c("Block","Year","Group","BAPERCM")
BA_trans<-BA_groups[keeps2]
head(BA_trans)
BA_trans<-subset(BA_trans,Year>1964)

#create a loop to class groups as numeric values
Group_num<-data.frame(max=c(-0.75,-0.50,-0.25,0,3),min=c(-1.00,-0.75,-0.50,-0.25,0),group=c(5,4,3,2,1))
BA_trans3<-NULL
for(i in 1:nrow(BA_trans)){
  BA_trans2<-subset(BA_trans,BAPERCM>Group_num[i,2]&BAPERCM<Group_num[i,1])
  BA_trans2$Group<-Group_num[i,3]
  BA_trans3<-rbind(BA_trans2,BA_trans3)
}


#create a loop to produce a column to concatenate changes in status
Blocks<-unique(BA_trans3$Block)
Blocks<-sort(Blocks)
BA_blocks<-NULL
for (i in 1:nrow(BA_trans3)){
  BA_block_sub<-subset(BA_trans3,Block==Blocks[i])
  BA_block_sub$Trans<-NA
  BA_block_sub$Trans[1]<-paste("1-",BA_block_sub$Group[1],sep ="")
  for (y in 2:nrow(BA_block_sub)){
    BA_block_sub$Trans[y]<-paste(BA_block_sub$Group[y-1],"-",BA_block_sub$Group[y],sep ="")
  }
BA_blocks<-rbind(BA_block_sub,BA_blocks)
}


#write this csv 
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
write.csv(BA_blocks,file="Transitions_table.csv",row.names=F)

