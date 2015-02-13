#script to produce table with plots classified by collapse status for each year

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)

#load in data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("Data/Denny_plots.csv")
BA$Open_transect<-ifelse(BA$Block>51,1,0)

#calculate Tanner index the mean of Sorensen weighted by BA and SD
BA$Tanner<-(BA$Sor_BA+BA$SorM)/2

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

head(BA_groups)

#now do calculations of means for year and group
#first drop the colums I don't need
#I want to keep BA, species richness,sorenson, etc
keeps<-c("Year","Group","BAM","BAPERCM","SDM","Sor_BA","SorM","Tanner","SPRM","LightM","NitM","MoistM","Open_transect")
BA_groups2<-BA_groups[keeps]
BA_groups2$Year[BA_groups2$Year==1999]<-1996

head(BA_groups2)
Group_summ<-ddply(BA_groups2,.(Year,Group),summarize,
      MEAN_BA=paste(round(mean(BAM),2)," (",round(std(BAM),2),")",sep =""),
      MEAN_BAPERC=paste(round(mean(BAPERCM*100),2)," (",round(std(BAPERCM*100),2),")",sep =""),
      SDM=paste(round(mean(SDM),2)," (",round(std(SDM),2),")",sep =""),
      Tanner=paste(round(mean(Tanner),2)," (",round(std(Tanner),2),")",sep =""),
      SPRM=paste(round(mean(SPRM),2)," (",round(std(SPRM),2),")",sep =""),
      LightM=paste(round(mean(LightM,na.rm = TRUE),2)," (",round(std(LightM),2),")",sep =""),
      NitM=paste(round(mean(NitM,na.rm = TRUE),2)," (",round(std(NitM),2),")",sep =""),
      No=length(BAPERCM),
      Number_open=sum(Open_transect))

#rename columns
colnames(Group_summ)<-c("Year","Group","BA","BA_perc","SD","Tanner","Spr","Light","Nit","no_of_plots","No_open")

#write this to a csv 
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
write.csv(Group_summ,file="Figures/Group_table.csv",row.names=F)

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
for(i in 1:nrow(Group_num)){
  BA_trans2<-subset(BA_trans,BAPERCM>Group_num[i,2]&BAPERCM<Group_num[i,1])
  BA_trans2$Group<-Group_num[i,3]
  BA_trans3<-rbind(BA_trans2,BA_trans3)
}

#for the sake of this work group together years 1999 and 1996 into 1999

BA_trans3$Year<-ifelse(BA_trans3$Year==1996,1999,BA_trans3$Year)

#create a loop to produce a column to concatenate changes in status
Blocks<-unique(BA_trans3$Block)
Blocks<-sort(Blocks)
BA_trans3<-BA_trans3[with(BA_trans3, order(Year,Block)), ]
BA_blocks<-NULL
for (i in 1:length(Blocks)){
  BA_block_sub<-subset(BA_trans3,Block==Blocks[i])
  BA_block_sub$Trans<-NA
  BA_block_sub$Trans[1]<-paste("1-",BA_block_sub$Group[1],sep ="")
  for (y in 2:nrow(BA_block_sub)){
    BA_block_sub$Time1[1]<-1
    BA_block_sub$Time1[y]<-ifelse(is.na(BA_block_sub$Group[y-1]),BA_block_sub$Time1[y-1],BA_block_sub$Group[y-1])
    BA_block_sub$Trans[y]<-paste(BA_block_sub$Group[y-1],"-",BA_block_sub$Group[y],sep ="")
  }
BA_blocks<-rbind(BA_block_sub,BA_blocks)
}

BA_blocks<-BA_blocks[with(BA_blocks, order(Year,Block)), ]



#write this csv 
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
write.csv(BA_blocks,file="Transitions_table.csv",row.names=F)

#create matrices for group transitions
Transition_matrix<-count(BA_blocks,vars=c("Trans","Year","Time1","Group"))

Transition_matrix[with(Transition_matrix, order(Year, Trans)), ]

setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
write.csv(Transition_matrix,file="Transition_matrix.csv",row.names=F)




