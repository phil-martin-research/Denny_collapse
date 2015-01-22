#script to do analysis of changes in abundance of trees in different size classes

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)

#load in data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Trees<-read.csv("Denny_trees_cleaned.csv")
BA<-read.csv("BA_Gradient.csv")

#remove dead trees
Trees_live<-subset(Trees,Status==1)
#remove trees <10cm
Trees_M<-subset(Trees_live,DBH>10)

#create a loop to classify trees into different size classes into quantiles
quantile(Trees_M$DBH)
Trees_M_Size<-NULL
Size_class<-data.frame(min=c(0,15,25,45,150))
for (i in 2:nrow(Size_class)){
  Tree_subset<-subset(Trees_M,DBH>Size_class$min[i-1])
  Tree_subset<-subset(Tree_subset,DBH<=Size_class$min[i])
  Tree_subset$Size_Class<-Size_class$min[i]
  Trees_M_Size<-rbind(Tree_subset,Trees_M_Size)
}

#create count of stems in certain size classes
Stem_density_Size<-count(Trees_M_Size,var=c("Block","Year","Size_Class"))

Stem_density_Size2<-merge(Stem_density_Size,BA,by=c("Block","Year"))

head(Stem_density_Size2)

#plot this relationship
ggplot(Stem_density_Size2,aes(x=BAPERCM,y=freq,group=Block))+geom_point()+facet_wrap(~Size_Class)+geom_smooth(se=F,colour="blue",size=3,method="lm",aes(group=NULL))

#now work out the basal area of trees in each size class
for (i in 1:nrow(Trees_M_Size)){
  Trees_M_Size$BA[i]<-ifelse(Trees_M_Size$DBH[i]>10,(Trees_M_Size$DBH[i]^2*(pi/4))/400,0)
}

Trees_BA_Size<-ddply(Trees_M_Size,.(Block,Year,Size_Class),summarise,T_BA=sum(BA))
Trees_BA_Size2<-merge(Trees_BA_Size,BA,by=c("Block","Year"))

#now loop through to calculate proportional change in basal area for each size class
Trees_unique<-unique(Trees_BA_Size2[,c("Block","Size_Class")])
Trees_BA_Size3<-NULL
for (i in 1:nrow(Trees_unique)){
  Trees_sub<-subset(Trees_BA_Size2,Block==Trees_unique$Block[i])
  Trees_sub<-subset(Trees_sub,Size_Class==Trees_unique$Size_Class[i])
  Trees_sub<-Trees_sub[with(Trees_sub, order(Year)), ]
  Trees_sub$BA_Change<-Trees_sub$T_BA/Trees_sub$T_BA[1]
  Trees_BA_Size3<-rbind(Trees_BA_Size3,Trees_sub)
}


#now plot the relationship between basal area for each size class and time
ggplot(Trees_BA_Size3,aes(x=Year,y=T_BA,group=Block))+geom_point()+facet_wrap(~Size_Class,scales = "free_y")+geom_line()+geom_smooth(se=F,colour="blue",size=3,method="lm",aes(group=NULL))
ggplot(Trees_BA_Size3,aes(x=Year,y=BA_Change,group=Block))+geom_point()+facet_wrap(~Size_Class,scales = "free_y")+geom_line()+geom_smooth(se=F,colour="blue",size=3,method="lm",aes(group=NULL))


#and now over the gradient
ggplot(Trees_BA_Size3,aes(x=BAPERCM,y=T_BA,group=Block))+geom_point()+facet_wrap(~Size_Class,scales = "free_y")+geom_smooth(se=F,colour="blue",size=3,method="lm",aes(group=NULL))
ggplot(Trees_BA_Size3,aes(x=BAPERCM,y=BA_Change,group=Block))+geom_point()+facet_wrap(~Size_Class,scales = "free_y")+geom_smooth(se=F,colour="blue",size=3,method="lm",aes(group=NULL))

