#script to produce figure of tree abundances over collapse gradient

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)

setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("Denny_plots.csv")

keeps<-c(names(BA)[1:12],"BAPERCM")
BA_2<-BA[keeps]

#melt dataframe by plot and year
BA_melt<-melt(BA_2,id.vars=c("Year","Block","BAPERCM"))

#now run a loop 
Block_sp<-unique(BA_melt[c("Block","variable")])
Rel_ab<-NULL
for (i in 1:nrow(Block_sp)){
  BA_subset<-subset(BA_melt,Block==Block_sp[i,1])
  BA_subset2<-subset(BA_subset,variable==Block_sp[i,2])
  BA_subset2$diff<-BA_subset2$value-BA_subset2$value[1]
  Rel_ab<-rbind(Rel_ab,BA_subset2)
}


ggplot(Rel_ab,aes(x=BAPERCM,y=diff,colour=as.factor(Year)))+geom_point()+facet_wrap(~variable,scales ="free_y")
