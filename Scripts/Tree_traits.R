#script to look at changes in species traits

rm(list=ls(all=TRUE))

#open packages neeeded for analysis
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(nlme)
library(MuMIn)
library(gridExtra)
library(MASS)
library(survival)
library(GGally)
library(lme4)
library(fields)
library(ROCR)

#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Location<-read.csv("Plot_coords.csv")
Location<-unique(Location[,c(3,5:6)])
DBH<-read.csv("Denny_trees_cleaned.csv")
head(DBH)
unique(DBH$Year)
head(Location)
#subset trees to give only those inside plots
DBH<-subset(DBH,In_out=="In")
#get rid of non-live stems
DBH<-subset(DBH,Status==1)

#import trait table
Traits<-read.csv("Tree_traits.csv")

#merge trait and tree data
head(DBH)
head(Traits)
Tree_traits<-merge(DBH,Traits,by.x="Species",by.y="Code")


#####################################
#analyses of trait change############
#####################################

#calculate basal area
head(Tree_traits)
#calculate basal area per plot per time period
#for all species
Tree_traits$BA<-ifelse(DBH$DBH>10,Tree_traits$DBH^2*(pi/4),0)

#calculate the traits weighted by BA
Tree_traits$L_w<-Tree_traits$Light*Tree_traits$BA
Tree_traits$N_w<-Tree_traits$Nitrogen*Tree_traits$BA
Tree_traits$H_w<-Tree_traits$Height*Tree_traits$BA
Tree_traits<-Tree_traits[,-c(1,5:17)]
Tree_trait_melt<-melt(Tree_traits, id = c("Block", "Year","Tree_ID"))
head(Tree_trait_melt)
TT_block<-dcast(Tree_trait_melt, Year + Block ~ variable, mean)

#calculate total BA
Tree_traits<-merge(DBH,Traits,by.x="Species",by.y="Code")
head(Tree_traits)
Tree_traits$BA<-ifelse(DBH$DBH>10,Tree_traits$DBH^2*(pi/4),0)
Tree_BA<-Tree_traits[,-c(1,5:16)]
BA_melt<-melt(Tree_BA, id = c("Block", "Year","Tree_ID"))
BA_block<-dcast(BA_melt, Year + Block ~ variable, function(x) sum(na.omit(x)/400))

#add dbh to traits
TT_block$BA_sum<-BA_block$BA


plot(BA_block$BA,TT_block$H_w)


ggplot(TT_block,aes(x=Year,y=BA_sum,group=Block))+geom_point(shape=1,alpha=0.2)+geom_line(alpha=0.2)+geom_smooth(data=TT_block,aes(group=NULL),size=3,colour="blue",method="lm")


