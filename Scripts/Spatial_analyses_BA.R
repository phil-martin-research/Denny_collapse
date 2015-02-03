#script to look at whether collapse is driven by neighbouring plots
rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(vegan)
library(reshape2)
library(lme4)
library(nlme)
library(MuMIn)
library(quantreg)
library(car)

#save all this as a csv
Plots<-read.csv("Data/BA_gradient_spatial.csv")

head(Plots)

Plots2<-subset(Plots,Block<51)

ggplot(Plots,aes(x=as.factor(Collapsed_adj),y=Collapsed))+geom_boxplot()+facet_wrap(~Year)


M1<-lmer(BAPERCM~BA_prev_adj+(1|Block),data=Plots)

