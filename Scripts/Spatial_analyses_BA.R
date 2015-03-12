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
library(arm)
library(lmerTest)

#save all this as a csv
Plots<-read.csv("Data/BA_gradient_spatial.csv")

head(Plots)

Plots$Transect<-ifelse(Plots$Block>=51,"Undenclosed","Enclosed")
Plots$Collapsed_adj3<-ifelse(Plots$Collapsed_adj==1,"Collapsed","Stable")
Plots$Collapsed_adj4<-ifelse(Plots$Collapsed_adj2==1,"Collapsed","Stable")
Plots$Year2<-NA
for (i in 1:nrow(Plots)){
  Plots$Year[i]<-ifelse(Plots$Year[i]==1996,1999,Plots$Year[i])
  Plots$Year2[i]<-ifelse(Plots$Year[i]==1999,"1996/9",Plots$Year[i])
}


#produce table for Adrian

Plots_1<-ddply(Plots2,.(Collapsed,Collapsed_adj3,Year),summarise,Number_of_plots=length(Collapsed))
Plots_2<-ddply(Plots,.(Collapsed,Collapsed_adj4,Year),summarise,Number_of_plots=length(Collapsed))

Plots_1<-Plots_1[with(Plots_1, order(Year)), ]
Plots_2<-Plots_2[with(Plots_2, order(Year)), ]
colnames(Plots_1)<-c("Plot_status","Adjacent_plot_status","Year","Number_of_plots")
colnames(Plots_2)<-c("Plot_status","Adjacent_plot_status","Year","Number_of_plots")

write.csv(Plots_1,file="Figures/Plots_collapse1.csv",row.names = F)
write.csv(Plots_2,file="Figures/Plots_collapse2.csv",row.names = F)

Plots1984<-subset(Plots,Year==1984)
Plots1988<-subset(Plots,Year==1988)
Plots1999<-subset(Plots,Year==1999)
Plots2014<-subset(Plots,Year==2014)

chisq.test(table(Plots1984$Collapsed,Plots1984$Collapsed_adj2))
chisq.test(table(Plots1988$Collapsed,Plots1988$Collapsed_adj2))
chisq.test(table(Plots1999$Collapsed,Plots1999$Collapsed_adj2))
chisq.test(table(Plots2014$Collapsed,Plots2014$Collapsed_adj2))

