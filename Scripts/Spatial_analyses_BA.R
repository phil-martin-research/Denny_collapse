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
library(ncf)
library(grid)

#save all this as a csv
Plots<-read.csv("Data/BA_gradient_spatial.csv")

head(Plots)

Plots$Transect<-ifelse(Plots$Block>=51,"Unenclosed","Enclosed")
Plots$Collapsed_adj3<-ifelse(Plots$Collapsed_adj==1,"Collapsed","Stable")
Plots$Collapsed_adj4<-ifelse(Plots$Collapsed_adj2==1,"Collapsed","Stable")
Plots$Year2<-NA
for (i in 1:nrow(Plots)){
  Plots$Year[i]<-ifelse(Plots$Year[i]==1996,1999,Plots$Year[i])
  Plots$Year2[i]<-ifelse(Plots$Year[i]==1999,"1996/9",Plots$Year[i])
}



#produce chi square table for Adrian
Plots2<-subset(Plots,Transect=="Enclosed")
Plots2<-subset(Plots2,Year>1984)
Plots2<-subset(Plots2, !is.na(Collapsed_adj))


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

#produce Moran's I spline for each period
#loop to create correlograms for each year


Y_Plots<-unique(Plots[c("Year2","Transect")])
Plot_corr<-NULL
for (i in 1:nrow(Y_Plots)){
  Plot_sub<-subset(Plots,Year2==Y_Plots[i,1]&Transect==Y_Plots[i,2])
  b.cor<-spline.correlog(Plot_sub$Easting, Plot_sub$Northing, Plot_sub$BAPERCM)
  b.cor2<-data.frame(Dist=b.cor$boot$boot.summary$predicted$x[1,],Cor=b.cor$boot$boot.summary$predicted$y[6,],UCI=b.cor$boot$boot.summary$predicted$y[2,],LCI=b.cor$boot$boot.summary$predicted$y[10,],Year=Y_Plots[i,1],Transect=Y_Plots[i,2])
  Plot_corr<-rbind(b.cor2,Plot_corr)
}

#
Plot_corr$Year2<-factor(Plot_corr$Year, c("1984","1988","1996/9","2014"))
Plot_corr$Transect<-factor(Plot_corr$Transect, c("Enclosed","Unenclosed"))


head(Plot_corr)
theme_set(theme_bw(base_size=12))
Moran_plot1<-ggplot(Plot_corr,aes(x=Dist,y=Cor,ymax=LCI,ymin=UCI))+geom_ribbon(alpha=0.2)+geom_line(size=1,colour="black")+facet_grid(Transect~Year2)
Moran_plot2<-Moran_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")
Moran_plot2+geom_hline(y=0,lty=2)+xlab("Distance between subplots (m)")+ylab("Moran's I correlation")+scale_size_continuous(range = c(1,3))+ theme(panel.margin = unit(1, "lines"))
ggsave("Figures/BA_correl.png",height=6,width=8,units="in",dpi=600)


