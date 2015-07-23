#script to produce histogram of percentage change in  basal area for Denny

library(ggplot2)
library(plyr)
library(reshape)
library(grid)

plots<-read.csv("Data/Denny_plots.csv",header = T)
keeps<-c("Year","BAPERCM","BAFPERCM","BAQPERCM")
plots<-plots[keeps]
plots<-subset(plots,Year>1964)
plot_melt<-melt(plots,id.vars = "Year")
plot_melt$value2<-plot_melt$value*100

plot_melt$variable2<-ifelse(plot_melt$variable=="BAPERCM","Total basal area",plot_melt$variable)
plot_melt$variable2<-ifelse(plot_melt$variable=="BAFPERCM","Beech basal area",plot_melt$variable2)
plot_melt$variable2<-ifelse(plot_melt$variable=="BAQPERCM","Oak basal area",plot_melt$variable2)
plot_melt$variable2<-factor(plot_melt$variable2,levels=c("Total basal area","Beech basal area","Oak basal area"))

theme_set(theme_bw(base_size=12))
Plot1<-ggplot(plot_melt,aes(x=value2))+geom_histogram(fill=NA,colour="grey",bin=20)+facet_grid(Year~variable2)+coord_cartesian(ylim = c(0,25))
Plot2<-Plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour="black",fill=NA))                                                                                                                                          
Plot2+geom_vline(x=0,lty=2)+ theme(panel.margin = unit(0.7, "lines"))+ylab("Number")+xlab("Percentage change in Basal Area since 1964")
ggsave("Figures/BA_histogram.jpeg",width = 8,height=6,units = "in",dpi=400)

Plots_2014<-subset(plot_melt,Year==2014)

theme_set(theme_bw(base_size=12))
Plot1<-ggplot(Plots_2014,aes(x=value2))+geom_histogram(fill="black",bin=20)+facet_wrap(~variable2)+coord_cartesian(ylim = c(0,25))
Plot2<-Plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour="black",fill=NA))                                                                                                                                          
Plot2+ theme(panel.margin = unit(0.7, "lines"))+ylab("Number")+xlab("Percentage change in Basal Area since 1964")
ggsave("Figures/BA_histogram2.jpeg",width = 8,height=6,units = "in",dpi=800)

