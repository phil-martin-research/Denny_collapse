#script for plotting trajectories of basal area over time

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)

#load in data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("Denny_plots.csv")

#basal area over time for beech
theme_set(theme_bw(base_size=12))
BA_F1<-ggplot(BA,aes(x=Year,y=BAFM))+geom_point()+geom_line()+facet_wrap(~Block)
BA_F2<-BA_F1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
BA_F2+ylab("Basal area of beech (metres squared per hectare)")+xlab("Year of survey")+ theme(axis.text.x = element_text(angle = 90))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("BA_Beech.png",width = 8,height=6,units = "in",dpi=300)

#basal area over time for oak
theme_set(theme_bw(base_size=12))
BA_Q1<-ggplot(BA,aes(x=Year,y=BAQM))+geom_point()+geom_line()+facet_wrap(~Block)
BA_Q2<-BA_Q1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
BA_Q2+ylab("Basal area of oak (metres squared per hectare)")+xlab("Year of survey")+ theme(axis.text.x = element_text(angle = 90))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("BA_Oak.png",width = 8,height=6,units = "in",dpi=300)

#now produce a table based on this
keeps <- c("Block","Year","BAM","BAFM","BAQM")
BA_table<-BA[keeps]
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
write.csv(BA_table,file="BA_table.csv",row.names=F)
