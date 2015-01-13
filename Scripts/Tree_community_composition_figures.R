#community composition for different measures - trees

library(ggplot)

#save all this as a csv
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest
Plots<-read.csv("Denny_plots.csv")

head(Plots)

Plots$Tanner<-(Plots$Sorensen_BA+Plots$Sorensen.SD)/2

keeps<-c("BAPERCM","Tanner","Sorensen_BA","Sorensen.SD","Block","Year")

Plots2<-Plots[keeps]
Plot_melt<-melt(Plots2,id.vars =c("Block","Year","BAPERCM"))
head(Plot_melt)
ggplot(Plot_melt,aes(x=BAPERCM,y=value,colour=variable))+geom_point(shape=1)+facet_wrap(~variable)+ theme(legend.position="none")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Tree_similarity.png",width = 12,height=6,units = "in",dpi=300)
