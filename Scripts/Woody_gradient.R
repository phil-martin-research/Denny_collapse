#plot of woody species richness over gradient

setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Plots<-read.csv("Denny_plots.csv")

library(ggplot2)

head(Plots)

ggplot(Plots,aes(x=BAPERCM,y=SPRRAWM))+geom_point()

ggplot(Plots,aes(x=BAPERCM,y=SDPERCM))+geom_point()
