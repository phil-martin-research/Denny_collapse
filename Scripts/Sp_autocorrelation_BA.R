#script to produce look at spatial autocorrelation for degree of collapse

library(ggplot2)


#########################################################
#exploratory analysis to look at spatial autocorrelation#
#########################################################
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("BA_Gradient.csv")


BA_2014<-subset(BA,Year==2014)


plot(BA_1996[,6:5])

dists<-dist(BA_1996[,6:5])
breaks<-seq(0,1100,10)
v1 <- variog(coords = BA_1996[,6:5], data = BA_1996[,4], breaks = breaks)
v1.summary <- cbind(c(1:length(v1$v)), v1$v, v1$n)
colnames(v1.summary) <- c("lag", "semi", "No_pairs")
v1.summary<-data.frame(v1.summary)

ggplot(v1.summary,aes(x=lag*10,y=semi,size=No_pairs))+geom_point()+geom_line(aes(size=NULL))+geom_smooth(method="lm",se=F,size=2)


#now do comparison for each years worth of data
Years<-unique(SD_plots_merge$Year)
for (i in 1:length(Years)){
  Plots_year<-subset(SD_plots_merge,Year==Years[i])
  dists<-dist(Plots_year[,5:6])
  breaks<-seq(0,0.01,0.0001)
  v1 <- variog(coords = Plots_year[,5:6], data = Plots_year[,3], breaks = breaks)
  v1.summary <- cbind(c(1:length(v1$v)), v1$v, v1$n,Years[i])
  colnames(v1.summary) <- c("lag", "semi", "No_pairs","Year")
  setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data/Processing/SD")
  write.csv(v1.summary,paste("Plot_SD_AC",Years[i],".csv"))
}

#rbind all years data together
files  <- list.files(pattern = '\\.csv')
tables <- lapply(files, read.csv, header = TRUE)
combined.df <- do.call(rbind , tables)

#plot semi-variogram for all years
ggplot(data=combined.df,aes(x=lag,y=semi,size=No_pairs))+geom_line(size=0.5)+geom_point(alpha=0.5)+facet_wrap(~Year)


#next look at plot level - BA
BA_plots_merge<-merge(BA_change_CC,Transect_coord,by="Block")
head(BA_plots_merge)


#now do comparison for each years worth of data
Years<-unique(BA_plots_merge$Year)
for (i in 1:length(Years)){
  Plots_year<-subset(BA_plots_merge,Year==Years[i])
  dists<-dist(Plots_year[,5:6])
  breaks<-seq(0,0.01,0.0001)
  v1 <- variog(coords = Plots_year[,5:6], data = Plots_year[,3], breaks = breaks)
  v1.summary <- cbind(c(1:length(v1$v)), v1$v, v1$n,Years[i])
  colnames(v1.summary) <- c("lag", "semi", "No_pairs","Year")
  setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data/Processing/BA")
  write.csv(v1.summary,paste("Plot_BA_AC",Years[i],".csv"))
}

#rbind all years data together
files  <- list.files(pattern = '\\.csv')
tables <- lapply(files, read.csv, header = TRUE)
combined.df <- do.call(rbind , tables)

#plot semi-variogram for all years
ggplot(data=combined.df,aes(x=lag,y=semi,size=No_pairs))+geom_line(size=0.5)+geom_point(alpha=0.5)+facet_wrap(~Year)
