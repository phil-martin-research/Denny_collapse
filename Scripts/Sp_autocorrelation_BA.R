#script to produce look at spatial autocorrelation for degree of collapse

library(ggplot2)
library(ape)
library(geoR)
library(ncf)

#########################################################
#exploratory analysis to look at spatial autocorrelation#
#########################################################
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
BA<-read.csv("BA_Gradient.csv")

#test for autocorrelation in the data
YU<-unique(BA$Year)
Morans_stat<-NULL
for (i in 2:length(YU)){
  BA_sub<-subset(BA,Year==YU[i])
  Dist_mat<-as.matrix(dist(cbind(BA_sub$Northing, BA_sub$Easting)))
  Dist_mat_Inv<-1/Dist_mat
  Dist_mat_Inv[is.infinite(Dist_mat_Inv)] <- 0
  diag(Dist_mat_Inv) <- 0
  Moran_sub<-Moran.I(BA_sub[,4], Dist_mat_Inv)
  Moran_sub_df<-data.frame(Moran_sub)
  Moran_sub_df$Year<-YU[i]
  Morans_stat<-rbind(Moran_sub_df,Morans_stat)
}

data.frame(Moran.I(BA_2014[,4], Dist_mat_Inv))

Dist_mat<-as.matrix(dist(cbind(BA_2014$Northing, BA_2014$Easting)))
Dist_mat_Inv<-1/Dist_mat
Dist_mat_Inv[is.infinite(Dist_mat_Inv)] <- 0
diag(Dist_mat_Inv) <- 0
Moran.I(BA_2014[,4], Dist_mat_Inv)

plot(BA_2014[,6:5])

dists<-dist(BA_2014[,6:5])
breaks<-seq(0,1100,10)
v1 <- variog(coords = BA_2014[,6:5], data = BA_2014[,4], breaks = breaks)
v1.summary <- cbind(c(1:length(v1$v)), v1$v, v1$n)
colnames(v1.summary) <- c("lag", "semi", "No_pairs")
v1.summary<-data.frame(v1.summary)

ggplot(v1.summary,aes(x=lag*10,y=semi,size=No_pairs))+geom_point()+geom_line(aes(size=NULL))+geom_smooth(method="lm",se=F,size=2)

#look at variation in morans I over distance
ncf.cor <- correlog(BA_2014$Easting, BA_2014$Northing, BA_2014$BAPERCM,
                    increment=25, resamp=1000)

Spatial_correlation<-data.frame(Dist=ncf.cor$mean.of.class,MoranI=ncf.cor$correlation,Pval=ncf.cor$p,Number=ncf.cor$n)
Spatial_correlation$Sig<-ifelse(Spatial_correlation$Pval<0.05,"S","NS")

#plot how morans I changes over distance
theme_set(theme_bw(base_size=12))
Morans_I_plot1<-ggplot(Spatial_correlation,aes(x=Dist,y=MoranI,size=Number,colour=Sig))+geom_point()+geom_line(aes(size=NULL,group=NULL,colour=NULL))+geom_hline(y=0,lty=2)
Morans_I_plot2<-Morans_I_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Morans_I_plot2+ylab("Moran's I")+xlab("Distance between plots (m)")+theme(legend.position="none")


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
