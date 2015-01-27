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



BA$Year<-ifelse(BA$Year==1996,1999,BA$Year)
BA$Transect<-ifelse(BA$Block>51,"Unenclosed","Enclosed")


#test for autocorrelation in the data
YU<-unique(BA$Year)

unique(BA[c("Year", "Transect")])
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


#look at variation in morans I over distance
Spatial_correlation<-NULL
YU<-unique(BA[c("Year", "Transect")])
YU<-subset(YU,Year>1964)
head(YU)
for (i in 1:nrow(YU)){
  BA_sub<-subset(BA,Year==YU[i,1])
  BA_sub<-subset(BA_sub,Transect==YU[i,2])
  ncf.cor <- correlog(BA_sub$Easting, BA_sub$Northing, BA_sub$BAPERCM,
                      increment=25, resamp=1000)
  Spatial_correlation_sub<-data.frame(Dist=ncf.cor$mean.of.class,MoranI=ncf.cor$correlation,Pval=ncf.cor$p,Number=ncf.cor$n)
  Spatial_correlation_sub$Sig<-ifelse(Spatial_correlation_sub$Pval<0.05,"S","NS")
  Spatial_correlation_sub$Year<-YU[i,1]
  Spatial_correlation_sub$Transect<-YU[i,2]
  Spatial_correlation<-rbind(Spatial_correlation,Spatial_correlation_sub)
}

#subset to give only comparisons with >15 pairs
Spatial_correlation_sub<-subset(Spatial_correlation,Number>15)

#plot how morans I changes over distance
theme_set(theme_bw(base_size=12))
Morans_I_plot1<-ggplot(Spatial_correlation,aes(x=Dist,y=MoranI,size=Number,colour=Sig))+geom_point()+geom_line(aes(size=NULL,group=NULL,colour=NULL))+geom_hline(y=0,lty=2)+facet_grid(Transect~Year)
Morans_I_plot2<-Morans_I_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Morans_I_plot2+ylab("Moran's I")+xlab("Distance between plots (m)")+theme(legend.position="none")+scale_size_continuous(range = c(1, 3))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("MoransI_distance.png",width = 8,height=6,units = "in",dpi=300)

