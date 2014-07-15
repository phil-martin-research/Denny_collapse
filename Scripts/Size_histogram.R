#script to look at size shifts of trees in Denny wood

library(ggplot2)
library(plyr)
library(reshape2)
library(geoR)
library(ape)

#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse")
Coord<-read.csv("Transect_coords.csv")
Plots<-read.csv("Denny_plots_edit2.csv")
str(Plots)
str(Coord)


#create column to give size classes of 10 cm intervals
Class<-c(seq(0,100,10),seq(120,140,20))
Plots$Class<-NA
for (i in 1:12) {
  Plots$Class<-ifelse(Plots$DBH>=Class[i]&Plots$DBH<=Class[i+1],Class[i+1],Plots$Class)
}

#change status column only be live or dead
Status<-data.frame(seq(1:4),c(1,2,1,1))
Plots$Status2<-NA
for (i in 1:length(Status)){
  Plots$Status2<-ifelse(Plots$Status==Status[1,i],Status[1,i],Plots$Status2)
}

#merge data from 1950s where data for years in not available
Plots_pre60<-subset(Plots,Year<=1959)
head(Plots_pre60)
Pre_mean<-data.frame(tapply(Plots_pre60$DBH,Plots_pre60$Tree_ID,mean, na.rm=TRUE))
colnames(Pre_mean)<-c("DBH")
Pre_mean$ID<-row.names(Pre_mean)

for (i in 1:length(Plots)){
  Plots_pre60$DBH_mean<-ifelse(Plots_pre60$Tree_ID==Pre_mean$ID[i],Pre_mean$DBH[i],Plots$DBH)
}
summary(Plots_pre60)
Plots_pre60_2<-Plots_pre60[complete.cases(Plots_pre60[,27]),]
Dup_rm<-subset(Plots_pre60_2, !duplicated(Plots_pre60_2[,27])) 
head(Dup_rm)

Dup_rm2<-data.frame(Tree_ID=Dup_rm$Tree_ID,DBH=Dup_rm$DBH_mean,Year=Dup_rm$Year)
head(Dup_rm2)


#merge datasets together
Plots2<-(merge(x=Plots,y=Dup_rm2,by=c("Tree_ID","Year")))

for (i in 1:length(Plots2)){
  Plots2$DBH_mean<-ifelse(Plots2$Year[i]<=1959,Plots2$DBH.y[i],Plots2$DBH.x[i])
}

head(Plots2)

Plots<-subset(Plots2,Year>=1959)

#remove trees with no dbh measurements
Plots2<-Plots[complete.cases(Plots[,8]),]
head(Plots2)
#set location as numeric
Plots2$Dist_west<-as.numeric(as.character(Plots2$Dist_west))
Plots2$Dist_south<-as.numeric(as.character(Plots2$Dist_south))
str(Plots2)

#subset to remove data pre 1959 because it is incomplete
Plots3<-subset(Plots2,Year>=1959)
Plots3<-subset(Plots3,Dist_west>=0)
Plots3<-subset(Plots3,Dist_west<=20)
head(Plots3)

ggplot(data=Plots2,aes(DBH))+geom_histogram()+facet_grid(En.Un~Year)



ggplot(data=Plots3,aes(x=Dist_west,y=Dist_south,size=DBH,alpha=0.5))+geom_point(shape=1)+facet_grid(En.Un~Year)

#produce plots of change in size structure
DBH_count<-count(Plots3,c("Year","Class"))
Count_1959<-subset(DBH_count,Year==1959)
DBH_count$LnRR<-log(DBH_count$freq)-log(Count_1959$freq)
DBH_count$Change<-(DBH_count$freq)-(Count_1959$freq)
DBH_count2<-subset(DBH_count,Year!=1959)

ggplot(DBH_count2,aes(x=Class,y=Change))+geom_point()+facet_wrap(~Year)+geom_hline(y=0,lty=2)
ggplot(DBH_count2,aes(x=as.factor(Class),y=exp(LnRR)-1))+geom_bar(stat="identity")+facet_wrap(~Year)+geom_hline(y=0,lty=2)+xlab("Upper limit for DBH size class")+ylab("Change in stem density relative to 1958/9")

ggplot(DBH_count,aes(x=Year-1959,y=exp(LnRR)-1,colour=as.factor(Class)))+geom_smooth(se=F,method="lm")+geom_hline(y=0,lty=2)


#look at variation in DBH in space
Plots_1996<-subset(Plots2,Year==1996)
head(Plots_1996)
dists<-dist(Plots_1996[,3:4])

summary(dists)
breaks<-seq(0,1000,20)
v1 <- variog(coords = Plots_1996[,3:4], data = Plots_1996[,8], breaks = breaks)
v1.summary <- cbind(c(1:51), v1$v, v1$n)
colnames(v1.summary) <- c("lag", "semi", "No_pairs")

plot(v1, type = "b", main = "Variogram: Av8top") 

VG<-data.frame(v1.summary)
VG$distance<-seq(0,1000,20)
head(VG)

ggplot(data=VG,aes(x=distance,y=semi,size=No_pairs))+geom_line(size=0.5)+geom_point(alpha=0.5)

#now produce morans I statistic
dists2<-as.matrix(dist(cbind(Plots_1996$Dist_south, Plots_1996$Dist_west)))
inv.dists<-1/dists2
diag(inv.dists) <- 0
inv.dists[1:5, 1:5]
Moran.I(Plots_1996$DBH, inv.dists)

#produce basal area per plot
Plots2$BA<-(((Plots2$DBH^2)*(pi/4))/10000)
Plots2$BA2<-(Plots2$DBH^2)*0.00007854

tapply(Plots2$BA,Plots2$Block,sum)*4

head(Plots2)

BA_change<-data.frame(with(Plots2, tapply(BA2,list(Block,Year), sum)))
BA_change$Block<-as.factor(rownames(BA_change))

BA_change2<-melt(BA_change,id.vars = "Block")
head(BA_change2)
subset(BA_change2)
?gsub
BA_change2$Year<-as.numeric(gsub("X","",BA_change2$variable))
BA_change3<-subset(BA_change2,grepl("out",BA_change2$Block)==F&Year>1958)

a<-ggplot(BA_change3,aes(x=Year,y=(value)*25,group=Block))+geom_line()
a+geom_smooth(aes(group=NA),method="lm",se=F,size=4)+ylab("Basal area")
