#script to produce exploratory plots for Denny Wood data

rm(list=ls(all=TRUE))

#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Denny<-read.csv("Denny_cleaned.csv")
head(Denny)




ggplot(data=Denny,aes(DBH_mean))+geom_histogram()+facet_grid(En.Un~Year)

summary(Plots3)

ggplot(data=Plots3,aes(x=Long,y=Lat,size=DBH_mean,alpha=0.5,colour=as.factor(Status2)))+geom_point(shape=16)+facet_wrap(~Year)


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
