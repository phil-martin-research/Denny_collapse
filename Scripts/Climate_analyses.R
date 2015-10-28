#script to look at changes in climate near to new forest
library(ggplot2)
library(plyr)
library(reshape2)

#import climate data from Hurn weather station
Climate<-read.csv("data/Climate_data.csv")
#produce mean temperature variable by getting mean of max and min temperatures
Climate<-ddply(Climate,.(Year,Month),mutate,m_temp=(tmax+tmin)/2)
#subset to keep only April - September
Climate2<-subset(Climate,Month>=4&Month<10)
Climate_winter<-subset(Climate,Month<4|Month>10)
#produce mean temps and total rainfall for each year
Climate3<-ddply(Climate2,.(Year),summarise,mean_temp=mean(m_temp),sum_rain=sum(rain))
Climate_winter_2<-ddply(Climate_winter,.(Year),summarise,mean_temp=mean(m_temp),sum_rain=sum(rain))
#produce standard deviations for temperature and rainfall
Temp_SD<-sd(Climate3$mean_temp-mean(Climate3$mean_temp))
Rain_SD<-sd(Climate3$sum_rain-mean(Climate3$sum_rain))
#code temperature as extreme if it it >1SD away from mean
Climate3$Ex_temp<-ifelse(Climate3$mean_temp>mean(Climate3$mean_temp)+Temp_SD,TRUE,FALSE)



#produce models of changes in climate
#first rain
Rain_M0<-lm(sum_rain~1,data=Climate3)
Rain_M1<-lm(sum_rain~Year,data=Climate3)
summary(Rain_M1)

par(mfrow=(c(2,2)))
plot(Temp_M1)
#now temperature
Temp_M0<-lm(mean_temp~1,data=Climate3)
Temp_M1<-lm(mean_temp~Year,data=Climate3)
summary(Temp_M1)
par(mfrow=c(2,2))
plot(Temp_M1)
Climate3$temp_pred<-predict(Temp_M1)
Climate3$SE<-predict(Temp_M1,se.fit = T)$se.fit

#plot these results of temperature model
theme_set(theme_bw(base_size=12))
Temp_plot1<-ggplot(Climate3,aes(x=Year,y=mean_temp))+
  geom_point(size=3,shape=1)+
  geom_line(lty=2,size=0.5,colour="grey",alpha=0.5)+
  geom_line(data=Climate3,aes(y=temp_pred),size=1)+
  geom_ribbon(data=Climate3,aes(y=temp_pred,ymin=temp_pred-(1.96*SE),ymax=temp_pred+(1.96*SE)),fill="grey",alpha=0.5)
Temp_plot2<-Temp_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour="black",fill=NA))     
Temp_plot3<-Temp_plot2+ylab(expression(paste("Mean April-September temperature(",degree,"C)")))
Temp_plot4<-Temp_plot3+annotate("text", label = "(a)", x = 1955, y = 15.5, size=4)

