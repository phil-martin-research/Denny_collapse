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
#produce mean temps and total rainfall for each year
Climate3<-ddply(Climate2,.(Year),summarise,mean_temp=mean(m_temp),sum_rain=sum(rain))
#produce standard deviations for temperature and rainfall
Temp_SD<-sd(Climate3$mean_temp-mean(Climate3$mean_temp))
Rain_SD<-sd(Climate3$sum_rain-mean(Climate3$sum_rain))
#code temperature as extreme if it it >1SD away from mean
Climate3$Ex_temp<-ifelse(Climate3$mean_temp>mean(Climate3$mean_temp)+Temp_SD,TRUE,FALSE)

#plot climate anomolies
theme_set(theme_bw(base_size=12))
Anom_plot1<-ggplot(Climate3,aes(x=Year,y=sum_rain-mean(Climate3$sum_rain),fill=mean_temp-mean(Climate3$mean_temp)))+
  geom_bar(stat="identity")+scale_fill_gradient2(expression(paste("Deviation from mean \nApril-September temperature(",degree,"C)")),low="blue",mid="grey",high="red")
Anom_plot2<-Anom_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour="black",fill=NA))     
Anom_plot2+ylab("Deviation from mean April-September rainfall (mm)")+geom_hline(y=0,lty=1)
ggsave("Figures/Climate_anomolies.png",width = 8,height=6,units = "in",dpi=800)


#produce models of changes in climate
#first rain
Rain_M0<-lm(sum_rain~1,data=Climate3)
Rain_M1<-lm(sum_rain~Year,data=Climate3)

#now temperature
Temp_M0<-lm(mean_temp~1,data=Climate3)
Temp_M1<-lm(mean_temp~Year,data=Climate3)
par(mfrow=c(2,2))
plot(Temp_M1)


Climate3$temp_pred<-predict(Temp_M1)

#plot these results
theme_set(theme_bw(base_size=12))
Temp_plot1<-ggplot(Climate3,aes(x=Year,y=mean_temp))+geom_point(size=3,shape=1)+geom_line(lty=2,size=0.5,colour="grey",alpha=0.5)+geom_line(data=Climate3,aes(y=temp_pred),size=2)
Temp_plot2<-Temp_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour="black",fill=NA))     
Temp_plot2+ylab(expression(paste("Mean annual April-September temperature(",degree,"C)")))
ggsave("Figures/Temperature_change.png",width = 8,height=6,units = "in",dpi=800)


#plot these results
theme_set(theme_bw(base_size=12))
Temp_plot1<-ggplot(Climate3,aes(x=Year,y=mean_temp))+geom_point(size=3,shape=1)+geom_line(lty=2,size=0.5,colour="grey",alpha=0.5)+geom_line(data=Climate3,aes(y=temp_pred),size=2)
Temp_plot2<-Temp_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour="black",fill=NA))     
Temp_plot2+ylab(expression(paste("Mean annual April-September temperature(",degree,"C)")))
ggsave("Figures/Temperature_change.png",width = 8,height=6,units = "in",dpi=800)



#plot these results for ICCB poster
theme_set(theme_grey(base_size=30))
Temp_plot1<-ggplot(Climate3,aes(x=Year,y=mean_temp))+geom_point(size=5,alpha=0.5)+geom_line(lty=2,size=0.5,colour="black",alpha=0.5)+geom_line(data=Climate3,aes(y=temp_pred),size=2)
Temp_plot2<-Temp_plot1    
Temp_plot2+ylab(expression(paste("Mean growing season temperature(",degree,"C)")))
ggsave("Figures/Temperature_change_ICCB.jpeg",width = 26,height=25,units = "cm",dpi=800)

