#script to look at changes in climate near to new forest
library(ggplot2)
library(plyr)
library(reshape2)

Climate<-read.csv("data/Climate_data.csv")
se <- function(x) sqrt(var(x)/length(x))
head(Climate)

Climate<-ddply(Climate,.(Year,Month),mutate,m_temp=(tmax+tmin)/2)
Climate2<-ddply(Climate,.(Month),mutate,mean_temp=mean(m_temp),sd_temp=sd(m_temp),
                mean_rain=mean(rain),sd_rain=sd(rain))



ggplot(Climate2,aes(x=Year,y=m_temp))+geom_point()+facet_wrap(~Month,scales = "free_y")+geom_smooth(method="lm")
ggplot(Climate2,aes(x=Year,y=rain))+geom_point()+facet_wrap(~Month,scales = "free_y")+geom_smooth(method="lm")

ggplot(Climate2,aes(x=Year,y=rain-mean_rain))+geom_bar(stat="identity")+facet_wrap(~Month,scales = "free_y")

Climate3<-subset(Climate2,Month>=4&Month<10)

Climate4<-ddply(Climate3,.(Year),summarise,mean_temp=mean(m_temp),sd_temp=sd(m_temp),
                mean_rain=sum(rain))

Climate4$extreme_temp<-ifelse(Climate4$mean_temp>(mean(Climate4$mean_temp)+sd(Climate4$mean_temp)),"Extreme","Not extreme")

sd(Climate4$mean_rain-mean(Climate4$mean_rain))

ggplot(Climate4,aes(x=Year,y=mean_temp))+geom_point()+geom_smooth(method="lm")

ggplot(Climate4,aes(x=Year,y=mean_rain-mean(Climate4$mean_rain),fill=mean_temp-mean(mean_temp)))+geom_bar(stat="identity")+geom_hline(y=86,lty=2)+geom_hline(y=-86,lty=2)+scale_fill_gradient2(low="blue",mid="grey",high="red")


Climate4$Year2<-cut(Climate4$Year, c(0,1960,1970,1980,1990,2000,2014),include.lowest=F,labels =c("1960s","1970s","1980s","1990s","2000s","2010s"))

#generate minimum convex hull
find_hull <- function(Climate4) Climate4[chull(Climate4$mean_rain, Climate4$mean_temp), ]
hulls <- ddply(Climate4, "Year2", find_hull)


ggplot(Climate4,aes(x=mean_rain,y=mean_temp,colour=Year2,fill=NULL))+geom_point()+
  geom_polygon(data=hulls,alpha=0.5,fill=NA)

Climate5<-ddply(Climate4,.(Year2),summarise,temp_sum=mean(mean_temp),se_temp=se(mean_temp),rain_sum=mean(mean_rain),se_rain=se(mean_rain))
ggplot(Climate5,aes(x=rain_sum,y=temp_sum,colour=Year2,fill=NULL))+geom_point(size=3)+
  geom_errorbar(aes(ymin = temp_sum+(2*se_temp),ymax = temp_sum-(2*se_temp)))+geom_errorbarh(aes(xmin = rain_sum+(2*se_rain),xmax = rain_sum-(2*se_rain)))
