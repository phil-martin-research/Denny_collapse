# script to calculate day length for use in climatic water deficit calculations
library(ggplot)
library(plyr)

Days<-expand.grid(Lat=-50.85,Day=seq(1,365))

Days$theta<-0.2163108+(2*(atan(0.9671396*(tan(0.00860*(Days$Day))))))
Days$phi<-asin(0.39795*(cos(Days$theta)))
Days$Daylight<-(((sin((0.833*pi)/180))+(sin((Days$Lat*pi)/180))*(sin(Days$phi)))/((cos((Days$Lat*pi)/180))*(cos(Days$phi))))
Days$D<-24-((24/pi*(acos(Days$Daylight))))

Days$Month<-cut(Days$Day,c(0,31,59,90,120,151,181,212,243,273,304,334,365),
    labels = c("Jan","Feb","March","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec"))

ddply(Days,.(Month),summarise,Length=mean(D))

ggplot(data=Days,aes(x=Day,y=D,group=as.factor(Lat),colour=as.factor(Lat)))+geom_line(size=2)

