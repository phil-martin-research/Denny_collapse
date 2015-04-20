#script to look at changes in climate near to new forest
library(ggplot2)
library(plyr)
library(reshape2)

Climate<-read.csv("data/Climate_data.csv")
Climate$rain<-as.numeric(as.character(Climate$rain))
Climate$Season<-NA
for (i in 1:nrow(Climate)){
  if (Climate$Month[i]>=1&Climate$Month[i]<=3){
    Climate$Season[i]<-"Winter"
  }else if (Climate$Month[i]>=4&Climate$Month[i]<=6){
    Climate$Season[i]<-"Spring"
  }else if (Climate$Month[i]>=7&Climate$Month[i]<=9){
    Climate$Season[i]<-"Summer"
  }else if (Climate$Month[i]>=10&Climate$Month[i]<=12){
    Climate$Season[i]<-"Autumn"
  }
}

head(Climate)

#run a loop linear models to test the change over time
Climate<-subset(Climate,Year<=2014)
month_unique<-unique(Climate$Month)
Coef_comb<-NULL
Climate$Year2<-Climate$Year-min(Climate$Year)
for (i in 1:length(month_unique)){
  climate_sub<-subset(Climate,Month==month_unique[i])
  M1<-lm(rain~Year2,data=climate_sub)
  Coefs<-data.frame(round(coef(summary(M1)),3))
  colnames(Coefs)<-c("Estimate","SE","T_value","P_value")
  Coefs$Coefficient<-c("Intercept","Year")
  row.names(Coefs)<-NULL
  Coefs$Month<-month_unique[i]
  Coef_comb<-rbind(Coefs,Coef_comb)
}


Climate$temp<-(Climate$tmax+Climate$tmin)/2
Coef_comb_temp<-NULL
for (i in 1:length(month_unique)){
  climate_sub<-subset(Climate,Month==month_unique[i])
  M1<-lm(temp~Year2,data=climate_sub)
  Coefs<-data.frame(round(coef(summary(M1)),3))
  colnames(Coefs)<-c("Estimate","SE","T_value","P_value")
  Coefs$Coefficient<-c("Intercept","Year")
  row.names(Coefs)<-NULL
  Coefs$Month<-month_unique[i]
  Coef_comb_temp<-rbind(Coefs,Coef_comb_temp)
}

theme_set(theme_bw(base_size=12))
ggplot(Climate,aes(x=Year,y=rain))+geom_point(shape=1)+geom_line(alpha=0.2,lty=2)+geom_smooth(method=lm)+facet_wrap(~Month,scales="free_y")+xlim(c(1964,2014))
ggplot(Climate,aes(x=Year,y=(tmax+tmin)/2))+geom_point(shape=1)+geom_line(alpha=0.2,lty=2)+geom_smooth(method=lm)+facet_wrap(~Month,scales="free_y")+xlim(c(1964,2014))

head(Climate)

Climate_year<-ddply(Climate,.(Year),summarise,Precip=sum(rain,na.rm = T),Temp=mean(temp))

Clim_61_90<-subset(Climate_year,Year>=1961&Year<=1990)
Clim_91<-subset(Climate_year,Year>=1991)

mean(Climate_year$Precip)
sd(Climate_year$Precip)

mean(Climate_year$Temp)
sd(Climate_year$Temp)

#now look at seasonal trends
Season_climate<-ddply(Climate,.(Year,Season),summarise,Precip=sum(rain,na.rm = T),Temp_max=mean(tmax,na.rm=T),Temp_min=mean(tmin,na.rm=T))
Season_climate_mean<-ddply(Season_climate,.(Season),summarise,Precip=mean(Precip,na.rm = T))


ggplot(Season_climate,aes(x=Year,y=Precip))+geom_point(shape=1)+geom_line(alpha=0.2)+geom_smooth(method=lm)+facet_wrap(~Season,scales="free_y")
ggplot(Season_climate,aes(x=Year,y=(Temp_max+Temp_min/2)))+geom_point(shape=1)+geom_line(alpha=0.2)+geom_smooth(method=lm)+facet_wrap(~Season,scales="free_y")

Climate$Fm<-NA

#calculate water defecit following Lutz et al 2010
for (i in 1:nrow(Climate)){
 if (Climate$temp[i]<0){
   Climate$Fm[i]<-0
 } else if (Climate$temp[i]>0&Climate$temp[i]<6){
   Climate$Fm[i]<-0.167*Climate$temp[i]
 } else {
   Climate$Fm[i]<-1
 }
}

Climate$Rain_m<-Climate$Fm*Climate$rain
Climate$Snow_m<-(1-Climate$Fm)*Climate$rain


Climate$Pack<-((1-Climate$Fm)^2)*Climate$rain
Climate$Pack<-ifelse(is.na(Climate$Pack),0,Climate$Pack)
Climate$Pack2<-NA
for (i in 2:nrow(Climate)){
Climate$Pack2[i]<-Climate$Pack[i]+((1-Climate$Fm[i])*Climate$Pack[i-1])
}

Climate$Melt<-NA
for (i in 2:nrow(Climate)){
Climate$Melt[i]<-Climate$Fm[i]*(Climate$Snow_m[i]+Climate$Pack2[i-1])
}

Climate$Wm<-Climate$Melt+Climate$Rain

#add details on day length and number of days in month
#first produce a dataframe of how day lenth varies by month
Days<-data.frame(Lat=50.8,Day=seq(1,365))
Days$theta<-0.2163108+(2*(atan(0.9671396*(tan(0.00860*(Days$Day-186))))))
Days$phi<-asin(0.39795*(cos(Days$theta)))
Days$Daylight<-(((sin((0.833*pi)/180))+(sin((Days$Lat*pi)/180))*(sin(Days$phi)))/((cos((Days$Lat*pi)/180))*(cos(Days$phi))))
Days$D<-24-((24/pi*(acos(Days$Daylight))))
ggplot(data=Days,aes(x=Day,y=D,group=as.factor(Lat)))+geom_line(size=2)
#now get mean day length for each month
Day_length<-data.frame(Month=seq(1,12,1),Days=c(31,28,31,30,31,30,31,31,30,31,30,31))
Day_length$Day_end<-cumsum(Day_length$Days)
Day_length$Day_start<-(Day_length$Day_end-Day_length$Days+1)

Day_length$D<-NA
for (i in 1:nrow(Day_length)){
  Days_sub<-subset(Days,Day>=Day_length$Day_start[i]&Day<=Day_length$Day_end[i])
  Day_length$D[i]<-mean(Days_sub$D)
}

#merge day length data with the climate data
keeps<-c("Month","Days","D")
Day_length2<-Day_length[,c(keeps)]
Climate2<-merge(Climate,Day_length2,by="Month")

#now calculate potential evapotransipration
#first by calculating saturation pressure at the mean temperature
Aspect<-(abs((180) - abs(0 - 225)))*0.0174532925
HL<-0.339+0.808*(cos(0.904080551)*cos(0.034906585))-0.196*(sin(0.904080551)*sin(0.034906585))-0.482*(cos(Aspect)*sin(0.034906585))
Climate2$Ea<-exp((17.3*Climate2$temp)/(Climate2$temp+273.3))*0.611
Climate2$PETm<-ifelse(Climate2$temp<=0,0,(((Climate2$Ea*Climate2$temp)/(Climate2$temp+273.3))*29.8*Climate2$Days*Climate2$D)*(HL/10))
                      
#now calculate soil water balance
Climate3<-Climate2[complete.cases(Climate2),]
Climate3<-Climate3[with(Climate3, order(Year, Month)), ]
Climate3$Soil_m<-NULL
Climate3$Soil_m[1]<-0
head(Climate3,n = 20)

for (i in 2:nrow(Climate3)){
    Climate3$Soil_m[i]<-min(100,ifelse((Climate3$Wm[i]-Climate3$PETm[i])+Climate3$Soil_m[i-1]<0,0,Climate3$Wm[i]-Climate3$PETm[i])+Climate3$Soil_m[i-1])
 }

for (i in 2:nrow(Climate3)){
Climate3$Delta_soil[i]<-Climate3$Soil_m[i-1]-Climate3$Soil_m[i]
}

for (i in 2:nrow(Climate3)){
  Climate3$AET[i]<-min(ifelse(Climate3$Delta_soil[i]+Climate3$Wm[i]>0,Climate3$Delta_soil[i]+Climate3$Wm[i],0),Climate2$PETm[i])
}

Climate3$CWD<-ifelse(Climate3$PETm-Climate3$AET<0,0,Climate3$PETm-Climate3$AET)

ggplot(Climate3,aes(x=Year,y=CWD))+geom_point(shape=1)+geom_line(alpha=0.2,lty=2)+facet_wrap(~Month)
