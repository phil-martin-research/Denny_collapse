#script to look at changes in climatic water deficit
#at Denny
rm(list=ls(all=TRUE))
library(ggplot2)
library(plyr)
library(gridExtra)
library(grid)


CWD<-read.csv("Data/CWD_Denny.csv")
CWD<-CWD[complete.cases(CWD),]
CWD<-subset(CWD,Month>4&Month<10)
CWD_calc<-ddply(CWD,.(Year),summarise,m_CWD=mean(CWD))
CWD_calc<-subset(CWD_calc,Year>=1964)

CWD_calc$m_CWD_diff<-CWD_calc$m_CWD-mean(CWD_calc$m_CWD)
SD_CWD<-sd(CWD_calc$m_CWD_diff)

CWD_calc$Ex<-ifelse(CWD_calc$m_CWD_diff>SD_CWD,"Drought","Non-drought")
CWD_calc$Ex<-ifelse(CWD_calc$m_CWD_diff>(SD_CWD*2),"Extreme drought",CWD_calc$Ex)
CWD_calc$Ex<-factor(CWD_calc$Ex,c("Extreme drought","Drought","Non-drought"))


#plot CWD anomolies
theme_set(theme_bw(base_size=12))
CWD_1<-ggplot(CWD_calc,aes(x=Year,y=m_CWD_diff,fill=as.factor(Ex)))+geom_bar(stat="identity")+geom_hline(y=SD_CWD,lty=2)+geom_hline(y=-SD_CWD,lty=2)+geom_hline(y=0,size=1)
CWD_2<-CWD_1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour="black",fill=NA))
CWD_3<-CWD_2+scale_fill_manual("Drought status",values=c("black","dark grey","light grey"))
CWD_4<-CWD_3+ylab("April-September climatic water deficit anomaly (mm)")
CWD_5<-CWD_4+annotate("text", label = "(b)", x = 1964, y = 45, size=4)

#combine this with temperature plot
png("Figures/Temp_drought_Figure2.png",height=4,width=8,res=1000,units="in")
grid.draw(cbind(ggplotGrob(Temp_plot4), ggplotGrob(CWD_5), size="last"))
dev.off()
