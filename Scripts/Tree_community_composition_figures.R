#community composition for different measures - trees

library(ggplot)

#save all this as a csv
Plots<-read.csv("Data/Denny_plots.csv")

head(Plots)

Plots$Tanner<-(Plots$Sor_BA+Plots$SorM)/2

keeps<-c("BAPERCM","Tanner","Block","Year")

Plots2<-Plots[keeps]
#remove plots from 1964
Plots2<-subset(Plots2,Year>1964)

#create variable for tanner that is bounded above 0
Plots2$Tanner2<-ifelse(Plots2$Tanner==0,0.001,Plots2$Tanner)
Plots2$Tanner2<-qlogis(Plots2$Tanner2)

M0.1<-lmer(Tanner2~1+(1|Block),data=Plots2)
M0.2<-lmer(Tanner2~1+(Block|Year),data=Plots2)
AICc(M0.1,M0.2)
#use first random effect specification
M1<-lmer(Tanner2~BAPERCM+(1|Block),data=Plots2)
M2<-lmer(Tanner2~BAPERCM+I(BAPERCM^2)+(1|Block),data=Plots2)
AICc(M1,M2,M0.1)
plot(M1)
plot(plogis(fitted(M2)),resid(M2))
r.squaredGLMM(M1)

#put prediction into dataframe
Plots2$Pred<-plogis(predict(M2,re.form=NA))
Plots3<-subset(Plots2,BAPERCM<1)



#plot this
theme_set(theme_bw(base_size=12))
Coll_Tanner1<-ggplot(Plots2,aes(x=BAPERCM*100,y=Tanner,colour=as.factor(Year)))+geom_point(shape=1)
Coll_Tanner2<-Coll_Tanner1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Coll_Tanner3<-Coll_Tanner2+geom_line(data=Plots3,aes(x=BAPERCM*100,y=Pred,colour=NULL),size=2)+ylab("Tanner similarity to same plot in 1964")+xlab("Percentage basal area change since 1964")
Coll_Tanner3+scale_colour_brewer(palette="Set1","Year")
ggsave("Figures/BA_Tanner.png",width = 8,height=6,units = "in",dpi=300)
