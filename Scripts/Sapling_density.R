#script to predict density of saplings (<10cm DBH) of
#all trees in collapsed and uncollapsed plots

#import data
rm(list=ls(all=TRUE))
Saplings<-read.csv("Data/Denny_plots.csv")
head(Saplings)
#load packages
library(ggplot2)
library(lme4)
library(MuMIn)
library(plyr)

#classify plots by collapse status - collapsed (1) or not (0)
Saplings$Collapse<-NA
for (i in 1:nrow(Saplings)){
  Saplings$Collapse[i]<-ifelse(Saplings$BAPERCM[i]<=-0.25,1,0)
}
#classify plots to identify those that have *at some point* been classed as collapsed
Saplings$Collapse2<-NA
Saplings2<-NULL
Block_unique<-unique(Saplings$Block)
for (i in 1:length(Block_unique)){
  Block_sub<-subset(Saplings,Block==Block_unique[i])
  Block_sub$Collapse2<-ifelse(sum(Block_sub$Collapse)>0,1,0)
  Saplings2<-rbind(Block_sub,Saplings2)
}

#subset to give only data from 1964 and 2014
Saplings3<-subset(Saplings2,Year==1964|Year==2014)
Saplings3$Year2<-as.factor(Saplings3$Year)
Saplings3$Collapse3<-as.factor(Saplings3$Collapse2)

head(Saplings3)

#model to see if SD for saplings differs for 
#collapsed and uncollapsed plots

M0<-glmer(SDS~1+(1|Block),data=Saplings3,family="poisson")
M1<-glmer(SDS~Year2*Collapse3+(1|Block),data=Saplings3,family="poisson")
M2<-glmer(SDS~Year2+Collapse3+(1|Block),data=Saplings3,family="poisson")
M3<-glmer(SDS~Collapse3+(1|Block),data=Saplings3,family="poisson")
M4<-glmer(SDS~Year2+(1|Block),data=Saplings3,family="poisson")


model_list<-list(M0,M1,M2,M3,M4)
Model_sel<-model.sel(model_list)
Model_sel$R_2<-c(r.squaredGLMM(M1)[1])
Model_avg<-model.avg(Model_sel)
summary(Model_avg)

r.squaredGLMM(x=M0)[1]

write.csv(Model_sel,"Figures/Mod_sel_Saplings.csv")


#now predictions
new.data<-expand.grid(Year2=as.factor(c(1964,2014)),Collapse3=as.factor(c(0,1)))
new.data$SDS<-0
new.data$UCI<-0
new.data$LCI<-0

Coefs<-coefTable(Model_avg,full = T)
Coefs2<-data.frame(Par=as.character(rownames(Coefs)),Coefs)
Coefs2$Par<-as.character(Coefs2$Par)

new.data$SDS[1]<-Coefs2[1,2]
new.data$UCI[1]<-new.data$SDS[1]+(1.96*Coefs2[1,3])
new.data$LCI[1]<-new.data$SDS[1]-(1.96*Coefs2[1,3])

for (j in 2:3){
  new.data$SDS[j]<-Coefs2[1,2]+Coefs2[j,2]
  new.data$UCI[j]<-new.data$SDS[j]+(1.96*Coefs2[j,3])
  new.data$LCI[j]<-new.data$SDS[j]-(1.96*Coefs2[j,3])
}
new.data$SDS[4]<-Coefs2[1,2]+Coefs2[2,2]+Coefs2[4,2]
new.data$UCI[4]<-new.data$SDS[4]+(1.96*Coefs2[4,3])
new.data$LCI[4]<-new.data$SDS[4]-(1.96*Coefs2[4,3])

#now plot these results
theme_set(theme_bw(base_size=12))
dodge <- position_dodge(width=0.9)
Sapling_plot<-ggplot(new.data,aes(x=Year2,y=(exp(SDS)),ymin=(exp(LCI)),ymax=(exp(UCI)),fill=Collapse3))+
  geom_bar(stat="identity",position =dodge)+geom_errorbar(position =dodge,width=0.25)
Sapling_plot2<-Sapling_plot+ylab("Plot sapling density")+xlab("Year")+ theme(legend.position = "none") 
Sapling_plot2+scale_fill_brewer(palette="Set1")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))                                                                                                                                          
ggsave("Figures/Saplings_collapse.png",width = 8,height=6,units = "in",dpi=300)


exp(2.454254)
exp(0.136664)
exp(2.454254-1.426211)
exp(0.097955)
exp(2.454254+0.098886)
exp(0.162246)
exp(2.454254-1.426211-0.007971)
exp(0.067723)
