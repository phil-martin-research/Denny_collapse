#script to predict proportion of plot BA represented by Oak
#in collapsed and uncollapsed plots

#import data
rm(list=ls(all=TRUE))
BA<-read.csv("Data/BA_tree_ab.csv")
head(BA)
BA$Transect<-ifelse(BA$Block>51,"Unenclosed","Enclosed")
#load packages
library(ggplot2)
library(lme4)
library(MuMIn)
library(plyr)

#classify plots by collapse status - collapsed (1) or not (0)
BA$Collapse<-NA
for (i in 1:nrow(BA)){
  BA$Collapse[i]<-ifelse(BA$BAPERCM[i]<=-0.25,1,0)
}
#classify plots to identify those that have *at some point* been classed as collapsed
BA$Collapse2<-NA
BA2<-NULL
Block_unique<-unique(BA$Block)
for (i in 1:length(Block_unique)){
  Block_sub<-subset(BA,Block==Block_unique[i])
  Block_sub$Collapse2<-ifelse(sum(Block_sub$Collapse)>0,1,0)
  BA2<-rbind(Block_sub,BA2)
}

#calculate the proportion of BA represented by oak
BA2$Prop_Q<-BA2$Q_BA/BA2$BAM
BA2$Prop_I<-BA2$I_BA/BA2$BAM
BA2$Prop_F<-BA2$F_BA/BA2$BAM

ggplot(BA2,aes(x=BAPERCM,y=Prop_Q))+geom_point()+geom_smooth(se=F,method="glm")+facet_wrap(~Year)
ggplot(BA2,aes(x=BAPERCM,y=Prop_I))+geom_point()+geom_smooth(se=F,method="glm")+facet_wrap(~Year)
ggplot(BA2,aes(x=BAPERCM,y=Prop_F))+geom_point()+geom_smooth(se=F,method="glm")+facet_wrap(~Year)

#now as boxplots
ggplot(BA2,aes(colour=as.factor(Collapse2),y=Prop_Q,x=as.factor(Year)))+geom_violin()
ggplot(BA2,aes(colour=as.factor(Collapse2),y=Prop_I,x=as.factor(Year)))+geom_violin()
ggplot(BA2,aes(colour=as.factor(Collapse2),y=Prop_F,x=as.factor(Year)))+geom_violin()



#now produce models of change in proportion of Oak BA
BA3<-subset(BA2,Year==1964|Year==2014)

#
std <- function(x) sd(x)/sqrt(length(x))
ddply(BA3,.(Year,Collapse2),summarize,Oak=mean(Prop_Q),Oak_SE=std(Prop_Q),
      Holly=mean(Prop_I),Holly_SE=std(Prop_I),Beech=mean(Prop_F),Beech_SE=std(Prop_F))



BA3$Q_PT<-qlogis(BA3$Prop_Q+0.01)
BA3$Year2<-as.factor(BA3$Year)
BA3$Collapse3<-as.factor(BA3$Collapse2)

M0<-lmer(Q_PT~1+(1|Block),data=BA3)
M1<-lmer(Q_PT~Year2*Collapse3+(1|Block),data=BA3)
M2<-lmer(Q_PT~Year2+Collapse3+(1|Block),data=BA3)
M3<-lmer(Q_PT~Year2+(1|Block),data=BA3)
M4<-lmer(Q_PT~Collapse3+(1|Block),data=BA3)

model_list<-list(M0,M1,M2,M3,M4)
Model_sel<-model.sel(model_list)
Model_sel$R_2<-c(r.squaredGLMM(M4)[1],r.squaredGLMM(M2)[1],r.squaredGLMM(M0)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M3)[1])
Model_avg<-model.avg(Model_sel)
summary(Model_avg)

write.csv(Model_sel,"Figures/Mod_sel_Oak_prop.csv")

#now predictions
new.data<-expand.grid(Year2=as.factor(c(1964,2014)),Collapse3=as.factor(c(0,1)))
new.data$Q_PT<-0
new.data$UCI<-0
new.data$LCI<-0

Coefs<-coefTable(Model_avg,full = T)
Coefs2<-data.frame(Par=as.character(rownames(Coefs)),Coefs)
Coefs2$Par<-as.character(Coefs2$Par)

new.data$Q_PT[1]<-Coefs2[1,2]
new.data$UCI[1]<-new.data$Q_PT[1]+(1.96*Coefs2[1,3])
new.data$LCI[1]<-new.data$Q_PT[1]-(1.96*Coefs2[1,3])

for (j in 2:3){
  new.data$Q_PT[j]<-Coefs2[1,2]+Coefs2[j,2]
  new.data$UCI[j]<-new.data$Q_PT[j]+(1.96*Coefs2[j,3])
  new.data$LCI[j]<-new.data$Q_PT[j]-(1.96*Coefs2[j,3])
}
new.data$Q_PT[4]<-Coefs2[1,2]+Coefs2[2,2]+Coefs2[4,2]
new.data$UCI[4]<-new.data$Q_PT[4]+(1.96*Coefs2[4,3])
new.data$LCI[4]<-new.data$Q_PT[4]-(1.96*Coefs2[4,3])

#now plot these results
theme_set(theme_bw(base_size=12))
dodge <- position_dodge(width=0.9)
Oak_plot<-ggplot(new.data,aes(x=Year2,y=plogis(Q_PT),ymin=plogis(LCI),ymax=plogis(UCI),fill=Collapse3))+
  geom_bar(stat="identity",position =dodge)+geom_errorbar(position =dodge,width=0.25)
Oak_plot2<-Oak_plot+ylab("Proportion of plot BA represented by oak")+xlab("Year")+ theme(legend.position = "none") 
Oak_plot2+scale_fill_brewer(palette="Set1")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))                                                                                                                                          
ggsave("Figures/Oak_prop_collapse.png",width = 8,height=6,units = "in",dpi=300)


#now produce models of change in proportion of holly BA
BA3$I_PT<-ifelse(BA3$Prop_I==0,BA3$Prop_I+0.01,BA3$Prop_I)
BA3$I_PT<-ifelse(BA3$Prop_I==1,BA3$Prop_I-0.01,BA3$I_PT)
BA3$I_PT<-qlogis(BA3$I_PT)

#now model this
M0<-lmer(I_PT~1+(1|Block),data=BA3)
M1<-lmer(I_PT~Year2*Collapse3+(1|Block),data=BA3)
M2<-lmer(I_PT~Year2+Collapse3+(1|Block),data=BA3)
M3<-lmer(I_PT~Year2+(1|Block),data=BA3)
M4<-lmer(I_PT~Collapse3+(1|Block),data=BA3)
M5<-lmer(I_PT~Transect+(1|Block),data=BA3)
M6<-lmer(I_PT~Transect*Year2+(1|Block),data=BA3)

model_list<-list(M0,M1,M2,M3,M4)
Model_sel<-model.sel(model_list)
Model_sel$R_2<-c(r.squaredGLMM(M4)[1],r.squaredGLMM(M0)[1],r.squaredGLMM(M2)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M3)[1])
Model_avg<-model.avg(Model_sel,subset = delta<=7)
summary(Model_avg)


#now predictions
new.data<-expand.grid(Year2=as.factor(c(1964,2014)),Collapse3=as.factor(c(0,1)))
new.data$I_PT<-0
new.data$UCI<-0
new.data$LCI<-0

Coefs<-coefTable(Model_avg,full = T)
Coefs2<-data.frame(Par=as.character(rownames(Coefs)),Coefs)
Coefs2$Par<-as.character(Coefs2$Par)

new.data$I_PT[1]<-Coefs2[1,2]
new.data$UCI[1]<-new.data$I_PT[1]+(1.96*Coefs2[1,3])
new.data$LCI[1]<-new.data$I_PT[1]-(1.96*Coefs2[1,3])

for (j in 2:3){
  new.data$I_PT[j]<-Coefs2[1,2]+Coefs2[j,2]
  new.data$UCI[j]<-new.data$I_PT[j]+(1.96*Coefs2[j,3])
  new.data$LCI[j]<-new.data$I_PT[j]-(1.96*Coefs2[j,3])
}
new.data$I_PT[4]<-Coefs2[1,2]+Coefs2[2,2]+Coefs2[4,2]
new.data$UCI[4]<-new.data$I_PT[4]+(1.96*Coefs2[4,3])
new.data$LCI[4]<-new.data$I_PT[4]-(1.96*Coefs2[4,3])

#now plot these results
theme_set(theme_bw(base_size=12))
dodge <- position_dodge(width=0.9)
Holly_plot<-ggplot(new.data,aes(x=Year2,y=plogis(I_PT),ymin=plogis(LCI),ymax=plogis(UCI),fill=Collapse3))+
  geom_bar(stat="identity",position =dodge)+geom_errorbar(position =dodge,width=0.25)
Holly_plot2<-Holly_plot+ylab("Proportion of plot BA represented by holly")+xlab("Year")+ theme(legend.position = "none") 
Holly_plot2+scale_fill_brewer(palette="Set1")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))                                                                                                                                          
ggsave("Figures/Holly_prop_collapse.png",width = 8,height=6,units = "in",dpi=300)


#now do the same for beech
#now produce models of change in proportion of holly BA
BA3$F_PT<-ifelse(BA3$Prop_F==0,BA3$Prop_F+0.01,BA3$Prop_F)
BA3$F_PT<-ifelse(BA3$Prop_F==1,BA3$Prop_F-0.01,BA3$F_PT)
BA3$F_PT<-qlogis(BA3$F_PT)

#now model this
M0<-lmer(F_PT~1+(1|Block),data=BA3)
M1<-lmer(F_PT~Year2*Collapse3+(1|Block),data=BA3)
M2<-lmer(F_PT~Year2+Collapse3+(1|Block),data=BA3)
M3<-lmer(F_PT~Year2+(1|Block),data=BA3)
M4<-lmer(F_PT~Collapse3+(1|Block),data=BA3)

model_list<-list(M0,M1,M2,M3,M4)
Model_sel<-model.sel(model_list)
Model_sel$R_2<-c(r.squaredGLMM(M4)[1],r.squaredGLMM(M0)[1],r.squaredGLMM(M2)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M3)[1])
Model_avg<-model.avg(Model_sel,subset = delta<=7)
summary(Model_avg)
