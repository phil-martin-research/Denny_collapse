#script to do analysis of changes in abundance of trees in different size classes

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)
library(lme4)
library(MuMIn)

#load in data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Trees<-read.csv("Denny_trees_cleaned.csv")
BA<-read.csv("BA_Gradient.csv")

#remove dead trees
Trees_live<-subset(Trees,Status==1)
#remove trees <10cm
Trees_M<-subset(Trees_live,DBH>10)

#create a loop to classify trees into different size classes into quantiles
quantile(Trees_M$DBH)
Trees_M_Size<-NULL
Size_class<-data.frame(min=c(10,15,25,45,150))
for (i in 2:nrow(Size_class)){
  Tree_subset<-subset(Trees_M,DBH>Size_class$min[i-1])
  Tree_subset<-subset(Tree_subset,DBH<=Size_class$min[i])
  Tree_subset$Size_Class<-Size_class$min[i]
  Trees_M_Size<-rbind(Tree_subset,Trees_M_Size)
}


#create count of stems in certain size classes
Stem_density_Size<-count(Trees_M_Size,var=c("Block","Year","Size_Class"))

Stem_density_Size2<-merge(Stem_density_Size,BA,by=c("Block","Year"))

head(Stem_density_Size2)

#plot this relationship
ggplot(Stem_density_Size2,aes(x=BAPERCM,y=freq,group=Block))+geom_point()+facet_wrap(~Size_Class)+geom_smooth(se=F,colour="blue",size=3,method="lm",aes(group=NULL))

#now work out the basal area of trees in each size class
for (i in 1:nrow(Trees_M_Size)){
  Trees_M_Size$BA[i]<-ifelse(Trees_M_Size$DBH[i]>10,(Trees_M_Size$DBH[i]^2*(pi/4))/400,0)
}

Trees_BA_Size<-ddply(Trees_M_Size,.(Block,Year,Size_Class),summarise,T_BA=sum(BA))
Trees_BA_Size2<-merge(Trees_BA_Size,BA,by=c("Block","Year"))

#now loop through to calculate proportional change in basal area for each size class
Trees_unique<-unique(Trees_BA_Size2[,c("Block","Size_Class")])
Trees_BA_Size3<-NULL
for (i in 1:nrow(Trees_unique)){
  Trees_sub<-subset(Trees_BA_Size2,Block==Trees_unique$Block[i])
  Trees_sub<-subset(Trees_sub,Size_Class==Trees_unique$Size_Class[i])
  Trees_sub<-Trees_sub[with(Trees_sub, order(Year)), ]
  Trees_sub$BA_Change<-(Trees_sub$T_BA-Trees_sub$T_BA[1])/Trees_sub$T_BA[1]
  Trees_BA_Size3<-rbind(Trees_BA_Size3,Trees_sub)
}

#now loop through to create better label name for each size class
Size_reclass<-data.frame(Size=c(15,25,45,150),New_size=c("10-15","15-25","25-45","45-150"))
Trees_BA_Size3$Size_reclass<-character(length = 902)
for (i in 1:nrow(Size_reclass)){
  for (y in 1:nrow(Trees_BA_Size3)){
  Trees_BA_Size3$Size_reclass[y]<-ifelse(Trees_BA_Size3$Size_Class[y]==Size_reclass[i,1],as.character(Size_reclass[i,2]),Trees_BA_Size3$Size_reclass[y])
}
}

#now plot the relationship between basal area for each size class and time
ggplot(Trees_BA_Size3,aes(x=Year,y=T_BA,group=Block))+geom_point()+facet_wrap(~Size_Class)+geom_line()+geom_smooth(se=F,colour="blue",size=3,method="lm",aes(group=NULL))
ggplot(Trees_BA_Size3,aes(x=Year,y=BA_Change,group=Block))+geom_point()+facet_wrap(~Size_Class,scales = "free_y")+geom_line()+geom_smooth(se=F,colour="blue",size=3,method="lm",aes(group=NULL))


#subset to remove blocks from 1964
Trees_BA_Size4<-subset(Trees_BA_Size3,Year>1964)
Trees_BA_Size4<-subset(Trees_BA_Size4,BAPERCM<1)
Trees_BA_Size4$BA_Change2<-Trees_BA_Size4$BA_Change*(-1)
Trees_BA_Size4$BAPERCM2<-Trees_BA_Size4$BAPERCM*(-1)

#now model this
#first a null model
#first for trees <15cm
Trees_BA_Size_15<-subset(Trees_BA_Size4,Size_Class<=15)

M0_15<-lmer(BA_Change~1+(1|Block),data=Trees_BA_Size_15)
plot(M0_15)
M1_15<-lmer(BA_Change~BAPERCM+(1|Block),data=Trees_BA_Size_15)
plot(M1_15)
r.squaredGLMM(M1)
AICc(M0_15,M1_15)
#second model marginally better
#now plot this
plot(Trees_BA_Size_15$BAPERCM,Trees_BA_Size_15$BA_Change)
points(Trees_BA_Size_15$BAPERCM,predict(M1_15,re.form = NA),col="red")

#next for trees <25cm
Trees_BA_Size_25<-subset(Trees_BA_Size4,Size_Class<=25)
M0_25<-lmer(BA_Change~1+(1|Block),data=Trees_BA_Size_25)
plot(M0_25)
M1_25<-lmer(BA_Change~BAPERCM+(1|Block),data=Trees_BA_Size_25)
plot(M1_25)
r.squaredGLMM(M1_25)
AICc(M0_25,M1_25)
#null model is better
#now plot this
plot(Trees_BA_Size_25$BAPERCM,Trees_BA_Size_25$BA_Change)
points(Trees_BA_Size_25$BAPERCM,predict(M0_25,re.form = NA),col="red")


#next for trees <45cm
Trees_BA_Size_45<-subset(Trees_BA_Size4,Size_Class<=45)
M0_45<-lmer(BA_Change~1+(1|Block),data=Trees_BA_Size_45)
plot(M0_45)
M1_45<-lmer(BA_Change~BAPERCM+(1|Block),data=Trees_BA_Size_45)
plot(M1_45)
r.squaredGLMM(M1_45)
AICc(M0_45,M1_45)
#null model is better
#now plot this
plot(Trees_BA_Size_45$BAPERCM,Trees_BA_Size_45$BA_Change)
points(Trees_BA_Size_45$BAPERCM,predict(M0_45,re.form = NA),col="red")


#next for trees <150cm
Trees_BA_Size_150<-subset(Trees_BA_Size4,Size_Class>45)
M0_150<-lmer(BA_Change~1+(1|Block),data=Trees_BA_Size_150)
plot(M0_150)
M1_150<-lmer(BA_Change~BAPERCM+(1|Block),data=Trees_BA_Size_150)
plot(M1_150)
r.squaredGLMM(M1_150)
AICc(M0_150,M1_150)
#model 1 is better
#now plot this
plot(Trees_BA_Size_150$BAPERCM,Trees_BA_Size_150$BA_Change)
points(Trees_BA_Size_150$BAPERCM,predict(M1_150,re.form = NA),col="red")



#and now over the gradient
theme_set(theme_bw(base_size=12))
Size_class_plot<-ggplot(Trees_BA_Size4,aes(x=BAPERCM*100,y=BA_Change*100,group=Block))+geom_point()+facet_wrap(~Size_reclass,scales = "free_y")+geom_smooth(se=F,colour="blue",size=2,alpha=0.2,method="lm",aes(group=NULL))
Size_class_plot2<-Size_class_plot+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
Size_class_plot2+xlab("Total basal area percentage change since 1964")+ylab("Basal area percentage change since 1964 for tree size class")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Collapse_Size_class_gradient.png",width = 8,height=6,units = "in",dpi=300)
