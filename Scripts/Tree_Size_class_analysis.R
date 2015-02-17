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
Trees<-read.csv("Data/Denny_trees_cleaned.csv")
BA<-read.csv("Data/BA_Gradient.csv")

#remove dead trees
Trees_live<-subset(Trees,Status==1)
#remove trees <10cm
Trees_M<-subset(Trees_live,DBH>10)
Trees_M<-subset(Trees_live,Year>1960)
Trees_M$Year2<-as.character(Trees_M$Year)
Trees_M$Year2<-ifelse(Trees_M$Year==1996,"1996/9",Trees_M$Year2)
Trees_M$Year2<-ifelse(Trees_M$Year==1999,"1996/9",Trees_M$Year2)
Trees_M$BA<-(Trees_M$DBH^2*(pi/4))/400
Trees_M$BA2<-(Trees_M$DBH^2)*0.0007854
plot(Trees_M$BA,Trees_M$BA2)



#load data
Plots<-read.csv("Data/Denny_plots.csv")

#classify plots by collapse status - collapsed (1) or not (0)
Plots$Collapse<-NA
for (i in 1:nrow(Plots)){
  Plots$Collapse[i]<-ifelse(Plots$BAPERCM[i]<=-0.25,1,0)
}
#classify plots to identify those that have *at some point* been classed as collapsed
Plots$Collapse2<-NA
Plots2<-NULL
Block_unique<-unique(Plots$Block)
for (i in 1:length(Block_unique)){
  Block_sub<-subset(Plots,Block==Block_unique[i])
  Block_sub$Collapse2<-ifelse(sum(Block_sub$Collapse)>0,1,0)
  Plots2<-rbind(Block_sub,Plots2)
}

#merge together the two datasets
Trees_blocks<-merge(Plots2,Trees_M,by="Block",all=T)
keeps<-c("DBH","Year.y","Year2","Collapse2","Block","BA","BA2")
head(Trees_blocks)
Tree_collapse<-Trees_blocks[keeps]

#remove data for years I'm not interested in (1984/88)
Tree_collapse<-subset(Tree_collapse,Year.y!=1984&Year.y!=1988)
Tree_collapse<-subset(Tree_collapse,DBH>10)
Tree_collapse<-subset(Tree_collapse,!is.na(Collapse2))
Tree_collapse$No<-ifelse(Tree_collapse$Collapse2==1,38,22)
head(Tree_collapse)
Tree_collapse<-unique(Tree_collapse)


#create a loop to divide plots into DBH classes
Size_class<-data.frame(Size_min=c(seq(10,90,10),100),Size_max=c(seq(20,100,10),150),Class=c("10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100",">100"))
Tree_class<-NULL
for (i in 1:nrow(Size_class)){
  Tree_sub<-subset(Tree_collapse,DBH>=Size_class$Size_min[i]&DBH<=Size_class$Size_max[i])
  Tree_sub$Size<-Size_class$Size_min[i]
  Tree_sub$Size_class<-Size_class$Class[i]
  Tree_class<-rbind(Tree_sub,Tree_class)
}
Tree_class$Size_class<-factor(Tree_class$Size_class, c("10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100",">100"))

#now do a count of the stem number for collapsed and uncollapsed plots over time
CU_change<-ddply(Tree_class,.(Collapse2,Year2,Size,Size_class),summarize,BA=sum(BA2),N=length(BA2))
head(CU_change)
Coll_Change<-NULL
Block_size<-unique(CU_change[c("Collapse2","Size")])
for (i in 1:nrow(Block_size)){
  Change_sub<-subset(CU_change,Collapse2==Block_size$Collapse2[i])
  Change_sub<-subset(Change_sub,Size==Block_size$Size[i])
  Change_sub$N_Change<-log(Change_sub$N)-log(Change_sub$N[1])
  Change_sub$BA_Change<-log(Change_sub$BA)-log(Change_sub$BA[1])
  Coll_Change<-rbind(Change_sub,Coll_Change)
}
#remove 1964 for plotting
Coll_Change2<-subset(Coll_Change,Year2!="1964")

#plot change in SD
Coll_Change2$Collapse3<-ifelse(Coll_Change2$Collapse2==1,"Collapsed","Stable")
theme_set(theme_bw(base_size=12))
SD_plot1<-ggplot(Coll_Change2,aes(x=Size_class,y=(exp(N_Change)-1)*100))+geom_bar(stat="identity")+facet_grid(Collapse3~Year2)+geom_hline(x=0,lty=2)
SD_plot2<-SD_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
SD_plot2+xlab("DBH size class (cm)")+ylab("Percentage change in stem density since 1964")+ theme(axis.text.x=element_text(angle = -90, hjust = 0,vjust = 0))
ggsave("Figures/SD_Size_Change.png",width = 8,height=6,units = "in",dpi=300)


#create a loop for all trees in 1964
#order them from lowest DBh to highest
#and then produce a cumulative sum
Trees_1964<-subset(Tree_collapse,Year.y==1964)
BA_Sum<-sum(Trees_1964$BA2)
Trees_1964<-Trees_1964[with(Trees_1964, order(DBH)), ]
Trees_1964$Cum_Total<-NA
Trees_1964$Cum_Total[1]<-Trees_1964$BA2[1]
Trees_1964$Cum_Prop<-NA
Trees_1964$Cum_Prop[1]<-Trees_1964$BA2[1]/BA_Sum

for (i in 2:nrow(Trees_1964)){
  Trees_1964$Cum_Total[i]<-Trees_1964$Cum_Total[i-1]+Trees_1964$BA2[i]
  Trees_1964$Cum_Prop[i]<-Trees_1964$Cum_Total[i]/BA_Sum
}

#plot cumulative basal area over DBH
theme_set(theme_bw(base_size=12))
BA_Cumplot1<-ggplot(Trees_1964,aes(x=DBH,y=Cum_Prop*100))+geom_line()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
BA_Cumplot1+xlim(10,150)+scale_x_continuous(breaks=c(10,40,80,120,150))+xlab("Tree DBH")+ylab("Percentage of total BA")+geom_rug(sides="b")
ggsave("Figures/BA_cumulative.png",width = 8,height=6,units = "in",dpi=300)

#now work out what proportion of the declines were due to declines in stems >80cm


#create factor to class trees as either over or under 50 cm DBH
Tree_collapse$Over50<-ifelse(Tree_collapse$DBH>50,1,0)
BA_loss50<-ddply(Tree_collapse,.(Over50,Year.y),summarize,BA=sum(BA2))

#loss of BA from stems >50cm DBH accounts for 82% of losses in this case


#now produce time series type analyses of change in stem density for each plot for different size classes
quantile(Trees_M$DBH)

#create a loop to classify trees by different size classes
Trees_M_Size<-NULL
Size_class<-data.frame(minimum=c(10,15,25,45,150))
for (i in 2:nrow(Size_class)){
  Tree_subset<-subset(Trees_M,DBH>Size_class$minimum[i-1])
  head(Tree_subset)
  Tree_subset<-subset(Tree_subset,DBH<Size_class$minimum[i])
  Tree_subset$Size_Class<-Size_class$minimum[i]
  Trees_M_Size<-rbind(Tree_subset,Trees_M_Size)
}

#now summarise this by block and year


SD_class<-ddply(Trees_M_Size,.(Block,Year,Size_Class),summarise,SD=(length(DBH)*25),BA=sum(BA))

Trees_blocks<-merge(SD_class,Plots2,by=c("Block","Year"))
head(Trees_blocks)
ggplot(Trees_blocks,aes(x=Year,y=SD,group=Size_Class,colour=as.factor(Size_Class)))+geom_point()+facet_grid(Size_Class~Collapse2)+geom_smooth(se=F,size=3,method=lm,alpha=0.5)


#count the number of stems per plot
Block_size<-unique(Trees_blocks[c("Block", "Size_Class","Year","Collapse2")])
Size_grid<-expand.grid(Block=unique(Block_size$Block),Size_Class=unique(Block_size$Size_Class),Year=unique(Block_size$Year))
Size_grid$SD<-NA
for (i in 1:nrow(Size_grid)){
  Block_sub1<-subset(Trees_blocks,Block==Size_grid$Block[i])
  Block_sub2<-subset(Block_sub1,Size_Class==Size_grid$Size_Class[i])
  Block_sub3<-subset(Block_sub2,Year==Size_grid$Year[i])
  if (nrow(Block_sub3)==0){
    Size_grid$SD[i]<-0
    Size_grid$BA[i]<-0
    } else {
      Size_grid$SD[i]<-Block_sub3$SD
      Size_grid$BA[i]<-Block_sub3$BA
      }
  Size_grid$Collapse[i]<-Block_sub1$Collapse2[1]
}


head(Size_grid)

ggplot(Size_grid,aes(x=Year,y=SD,group=Size_Class,colour=as.factor(Size_Class)))+geom_point()+facet_grid(Size_Class~Collapse)+geom_smooth(size=2,method=lm,alpha=0.5)
  
#Rescale year for models
Size_grid$Year2<-Size_grid$Year-mean(Size_grid$Year)

#model of stem density change for different size classes over time
#first look at small trees <15cm
Size_grid15<-subset(Size_grid,Size_Class==15)
#the first construction seems best
SD_15<-glmer(SD~Year2*as.factor(Collapse)+(1|Block),data=Size_grid15,family=poisson)
summary(SD_M3)
Size_grid15$SD_pred<-exp(predict(SD_15,re.form=NA))

#do the same with 25
Size_grid25<-subset(Size_grid,Size_Class==25)

SD_M0.1<-glmer(SD~1+(1|Block),data=Size_grid25,family=poisson)
SD_25<-glmer(SD~Year2*as.factor(Collapse)+(1|Block),data=Size_grid25,family=poisson)
r.squaredGLMM(SD_M3)
Size_grid25$SD_pred<-exp(predict(SD_25,re.form=NA))

#do the same with 45
Size_grid45<-subset(Size_grid,Size_Class==45)

SD_M0.1<-glmer(SD~1+(1|Block),data=Size_grid45,family=poisson)

SD_45<-glmer(SD~Year2*as.factor(Collapse)+(1|Block),data=Size_grid45,family=poisson)
r.squaredGLMM(SD_M3)
Size_grid45$SD_pred<-exp(predict(SD_45,re.form=NA))

#do the same with large trees
Size_grid150<-subset(Size_grid,Size_Class==150)

SD_M0.1<-glmer(SD~1+(1|Block),data=Size_grid150,family=poisson)
t
SD_150<-glmer(SD~Year2*as.factor(Collapse)+(1|Block),data=Size_grid150,family=poisson)
r.squaredGLMM(SD_M3)

Size_grid150$SD_pred<-exp(predict(SD_150,re.form=NA))

#now do BA

#for small trees

BA_15<-lmer(BA~Year2*as.factor(Collapse)+(Block|Year2),data=Size_grid15)
r.squaredGLMM(BA_15)
summary(Size_grid15)

Size_grid15$BA_pred<-predict(BA_15,re.form=NA)
#for trees ~25cm

BA_25<-lmer(BA~Year2*as.factor(Collapse)+(Block|Year2),data=Size_grid25)
r.squaredGLMM(SD_M3)
Size_grid25$BA_pred<-predict(BA_25,re.form=NA)

#for trees ~45cm

BA_45<-lmer(BA~Year2*as.factor(Collapse)+(Block|Year2),data=Size_grid45)
r.squaredGLMM(SD_M3)
AICc(SD_M1,SD_M2,SD_M3)
Size_grid45$BA_pred<-predict(BA_45,re.form=NA)


#for large trees
BA_150<-lmer(BA~Year2*as.factor(Collapse)+(Block|Year2),data=Size_grid150)
r.squaredGLMM(SD_M3)
Size_grid150$BA_pred<-predict(BA_150,re.form=NA)


BA_preds<-rbind(Size_grid15,Size_grid25,Size_grid45,Size_grid150)

BA_preds$Collapse2<-ifelse(BA_preds$Collapse==1,"Collapsed","Stable")
BA_preds$Size_Class2<-ifelse(BA_preds$Size_Class==15,"10-15cm",BA_preds$Size_Class)
BA_preds$Size_Class2<-ifelse(BA_preds$Size_Class==25,"15-25cm",BA_preds$Size_Class2)
BA_preds$Size_Class2<-ifelse(BA_preds$Size_Class==45,"25-45cm",BA_preds$Size_Class2)
BA_preds$Size_Class2<-ifelse(BA_preds$Size_Class==150,">45cm",BA_preds$Size_Class2)

BA_preds$Size_Class2<-factor(BA_preds$Size_Class2,c(">45cm","25-45cm","15-25cm","10-15cm"))


#create a figure to show changes in BA per size class for collapsed and non-collapsed plots
theme_set(theme_bw(base_size=12))
BA_size1<-ggplot(BA_preds,aes(x=Year,y=BA,group=Size_Class2,colour=as.factor(Size_Class2)))+geom_point(shape=1)+geom_line(aes(y=BA_pred))+facet_grid(Size_Class2~Collapse2,scales="free")
BA_size2<-BA_size1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
BA_size2+scale_colour_brewer(palette="Set1","DBH size class")+ylab("BA (metres squared per ha)")
ggsave("Figures/BA_Size_class.png",width = 8,height=8,units = "in",dpi=1200)

#create a figure to show changes in BA per size class for collapsed and non-collapsed plots
theme_set(theme_bw(base_size=12))
SD_size1<-ggplot(BA_preds,aes(x=Year,y=SD,group=Size_Class2,colour=as.factor(Size_Class2)))+geom_jitter(shape=1)+geom_line(aes(y=SD_pred))+facet_grid(Size_Class2~Collapse2,scales="free")
SD_size2<-SD_size1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
SD_size2+scale_colour_brewer(palette="Set1","DBH size class")+ylab("Stem density per ha")
ggsave("Figures/SD_Size_class.png",width = 8,height=8,units = "in",dpi=1200)

