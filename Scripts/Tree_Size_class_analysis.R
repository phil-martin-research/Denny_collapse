#script to do analysis of changes in abundance of trees in different size classes

rm(list=ls(all=TRUE))

#open packages neeeded
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)
library(lme4)
library(MuMIn)
library(gridExtra)

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


SD_class<-ddply(Trees_M_Size,.(Block,Year,Size_Class),summarise,SD=length(DBH),BA=sum(BA))

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

Size_grid2<-subset(Size_grid,Year!=1999&Block<51)
Size_grid3<-subset(Size_grid,Year==1964&Block>51|Year==1999&Block>51|Year==2014&Block>51)
Size_grid4<-rbind(Size_grid2,Size_grid3)
head(Size_grid4,n=20)

ggplot(Size_grid4,aes(x=Year,y=SD,group=Size_Class,colour=as.factor(Size_Class)))+geom_point()+facet_grid(Size_Class~Collapse)+geom_smooth(size=2,method=lm,alpha=0.5,aes(group=as.factor(Block)),se=F)
  
#Rescale year for models
Size_grid4<-subset(Size_grid4,Year==1964|Year==2014)
Size_grid4$Year2<-as.factor(Size_grid4$Year)

#model of stem density change for different size classes over time
#first look at small trees <15cm
Size_grid15<-subset(Size_grid4,Size_Class==15)

SD_15_M0<-glmer(SD~1+(1|Block),data=Size_grid15,family=poisson)
SD_15_M1<-glmer(SD~Year2*as.factor(Collapse)+(1|Block),data=Size_grid15,family=poisson)
SD_15_M2<-glmer(SD~Year2+as.factor(Collapse)+(1|Block),data=Size_grid15,family=poisson)
SD_15_M3<-glmer(SD~Year2+(1|Block),data=Size_grid15,family=poisson)
SD_15_M4<-glmer(SD~as.factor(Collapse)+(1|Block),data=Size_grid15,family=poisson)

SD_15_models<-list(SD_15_M0,SD_15_M1,SD_15_M2,SD_15_M3,SD_15_M4)
Mod_sel_SD15<-model.sel(SD_15_models)
Mod_sel_SD15$R_2<-c(r.squaredGLMM(SD_15_M3)[1],r.squaredGLMM(SD_15_M2)[1],r.squaredGLMM(SD_15_M1)[1],
                    r.squaredGLMM(SD_15_M0)[1],r.squaredGLMM(SD_15_M4)[1])
Models_SD15<-get.models(Mod_sel_SD15,subset= delta <7)
Mod.avg_SD15<-model.avg(Models_SD15)
summary(Mod.avg_SD15)

write.csv(Mod_sel_SD15,"Figures/Mod_sel_SD_15.csv")
write.csv(Mod.coef_SD15,"Figures/Mod_coef_SD_15.csv")
summary(Mod.avg_SD15)

#do the same with 25
Size_grid25<-subset(Size_grid4,Size_Class==25)

SD_25_M0<-glmer(SD~1+(Year2|Block),data=Size_grid25,family=poisson)
SD_25_M1<-glmer(SD~Year2*as.factor(Collapse)+(Year2|Block),data=Size_grid25,family=poisson)
SD_25_M2<-glmer(SD~Year2+as.factor(Collapse)+(Year2|Block),data=Size_grid25,family=poisson)
SD_25_M3<-glmer(SD~Year2+(Year2|Block),data=Size_grid25,family=poisson)
SD_25_M4<-glmer(SD~as.factor(Collapse)+(Year2|Block),data=Size_grid25,family=poisson)

SD_25_models<-list(SD_25_M0,SD_25_M1,SD_25_M2,SD_25_M3,SD_25_M4)
Mod_sel_SD25<-mod.sel(SD_25_models)
Mod_sel_SD25$R_2<-c(r.squaredGLMM(SD_25_M2)[1],r.squaredGLMM(SD_25_M1)[1],r.squaredGLMM(SD_25_M3)[1],
                    r.squaredGLMM(SD_25_M4)[1],r.squaredGLMM(SD_25_M0)[1])
Mod.avg_SD25<-model.avg(Mod_sel_SD25,subset= delta <7)
summary(Mod.avg_SD25)                    

Models_SD25<-get.models(Mod_sel_SD25,subset= delta <7)                  
Mod.avg_SD25<-model.avg(Models_SD25,subset= delta <7)
Mod.coef_SD25<-(Mod.avg_SD25$av)                    

write.csv(Mod_sel_SD25,"Figures/Mod_sel_SD_25.csv")
write.csv(Mod.coef_SD25,"Figures/Mod_coef_SD_25.csv")


#do the same with 45
Size_grid45<-subset(Size_grid4,Size_Class==45)

SD_45_M0<-glmer(SD~1+(Year2|Block),data=Size_grid45,family=poisson)
SD_45_M1<-glmer(SD~Year2*as.factor(Collapse)+(Year2|Block),data=Size_grid45,family=poisson)
SD_45_M2<-glmer(SD~Year2+as.factor(Collapse)+(Year2|Block),data=Size_grid45,family=poisson)
SD_45_M3<-glmer(SD~Year2+(Year2|Block),data=Size_grid45,family=poisson)
SD_45_M4<-glmer(SD~as.factor(Collapse)+(Year2|Block),data=Size_grid45,family=poisson)

SD_45_models<-list(SD_45_M0,SD_45_M1,SD_45_M2,SD_45_M3,SD_45_M4)
Mod_sel_SD45<-mod.sel(SD_45_models)
Mod_sel_SD45$R_2<-c(r.squaredGLMM(SD_45_M4)[1],r.squaredGLMM(SD_45_M2)[1],r.squaredGLMM(SD_45_M1)[1],
                    r.squaredGLMM(SD_45_M0)[1],r.squaredGLMM(SD_45_M3)[1])

Models_SD45<-get.models(Mod_sel_SD45,subset= delta <7)  
Mod.avg_SD45<-model.avg(Models_SD45)
(summary(Mod.avg_SD45))
Mod.coef_SD45<-(Mod.avg_SD45$avg.model)                    

Mod.avg_SD45$avg.model

Mod.avg_SD45$coefficients

?model.avg

write.csv(Mod_sel_SD45,"Figures/Mod_sel_SD_45.csv")
write.csv(Mod.coef_SD45,"Figures/Mod_coef_SD_45.csv")

#do the same with large trees
Size_grid150<-subset(Size_grid4,Size_Class==150)

SD_150_M0<-glmer(SD~1+(Year2|Block),data=Size_grid150,family=poisson)
SD_150_M1<-glmer(SD~Year2*as.factor(Collapse)+(Year2|Block),data=Size_grid150,family=poisson)
SD_150_M2<-glmer(SD~Year2+as.factor(Collapse)+(Year2|Block),data=Size_grid150,family=poisson)
SD_150_M3<-glmer(SD~Year2+(Year2|Block),data=Size_grid150,family=poisson)
SD_150_M4<-glmer(SD~as.factor(Collapse)+(Year2|Block),data=Size_grid150,family=poisson)

SD_150_models<-list(SD_150_M0,SD_150_M1,SD_150_M2,SD_150_M3,SD_150_M4)
Mod_sel_SD150<-mod.sel(SD_150_models)
Mod_sel_SD150$R_2<-c(r.squaredGLMM(SD_150_M1)[1],r.squaredGLMM(SD_150_M2)[1],r.squaredGLMM(SD_150_M4)[1],
                    r.squaredGLMM(SD_150_M3)[1],r.squaredGLMM(SD_150_M0)[1])

Mod.coef_SD150<-coef(summary(SD_150_M1))
                

write.csv(Mod_sel_SD150,"Figures/Mod_sel_SD_150.csv")
write.csv(Mod.coef_SD150,"Figures/Mod_coef_SD_150.csv")

summary(Size_grid4$Year2)

#create new data for size classes 15,25, 45 and 150
new.data_15<-expand.grid(Size_Class=15,Size_Class2="10-15cm",Year2=seq(-1.497,1.414,0.01),Collapse=c(0,1))
new.data_25<-expand.grid(Size_Class=25,Size_Class2="15-25cm",Year2=seq(-1.497,1.414,0.01),Collapse=c(0,1))
new.data_45<-expand.grid(Size_Class=45,Size_Class2="25-45cm",Year2=seq(-1.497,1.414,0.01),Collapse=c(0,1))
new.data_150<-expand.grid(Size_Class=150,Size_Class2=">45cm",Year2=seq(-1.497,1.414,0.01),Collapse=c(0,1))

new.data_15$pred<-predict(Mod.avg_SD15,newdata =new.data_15)
new.data_15$UCI<-new.data_15$pred+(predict(Mod.avg_SD15,newdata =new.data_15,se.fit=T)$se.fit*1.96)
new.data_15$LCI<-new.data_15$pred-(predict(Mod.avg_SD15,newdata =new.data_15,se.fit=T)$se.fit*1.96)
new.data_15$Size_Class2<-"10-15cm"

new.data_25$pred<-predict(Mod.avg_SD25,newdata =new.data_25)
new.data_25$UCI<-new.data_25$pred+(predict(Mod.avg_SD25,newdata =new.data_25,se.fit=T)$se.fit*1.96)
new.data_25$LCI<-new.data_25$pred-(predict(Mod.avg_SD25,newdata =new.data_25,se.fit=T)$se.fit*1.96)
new.data_25$Size_Class2<-"15-25cm"


new.data_45$pred<-predict(Mod.avg_SD45,newdata =new.data_45)
new.data_45$UCI<-new.data_45$pred+(predict(Mod.avg_SD45,newdata =new.data_45,se.fit=T)$se.fit*1.96)
new.data_45$LCI<-new.data_45$pred-(predict(Mod.avg_SD45,newdata =new.data_45,se.fit=T)$se.fit*1.96)
new.data_45$Size_Class2<-"25-45cm"




plot((new.data_45$Year2*sd(Size_grid4$Year))+mean(Size_grid4$Year),exp(new.data_45$pred))
points((new.data_45$Year2*sd(Size_grid4$Year))+mean(Size_grid4$Year),col="red")
points((new.data_45$Year2*sd(Size_grid4$Year))+mean(Size_grid4$Year),exp(new.data_45$LCI),col="red")
head(new.data_15)

#now predictions for >45cm
new.data_150<-expand.grid(Size_Class=150,Size_Class2=">45cm",Year2=seq(-1.497,1.414,0.01),Collapse=c(0,1))
new.data_150$SD<-0

mm <- model.matrix(terms(SD_150_M1),new.data_150)
new.data_150$SD<- predict(SD_150_M1,new.data_150,re.form=NA)
## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(SD_150_M1),mm))
tvar1 <- pvar1+VarCorr(SD_150_M1)$Block[1]  ## must be adapted for more complex models
  new.data_150 <- data.frame(
    new.data_150
    , LCI = new.data_150$SD-2*sqrt(pvar1)
    , UCI = new.data_150$SD+2*sqrt(pvar1)
  )

plot(new.data_150$Year2,exp(new.data_150$SD))
points(new.data_150$Year2,exp(new.data_150$UCI),col="red")
points(new.data_150$Year2,exp(new.data_150$LCI),col="red")
new.data_150$pred<-new.data_150$SD
new.data_150<-(new.data_150[-c(5)])
new.data_150$Size_Class2<-">45cm"

SD_Preds<-rbind(new.data_15,new.data_25,new.data_45,new.data_150)
SD_Preds$Year<-(SD_Preds$Year*sd(Size_grid4$Year))+mean(Size_grid4$Year)
SD_Preds$Collapse2<-ifelse(SD_Preds$Collapse==1,"Collapsed","Stable")
SD_Preds$Size_Class2<-factor(SD_Preds$Size_Class2, c(">45cm", "25-45cm", "15-25cm", "10-15cm"))

#reclass size group and collapse group for dataframe
Size_grid4
Size_grid4$Collapse2<-ifelse(Size_grid4$Collapse==1,"Collapsed","Stable")
Size_grid4$Size_Class2<-ifelse(Size_grid4$Size_Class==15,"10-15cm",Size_grid4$Size_Class)
Size_grid4$Size_Class2<-ifelse(Size_grid4$Size_Class==25,"15-25cm",Size_grid4$Size_Class2)
Size_grid4$Size_Class2<-ifelse(Size_grid4$Size_Class==45,"25-45cm",Size_grid4$Size_Class2)
Size_grid4$Size_Class2<-ifelse(Size_grid4$Size_Class==150,">45cm",Size_grid4$Size_Class2)
head(Size_grid4)

Size_grid_size<-ddply(Size_grid4,.(Size_Class2,Collapse2,Year,SD),summarise,number=length(Year))

#create a figure to show changes in BA per size class for collapsed and non-collapsed plots
theme_set(theme_bw(base_size=12))
SD_size1<-ggplot(SD_Preds,aes(x=Year,y=exp(pred),ymax=exp(UCI),ymin=exp(LCI),fill=as.factor(Size_Class2),colour=as.factor(Size_Class2)))+geom_ribbon(alpha=0.5)+geom_line(size=1,colour="black")
SD_size2<-SD_size1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
SD_size2+ylab("Sub-plot stem density")+geom_point(data=Size_grid4,aes(y=SD,ymax=NULL,ymin=NULL,colour=as.factor(Size_Class2)),alpha=0.2,shape=1)+
  geom_line(data=Size_grid4,aes(group=Block,y=SD,ymax=NULL,ymin=NULL,colour=as.factor(Size_Class2)),alpha=0.2)+scale_colour_brewer(palette="Set1","DBH size class")+
  scale_fill_brewer(palette="Set1","DBH size class")+facet_grid(Size_Class2~Collapse2,scales="free")+ theme(legend.position="none")
ggsave("Figures/SD_Size_class.png",width = 8,height=8,units = "in",dpi=400)

SD_size2+ylab("Subplot stem density")+geom_point(data=Size_grid_size,aes(y=SD,ymax=NULL,ymin=NULL,colour=as.factor(Size_Class2),size=number),alpha=0.5,shape=16)+scale_colour_brewer(palette="Set1","DBH size class")+
  scale_fill_brewer(palette="Set1","DBH size class")+facet_grid(Size_Class2~Collapse2,scales="free")+ theme(legend.position="none")
ggsave("Figures/SD_Size_class2.png",width = 8,height=8,units = "in",dpi=400)

#make a composite figure of this

Size_grid150<-subset(Size_grid4,Size_Class==150)
Size_grid_size_150<-ddply(Size_grid150,.(Size_Class2,Collapse2,Year,SD),summarise,number=length(Year))
SD_Preds150<-subset(SD_Preds,Size_Class==150)

#create plot for >45cm
SD45_1<-ggplot(Size_grid_size_150,aes(x=Year,y=SD,colour=as.factor(Collapse2),size=number))+geom_point(shape=1,position = position_jitter(w=0,h = 0.1))
SD45_2<-SD45_1+geom_ribbon(data=SD_Preds150,aes(ymax=exp(UCI),ymin=exp(LCI),y=NULL,size=NULL,colour=NULL,fill=as.factor(Collapse2)),alpha=0.2)+geom_line(data=SD_Preds150,aes(y=exp(pred),group=Collapse2,ymax=NULL,ymin=NULL,colour=NULL,fill=NULL),size=1,colour="black")
SD45_3<-SD45_2+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
SD45_4<-SD45_3+ylab("Sub-plot stem density (DBH >45cm)")+scale_colour_brewer(palette="Set1","Collapse status")+
  scale_fill_brewer(palette="Set1","Collapse status")+ theme(legend.position="none")+ annotate("text", x = 1963, y = max(Size_grid_size_150$SD)+1, label = "(a)",size=5)+scale_x_continuous(breaks=c(1965,1975,1985,1995,2005,2015))

#now create plot for 25-45cm 

Size_grid45<-subset(Size_grid4,Size_Class==45)
Size_grid_size_45<-ddply(Size_grid45,.(Size_Class2,Collapse2,SD),summarise,number=length(Year))
SD_Preds45<-subset(SD_Preds,Size_Class==45)
SD_Preds45_2<-ddply(SD_Preds45,.(Collapse2),summarise,pred=exp(median(pred)),UCI=exp(median(UCI)),LCI=exp(median(LCI)))

SD25_1<-ggplot(SD_Preds45_2,aes(x=Collapse2,y=pred,ymax=UCI,ymin=LCI,fill=Collapse2))+geom_bar(stat="identity")+geom_errorbar(width=0.25)+scale_fill_brewer(palette="Set1","Collapse status")
SD25_2<-SD25_1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
SD25_3<-SD25_2+ylab("Sub-plot stem density (DBH 25-45cm)")+xlab("Collapse status")+ theme(legend.position="none")+ annotate("text", x = 0.5, y = max(SD_Preds45_2$pred)+0.5, label = "(b)",size=5)

#now create plot for 10-15cm
Size_grid15<-subset(Size_grid4,Size_Class==15)
Size_grid_size_15<-ddply(Size_grid15,.(Size_Class2,Collapse2,SD,Year),summarise,number=length(Year))
SD_Preds15<-subset(SD_Preds,Size_Class==15)
SD15_1<-ggplot(Size_grid_size_15,aes(x=Year,y=SD,colour=as.factor(Collapse2),size=number))+geom_point(shape=1,position = position_jitter(w=0,h = 0.1))
SD15_2<-SD15_1+geom_ribbon(data=SD_Preds15,aes(ymax=exp(UCI),ymin=exp(LCI),y=NULL,size=NULL,colour=NULL,fill=as.factor(Collapse2)),alpha=0.2)+geom_line(data=SD_Preds15,aes(y=exp(pred),group=Collapse2,ymax=NULL,ymin=NULL,colour=NULL,fill=NULL),size=1,colour="black")
SD15_3<-SD15_2+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
SD15_4<-SD15_3+ylab("Sub-plot stem density (DBH 10-15cm)")+scale_colour_brewer(palette="Set1","Collapse status")+
  scale_fill_brewer(palette="Set1","Collapse status")+ theme(legend.position="none")+ annotate("text", x = 1963, y = max(Size_grid_size_15$SD)+1, label = "(c)",size=5)+scale_x_continuous(breaks=c(1965,1975,1985,1995,2005,2015))

png("Figures/SD_Size_class_new.png",width = 12,height = 4,units = "in",res = 600)
grid.arrange(SD45_4,SD25_3,SD15_4,ncol=3)
dev.off()
