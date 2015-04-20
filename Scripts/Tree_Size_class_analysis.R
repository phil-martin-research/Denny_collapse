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
library(data.table)

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


Size_grid2<-subset(Size_grid,Year!=1999&Block<51)
Size_grid3<-subset(Size_grid,Year==1964&Block>51|Year==1999&Block>51|Year==2014&Block>51)
Size_grid4<-rbind(Size_grid2,Size_grid3)

  
#Change year to categorical
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
Mod_sel_SD15$R_2<-c(r.squaredGLMM(SD_15_M3)[1],r.squaredGLMM(SD_15_M1)[1],r.squaredGLMM(SD_15_M2)[1],
                    r.squaredGLMM(SD_15_M0)[1],r.squaredGLMM(SD_15_M4)[1])
Mod.avg_SD15<-model.avg(Mod_sel_SD15,subset= delta <7)

write.csv(Mod_sel_SD15,"Figures/Mod_sel_SD_15.csv")
summary(Mod.avg_SD15)

#do the same with 25
Size_grid25<-subset(Size_grid4,Size_Class==25)

SD_25_M0<-glmer(SD~1+(1|Block),data=Size_grid25,family=poisson)
SD_25_M1<-glmer(SD~Year2*as.factor(Collapse)+(1|Block),data=Size_grid25,family=poisson)
SD_25_M2<-glmer(SD~Year2+as.factor(Collapse)+(1|Block),data=Size_grid25,family=poisson)
SD_25_M3<-glmer(SD~Year2+(1|Block),data=Size_grid25,family=poisson)
SD_25_M4<-glmer(SD~as.factor(Collapse)+(1|Block),data=Size_grid25,family=poisson)

SD_25_models<-list(SD_25_M0,SD_25_M1,SD_25_M2,SD_25_M3,SD_25_M4)
Mod_sel_SD25<-model.sel(SD_25_models)
Mod_sel_SD25$R_2<-c(r.squaredGLMM(SD_25_M1)[1],r.squaredGLMM(SD_25_M2)[1],r.squaredGLMM(SD_25_M4)[1],
                    r.squaredGLMM(SD_25_M3)[1],r.squaredGLMM(SD_25_M0)[1])
Mod.avg_SD25<-model.avg(Mod_sel_SD25,subset= delta <7)
summary(Mod.avg_SD25)             

write.csv(Mod_sel_SD25,"Figures/Mod_sel_SD_25.csv")

#do the same with 45
Size_grid45<-subset(Size_grid4,Size_Class==45)

SD_45_M0<-glmer(SD~1+(1|Block),data=Size_grid45,family=poisson)
SD_45_M1<-glmer(SD~Year2*as.factor(Collapse)+(1|Block),data=Size_grid45,family=poisson)
SD_45_M2<-glmer(SD~Year2+as.factor(Collapse)+(1|Block),data=Size_grid45,family=poisson)
SD_45_M3<-glmer(SD~Year2+(1|Block),data=Size_grid45,family=poisson)
SD_45_M4<-glmer(SD~as.factor(Collapse)+(1|Block),data=Size_grid45,family=poisson)

SD_45_models<-list(SD_45_M0,SD_45_M1,SD_45_M2,SD_45_M3,SD_45_M4)
Mod_sel_SD45<-model.sel(SD_45_models)
Mod_sel_SD45$R_2<-c(r.squaredGLMM(SD_45_M4)[1],r.squaredGLMM(SD_45_M2)[1],r.squaredGLMM(SD_45_M1)[1],
                    r.squaredGLMM(SD_45_M3)[1],r.squaredGLMM(SD_45_M0)[1])

Models_SD45<-get.models(Mod_sel_SD45,subset= delta <7)  
Mod.avg_SD45<-model.avg(Models_SD45)
(summary(Mod.avg_SD45))


write.csv(Mod_sel_SD45,"Figures/Mod_sel_SD_45.csv")
write.csv(Mod.coef_SD45,"Figures/Mod_coef_SD_45.csv")

#do the same with large trees
Size_grid150<-subset(Size_grid4,Size_Class==150)

SD_150_M0<-glmer(SD~1+(1|Block),data=Size_grid150,family=poisson)
SD_150_M1<-glmer(SD~Year2*as.factor(Collapse)+(1|Block),data=Size_grid150,family=poisson)
SD_150_M2<-glmer(SD~Year2+as.factor(Collapse)+(1|Block),data=Size_grid150,family=poisson)
SD_150_M3<-glmer(SD~Year2+(1|Block),data=Size_grid150,family=poisson)
SD_150_M4<-glmer(SD~as.factor(Collapse)+(1|Block),data=Size_grid150,family=poisson)

SD_150_models<-list(SD_150_M0,SD_150_M1,SD_150_M2,SD_150_M3,SD_150_M4)
Mod_sel_SD150<-model.sel(SD_150_models)
Mod_sel_SD150$R_2<-c(r.squaredGLMM(SD_150_M1)[1],r.squaredGLMM(SD_150_M2)[1],r.squaredGLMM(SD_150_M4)[1],
                    r.squaredGLMM(SD_150_M3)[1],r.squaredGLMM(SD_150_M0)[1])

Mod.coef_SD150<-coef(summary(SD_150_M1))
                

write.csv(Mod_sel_SD150,"Figures/Mod_sel_SD_150.csv")
write.csv(Mod.coef_SD150,"Figures/Mod_coef_SD_150.csv")

object 


#create loop to produce predictions for these models
Model_preds<-NULL
Size_unique<-data.frame(Size=c(15,25,45,150),Size2=c("10-15cm","15-25cm","25-45cm",">45cm"))
Model_list<-list(SD_15_M1,SD_25_M1,SD_45_M1,SD_150_M1)

for (i in 1:3){
  new.data<-expand.grid(Size_Class=Size_unique$Size[i],Size_Class2=Size_unique$Size2[i],Year2=as.factor(c(1964,2014)),Collapse=c(0,1))
  new.data$SD<-0
  new.data$UCI<-0
  new.data$LCI<-0
  new.data<-new.data[with(new.data, order(Collapse,Year2)), ]
  M1<-eval(parse(text=paste("SD_",Size_unique$Size[i],"_M1",sep=""))) 
  mm <- model.matrix(terms(M1),new.data)
  M_avg<-eval(parse(text=paste("Mod.avg_SD",Size_unique$Size[i],sep="")))
  summary(M_avg)
  Coefs<-coefTable(M_avg,full = T)
  Coefs2<-data.frame(Par=as.character(rownames(Coefs)),Coefs)
  Coefs2$Par<-as.character(Coefs2$Par)
  Coefs2$Par<-factor(Coefs2$Par,c("(Intercept)","Year22014","as.factor(Collapse)1","as.factor(Collapse)1:Year22014"))
  Coefs2<-Coefs2[with(Coefs2, order(Par)), ]
  new.data$SD[1]<-Coefs2[1,2]
  new.data$UCI[1]<-new.data$SD[1]+(1.96*Coefs2[1,3])
  new.data$LCI[1]<-new.data$SD[1]-(1.96*Coefs2[1,3])
  head(new.data)
for (j in 2:3){
  new.data$SD[j]<-Coefs2[1,2]+Coefs2[j,2]
  new.data$UCI[j]<-new.data$SD[j]+(1.96*Coefs2[j,3])
  new.data$LCI[j]<-new.data$SD[j]-(1.96*Coefs2[j,3])
}
new.data$SD[4]<-Coefs[1,2]+Coefs[2,2]+Coefs2[4,2]
new.data$UCI[4]<-new.data$SD[4]+(1.96*Coefs2[4,3])
new.data$LCI[4]<-new.data$SD[4]-(1.96*Coefs2[4,3])
 Model_preds<-rbind(Model_preds,new.data)
}
  


#now predictions for >45cm
new.data_150<-expand.grid(Size_Class=150,Size_Class2=">45cm",Year2=as.factor(c(1964,2014)),Collapse=c(0,1))
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

SD_Preds<-rbind(Model_preds,new.data_150)
SD_Preds$Collapse2<-ifelse(SD_Preds$Collapse==1,"Collapsed","Stable")
SD_Preds$Size_Class2<-factor(SD_Preds$Size_Class2, c(">45cm", "25-45cm", "15-25cm", "10-15cm"))

#create plots of sd changes
dodge <- position_dodge(width=0.9)
labels<-data.frame(lab=c("(a)","(b)","(c)","(d)"),x=0.5,y=5,Size_Class2=as.factor(c(">45cm","25-45cm","15-25cm","10-15cm")))
SD_plot<-ggplot(SD_Preds,aes(x=Year2,y=(exp(SD))*25,ymin=(exp(LCI))*25,ymax=(exp(UCI))*25,fill=Collapse2))+
  geom_bar(stat="identity",position =dodge)+geom_errorbar(position =dodge,width=0.25)
SD_plot2<-SD_plot+ylab("Plot stem density")+xlab("Year")+ theme(legend.position = "none") 
SD_plot3<-SD_plot2+scale_fill_brewer(palette="Set1")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))                                                                                                                                          
SD_plot3+geom_text(data=labels,aes(x=x,y=y*25,label=lab,ymax=NULL,ymin=NULL,fill=NULL,group=NULL),colour="black")+facet_wrap(~Size_Class2,ncol=2)
ggsave("Figures/Tree_size_change.png",width = 8,height=6,units = "in",dpi=300)

exp(1.019831411+0.199408866)
exp(0.154819879)
exp(1.019831411+0.319942935+-0.926078738)
exp(0.223075846)
