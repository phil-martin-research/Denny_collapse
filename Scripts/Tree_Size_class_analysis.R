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
Trees_M$BA<-(Trees_M$DBH^2*(pi/4))
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

Coll_Change2<-subset(Coll_Change,Year2!="1964")

#plot change in SD
Coll_Change2$Collapse3<-ifelse(Coll_Change2$Collapse2==1,"Collapsed","Stable")
theme_set(theme_bw(base_size=12))
SD_plot1<-ggplot(Coll_Change2,aes(x=Size_class,y=(exp(N_Change)-1)*100))+geom_bar(stat="identity")+facet_grid(Collapse3~Year2)+geom_hline(x=0,lty=2)
SD_plot2<-SD_plot1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
SD_plot2+xlab("DBH size class (cm)")+ylab("Percentage change in stem density since 1964")+ theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("Figures/SD_Size_Change.png",width = 8,height=6,units = "in",dpi=300)


Tree_class2<-subset(Tree_class,DBH>10)

sum(Tree_class2$BA)

Tree_class$ID<-rownames(Tree_class2)

DBH_BA<- ddply(Tree_class, .(ID), summarize,
                      DBH = unique(DBH),
                      ecdf = ecdf(BA2)(unique(ID))*length(ID))
head(DBH_BA)

?ecdf

BA_Class<-ddply(Tree_class,.(Collapse2,Year2,Size,No,Block),summarise,N=length(Block),BA=sum(BA))
BA_Class$BA2<-BA_Class$BA/(BA_Class$No*400)
BA_Class$N2<-BA_Class$N*((BA_Class$No*400)/10000)
BA_Class$Year<-BA_Class$Year2
head(BA_Class)


#create a plot to show the percentage basal area change for different size classes over time

#first create a loop to give a dataframe with numbers for each size class for each block for each year
BA_Change<-NULL
Block_size<-unique(BA_Class[c("Block", "Size","Year2")])

head(Block_size)

Size_grid<-expand.grid(Block=unique(Block_size$Block),Size=unique(Block_size$Size),Year=unique(Block_size$Year2))
Size_grid$BA<-NA
for (i in 1:nrow(Size_grid)){
  Block_sub<-subset(BA_Class,Block==Size_grid$Block[i])
  Block_sub<-subset(Block_sub,Size==Size_grid$Size[i])
  Block_sub<-subset(Block_sub,Year2==Size_grid$Year[i])
  if (nrow(Block_sub)==0){
    Size_grid$BA[i]<-0
    Size_grid$N[i]<-0
  } else {
    Size_grid$BA[i]<-Block_sub$BA2
    Size_grid$N[i]<-Block_sub$N
  }
}
    
head(BA_class)

keeps<-c("Collapse2","Year","Block","Size")

BA_class2<-BA_Class[keeps]
head(Size_grid)
head(BA_class2)

Size_grid<-merge(BA_class2,Size_grid,by=c("Block","Year","Size"))

#now create a loop comparing each block and size class to itself
Block_change<-NULL
Block_size<-unique(Size_grid[c("Block", "Size")])
for (i in 1:nrow(Block_size)){
  Block_sub<-subset(Size_grid,Block==Block_size$Block[i])
  Block_sub<-subset(Block_sub,Size==Block_size$Size[i])
  Block_sub$N_Raw<-Block_sub$N-Block_sub$N[1]
  Block_sub$BA_Raw<-Block_sub$BA-Block_sub$BA[1]
  Block_sub$N_Perc<-(Block_sub$N-Block_sub$N[1])/Block_sub$N[1]
  Block_sub$BA_Perc<-(Block_sub$BA-Block_sub$BA[1])/Block_sub$BA[1]
  Block_change<-rbind(Block_change,Block_sub)
}
summary(Block_change)

Block_change_Sum<-ddply(Block_change,.(Year,Collapse2,Size),summarize,BA=mean(BA_Perc),N=mean(N_Perc))
  
Block_change_Sum2<-subset(Block_change_Sum,Year>1964)

ggplot(Block_change_Sum2,aes(x=Size,y=N,colour=Year))+geom_point()+facet_wrap(~Collapse2)
ggplot(Block_change,aes(x=as.factor(Size),y=BA_Perc,colour=Collapse2))+geom_boxplot()+facet_grid(Collapse2~Year)



