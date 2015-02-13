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
