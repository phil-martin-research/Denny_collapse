#script to produce edits for Adrian
rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(lme4)
library(MuMIn)

citation(package = "ncf", lib.loc = NULL)

#import data
Plots<-read.csv("Data/Denny_plots.csv")
Plot_BA<-Plots[c("Block","Year","BAM","BAPERCM")]
Plot_BA$Transect<-ifelse(Plot_BA$Block>51,"Unenclosed","Enclosed")

###################################################
#first calculate change in each period relative to#
#previous survey i.e. 1988 relative to 1984, 1996
#relative to 1988 etc

BA_changes<-NULL
Blocks<-unique(Plot_BA$Block)
for (i in 1:length(Blocks)){
  Plot_sub<-subset(Plot_BA,Block==Blocks[i])
  Plot_sub$Change<-NA
  Plot_sub$Change_dir<-NA
  for (j in 2:nrow(Plot_sub)){
    Plot_sub$Change[j]<-(Plot_sub$BAM[j]-Plot_sub$BAM[j-1])/Plot_sub$BAM[j-1]
    if (Plot_sub$BAM[j]<Plot_sub$BAM[j-1]){
      Plot_sub$Change_dir[j]<-"Decrease"
    }else{
     Plot_sub$Change_dir[j]<-"Increase"
    }
  }
  BA_changes<-rbind(Plot_sub,BA_changes)
}

BA_changes$Year2<-ifelse(BA_changes$Year==1999|BA_changes$Year==1996,"1996/9",BA_changes$Year)

BA_changes$Collapse<-ifelse(BA_changes$BAPERCM<=-0.25,1,0)
ddply(BA_changes,.(Year2,Change_dir),summarise,Number=length(BAM),BAM=mean(BAM),BAPERC=mean(BAPERCM))
ddply(BA_changes,.(Year2, Change_dir,Transect),summarise,Number=length(BAM),BA_change=mean(Change),sem = sd(Change)/sqrt(length(Change)),BAPERC=mean(BAPERCM),BA_sem = sd(BAPERCM)/sqrt(length(BAPERCM)))


############################################################
#now work out the percentage of decline represented by loss#
#of beech and oak BA 
#how much BA was lost overall
#how much BA was lost by beech
#how much was lost by oak
#calculate percentage for each time step
head(Plots)
Plot_BA_comp<-Plots[c("Block","Year","BAM","F_BA","Q_BA")]
Plot_BA_comp$Year2<-ifelse(Plot_BA_comp$Year==1999|Plot_BA_comp$Year==1996,"1996/9",Plot_BA_comp$Year)


BA_comp_total<-ddply(Plot_BA_comp,.(Year2),summarise,BA_total=sum(BAM),BA_F=sum(F_BA),BA_Q=sum(Q_BA),No_Plots=length(Block))

BA_comp_total$Change<-c(NA,BA_comp_total$BA_total[2:5]-BA_comp_total$BA_total[1])
BA_comp_total$Change_F<-c(NA,BA_comp_total$BA_F[2:5]-BA_comp_total$BA_F[1])
BA_comp_total$Change_Q<-c(NA,BA_comp_total$BA_Q[2:5]-BA_comp_total$BA_Q[1])
BA_comp_total$Cont_F<-BA_comp_total$Change_F/BA_comp_total$Change
BA_comp_total$Cont_Q<-BA_comp_total$Change_Q/BA_comp_total$Change



#########################################################################
#now look at percentage of decline for which different size classes were#
#responsible for different species over the entire period################
#########################################################################

#load in data
Trees<-read.csv("Data/Denny_trees_cleaned.csv")

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
head(Trees_M)

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

Trees_M_Size2<-subset(Trees_M_Size,Year=="1964"|Year=="2014")
BA_comp_total<-ddply(Trees_M_Size2,.(Year,Species,Size_Class),summarise,BA_total=sum(BA))
head(BA_comp_total)
BA_comp_total2<-expand.grid(Species=unique(BA_comp_total$Species),Size_Class=unique(BA_comp_total$Size_Class),Year=unique(BA_comp_total$Year))
BA_comp_total3<-merge(BA_comp_total2,BA_comp_total,by=c("Species","Year","Size_Class"),all=T)
BA_comp_total3$BA_total<-ifelse(is.na(BA_comp_total3$BA_total),0,BA_comp_total3$BA_total)

BA_comp_total2<-ddply(Trees_M_Size2,.(Year),summarise,BA_total=sum(BA))


#loop to assess the proportional contribution of different tree sizes
#of different species to the changes in BA
BA_change_comp<-NULL
Species_un<-unique(BA_comp_total3[c("Species","Size_Class")])
colnames(Species_un)<-c("Sp","SC")
Species_un
for (i in 1:nrow(Species_un)){
  BA_comp_sub<-subset(BA_comp_total3,Species==Species_un$Sp[i])
  BA_comp_sub<-subset(BA_comp_sub,Size_Class==Species_un$SC[i])
  BA_comp_sub$Change<-NA
  BA_comp_sub$Change[2]<-BA_comp_sub$BA_total[2]-BA_comp_sub$BA_total[1]
  BA_comp_sub$Change_cont<-NA
  BA_comp_sub$Change_cont[2]<-(BA_comp_sub$Change[2]/(2184.461-2699.614))*100
  BA_change_comp<-rbind(BA_comp_sub,BA_change_comp)
}

BA_comp_total$Change<-c(NA,BA_comp_total$BA_total[2:5]-BA_comp_total$BA_total[1])
BA_comp_total$Change_F<-c(NA,BA_comp_total$BA_F[2:5]-BA_comp_total$BA_F[1])
BA_comp_total$Change_Q<-c(NA,BA_comp_total$BA_Q[2:5]-BA_comp_total$BA_Q[1])
BA_comp_total$Cont_F<-BA_comp_total$Change_F/BA_comp_total$Change
BA_comp_total$Cont_Q<-BA_comp_total$Change_Q/BA_comp_total$Change





###################################################################
#work out stem density changes for collapsed and uncollapsed plots#
#over the entire survey period - time series analysis##############
head(Plots)

Plot_SD<-Plots[c("Block","Year","BAM","BAPERCM","SDM")]

#classify plots by collapse status - collapsed (1) or not (0)
Plot_SD$Collapse<-NA
for (i in 1:nrow(Plots)){
  Plot_SD$Collapse[i]<-ifelse(Plot_SD$BAPERCM[i]<=-0.25,1,0)
}
#classify plots to identify those that have *at some point* been classed as collapsed
Plot_SD$Collapse2<-NA
PlotsSD_2<-NULL
Block_unique<-unique(Plot_SD$Block)
for (i in 1:length(Block_unique)){
  Block_sub<-subset(Plot_SD,Block==Block_unique[i])
  Block_sub$Collapse2<-ifelse(sum(Block_sub$Collapse)>0,1,0)
  PlotsSD_2<-rbind(Block_sub,PlotsSD_2)
}

#recscale year
PlotsSD_2$Year2<-PlotsSD_2$Year-1964

#first look at graphs of changes
ggplot(PlotsSD_2,aes(x=Year,y=SDM,group=Block))+geom_point()+geom_line()+facet_wrap(~Block)


#now try to model this
M0.1<-glmer(SDM~1+(1|Block),data=PlotsSD_2,family="poisson")
M0.2<-glmer(SDM~1+(Year2|Block),data=PlotsSD_2,family="poisson")
AICc(M0.1,M0.2)

#go with the second random effect specification
M1<-glmer(SDM~1+Year2+(Year2|Block),data=PlotsSD_2,family="poisson")
M2<-glmer(SDM~1+as.factor(Collapse2)+(Year2|Block),data=PlotsSD_2,family="poisson")
M3<-glmer(SDM~1+as.factor(Collapse2)+Year2+(Year2|Block),data=PlotsSD_2,family="poisson")
M4<-glmer(SDM~1+as.factor(Collapse2)*Year2+(Year2|Block),data=PlotsSD_2,family="poisson")
summary(M4)
AICc(M1,M2,M3,M4)
r.squaredGLMM(M4)

#create model selection output for these models
Mod_list<-list(M0.2,M1,M2,M3,M4)
Model_selection<-mod.sel(Mod_list)
Model_selection$R2<-c(r.squaredGLMM(M4)[1],r.squaredGLMM(M1)[1],r.squaredGLMM(M3)[1],r.squaredGLMM(M0.2)[1],r.squaredGLMM(M2)[1])
write.csv(Model_selection,"Figures/Mod_sel_SD_TS.csv")

coefs <- data.frame(coef(summary(M4)))

write.csv(coefs,"Figures/SD_TS_coefs.csv")


#now create plots of this
newdat<-expand.grid(Year=seq(1964,2014,1),Collapse2=as.factor(c(1,0)))
newdat$Year2<-newdat$Year-1964
newdat$SDM<-0

mm <- model.matrix(terms(M4),newdat)
newdat$SDM <- predict(M4,newdat,re.form=NA)
## or newdat$distance <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(M4),mm))
tvar1 <- pvar1+VarCorr(M4)$Block[1]  ## must be adapted for more complex models
tvar1 <- 
  newdat <- data.frame(
    newdat
    , plo = newdat$SDM-2*sqrt(pvar1)
    , phi = newdat$SDM+2*sqrt(pvar1)
    , tlo = newdat$SDM-2*sqrt(tvar1)
    , thi = newdat$SDM+2*sqrt(tvar1)
  )

newdat$Collapse2<-as.numeric(as.character(newdat$Collapse2))
newdat$Collapse3<-ifelse(newdat$Collapse2==1,"Collapsed","Stable/Increasing")

PlotsSD_2$Collapse3<-ifelse(PlotsSD_2$Collapse2==1,"Collapsed","Stable/Increasing")


#now plot this
SD2<-ddply(PlotsSD_2,.(SDM,Collapse2,Year),summarise,number=length(SDM))
head(SD2)
head(PlotsSD_2)


#plot in ggplot
theme_set(theme_bw(base_size=12))
SD_TS1<-ggplot(PlotsSD_2,aes(x=Year,y=SDM*25))+geom_point(shape=1,alpha=0.3)+geom_line(aes(group=Block,size=NULL),alpha=0.3)+facet_wrap(~Collapse3)
SD_TS2<-SD_TS1+geom_ribbon(data=newdat,aes(ymax=(exp(phi))*25,ymin=(exp(plo))*25),alpha=0.4)+geom_line(data=newdat,size=3,aes(y=(exp(SDM))*25))
SD_TS3<-SD_TS2+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+ theme(legend.position="none")                                                                                                                                            
SD_TS3+ylab(expression(paste("Number of stems >10cm dbh ",ha^bold("-1"))))
ggsave("Figures/Collapse_SD_TS.png",width = 8,height=6,units = "in",dpi=300)
