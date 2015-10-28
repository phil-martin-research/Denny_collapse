#script to produce figures and statistics 
#on change in BA and SD in denny

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(lme4)
library(MuMIn)
library(reshape)
library(grid)
library(gridExtra)

citation(package = "ncf", lib.loc = NULL)

#import data
Plots<-read.csv("Data/Denny_plots.csv")
Plot_BA<-Plots[c("Block","Year","BAM","BAPERCM")]
Plot_BA$Transect<-ifelse(Plot_BA$Block>51,"Unenclosed","Enclosed")

ddply(Plots,.(Year),summarise,M_BA=mean(BAM),M_SD=mean(SDM*25))

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

#now create histogram of changes in BA

plots<-read.csv("Data/Denny_plots.csv",header = T)
keeps<-c("Year","BAPERCM","BAFPERCM","BAQPERCM")
plots<-plots[keeps]
plots<-subset(plots,Year>1964)
plot_melt<-melt(plots,id.vars = "Year")
plot_melt$value2<-plot_melt$value*100

plot_melt$variable2<-ifelse(plot_melt$variable=="BAPERCM","Total basal area",plot_melt$variable)
plot_melt$variable2<-ifelse(plot_melt$variable=="BAFPERCM","Beech basal area",plot_melt$variable2)
plot_melt$variable2<-ifelse(plot_melt$variable=="BAQPERCM","Oak basal area",plot_melt$variable2)
plot_melt$variable2<-factor(plot_melt$variable2,levels=c("Total basal area","Beech basal area","Oak basal area"))

Plots_2014<-subset(plot_melt,Year==2014)

#produce plot for paper
Plots_2014_2<-subset(Plots_2014,variable=="BAPERCM")
median(Plots_2014_2$value)*100
theme_set(theme_bw(base_size=12))
Plot1<-ggplot(Plots_2014_2,aes(x=value*100))+geom_histogram(fill="white",colour="black")+coord_cartesian(xlim = c(-105,60))+geom_vline(x=median(Plots_2014_2$value)*100,size=2,lty=2)                                                                                                                                        
Plot2<-Plot1+ theme(panel.margin = unit(0.7, "lines"))+ylab("Number of plots")+xlab("Percentage change in Basal Area since 1964")
Plot3<-Plot2+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour="black",fill=NA))                                                                                                                                          
Plot4<-Plot3+annotate(geom = 'text',x = -98,y=10,label="(a)")

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
SD2<-ddply(PlotsSD_2,.(SDM,Collapse3,Year),summarise,number=length(SDM))
head(SD2)
head(PlotsSD_2)


#plot in ggplot

theme_set(theme_bw(base_size=12))
SD_TS1<-ggplot(SD2,aes(x=Year,y=SDM*25,size=number,colour=as.factor(Collapse3)))+geom_point(alpha=0.5)
SD_TS2<-SD_TS1+geom_ribbon(data=newdat,aes(ymax=(exp(phi))*25,ymin=(exp(plo))*25,shape=NULL,fill=as.factor(Collapse3),colour=NULL,size=NULL),alpha=0.4)+geom_line(data=newdat,size=1,aes(y=(exp(SDM))*25,shape=NULL))
SD_TS3<-SD_TS2+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour="black",fill=NA))+ scale_size(guide = 'none')+ guides(fill = 'none',colour='none')+scale_colour_brewer(palette="Set1")+scale_fill_brewer(palette="Set1")                                                                     
SD_TS4<-SD_TS3+ylab(expression(paste("Number of stems >10cm DBH ",ha^bold("-1"))))+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
SD_TS5<-SD_TS4+annotate(geom = 'text',x = 1964,y=1800,label="(b)")

png("Figures/BA_SD_Figure1.png",height=4,width=8,res=800,units="in")
grid.arrange(Plot4,SD_TS5,ncol=2)
dev.off()

grid.draw(cbind(ggplotGrob(Plot4), ggplotGrob(SD_TS5), size="last"))
