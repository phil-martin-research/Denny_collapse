#script to calculate changes in community similarity in Denny wood over time#

rm(list=ls(all=TRUE))

#open packages neeeded for exploration
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(vegan)
library(reshape2)


#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
Trees<-read.csv("Denny_trees_cleaned.csv")
head(Trees)
#subset trees to give only those inside plots
Tress<-subset(Trees,In_out=="In")



#produce counts of species per block per year
Sp_counts<-count(Trees,vars = c("Species","Block","Year"))
head(Sp_counts)
#and put in form that is usable by vegan
Sp_counts2<-dcast(Sp_counts,Block + Year ~Species)

#now set up loop to carry out similarity analysis comparing each block to itself in 1959
Blocks<-unique(Sp_counts2$Block)
Sor_similarity<-NULL
for (i in 1:length(Blocks)){
  Block_subset<-subset(Sp_counts2,Block==Blocks[i])
  Block_subset[is.na(Block_subset)]<-0
  Block_subset2<-Block_subset[-c(1:2)]
  Block_subset2[is.na(Block_subset2)]<-0
  Block_subset$Sorensen<-c(1,1-vegdist(Block_subset2)[1:nrow(Block_subset2)-1])
  Sor_similarity<-rbind(Sor_similarity,Block_subset)
}


#plot of similarity change over time
head(Sor_similarity)
ggplot(Sor_similarity,aes(x=Year,y=Sorensen,group=Block))+geom_point()+geom_line()+facet_wrap(~Block)

head(Sor_similarity)

#model of species turnover measured using sorensen similarity
#analyse this in a mixed model

#remove fist value where sorensen is = 1
Sor_similarity2<-subset(Sor_similarity,Sorensen!=1)
#and add new varibale to reduce correlation of fixed effects
Sor_similarity2$Year2<-Sor_similarity2$Year-mean(Sor_similarity2$Year)

#now run models
options(na.action = "na.fail")
M0<-lme(qlogis(Sorensen)~1,random=~1|Block,data=Sor_similarity2,method="REML")
M0.1<-lme(qlogis(Sorensen)~1,random=~Year|Block,data=Sor_similarity2,method="REML")
AICc(M0,M0.1)
#AICc of M0 is lower so we go with random intercepts but not random slopes
ctrl <- lmeControl(opt='optim')
M1<-lme(qlogis(Sorensen)~Year2,random=~1|Block,data=Sor_similarity2,method="REML",control=ctrl)
plot(M1)
qqnorm(M1)

#model avaeraging
Models<-dredge(M1,rank = AICc,trace = T,REML=F)
Model_sel<-mod.sel(Models)
Model_sel$R_squared<-c(r.squaredGLMM(M1)[1],r.squaredGLMM(M0)[1])
Model_average<-model.avg(Models,fit=T)

#predicts change in sorenson

summary(Sor_similarity2$Year2)
Time<-data.frame(Year2=seq(1964,2014,1)-(mean(Sor_similarity2$Year)))
Sor_pred<-predict(Model_average,newdata=Time,se.fit=T,level=0)
Sor_pred$Time<-Time+(mean(Sor_similarity2$Year))
Sor_pred<-data.frame(Sor_pred)
Sor_pred$U_CI<-plogis(Sor_pred$fit+(1.96*Sor_pred$se.fit))
Sor_pred$L_CI<-plogis(Sor_pred$fit-(1.96*Sor_pred$se.fit))
Sor_pred$fit<-plogis(Sor_pred$fit)
head(Sor_pred)

plogis(1)
plogis(0.8)
plogis(0.6)

#plot changes in sorenson
theme_set(theme_bw(base_size=12))
Sor_plot1<-ggplot(Sor_similarity,aes(x=Year,y=Sorensen,group=Block))+geom_point(alpha=0.5,shape=1)+geom_line(alpha=0.2)
Sor_plot2<-Sor_plot1+geom_line(data=Sor_pred,size=3,aes(x=Year2,y=fit,group=NULL,colour=NULL),colour="blue")
Sor_plot3<-Sor_plot2+geom_line(data=Sor_pred,size=2,lty=2,aes(x=Year2,y=U_CI,group=NULL,colour=NULL),colour="blue")+geom_line(data=Sor_pred,size=2,lty=2,aes(x=Year2,y=L_CI,group=NULL,colour=NULL),colour="blue")
Sor_plot4<-Sor_plot3+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+scale_x_continuous(breaks=c(1960,1970,1980,1990,2000,2010,2020))+ theme(legend.position="none")
Sor_plot4+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+xlab("Year")+ylab("Sorensen similarity")
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("Sorensen_change.png",width = 8,height=6,units = "in",dpi=300)



#add ndms plot here
Sp_counts2[is.na(Sp_counts2)]<-0
#remove block and year
Sp_counts3<-Sp_counts2[,-c(1:2)]

#compute distance matricies
vare.dis<-vegdist(Sp_counts3)
vare.mds0<-metaMDS(vare.dis)

#create dataframe for nmds
NMDS<-data.frame(MDS1 = vare.mds0$points[,1], MDS2 = vare.mds0$points[,2],Year=Sp_counts2$Year,Plot=Sp_counts2$Block)
NMDS$transect<-ifelse(NMDS$Plot>51,"Unenclosed","Enclosed")

#make a plot of the nmds
theme_set(theme_bw(base_size=12))
NMDS_p1<-ggplot(NMDS,aes(x=MDS1,y=MDS2,colour=as.factor(Year),group=Plot))+geom_point(size=1,shape=1)+facet_grid(transect~Year)
NMDS_p2<-NMDS_p1+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))+scale_x_continuous(breaks=c(-0.4,0,0.4))+ theme(legend.position="none")
NMDS_p2+theme(axis.text=element_text(size=8),axis.title=element_text(size=14,face="bold"))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures")
ggsave("NMDS.png",width = 8,height = 4,units = "in",dpi = 300)


head(NMDS)

#add elipses around points
?ordiellipse

ord<-ordiellipse(vare.mds0, NMDS$Year, display = "sites", kind = "se", conf = 0.95, label = T)
df_ell <- data.frame()
for(y in unique(NMDS$Year)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$Year==y,],
            veganCovEllipse(ord[[y]]$cov,ord[[y]]$center,ord[[y]]$scale))),group=y))
}

#create figure with ellipses
ggplot(NMDS,aes(x=MDS1,y=MDS2,colour=as.factor(Year),group=Plot))+geom_point(size=4,alpha=0.8)+facet_wrap(~Year)
