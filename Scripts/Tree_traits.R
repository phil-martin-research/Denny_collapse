#script to look at changes in species traits

rm(list=ls(all=TRUE))

#open packages neeeded for analysis
library(ggplot2)
library(plyr)
library(reshape2)
library(ape)
library(geoR)
library(nlme)
library(MuMIn)
library(gridExtra)
library(MASS)
library(survival)
library(GGally)
library(lme4)
library(fields)
library(ROCR)

#import data
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Data")
DBH<-read.csv("Denny_plots.csv")

#import trait table
Traits<-read.csv("Tree_traits.csv")

#merge trait and tree data
head(DBH)
head(Traits)
Tree_traits<-merge(DBH,Traits,by.x="Species",by.y="Code")


#####################################
#analyses of trait change############
#####################################

#calculate basal area
head(Tree_traits)

#calculate total BA
Tree_traits<-merge(DBH,Traits,by.x="Species",by.y="Code")
head(Tree_traits)

for (i in 1:nrow(Tree_traits)){
  Tree_traits$BA[i]<-ifelse(Tree_traits$DBH[i]>10,(Tree_traits$DBH[i]^2*(pi/4))/400,0)
}
Tree_BA<-Tree_traits[,-c(1,5:17)]
head(Tree_BA)
BA_melt<-melt(Tree_BA, id = c("Block", "Year","Tree_ID"))
BA_block<-dcast(BA_melt, Year + Block ~ variable, function(x) sum(na.omit(x)/400))

#for individuals

head(Tree_traits)

#calculate the traits weighted by BA
Tree_traits$L_w<-Tree_traits$Light*Tree_traits$BA
Tree_traits$N_w<-Tree_traits$Nitrogen*Tree_traits$BA
Tree_traits$H_w<-Tree_traits$Height*Tree_traits$BA

Tree_traits<-(Tree_traits[,-c(1,5:17)])
summary(Tree_traits)
Tree_trait_melt<-melt(Tree_traits, id = c("Block", "Year","Tree_ID"))
head(Tree_trait_melt)
TT_block<-dcast(Tree_trait_melt, Year + Block ~ variable, sum)
TT_block$Year<-ifelse(TT_block$Year==1996,1999,TT_block$Year)
BA_loss$Year.x<-ifelse(BA_loss$Year.x==1996,1999,BA_loss$Year.x)

summary(TT_block)
summary(BA_loss)
head(TT_block)
head(BA_loss)

Traits_BA<-merge(x=TT_block,y=BA_loss,by.x=c("Block","Year"),by.y=c("Block","Year.x"))
head(Traits_BA)

theme_set(theme_bw(base_size=30))
ggplot(Traits_BA,aes(x=BA_loss*100,y=L_w/BA.x))+geom_point()+facet_wrap(~Year)+xlab("Percentage change in basal area since 1964")+ylab("Community weighted mean \nlight dependancy")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures/Forest_collapse")
ggsave("Light_BA_loss.png",width = 12,height = 8,units = "in",dpi = 300)



theme_set(theme_bw(base_size=30))
ggplot(Traits_BA,aes(x=BA_loss*100,y=N_w/BA.x))+geom_jitter(shape=1)+facet_wrap(~Year)+xlab("Percentage change in basal area since 1964")+ylab("Community weighted mean \nnitrogen requirement")+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(size=1.5,colour="black",fill=NA))
setwd("C:/Users/Phil/Dropbox/Work/Active projects/Forest collapse/Denny_collapse/Figures/Forest_collapse")
ggsave("N_BA_loss.png",width = 12,height = 8,units = "in",dpi = 300)





#add dbh to traits
ggplot(TT_block,aes(x=Year,y=L_w/BA,group=Block))+geom_point(shape=1,alpha=0.2)+geom_line(alpha=0.2)+geom_smooth(se=F,colour="blue",aes(group=NULL),method="lm",size=2)
