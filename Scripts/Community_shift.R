#script to produce hypothetical graphs for Denny wood study

#shifts in community composition

Comm_shift<-data.frame(Block=rep(seq(1,50,1),5),Year=rep(c(1959,1964,1984,2001,2014),each = 50))
(Comm_shift$Block)<-as.factor(Comm_shift$Block)

Comm_shift$Comm<-ifelse(Comm_shift$Year==1959,1,NA)
Comm_shift$Comm<-ifelse(Comm_shift$Year==1964,1-abs(rnorm(n = 50,mean = 0.1,sd =0.05)),Comm_shift$Comm)
Comm_shift$Comm<-ifelse(Comm_shift$Year==1984,1-abs(rnorm(n = 50,mean = 0.2,sd =0.05)),Comm_shift$Comm)
Comm_shift$Comm<-ifelse(Comm_shift$Year==2001,1-abs(rnorm(n = 50,mean = 0.25,sd =0.05)),Comm_shift$Comm)
Comm_shift$Comm<-ifelse(Comm_shift$Year==2014,1-abs(rnorm(n = 50,mean = 0.3,sd =0.05)),Comm_shift$Comm)

str(Comm_shift)

ggplot(Comm_shift,aes(x=Year,y=Comm,group=Block))+geom_line()+geom_smooth(aes(group=NA),se=F,method="lm",size=3)+ylab("Sorenson similarity")

<-as.factor(Comm_shift$Block)
