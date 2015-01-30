#Hi Paul - this is an example bit of code to show you how to plot
#using ggplot2 in a similar way to that I have been using for our work

rm(list=ls(all=TRUE))#first detach all objects this is good practice

require(ggplot2) #now load up the packages you will use - ggplot

Grad<-read.csv("Data/BA_gradient.csv")#and now the data

###########################################################
#figures in ggplot2########################################
###########################################################

#ggplot2 work a bit differently to the normal plot function in R
#but generally I think it's more intuative and simpler

#the function ggplot tells R you want to make a plot and then you define your variables in this

ggplot(data=Grad,aes(x=Year,y=BAM,group=Block))
#this won't do anything, but it's just to show you the structure of ggplot
#aes defines the variables or 'aesthetics' for your plot
#x=the x variable, y the y (here basal area of trees >10cm DBH) 
#and group indicates how you want your data to be organised
#the importance of group will come in later

ggplot(data=Grad,aes(x=Year,y=BAM,group=Block))+geom_point()
#lovely, we have a plot. geom_point adds our points
#geoms are basically features you want to plot
#however it is a bit lacking in information

ggplot(data=Grad,aes(x=Year,y=BAM,group=Block))+geom_point()+geom_line()
#sticking geom_line on there allows us to follow the trajectories of the plots
#but it is a bit messy

ggplot(data=Grad,aes(x=Year,y=BAM,group=Block))+geom_point()+geom_line()+facet_wrap(~Block)
#now things are a bit clearer
#facet_wrap is an invaluable function in ggplot especially good for data
#exploration. It splits the data into seperate graphs using whatever variable
#you put after the ~. In our case it is the plot number 
#so we get the different trajectores for each plot on seperate graphs


ggplot(data=Grad,aes(x=Year,y=BAM,group=Block))+geom_point()+geom_line()+geom_smooth(aes(group=NULL),size=3)
#going back to the previous plot you can also get a rough idea of the change over time
#by fitting a geom_smooth - essentially a very rough model.
#it's useful for exploration of relationships but is not really analysis
#size specifies the line thickness and the default fit is a smoothed loess curve with 
#standard errors

ggplot(data=Grad,aes(x=Year,y=BAM,group=Block))+geom_point()+geom_line()+geom_smooth(aes(group=NULL),size=3,method="lm")

#for a slightly more reasonable plot you can constrain the relationship between x and y to be linear
#like we have done above

#you can do much, much more than this. The best advice I can offer is for you to have a play
#around and see what you can do. It probably has the best help of all the R packages
#which you can find here: http://docs.ggplot2.org/current/

