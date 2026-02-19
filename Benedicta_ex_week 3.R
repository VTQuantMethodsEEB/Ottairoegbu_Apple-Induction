rm(list=ls())
#loading packages
library(cowplot)
library(ggforce) 
library(gridExtra)
library(viridis)
library(ggthemes)
library(ggplot2)
library(tidyverse)
#calling in the data
fruit=read.csv("Fruit quality.csv")
#checking the structure of the data and converting sampleid, 
#variety and treatment from characters to factors
str(fruit)
fruit$sampleid=as.factor(fruit$sampleid)
fruit$variety=as.factor(fruit$variety)
fruit$treatment=as.factor(fruit$treatment)
str(fruit)
#removing NA
fruit=fruit%>%
  drop_na()

#Plot 1: I want to see if PDJ has any impact on the weight of the sampled apples
A= ggplot(data=fruit, aes(x=variety, y=weight, fill=treatment))+
  geom_boxplot()+
  #geom_point()
  geom_jitter(
     data = fruit,
    aes(x = variety, y = weight, color = treatment),
     position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
 size = 2,
 alpha = 0.5, show.legend=FALSE
)+ #i used geom_jitter instead of point here because it aligns the points clearly and not separated from the boxplot
  ylab("Weight (Kg)")+ #change the axes
  xlab("Variety") +
  scale_fill_manual(values=c("C"= "#3ECCBB","PDJ"="#E76A4B"),
                    labels=c("C"="Control", "PDJ"="PDJ"), name="Treatment")+
  scale_color_manual(values=c("C"= "#3ECCBB","PDJ"="#E76A4B"),
                    labels=c("C"="Control", "PDJ"="PDJ"), name="Treatment")+
  scale_x_discrete(labels=c("C"="Cortland","D"="Delicious","E"="Empire", "J"="Jonagold"))+ # this changed C,D, E and J to their complete names 
 theme_bw()+
  theme(axis.title=element_text(size=20, face="bold"),
        axis.text.x.bottom = element_text(size=15),
        axis.text.y.left = element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        legend.position="right",
        legend.title = element_blank(),
        legend.text = element_text(size=15, face="bold"),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white"))

#save the file
ggsave("Apple Weight.png", plot = A, width = 10, height =7 , units = "in", dpi = 600)

#because my sample size is quite small, I thought about having a dot plot and the data points. it is supposed to have the mean and error bars, maybe when we start doing the statistics i will incorporate it
#the next ggplot will show how PDJ impact color across the apple cultivars
B=ggplot(data=fruit, aes(x=variety, y=color, fill=treatment))+
  geom_boxplot()+
  #geom_point()
  geom_jitter(
    data = fruit,
    aes(x = variety, y = color, color = treatment),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    size = 2,
    alpha = 0.5, show.legend=FALSE
  )+ #i used geom_jitter instead of point here because it aligns the points clearly and not separated from the boxplot
  ylab("Apple Color Index")+ #change the axes
  xlab("Variety") +
  scale_fill_manual(values=c("C"= "#3ECCBB","PDJ"="#E76A4B"),
                    labels=c("C"="Control", "PDJ"="PDJ"), name="Treatment")+
  scale_color_manual(values=c("C"= "#3ECCBB","PDJ"="#E76A4B"),
                     labels=c("C"="Control", "PDJ"="PDJ"), name="Treatment")+
  scale_x_discrete(labels=c("C"="Cortland","D"="Delicious","E"="Empire", "J"="Jonagold"))+ # this changed C,D, E and J to their complete names 
  theme_bw()+
  theme(axis.title=element_text(size=20, face="bold"),
        axis.text.x.bottom = element_text(size=15),
        axis.text.y.left = element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        legend.position="right",
        legend.title = element_blank(),
        legend.text = element_text(size=15, face="bold"),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white"))

#save the file
ggsave("Apple Color Index.png", plot = B, width = 10, height =7 , units = "in", dpi = 600)


