rm(list=ls())
library(ggplot2)
library(tidyverse)
library(MASS)
library(glmmTMB)
library(reshape2)
library(DHARMa)
fruit=read.csv("Fruit quality.csv")
str(fruit)
fruit$sampleid=as.factor(fruit$sampleid)
fruit$variety=as.factor(fruit$variety)
fruit$treatment=as.factor(fruit$treatment)
str(fruit)
#running a mixed effect model
# I need to add the  block as a column here 
fruit <- fruit %>%
  mutate(Block = substr(sampleid, 1, 1))
fruit$Block[37:40] <- "10"

# from the method on the slide it shows 3 blocks for control but the data set shows 5 blocks for control and PDJ (N=20 each), hence the code

fruit$variety = factor(fruit$variety,
                       levels = c("C", "D", "E", "J"),
                       labels = c("Cortland", "Delicious", "Empire", "Jonagold"))

fruit=na.omit(fruit)
str(fruit)
fruit$Block=as.factor(fruit$Block)
str(fruit)


#Null Hypothesis 1: PDJ has not effect on the red coloration of apples
#trying out models
mod1=glmmTMB(color~treatment*variety+(1|Block), data=fruit)
summary(mod1) #this shows that there is a significant interaction between pdj and variety on color. with the interaction showing a decrease which is supported for empire. 

mod2=glmmTMB(color~treatment*variety+(1|Block), data=fruit, family=Gamma (link="log"))
summary(mod2) #the results is  similar here as in mod1, but the estimate is a different with the same direction.

#checking which model fit best
AIC(mod1, mod2) #mod1 with the Gaussian distribution is the best fit 
fruit$color
hist(fruit$color)
plot(simulateResiduals(mod1))
plot(simulateResiduals(mod2))

#this is quite tricky, using the AIC alone would have allowed me to go with the Gaussian model but checking the diagnostics confirms that the Gamma model is the best fit.

library(effects)
plot(allEffects(mod2))
#here, pdj increased the color of cortland and reduced that of empire significantly.
#predict 
fmd2 = expand.grid(treatment = unique(fruit$treatment),
                     variety=unique(fruit$variety),
                     Block = unique(fruit$Block)
)
fmd2$col= predict(mod2,newdata= fmd2,type="response")

#plot

p=ggplot(data=fmd2, aes(x=variety,y=col,col=treatment))+ 
  geom_line(size=1)+
  geom_point(data = fruit,
             aes(x = variety, y = color),
             size = 3, shape = 1,
             position = position_jitter(width = 0.1))+
  ylab("Apple Color Index")+
  xlab("Variety")+
  theme_bw() + 
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        legend.position=c(.9,.55),
        legend.text = element_text(size=12,face="italic"))
print(p)

# variety effect here is very high with delicious having high red coloration followed by empire, cortland and jonagold. treatment effect is smaller but visible.
