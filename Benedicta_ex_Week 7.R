rm(list=ls())
library(ggplot2)
library(performance)
library(tidyverse)
############   PART 1 ################################
# Making a univariate linear model for one of my hypotheses
#Hypothesis tested
#Null Hypothesis: PDJ will increase the weight of apples

fruit=read.csv("Fruit quality.csv")
#removing NAs
fruit=fruit%>%
  drop_na()
fruit
str(fruit)
fruit$treatment=as.factor(fruit$treatment)
fruit$sampleid=as.factor(fruit$sampleid)
fruit$variety=as.factor(fruit$variety)
str(fruit)
mod1=lm(weight~treatment, data=fruit)
summary(mod1) #here, it shows that the treatment PDJ reduces weight but there is no significant evidence to proof that

#Examine the assumptions of linearity (using tests or diagnostic plots)
# First part 

mod1
par(mfrow=c(2,2))
plot(mod1) #using this, the Q-Q residuals plot showed that the distribution is normal, however, looking at the other diagnostic test, it is looking weird

#using the performance package 
check_model(mod1) #here, there is a high heteroscedasticity as the shape of the line wasn't flat and showed a weird shape, although, the normality of residuals was looking good but a little deviation was detected
#I mean for this study, I am supposed to use two predictor variable and it is not looking like a linear model will be a good idea for answering this question. 

#using the shapiro-wilk test 
shapiro.test(fruit$weight) #this shows that the distribution is normal as the p value is higher than 0.05

#using the histogram
par(mfrow=c(1,1))
hist(fruit$weight) #this looks normal as it forms the normal distribution shape sort of

#Plotting the relationship in ggplot using stat_smooth

A=ggplot(aes(x=treatment, y=weight), data=fruit)+
  geom_point()+
  stat_summary(fun.data = "mean_se", colour="red", size=1)+
  theme_bw() + 
  theme(axis.title=element_text(size=20),
        axis.text=element_text(size=10),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        legend.position="top",
        legend.title=element_blank())
print(A)



#######Exercise 8-Advanced lms
# Making a linear model (with more than one variable) for one of the hypotheses.
# Null Hypothesis: the weight of apple cultivars in control in comparison to PDJ is the same but different across varieties
# First doing this with an interactive model
mod2= lm(weight~treatment*variety, data=fruit)
summary(mod2) # the intercept here is control and variety C. From the model, the direction of the
#effect of PDJ on weight is positive but not significant in comparison to the intercept. 
#the effect of variety on weight shows negative direction, meaning that empire, 
#delicious and jonagold in comparison to cortland has lower weight. 
#this was significant for delicious and empire only. when checking the interaction effect of PDJ and variety, it shows a negative direction and not significant
#the overall p value of the model shows that it is statistically significant
#using predict
B= predict.lm(mod2 <- lm(weight~treatment*variety,data=fruit)) #here, I can't really explain what is going on here as the output is not showing me the which is for control and PDJ. Although the values predicted are like the range of values of the weight measured in this study
plot (B) # same here 
#using allEffects
library(effects)
plot(allEffects(mod2)) #this showed a plot that is supporting my null hypothesis that the effect of control and PDJ on weight is almost equal, but different across varieties. 
#from the plot, we can see that cortland has the highest weight in both treatments and Empire having the lowest weight.

#using emmeans
library(emmeans)
em_mod2 = emmeans(mod2,specs = ~treatment*variety)
em_mod2 #the output from this gave the estimated means and standard error. it shows the same result as the plot(allEffects) with Cortland having the highest weight and empire having the lowest. here, I can see the values and know the exact difference but I can't tell if there is a significance here.
emm_mod2=emmeans(mod2,pairwise~treatment*variety) #using this, it gave a warning that I amy have generated more contrasts than i really wanted.
fruit$weight = predict(mod2)
##plotting the interactive model
wt <- with(fruit,
           expand.grid(variety=unique(variety),
                       treatment=unique(treatment)))

wt$weight <- predict(mod2,newdata=wt)

#plotting model (predict and main plot)

ggplot(wt,aes(x=variety,y=weight,colour=treatment))+
  geom_point()+
  geom_line(aes(group=treatment)) # this plot shows that the varieties in control had more weight than those in PDJ treatment except for cortland, where PDJ was a bit higher.


###ADD RAW data to plot###
ggplot(wt,aes(x=variety,y=weight,colour=treatment))+
  geom_point(data=fruit, aes(x=variety, y=weight, color=treatment))+
  geom_point(color="red")+
  geom_line(aes(group=treatment))
  # I feel I am doing something wrong here, as the predicted model alone and the combined model look alike. now with the red point, is this saying my predicted model is saying the same thing with my raw data?

#doing this with an additive model

mod3=lm (weight~treatment+variety, data=fruit)
summary(mod3) #the model shows that PDJ reduces the weight of apples and the varieties weight are also reduced in comparison to variety C which were all statistically significant

C= predict.lm(mod3 <- lm(weight~treatment+variety,data=fruit)) #same here, I can't really explain what is going on here as the output is not showing me  which is for control and PDJ. Although the values predicted are like the range of values of the weight measured in this study
plot (C) 
#using allEffects
plot(allEffects(mod3)) #the treatment effect plot showed that the weight of apples in the control is more than those in PDJ. in the variety effect, cortland has the highest weight followed by jonagold, delicious and empire.

#using emmeans

em_mod3 = emmeans(mod3,specs = ~treatment+variety)
em_mod3 #the output from this gave the estimated means and standard error. it shows the same result as the plot(allEffects) with Cortland having the highest weight and empire having the lowest. here, I can see the values and know the exact difference but I can't tell if there is a significance here.
emm_mod3=emmeans(mod3,pairwise~treatment+variety) #using this, it gave a warning that I may have generated more contrasts than i really wanted.
fruit$weight = predict(mod3)
##plotting the interactive model
wt1 <- with(fruit,
           expand.grid(variety=unique(variety),
                       treatment=unique(treatment)))

wt1$weight <- predict(mod3,newdata=wt1)

#plotting model (predict and main plot)

ggplot(wt1,aes(x=variety,y=weight,colour=treatment))+
  geom_point()+
  geom_line(aes(group=treatment)) # this plot shows that the varieties in control had more weight than those in PDJ treatment.


###ADD RAW data to plot###
ggplot(data=fruit, aes(x=variety, y=weight, color=treatment))+
  geom_point(data=wt1,aes(x=variety,y=weight,colour=treatment))+
  geom_point(color="red")+
  geom_line(aes(group=treatment)) #same issue here as the first one

  