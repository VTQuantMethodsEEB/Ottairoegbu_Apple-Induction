rm(list=ls())
library(ggplot2)
library(tidyverse)
library(DHARMa)
library(emmeans)
library(effects)

fruit=read.csv("Fruit quality.csv")
#removing NA
fruit=fruit%>%
  drop_na()
fruit
str(fruit)
fruit$sampleid=as.factor(fruit$sampleid)
fruit$variety=as.factor(fruit$variety)
fruit$treatment=as.factor(fruit$treatment)
str(fruit)

#Hypothesis: Apples treated with PDJ will have more red coloration compared to untreated apples
#Null Hypothesis: PDJ has not effect on the red coloration of apples
#GLM distribution testing with normal distribution
mod1=glm(color~treatment*variety, data=fruit) #using the Gaussian which is obviously a linear model too
summary(mod1) # this model shows that there is a significant effect of PDJ on color and there is a strong effect of variety on the color. Looking at the interaction, there was a significant decreasing effect of interaction between PDJ and Empire only. Although the interaction also showed reduced color in apples but not significant
#diagnostics
#using Dharma
simulationOutput=simulateResiduals(fittedModel=mod1)
plot(simulationOutput) # this looks good (normal), the points are following the line and it is not significant which means it is good!
predict.glm(mod1)
plot(predict.glm(mod1, type="response"))
plot(allEffects(mod1)) # the effect plot shows that the use of PDJ increases color in cortland, reduces in empire and somewhat similar in delicious and jonagold
#emmeans
emmod1=emmeans(mod1, ~treatment*variety, type="response")
emmod1 #with this, I can see that the color index increases in cortland, delicious and jonagold but reduced in empire

###come back here to Plot your model (e.g. using predict) and overlay the model on top of the underlying data

#GLM distribution testing with GAMMA
mod2=glm(color~treatment*variety, data=fruit, family=Gamma())
summary(mod2) #this model shows that PDJ decreases color and is significant; the effect of variety on color is also very strong and statistically significant  as the color decreaases and there is an interaction between treatment and variety which is significant in only Empire
#using diagnostics
simulationOutput=simulateResiduals(fittedModel=mod2)
plot(simulationOutput) # this also looks normal but i think it has a slight deviation but can still be passed.
AIC(mod1, mod2)
#checking shapiro-wilk
shapiro.test(fruit$color)
hist(fruit$color)
head(fruit) # the following confirms to use Gamma as the distribution here (is this accurate enough to go ahead?)
predict.glm(mod2, type="response")
plot(allEffects(mod2)) #the plot shows that PDJ reduces the color in cortland,delicious, slight decrease for jonagold but increased in empire

emmod2=emmeans(mod2, ~treatment*variety, type="response")
emmod2 # I don't know if i am reading the plot and emmeans wrong, but here, i can see that pdj increases color in cortland, delicious and jonagold and reduces it in empire
emmod3=emmeans(mod2, pairwise~treatment|variety, type="response")
emmod3 # using pairwise, I can see that PDJ significantly increases red coloration in cortland, while the rest were not statistically clear 
#new datapoint

wt <- expand.grid(
  variety = levels(fruit$variety),
  treatment = levels(fruit$treatment)
)
##getting prediction intervals##
preds  = predict(mod2,type="response",newdata = wt, se.fit = T)
wt= cbind(wt, preds[1:2])#bind together se's and fitted points on your newdata
#get the inverse link function for your glm
ilink <- family(mod2)$linkinv
#back transform the CIs (not the SEs!)
wt<- transform(wt, 
                     Fitted = ilink(fit), 
                     Upper = ilink(fit + (2 * se.fit)),
                     Lower = ilink(fit - (2 * se.fit)))

head(wt)
#plot the output
plot1=ggplot(data=fruit,aes(x=variety,y=color,color=treatment))+
  geom_point(size=2,shape =1) +
  facet_wrap(~treatment, nrow=1)+
  geom_line(data=wt, aes(x=variety,y=Fitted,col = treatment))+
  geom_ribbon(data = wt, aes(ymin = Lower, ymax = Upper, x = variety,y=Fitted),
              fill = "steelblue2", alpha = 0.2) 

plot1
#Result (first, to be honest, I don't know if this is supposed to be the right output, but here we go!!!)
#Overall, PDJ significantly increased the red coloration in cortland (p=0.30), there was increase of red coloration in delicious, jonagold and decrease in empire but they were not statistically significant.
#There was also a significant interaction effect of PDJ and variety in the red coloration of apples.


######Week 11

#here, I will be using the response variable weight 
#using Gamma family of distribution
m1=glm(weight~treatment*variety, data=fruit, family=Gamma())
summary(m1) #the model output shows that PDJ reduces weight overall but it is not significant in comparison to control. Empire significantly increases in weight. There is marginal significant increase in weight in delicious. there was also a marginal significant increasein weight in the interaction between PDJ and variety as seen in empire
m2=glm(weight~treatment+variety, data=fruit, family=Gamma())
summary(m2)  # without using the interaction, PDJ alone has a marginal significant effect by increasing the weight of apples overall. there is also statistically significant variation showing increase in the weight of varieties.
#using the Gaussian family of distribution
m3=glm(weight~treatment*variety, data=fruit)
summary(m3) # PDJ increases slightly increases the weight overall but not significant. There was also a significant decrease in weight in delicious and empire. the interaction  between PDJ and variety showed a reduction in weight but not statistically significant.
m4=glm(weight~treatment+variety, data=fruit)
summary(m4) #overall, PDJ decreases the weight in apples but it is marginally significant. for variety alone, I see that there is a statistically significant decrease in the weight of apple varieties in comparison to cortland.

#checking for likelihood ration test
anova(m1,m2, test="Chisq") #comparing the Gamma part
#model with interaction is not significant, so I will go with the model without interaction (m2)
anova(m3,m4, test="Chisq") #comparing the Gaussian part
# the same thing here too, so going with m4(model without interaction)

#using AIC
AIC(m1,m2,m3,m4)
#using AIC and comparing the 4 models, it shows here that m4 is the best model (Gaussian model without interaction) which was picked in the LRT. Overall, it supports that the model fitting is best fit without including the interaction between treatment and variety, except for the fact that for LRT, I can't combine models with different family of distributions together but I can do so using AIC.\
#confirming if Gaussian fitsss
hist(fruit$weight) #this shows normality
shapiro.test(fruit$weight)# this also shows normality... We are good to go
