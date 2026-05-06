rm(list=ls())
#Here we are analyzing fruit quality data to test whether PDJ application affects fruit quality.

library(tidyverse)
library(glmmTMB)
library(emmeans)
library(car)
library(performance)
library(ggplot2)
library(DHARMa)
library(scales)
library(ggbeeswarm)
library(ggpubr)
library(dplyr)


## 1) Load and check data
apple <- read.csv("Fruit quality.csv", header = T,
                  colClasses = list (variety = "factor", treatment = "factor"))
#Add block variable to code
apple <- apple %>%
  mutate(Block = substr(sampleid, 1, 1))
apple$Block[37:40] <- "10"

apple$variety = factor(apple$variety,
                       levels = c("C", "D", "E", "J"),
                       labels = c("Cortland", "Delicious", "Empire", "Jonagold"))
#removing the empty cells

apple=apple%>%
  drop_na()
str(apple)
apple$treatment=factor(apple$treatment,
                       levels=c("C","PDJ"),
                       labels=c("C","PDJ"))
apple$Block = factor(apple$Block)


levels(apple$treatment)
levels(apple$variety)

#checking for normality

shapiro.test(apple$weight) # The data distribution doesn't deviate from the normal distribution
shapiro.test(apple$color) # deviates
shapiro.test(apple$firmness) # deviates
shapiro.test(apple$sugar) # doesn't deviate

#checking other normality (histogram)
par(mfrow=c(2,2))
hist(apple$color)
hist(apple$firmness)
hist(apple$weight)
hist(apple$sugar)
#checking other normality (Q-Q plot)
qqnorm(apple$weight)
qqline(apple$weight, col = "blue")

qqnorm(apple$color)
qqline(apple$color, col = "blue")


qqnorm(apple$firmness)
qqline(apple$firmness, col = "blue")


qqnorm(apple$sugar)
qqline(apple$sugar, col = "blue")

## 2) run some basic GLMMS

m1=glmmTMB(color~treatment*variety + (1|Block), data=apple) #come back here because you use the same set of interaction but used a Gaussian error of distribution.
summary(m1)
Anova(m1)
hist(resid(m1))
boxplot(resid(m1)~apple$treatment)
plot(resid(m1)~fitted(m1))


#using the DHARMa diagnostics
simulationOutput=simulateResiduals(m1, plot=T) ## plot simulated residuals

#using the check_model()

check_model(m1)


color_mod=glmmTMB(color~treatment*variety+(1|Block), data=apple, family=Gamma(link="log"))
summary(color_mod)
Anova(color_mod)
hist(resid(color_mod))
boxplot(resid(color_mod)~apple$treatment)
plot(resid(color_mod)~fitted(color_mod))

simulationOutput=simulateResiduals(color_mod, plot=T)


AIC (color_mod, m1)
anova(color_mod,m1)

# the AIC difference here is too small to matter practically, both have the same degree of freedom (df=10), m1 residual plot shows mild quantile deviations, while color_mod shows cleaner residual behaviour and more reliable inference
# the diagnostics results shows that color_mod is the preferable model

library(effects)
plot(allEffects(color_mod))

#predict 
fmd1 = expand.grid(treatment = unique(apple$treatment),
                   variety=unique(apple$variety),
                   Block = unique(apple$Block)
)
fmd1$col= predict(color_mod,newdata= fmd1,type="response")

p=ggplot(data=fmd1, aes(x=variety,y=col,col=treatment))+ 
  geom_line(size=1)+
  geom_point(data = apple,
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
color_summary <- apple %>%
  group_by(variety, treatment) %>%
  summarise(
    mean_color = mean(color, na.rm = TRUE),
    sd_color   = sd(color, na.rm = TRUE),
    n          = n(),
    se_color   = sd_color / sqrt(n),
    .groups = "drop"
  )
q=ggplot() +
  geom_quasirandom(
    data = apple,
    aes(x = variety, y = color, color = treatment),
    dodge.width = 0.85,
    width = 0.18,
    alpha = 0.4,
    size = 1.2
  )+
  # Raw means (thick & prominent)
  geom_point(
    data = color_summary,
    aes(x = variety, y = mean_color, color = treatment),
    position = position_dodge(0.8),
    size = 4)+
  # Raw mean ± SE
  geom_errorbar(
    data = color_summary,
    aes(
      x = variety,
      ymin = mean_color - se_color,
      ymax = mean_color + se_color,
      color = treatment
    ),
    position = position_dodge(0.8),
    width = 0.15,
    linewidth = 1
  ) +
  
  scale_color_manual(values = c("C" = "#D55E00", "PDJ" = "#009E73")) +
  
  labs(
    x = "Variety",
    y = "Apple Color Index",
    color = "Treatment"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 23),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "right"
  )

print(q)
emmeans(color_mod, ~treatment|variety, type="response")
emmeans(color_mod, pairwise~treatment|variety, type="response")

ggsave(" (predict) Apple color index.png", plot = p, width = 18, height =10.5 , units = "in", dpi = 600)
ggsave("Apple color index.png", plot = q, width = 18, height =10.5 , units = "in", dpi = 600)

#Weight 

weight_mod=glmmTMB(weight~treatment*variety+(1|Block), data=apple)
summary(weight_mod) #there is no significance in the interaction effect
weight_mod1=glmmTMB(weight~treatment+variety+(1|Block), data=apple)
summary(weight_mod1) #Variety effect is significant: varieties have different weights and they are not 
AIC(weight_mod,weight_mod1) #weight_mod1 is lower
anova(weight_mod, weight_mod1) #weight_mod1 is preferred here 
hist(resid(weight_mod1))
boxplot(resid(weight_mod1)~apple$treatment)
plot(resid(weight_mod1)~fitted(weight_mod1))
Anova(weight_mod1)
Anova(weight_mod)
# all well-fitted 

#using the DHARMa diagnostics
simulationOutput=simulateResiduals(weight_mod1, plot=T) 
simulationOutput=simulateResiduals(weight_mod, plot=T)
#weight_mod looks like the preferred model for this 


plot(allEffects(weight_mod))

fmd2 = expand.grid(treatment = unique(apple$treatment),
                   variety=unique(apple$variety),
                   Block = unique(apple$Block)
)
fmd2$wei= predict(weight_mod,newdata= fmd2,type="response")

#plot

a=ggplot(data=fmd2, aes(x=variety,y=wei,col=treatment))+ 
  geom_line(size=1)+
  geom_point(data = apple,
             aes(x = variety, y = weight),
             size = 3, shape = 1,
             position = position_jitter(width = 0.1))+
  ylab("Weight (Kg)")+
  xlab("Variety")+
  theme_bw() + 
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        legend.position=c(.9,.29),
        legend.text = element_text(size=12,face="italic"))
print(a)



emmeans(weight_mod,pairwise~treatment|variety, type="response")

emmeans(weight_mod1,~treatment|variety, type="response")

emmeans(weight_mod1,~treatment+variety, type="response")

ggsave(" (predict) Apple weight.png", plot = a, width = 18, height =10.5 , units = "in", dpi = 600)
#Variety had a strong effect on fruit weight (p < 0.001 overall), while the PDJ treatment showed only a marginal tendency to reduce weight compared to control.
#Cortland apples were significantly heavier than the other varieties, while Empire produced the smallest fruits.

weight_summary <- apple %>%
  group_by(variety, treatment) %>%
  summarise(
    mean_weight = mean(weight, na.rm = TRUE),
    sd_weight  = sd(weight, na.rm = TRUE),
    n          = n(),
    se_weight   = sd_weight / sqrt(n),
    .groups = "drop"
  )
c=ggplot() +
  geom_quasirandom(
    data = apple,
    aes(x = variety, y = weight, color = treatment),
    dodge.width = 0.85,
    width = 0.18,
    alpha = 0.4,
    size = 1.2
  )+
  # Raw means (thick & prominent)
  geom_point(
    data = weight_summary,
    aes(x = variety, y = mean_weight, color = treatment),
    position = position_dodge(0.8),
    size = 4)+
  # Raw mean ± SE
  geom_errorbar(
    data = weight_summary,
    aes(
      x = variety,
      ymin = mean_weight - se_weight,
      ymax = mean_weight + se_weight,
      color = treatment
    ),
    position = position_dodge(0.8),
    width = 0.15,
    linewidth = 1
  ) +
  
  scale_color_manual(values = c("C" = "#D55E00", "PDJ" = "#009E73")) +
  
  labs(
    x = "Variety",
    y = "Weight (Kg)",
    color = "Treatment"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "right"
  )
ggsave("Apple weight.png", plot = c, width = 18, height =10.5 , units = "in", dpi = 600)

#firmness
firmness_mod=glmmTMB(firmness~treatment*variety+(1|Block), data=apple, family=Gamma(link="log"))
summary(firmness_mod) 
Anova(firmness_mod) #there is significance in the interaction between variety and treatment

f1=glmmTMB(firmness~treatment*variety+(1|Block), data=apple)
summary(f1)
anova(firmness_mod,f1) #f1=best model
Anova(f1) #Treatment doesn't show significance in the firmness, however, firmness vary across Variety with an interaction occuring between Variety and Treatment
hist(resid(f1))
boxplot(resid(f1)~apple$treatment)
plot(resid(f1)~fitted(f1))
shapiro.test(apple$firmness)
hist(apple$firmness)
# all well-fitted 

par(mfrow=c(1,2))
#using the DHARMa diagnostics
simulationOutput=simulateResiduals(f1, plot=T) ## showed deviations which was significant
hist(simulationOutput)
simulationOutput1=simulateResiduals(firmness_mod, plot=T) #no significant problems detected here
hist(simulationOutput1)
#using the check_model()


AIC(f1, firmness_mod)
#AIC favors the selection of f1 but using the model diagnostic, it supports firmness_mod as it passes all the checks, given that firmness data is continuous and positively skewed, the gamma distribution will be appropriate for it.

emmeans(firmness_mod, pairwise~treatment|variety, type="response")


plot(allEffects(firmness_mod))
#here, pdj increased the color of cortland and reduced that of empire significantly.
#predict 
fmd3 = expand.grid(treatment = unique(apple$treatment),
                   variety=unique(apple$variety),
                   Block = unique(apple$Block)
)
fmd3$fir= predict(firmness_mod,newdata= fmd3,type="response")

#plot

b=ggplot(data=fmd3, aes(x=variety,y=fir,col=treatment))+ 
  geom_line(size=1)+
  geom_point(data = apple,
             aes(x = variety, y = color),
             size = 3, shape = 1,
             position = position_jitter(width = 0.1))+
  ylab("Firmness (N)")+
  xlab("Variety")+
  theme_bw() + 
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        legend.position=c(.9,.55),
        legend.text = element_text(size=12,face="italic"))
print(b)
ggsave(" (predict) Apple firmness.png", plot = b, width = 18, height =10.5 , units = "in", dpi = 600)
firmness_summary <- apple %>%
  group_by(variety, treatment) %>%
  summarise(
    mean_firmness = mean(firmness, na.rm = TRUE),
    sd_firmness   = sd(firmness, na.rm = TRUE),
    n          = n(),
    se_firmness   = sd_firmness / sqrt(n),
    .groups = "drop"
  )
d=ggplot() +
  geom_quasirandom(
    data = apple,
    aes(x = variety, y = firmness, color = treatment),
    dodge.width = 0.85,
    width = 0.18,
    alpha = 0.4,
    size = 1.2
  )+
  # Raw means (thick & prominent)
  geom_point(
    data = firmness_summary,
    aes(x = variety, y = mean_firmness, color = treatment),
    position = position_dodge(0.8),
    size = 4)+
  # Raw mean ± SE
  geom_errorbar(
    data = firmness_summary,
    aes(
      x = variety,
      ymin = mean_firmness - se_firmness,
      ymax = mean_firmness + se_firmness,
      color = treatment
    ),
    position = position_dodge(0.8),
    width = 0.15,
    linewidth = 1
  ) +
  
  scale_color_manual(values = c("C" = "#D55E00", "PDJ" = "#009E73")) +
  
  labs(
    x = "Variety",
    y = "Firmness (N)",
    color = "Treatment"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "right"
  )
ggsave("Apple Firmness.png", plot = d, width = 18, height =10.5 , units = "in", dpi = 600)





#sugar
sugar_mod=glmmTMB(sugar~treatment*variety+(1|Block), data=apple)
summary(sugar_mod) #no significance in interaction 
Anova(sugar_mod)
sugar_mod1=glmmTMB(sugar~treatment+variety+(1|Block), data=apple)
summary(sugar_mod1)
Anova(sugar_mod1) #significance shown in Variety, PDJ doesn't affect the sugar content they only vary due to the varieties 
AIC(sugar_mod, sugar_mod1)
anova(sugar_mod, sugar_mod1) #sugar_mod1 is the best fit model for sugar as it has lower AIC/BIC and higher loglik and deviance in comparison to sugar_mod

hist(resid(sugar_mod1))
boxplot(resid(sugar_mod1)~apple$treatment)
plot(resid(sugar_mod1)~fitted(sugar_mod1))
# all well-fitted 

#using the DHARMa diagnostics
simulationOutput=simulateResiduals(sugar_mod1, plot=T) ## no significant problems detected
simualtionOutput=simulateResiduals(sugar_mod, plot=T) ## no significant problems detected 

#model diagnostics shows the two models shows no significant problems detected but AIC showed that sugar_mod1 as the best fit model




emmeans(sugar_mod1, ~treatment+variety, type="response")

emmeans(sugar_mod1, pairwise~treatment|variety, type="response")


plot(allEffects(sugar_mod1))

fmd4 = expand.grid(treatment = unique(apple$treatment),
                   variety=unique(apple$variety),
                   Block = unique(apple$Block)
)
fmd4$sug= predict(sugar_mod1,newdata= fmd4,type="response")

#plot

e=ggplot(data=fmd4, aes(x=variety,y=sug,col=treatment))+ 
  geom_line(size=1)+
  geom_point(data = apple,
             aes(x = variety, y = weight),
             size = 3, shape = 1,
             position = position_jitter(width = 0.1))+
  ylab("Sugar Content (°Brix)")+
  xlab("Variety")+
  theme_bw() + 
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        legend.position=c(.9,.29),
        legend.text = element_text(size=12,face="italic"))
print(e)

ggsave(" (predict) Sugar Content.png", plot = e, width = 18, height =10.5 , units = "in", dpi = 600)
sugar_summary <- apple %>%
  group_by(variety, treatment) %>%
  summarise(
    mean_sugar = mean(sugar, na.rm = TRUE),
    sd_sugar   = sd(sugar, na.rm = TRUE),
    n          = n(),
    se_sugar   = sd_sugar / sqrt(n),
    .groups = "drop"
  )
f=ggplot() +
  geom_quasirandom(
    data = apple,
    aes(x = variety, y = sugar, color = treatment),
    dodge.width = 0.85,
    width = 0.18,
    alpha = 0.4,
    size = 1.2
  )+
  # Raw means (thick & prominent)
  geom_point(
    data = sugar_summary,
    aes(x = variety, y = mean_sugar, color = treatment),
    position = position_dodge(0.8),
    size = 4)+
  # Raw mean ± SE
  geom_errorbar(
    data = sugar_summary,
    aes(
      x = variety,
      ymin = mean_sugar - se_sugar,
      ymax = mean_sugar + se_sugar,
      color = treatment
    ),
    position = position_dodge(0.8),
    width = 0.15,
    linewidth = 1
  ) +
  
  scale_color_manual(values = c("C" = "#D55E00", "PDJ" = "#009E73")) +
  
  labs(
    x = "Variety",
    y = "Sugar Content (°Brix)",
    color = "Treatment"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "right"
  )
ggsave("Apple Sugar.png", plot = f, width = 18, height =10.5 , units = "in", dpi = 600)

#combining plots
p2=ggarrange(p, a, b, e,
             labels = c("1A", "2A", "3A", "4A"),
             ncol = 2, nrow = 2)
ggsave("combined_predicted.png", plot = p2, width = 18, height =10.5 , units = "in", dpi = 600)

p3=ggarrange(q, c, d, f,
             labels = c("1B", "2B", "3B", "4B"),
             ncol = 2, nrow = 2)
ggsave("combined_main plots.png", plot = p3, width = 18, height =10.5 , units = "in", dpi = 600)
