#Exploring Tidyverse

rm(list=ls())

library(tidyverse)

#reading in the data
d=read.csv("Fruit quality.csv")

head(d)

str(d)
d$sampleid=as.factor(d$sampleid)

d$variety=as.factor(d$variety)

d$treatment=as.factor(d$treatment)
str(d)
summary(d)
unique(d)
# putting the data in tidy format
#removing NA
d1=d%>%
  drop_na()
#grouping by treatment
d2=d1%>%
  group_by(treatment)%>%
  summarise(mean.weight=mean(weight)) #this shows the mean of the weight of apples by treatment
d3=d1%>%
  group_by(treatment)%>%
  summarise(mean.weight=mean(weight),
            mean.color=mean(color),
            mean.firmness=mean(firmness),
            mean.sugar=mean(sugar)) #this shows the mean of all response variables by treatment
#grouping by variety

d4=d1%>%
  group_by(variety)%>%
  summarise(mean.weight=mean(weight),
            mean.color=mean(color),
            mean.sugar=mean(sugar),
            mean.firmness=mean(firmness)) #this shows the mean of all response variables by variety
#combining treatment and variety 

d5=d1%>%
  group_by(treatment, variety)%>%
  summarise(mean.weight=mean(weight),
            mean.color=mean(color),
            mean.sugar=mean(color),
            mean.firmness=mean(firmness))# all summarise functions created a new table 
#adding a column 

d1$weightlog=log(d1$weight)

z1=d1%>%
  mutate(sugarlog=log(sugar),
         corrected_color=color+2)#to add the sugar log and corrected_color column


d1
d1=d1%>%
  group_by(treatment, variety)%>%
  mutate(sugarlog=log(sugar),
         colorlog=log(color),
         firmnesslog=log(firmness),
         ) #this created new columns

#joining

#creating a dataframe of just weight with the explanatory data
d1
Weight_add=d1%>%
  group_by(treatment, variety)%>%
  summarise(mean.weight=mean(weight)) #this created a new dataframe 

#left_joining d5 and weight_add
full.data = left_join(
  x = d5,
  y = Weight_add,
  by = c("treatment","variety", "mean.weight")
)
full.data

#pivoting to full.data which is wide format to long format
full.data.long = full.data %>% 
  pivot_longer(
    cols = c("mean.weight","mean.color","mean.sugar","mean.firmness"), 
    #what are the existing columns I want to make into rows?
    names_to =   "measurements",
    #put the names of the columns in a column called 'species'
    values_to = "mean values"
    #the values that were in each of the columns get moved to a column called 'count'
  )

str(full.data.long)
#converting measurements to factor
full.data.long$measurements=as.factor(full.data.long$measurements)
str(full.data.long)



