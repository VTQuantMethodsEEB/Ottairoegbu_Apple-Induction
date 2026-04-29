rm(list=ls())
library(tidyverse)

fruit=read.csv("Fruit quality.csv")

fruit=fruit%>%
  drop_na()
set.seed(123)
#permutation test
#Null Hypothesis: The mean of color (response variable) is equal across the levels of treatment (predictor variable)
res <- NA ## set aside space for results
color.pdj = fruit$color[fruit$treatment=="PDJ"] #creating the list for the values of color in pdj treatment
color.c = fruit$color[fruit$treatment=="C"] #creating the list for the values of color in control treatment
mean(color.c)-mean(color.pdj) #finding the mean difference the mean difference is 1.106228
#ideally you would reverse these for your observed diff

comb_cp = c(color.c,color.pdj) #combining the two 
comb_cp
sample(comb_cp,4, replace=F) #i want to scramble 4 times 

#the for loop 
for (i in 1:1000) { # let i take the value to 1000
  color <- sample(c(color.c,color.pdj)) ## scramble
  ## pick out c and pdj samples 
  color.c <- color[1:length(color.c)] #this says assign the first six colonies to color.c
  color.pdj <- color[(length(color.c)+1):length(color)] #assign the rest of the colonies to color.pdj
  
  #if you had a dataframe it would look like
  #forestboot <- colonyboot[1:length(ants$place[ants$place=="forest"])] #this says assign the first six colonies to forest
  #fieldboot <- colonyboot[(length(ants$place[ants$place=="forest"])+1):length(ants$place)] #this says assign the rest of the observations to field
  
  ## compute & store difference in means
  res[i] <- mean(color.c)-mean(color.pdj) #calculate the difference in the color.c means and the color.pdj means
  #[i] says "where i", and i is a counter, after running this loop, i should be 1000
}

print(res)
obs <- mean(color.c)-mean(color.pdj)
obs

#checking the p value 
res[res>=obs]
length(res[res>=obs])
663/1000
mean(res>=obs)    # this gives a value of 0.663 which shows that the mean of color is not equal across treatments used in this study

# for the second test, I will be using the Shapiro-Wilk Test, to know what kind of distribution my data is 
#Null hypothesis: The distribution of the response variables are normal.
# if p<0.05 indicates that the distribution is not normal
weight=shapiro.test(fruit$weight)
weight
#p value=0.8311 (the distribution of weight is normal)

color=shapiro.test(fruit$color)
color
#p value=0.002 (the distribution of color is not normal)

firmness=shapiro.test(fruit$firmness)
firmness
#p value=0.00011 (the distribution of firmness is not normal)

sugar=shapiro.test(fruit$sugar)
sugar
#p value=0.1489 (the distribution of firmness is normal)