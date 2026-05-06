# Ottairoegbu_Apple-Induction

This data shows the effect of Prohydrojasmonic acid (PDJ) on:
a. the weight, firmness, sugar content and color of four varieties of apple. we have 40 observations (10 randomly selected apples times 4 varieties of apples) and 7 variables (sample id, treatment which is further divided into control and PDJ, weight (kg), firmness (N), sugar (Brix), and color)
b. reducing pest damage and comparing its efficacy with other commercial insecticides
c. the phenolic composition of apple fruits
My goals for this data:
1. find the right model to analyse the data
2. Proper interpretation of the output
3. good graphical presentation of the plots 
4. result interpretation for the wider audience

WEEKLY LOG
follow this template: Each week, you will update this log by describing the type of investigations you might do with your data, and how you will break them up into different components (e.g. I will do xx analysis, and then I will make xx figure..), and you will always include: (1) what data file you used (2) what you called your R script.

###############################################Week 1###############################
1. I uploaded the data file for fruit quality
2. I did some data wrangling (did some calculations and cleaning) to know the type of dataset I have before proper analysis


######################################### Week 2###################################
1. Code: Benedicta_ex_week 2.R 
Data: Fruit quality.csv
2. I used tidyverse code on my data
3. I used group_by, mutate and summarise on my data and was able to tell the difference between the three commands.
4. I left_joined two dataframes I created from my original data and pivoted it to a long format.

########################Week 3####################################
Code: Benedicta_ex_week 3.R
Data= Fruit quality.csv
I plotted two graphs using ggplot2 and other packages for this assignment to see how PDJ affect the weight and color of apple cultivars in comparison to the control
the response variables are weight and color (y), with variety as x and PDJ as the z which was used as the color scheme here. 
this was done to see if there is any interaction between PDJ and variety in the response variables tested. 



##################################Week 5 ##########################################
1. Code: Benedicta_ex_Week 5.R
Data= Fruit quality.csv
2. I formulated two hypotheses using permuation test and the shapiro wilk test
3. For the permutation test, I check if the mean of color across the two treatments (PDJ and Control) are equal
4. Using the shapiro wilk test, I was able to confirm the distribution of my response variables



######################################## Week 7 ####################################
1. Code= Benedicta_ex_Week 7.R
Data=Fruit quality.csv
2. I used the linear model (lm) and answered one of the hypothesis to see if PDJ increases the weight of Apple
3. To do this, I used the lm function having weight as a function of treatment.
went ahead to do some model diagnostic testing tools using the plot() and the check_model() from the performance package
4. I also used the shapiro-wilk test which confirms normality and the hist()


########################### Week 8_continuation from exercise 7 ########################
1. Code=Benedicta_ex_Week 7.R
2. Data=Fruit quality.csv
3. I used the linear model to answer if the weight of apples is affected by the interaction between PDJ and variety
4. To do this, I used the lm function having weight as a function of the interaction between treatment and variety (treatment*variety) and the additive effect (treatment+variety)
5. However, I had a bit of issue with interpreting the combined model, i wasn't sure if I was doing something right there. 


##########################Week 10 and 11##########################
1. Code: Benedicta_ex_week 10 glm.R
2. Data=Fruit quality.csv
3. for week 10, I use GLM on my data, then used predict and ovrlaid it with the model predictions (but I am not sure if the output is making any sense visually, helpppp!!!)
4. for week 11, I compared four models using the response variable weight. I did this using Likelihood Ratio Test and AIC which supports that the model is better fitted without includng interaction between treatment and variety. 


#############################Week 12############################
1. Benedicta_ex_week 12 mixed models.R
2. Data=Fruit quality.csv
3. Here, I used one of my hypothesis and tested it using glmmTMB package in R. 
4. I used the Gaussian and Gamma distribution to know which one fits my model and went ahead to use AIC to select the best model which wasn't clear, as their difference was less than 2. I went ahead to use disgnostic in DHarMa to know which model fits best. 


################################# Final Project#######################
1. Benedicta_Final Project Code. R
2. Data=Fruit quality.csv
3. Here, I used the glmmTMB package to answer the questions of PDJ impact on weight, color, firmness and sugar content of apples across four cultivars. 
4. I did the predicted model plots as taught in class and also plotted a main plot with just the raw data
5. I used Gamma and Gaussian distribution where needed.

