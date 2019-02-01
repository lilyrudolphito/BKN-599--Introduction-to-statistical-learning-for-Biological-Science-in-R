# BKN 599- Lecture 3- Jan 24th 2019

# clear workspace variables
rm(list = ls());
# it means ctrl+L. clear window
cat("\014") 
# close all plots
graphics.off() 

# Lab 3.6.3- Multiple Linear Refgression ---------------------------------------------------------------

# Using multiple variables
lm.fit =lm(medv~lstat+age ,data=Boston )
summary (lm.fit)

# Using all 13 variables for boston datasetdatasets
lm.fit =lm(medv~.,data=Boston )
summary (lm.fit)

# Finding particular aspects of the summary
summary(lm.fit)$r.sq
# Finding RSE or approx. the standard deviation of data from fit
summary(lm.fit)$sigma

# finding fit over all but 1 variables
lm.fit1=lm(medv~.-age ,data=Boston )
summary (lm.fit1)

# This can be also acheived with the update function
lm.fit1=update (lm.fit , ~.-age)


# Lab 3.6.4- Interaction terms -------------------------------------------------------------------

lm.fit =lm(medv~ lstat+age+lstat:age ,data=Boston )
summary (lm.fit)

lm.fit =lm(medv~lstat*age ,data=Boston )
summary (lm.fit)


# Lab 3.6.5 Non-linear transformations of the predictors ----------------------------------------

lm.fit2=lm(medv~lstat +I(lstat ^2))   #I() inhibits conversion of variable to a different type
summary (lm.fit2)

# Notice that the R squared value is improved for the quadratic fit

plot(lstat,medv)


lm.fit =lm(medv~lstat)
anova(lm.fit ,lm.fit2)

# Anova test results state that these 2 models are significantly different

# changing plots to 4 plots
par(mfrow=c(2,2))
# plotting the model with quadratic fit of lstat
plot(lm.fit)
plot(lm.fit2)
# There is little discernible pattern in the residuals if we include the quadratic model

# We can use the poly commant so we don't have to use ^2 ^3 and so on
lm.fit5=lm(medv~poly(lstat ,5))
summary (lm.fit5)

# ALong with polynomial transformations, we can also do other transformations (using log, etc)
summary (lm(medv~log(rm),data=Boston ))


# Lab 3.6.6 Using ISLR data for quadratic fits ------------------------------------
library(ISLR)
# See the carseats sales data
fix(Carseats)

# Summary with 2 specific interaction terms included
lm.fit =lm(Sales~.+ Income :Advertising +Price :Age ,data=Carseats )
summary (lm.fit)

# Understanding how dummy variables are created from the shelveloc categorical variablevariable.names
contrasts(Carseats$ShelveLoc)

# code for changing the baseline variable in categorical variables
Carseats$ShelveLoc <- relevel(Carseats$ShelveLoc, ref = "Medium")

lm.fit2= lm(Sales~ShelveLoc, Carseats)
summary(lm.fit2)

# Anova test between continous and categorical variable
x<-aov(Sales~ShelveLoc, Carseats)
summary(x)
plot(x)
