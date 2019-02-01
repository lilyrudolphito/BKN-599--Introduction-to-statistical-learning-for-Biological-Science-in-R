# BKN 599- Lecture 3- Homework Exercises- Jan 24th 2019

# clear workspace variables
rm(list = ls());
# it means ctrl+L. clear window
cat("\014") 
# close all plots
graphics.off() 


# Exercise 9 --------------------------------------------------------------

# a Scatterplot matrix
pairs(Auto)

Autowoname= Auto[-c(9)]
# b correlations 
cor(Autowoname)

# c 1
lm.fit=lm(mpg~.-name ,data=Auto )
summary (lm.fit)
# c 2
# All with p below 0.05
# c 3
# As year goes higher the mmpg goes up by 0.75 per year

# d 
par(mfrow=c(2,2))
plot(lm.fit)
# We see that the residuals get larger as the range increases

# e
lm.fit1=lm(mpg~.-name + mpg:cylinders ,data=Auto )
summary(lm.fit1)


# Exercise 10- Carseats -------------------------------------------------------------
# a)
lm.fit=lm(Sales~ Price+Urban+US ,data=Carseats )
summary (lm.fit)

# b) Define the interactions
# c) Write out the equation
# d) all exepct Urban yes
# e)
lm.fit=lm(Sales~ Price+US ,data=Carseats )
summary (lm.fit)

# f) e) fits it  lot better than a)
# g) Using the model from (e), obtain 95% confidence intervals for the coefficient(s).
# +_ 2 standard errors for each variable