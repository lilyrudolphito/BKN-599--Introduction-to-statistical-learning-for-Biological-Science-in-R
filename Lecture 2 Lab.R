# BKN 599- Lecture 2- Jan 17th 2019

# clear workspace variables
rm(list = ls());
# it means ctrl+L. clear window
cat("\014") 
# close all plots
graphics.off() 

# package on house value in neighborhood
# install.packages("MASS")
library(MASS)

# 1 way to examine the dataframe
fix(Boston)

# get names of the variables
names(Boston)

plot(Boston$lstat, Boston$medv)

# syntax: lm(y∼x,data),y is the response, x is the predictor
lm.fit =lm(medv~lstat, Boston )

# OR
attach (Boston )
lm.fit =lm(medv~lstat)

lm.fit
summary(lm.fit)


# We can use the names() function in order to find out what other pieces
# names() of information are stored in lm.fit.

names(lm.fit)

# or we can use the coeff() function to find the coefficients
coef(lm.fit)

abline(coef(lm.fit), col= "red")

# find confidence intervals
confint(lm.fit)

# predict confidence intervals of medv for a given value of lstat
predict (lm.fit ,data.frame(lstat =(c(5 ,10 ,15) )), interval ="confidence")

# Prediction intervals must account for both the uncertainty in knowing the value of the 
# population mean, plus data scatter. So a prediction interval is always wider than 
# a confidence interval.
predict (lm.fit ,data.frame(lstat =(c(5 ,10 ,15) )), interval ="prediction")

# plotting linear regression fit
plot(lstat ,medv)
abline (lm.fit, col= "red") #abline(a,b) draws any line with intercept a and slope b


#different use of abline
plot(lstat ,medv ,pch ="+")


# Splitting the screen into 4
par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(1,1))
plot(lm.fit)

# A residual is the difference between the observed y-value (from scatter plot) 
# and the predicted y-value (from regression equation line). 

# compute residuals from linear regression fit
plot(predict (lm.fit), residuals (lm.fit))

# function to plot residuals against fitted values
plot(predict (lm.fit), rstudent (lm.fit))

# leverage statistics and hat values
plot(hatvalues (lm.fit ))
which.max (hatvalues (lm.fit))

# Lab 3.6.7 Creating a function ---------------------------------------------------------------

# The { symbol informs R that multiple commands are about to be input.
# The } symbol informs R that no further commands will be entered.

LoadLibraries=function (){
  library (ISLR)
  library (MASS)
  print (" The libraries have been loaded .")
}

# Use the function
LoadLibraries()


# loading class plots -----------------------------------------------------

library(rstudioapi)
# set directory to R script folder
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

advertising= read.csv("Advertising.csv")
lm.fit =lm(sales~TV ,data=advertising )

summary(lm.fit)
plot(advertising$TV ,advertising$sales)
abline(lm.fit,col="red")

# producing contour maps
x=advertising$TV
y=advertising$sales

Beta0= seq(5,9,length=200)
Beta1= seq(0.02,0.07,length=200)

# Creating empty matrix
RSS= matrix(0, nrow = 200, ncol = 200)

# generating RSS values for different betas
for (i in 1:200)  {
  for (j in 1:200)
    {
    RSS[i,j]= sum((y- Beta0[i] - (Beta1[j])*x)^2)
    }
  }
# Contour
contour(Beta0,Beta1,RSS)

# producing heat map
image(Beta0,Beta1,RSS)

# 3d structure
persp(Beta1,Beta0,RSS)


# ploting figure 3.3 ------------------------------------------------------

values=matrix(0,nrow=10,ncol=2)

for (i in 1:10){
  # generate random x values
  x=runif(100,min=-2,max=2)
  
  # calculate y based on function + random noise 
  randnoise=runif(100,min=-5,max=5)
  y=2+3*x+randnoise
  
  # calculate B0 and B1 based on those x values
  linmod=lm(y~x)
  plot(x,y)
  
  values[i,]=coef(linmod)
  abline(2,3,lwd=1,col="red")
  abline(linmod,lwd=1,col="blue")
}

#plot()
# type= "n" makes the points in the scagtter plot disappear
plot(x,y,type='n')
abline(2,3,lwd=1,col="red")
for(i in 1:10){
  # lwd is just a parameter for setting the width of the plot and col is setting the color of the line
  abline(values[i,1],values[i,2],lwd=1,col="blue")
}