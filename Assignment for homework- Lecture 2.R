  # BKN 599- Lecture 2- Jan 17th 2019

# clear workspace variables
rm(list = ls());
# it means ctrl+L. clear window
cat("\014") 
# close all plots
graphics.off() 


library(ISLR)

# Use the lm() function to perform a simple linear regression with
# mpg as the response and horsepower as the predictor. Use the
# summary() function to print the results. Comment on the output.

  lm.fit =lm(mpg~horsepower ,data=Auto )
  summary(lm.fit)
  
# Predictions
  predict (lm.fit ,data.frame(horsepower=98), interval ="confidence")
  predict (lm.fit ,data.frame(horsepower=98), interval ="prediction")

# Plot
  attach(Auto)
  plot(horsepower,mpg)
  abline (lm.fit, col= "red") #abline(a,b) draws any line with intercept a and slope b
  
# Splitting the screen into 4
  par(mfrow=c(2,2))
  plot(lm.fit)
  par(mfrow=c(1,1))
  