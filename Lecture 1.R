# Nadir Nibras
# Lab for Lecture 1

rm(list=ls())

# install.packages("ISLR")
library(ISLR)

# set directory
setwd("C:/Users/nadir/Documents/Assignments & notes/Year 8- Teaching/BKN 599- Introduction to statistical learning for Biological Science in R")

x=matrix(data=c(1,2,3,4) , nrow=2, ncol =2)
# same as before
x=matrix (c(1,2,3,4) ,2,2)


# creating normally distributed 50 random numbers with a mean of 0
x= rnorm(5000)
y= rnorm(5000)

# Checking plots
plot(x,y, xlab=" this is the x-axis",ylab=" this is the y-axis",
     main=" Plot of X vs Y")

s= seq(0,1,length=10)

# producing contour maps
x=seq(-pi ,pi ,length =50)
y=x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour (x,y,f,nlevels =45, add=TZ  )
fa=(f-t(f))/2
contour (x,y,fa,nlevels =15)

# producing heat map
image(x,y,fa)

# 3d structure
persp(x,y,fa)


Auto=read.csv(" Auto.csv", header =T,na.strings ="?")


plot(Auto$horsepower, Auto$mpg )

attach(Auto)
plot(horsepower,mpg)

summary(Auto)
hist(mpg, col=2, breaks=15)


# Pg 54 Question 8
# a
college =read.csv("College.csv")

# b
column1= college[,1]
rownames(college)=column1

x=rownames(college)

fix(college)
college = college[,-1]


fix (college )

# c
summary(college)
pairs(college[,1:5])

plot(college$Private, college$Outstate)

Elite =rep("No",nrow(college ))
Elite [college$Top10perc >50]=" Yes"
Elite =as.factor(Elite)
college =data.frame(college ,Elite)

plot(college$Elite, college$Outstate)

par(mfrow=c(2,2))
hist(college$Outstate)
hist(college$Accept)
hist(college$Enroll)
hist(college$Room.Board)
