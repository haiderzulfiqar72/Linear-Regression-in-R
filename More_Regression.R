---
title: "Lab_Excercise 7"
author: "Haider Zulfiqar"
date: "October 15, 2022"
output:
  pdf_document: default
  word_document: default
---
  
# Question 1
## Reading the Data
```{r}
library(MASS)

SAT_Data <- read.table('SAT_Data.txt', header=T)
getwd()

head(SAT_Data)
```

## Exploratory Data Analysis
```{r}
attach(SAT_Data)
par(mfrow = c(1, 1))

hist(sat, main="Histogram of SAT Scores", xlab="Mean SAT Score", col=1)
hist(takers, main="Histogram of Takers", xlab="Percentage of Students Tested", col=2)
hist(income, main="Histogram of Income", xlab="Mean Household Income", col=3)
hist(years, main="Histogram of Years", xlab="Mean Years", col=4)
hist(public, main="Histogram of Public Schools Percentage", xlab="Percentage of Students", col=5)
hist(expend, main="Histogram of Expenditures", xlab="Expenditures", col=6)
hist(rank, main="Histogram of Class Rank", xlab="Median Class Ranking Percentile", col=7)

par(mfrow = c(1, 1))
plot(SAT_Data[,-1]) #scatterplot matrix

round(cor(SAT_Data[,-1]),2) #correlation matrix
```

There's negative high correlation between takers and sat, rank and takers. For sat and rank, there is a strong correlation.

## Fitting Regression Line and Residual plot
``` {r}
# Fitting Regression Line
lm_sat<- lm(sat~takers+income+years+expend+public+expend+rank, data= SAT_Data)
summary(lm_sat)
par(mfrow = c(2, 2))
plot(lm_sat) #residuals plot
```  


## Box Cox Transformation and Cook's Distance Computation

```{r}
library(MASS)
boxcox(lm(takers~sat))#boxcox transformation

par(mfrow = c(1, 2))
plot(takers, sat)
plot(log(takers), sat)

rstand= rstandard(lm_sat) #standard
rstud= rstudent(lm_sat)   #studentized

plot(fitted(lm_sat), rstud, xlab= "Fitted Value", ylab= "Studenzied Resisdual")
abline(h=0)

I = influence.measures(lm_sat)
summary(I)
cook = I$infmat[,10]
plot(cook,type="h",lwd=3,col="red", ylab = "Cookâs Distance")
```  


## AIC
```{r}
par(mfrow = c(1, 1))
AIC = c(346.7, 331.66, 323.9)
ommitted = c(325.85, 327.8)
plot(1:3, AIC, xlim=c(1,5),type="l",xaxt="n",xlab=" ",main="AIC Plot")
points(1:3, AIC, pch = 19)
points(4:5, ommitted)
axis(1, at=1:5, labels = c("log(takers)+ rank","expend","years","income","public"))
abline(h = 323.9, lty = 2)
```  

## Stepwise Regression
```{r}
full<- lm(sat~log(takers)+rank+expend+years+income+public)
mini<- lm(sat~log(takers)+rank)
reduced<- lm(sat~expend+years+income+public)

#stepwise regression
step(full, scope= list(lower=mini, upper =full), direction= "backward")
step(mini, scope= list(lower=mini, upper =full), direction= "both")
step(full, scope= list(lower=mini, upper =full), direction= "both")
```
From above results, it looks like with log transformation, the best model seems to be with 4 variables (sat~log(takers)+rank+expend+years)

## Best Model Fit
```{r}
best_m<- lm(sat~log(takers)+rank+expend+years, data=SAT_Data)
plot(best_m)
```

## Ridge Regression
```{r}
library(MASS)
takers_l<- log(takers)
predictors<- cbind(takers_l, years, public, expend, rank)
predictors<- scale(predictors)
sat_scaled<- scale(sat)

lambda<- seq(0,10,0.25)
length((lambda))

sat_ridge<- lm.ridge(sat_scaled~predictors, lambda=lambda)
which(sat_ridge$GCV== min(sat_ridge$GCV))

plot(lambda, sat_ridge$GCV, type='l')
points(lambda, sat_ridge$GCV, pch=19)

dim(sat_ridge$coef)
round(sat_ridge$coef[,10],4)

sat_cgv<- lm.ridge(sat_scaled~predictors,lambda=2.25)

par(mfrow = c(1, 1))
plot(lambda,sat_ridge$coef[1,], type='l', col=1, xlab= 'lamda', ylab='coefficients', main='Ridge Regression', ylim=c(min(sat_ridge$coef),max(sat_ridge$coef)))

```

## 2 Lasso Regression
```{r}
library(lars)

sat_lasso<- lars(x= predictors, y= sat_scaled)
sat_lasso
plot(sat_lasso)
sat_lasso$Cp

beta_sat<- sat_lasso$beta
plot.lars(sat_lasso, xvar='df', plottype= 'Cp')

n = 50
s.df.lasso<- summary(sat_lasso)$Df  
s.SSE.lasso<- summary(sat_lasso)$Rss
s.BIC<- n*log(s.SSE.lasso)+s.df.lasso*log(50)
which.min(s.BIC)
```  

# Question 2
## Reading the Data
```{r}
setwd("E:/Study Material/Masters/Studies/Semester 1/Regression Analysis - DATA.STAT.460/Lab Excercises/Lab Excercise 7")
Home_Data <- read.table('Home_Data.txt', header=T)
getwd()

head(Home_Data)
attach(Home_Data)
```  

## OLS Plot and Diagnoistics
```{r}
y= Home_Data$Price
x1= Home_Data$homeft2
x2= Home_Data$lotft2

home_lm<- lm(y~x1+x2) #OLS

par(mfrow=c(2,2))
plot(home_lm) 
```

## WLS
```{r}
home_wls<- lm(y~x1+x2, weight= 1/home_lm$fit) #WLS
summary(home_wls)
``` 
