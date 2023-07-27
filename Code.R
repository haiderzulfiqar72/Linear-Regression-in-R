---
title: "Lab_Excercise 3"
author: "Haider Zulfiqar"
date: "September 16, 2022"
output:
  word_document: default
  html_document:
    df_print: paged
---
  
# Question 1
## 1 (a) Loading the data and Plotting the Regression Line
    ```{r}
library(ggplot2)

hospital_data <- read.table('Hospital.txt',sep='\t', header=T)
getwd()

head(hospital_data)
```

```{r}
#Linear Plot via ggplot library
ggplot(hospital_data, aes(x = Stay, y = InfctRsk)) + geom_point(alpha = 0.6) +
  stat_smooth(method = "lm", color = "red", se = FALSE)

#Linear Model Summary
hospital_lm <- lm(InfctRsk~Stay, data=hospital_data)

#Plot and Fitted Line
plot(InfctRsk~Stay, data= hospital_data)
abline(hospital_lm, col="red")
```

## 1 (b) Estimates and Standard Error
```{r}
summary(hospital_lm)
```  

A strong linear relationship exists between the two estimates.

## 1 (c) Confidence Interval for the parameters

```{r}
#For beta0, Confidence Interval
lower.bound<- hospital_lm$coefficients[1]-qt(0.975,56)*0.95580
upper.bound<- hospital_lm$coefficients[1]+qt(0.975,56)*0.95580
print(c(lower.bound,upper.bound))

#For beta1, Confidence Interval
lwr.bound<- hospital_lm$coefficients[2]-qt(0.975,56)*0.09416
upr.bound<- hospital_lm$coefficients[2]+qt(0.975,56)*0.09416
print(c(lwr.bound,upr.bound))
```  

95% of the intercept values lie between -3.07 and 0,75 while they lie between -1.35 and -0.97 for slop estimate

## 1 (d) Error Variability Unbiased Estimator
```{r}
anova(hospital_lm)
```  

Unbiased estimate i.e MSE= 1.050

# Question 2
## 2 (a) Loading the data and Plotting the Regression Line
```{r}
sign_data <- read.table('Sign.txt',sep='\t', header=T)
getwd()

head(sign_data)

#Linear Plot via ggplot library
ggplot(sign_data, aes(x = age, y = distance)) + geom_point(alpha = 0.6) +
  stat_smooth(method = "lm", color = "red", se = FALSE)

#Linear Model 
sign_lm <- lm(distance~age, data=sign_data)

#Plot and Fitted Line
plot(distance~age, data= sign_data)
abline(sign_lm,col="orange")

#Linear Model Summary
summary(sign_lm)
```

## 2 (b) Confidence Interval at Age 75 Years 
```{r}
sign_CI <- predict(sign_lm, interval= "c", newdata= data.frame(age=75))
sign_CI
```

## 2 (c) Prediction Interval at Age 75 Years 
```{r}
sign_PI <- predict(sign_lm, interval= "p", newdata= data.frame(age=75))
sign_PI
```

## 2 (d) Overlay Plot
```{r}
#Linear Model Plot
plot(distance~age, data= sign_data, ylim = c(200, 600))
abline(sign_lm$coefficients,col="red")

#CI Plot 
lines(x=c(75,75),y=sign_CI[2:3],lwd=3, col="blue")
lines(x=c(72,78),y=rep(sign_CI[2],2),lwd=3, col="blue")
lines(x=c(72,78),y=rep(sign_CI[3],2),lwd=3, col="blue")

#PI Plot
lines(x=c(75,75),y=sign_PI[2:3],lwd=4, col="orange")
lines(x=c(72,78),y=rep(sign_PI[2],2),lwd=3, col="orange")
lines(x=c(72,78),y=rep(sign_PI[3],2),lwd=3, col="orange")
```  

The blue line i.e confidence interval represents the range of values likely to contain the true mean value for distance (response variable) while the yellow line i.e prediction interval represents the range of values likely to contrain the true value for distance for a newer observation

## 2 (e) #Diagnostics Plot __ Residual Plots
```{r}
par(mfrow=c(2,2))
plot(sign_lm)
```  

All assumptions for the linear model seem to be true from the above diagnostic plots.

# Question 3
## 1 (a) Loading the data and Plotting the Regression Line
```{r}
Stop_data <- read.table('Stop.txt',sep='\t', header=T)
getwd()

head(Stop_data)
summary(Stop_data)

#Linear Plot via ggplot library
ggplot(Stop_data, aes(x = Speed, y = StopDist)) + geom_point(alpha = 0.6) +
  stat_smooth(method = "lm", color = "red", se = FALSE)

#Linear Model Summary
Stop_lm <- lm(StopDist~Speed, data=Stop_data)

#Plot and Fitted Line
plot(StopDist~Speed, data=Stop_data)
abline(Stop_lm, col="blue")
```

## 3 (b) Diagnostics Plot __ Residual Plots
```{r}
par(mfrow=c(2,2))
plot(Stop_lm)
``` 

There seem to be an inrease in variance with the increase in speed and residals seem to be spread out for that as well. Model assumtion for linearity still hold though.

## 3 (c) Y Transformation -- Square Root 'Y'
```{r}
Stop_lm_transformed <- lm(sqrt(StopDist)~Speed, data=Stop_data)
summary(Stop_lm_transformed)
```

## 3 (d) 95% PI with Transformed Y at X= 10, 20, .. , 50
```{r}
updated_speed<- data.frame(Speed=seq(10,50,10))
stop_PI <- predict(Stop_lm_transformed, interval= "p", newdata= updated_speed)
stop_PI
```

## 3 (e) Undo Y Transformation
```{r}
original_scale<- stop_PI^2
original_scale
```
