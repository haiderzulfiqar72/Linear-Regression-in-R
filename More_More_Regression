---
title: "Take Home Task"
author: "Haider Zulfiqar"
date: "October 25, 2022"
output:
  word_document: default
  pdf_document: default
---

#Libraries
```{r}
library(ggplot2)
library(effects)
library(predict3d)
library(ggiraph)
library(ggeffects)
library(ggiraphExtra)
library(car)
library(data.table)
library(MASS)
```  

# Question 1
## Reading the Data
```{r}
BGS_Data <- read.table('BGSgirls.txt', header=T)
setwd("E:/Study Material/Masters/Studies/Semester 1/Regression Analysis - DATA.STAT.460/Take Home Task")

head(BGS_Data)
summary(BGS_Data)
```

## Fitting the Full Model
```{r}
lm_BGS<- lm(Ht18~Wt2+Ht2+Wt9+Ht9+Lg9+St9, data= BGS_Data)
summary(lm_BGS)
```
It can be seen that p-value of the F-statistic is < 2.2e-16, which is highly significant. This means that, at least, one of the predictor variables is significantly related to the outcome variable.
From the coefficients in the model estimates, it can be interpreted that changing in Wt2, Ht2, Ht9, and Lg9 are significantly associated to changes in Ht18, our reponse variable, while changes in Wt9 amd St9 are not significantly associated with Ht18.

## Dependency Plotting _ Extra
``` {r}
avPlots(lm_BGS)
plot(allEffects(lm_BGS, residuals=TRUE))
```  
Also known as partial-regression leverage plot, avPlots function shows unique partial relationship of response variable Ht18 to each predictor variable while keeping all other predictor variables constant.
Similar to avplots, allEffects does the same thing but differs in only aspect that all other predictors in the model are averaged over by being set equal to their means (or medians).

## fitting the reduced model where  ??1 = ??2 = 0
```{r}
reduced_lm1<- lm(Ht18~Wt9+Ht9+Lg9+St9, data= BGS_Data)
summary(reduced_lm1)
avPlots(reduced_lm1)
```  
It can be seen that p-value of the F-statistic is < 2.2e-16, which is highly significant. This again means that, at least, one of the predictor variables is significantly related to the outcome variable.
From the coefficients in the model estimates, it can be interpreted that changing in Ht9, and Lg9 are significantly associated to changes in Ht18, our reponse variable, while changes in Wt9 amd St9 are not significantly associated with Ht18.

## Comparison of the two models
```{r}
anova(reduced_lm1, lm_BGS)
```  
As we can see, the result indicate that the full or the more complex model has a relatively high p-value @ Pr(>F) (> .05); meaning the alternate hypothesis is weak, so we do not reject the null hypothesis. i.e the reduced model does lead to better clarity, thus significantly improved fit.

## fitting the 2nd reduced model where  ??1 = ??2 = ??3 = ??5 = ??6 = 0
```{r}
reduced_lm2<- lm(Ht18~Ht9, data= BGS_Data)
summary(reduced_lm2)
```
It can be seen that p-value of the F-statistic is < 2.2e-16, which is highly significant. This means that the only predictor variable here i.e. Ht9 is significantly related to the outcome variable i.e changing in Ht9 are significantly associated to changes in Ht18, our reponse variable.

## Comparision of the the above model with Full Model or the alternative hypothesis
```{r}
anova(reduced_lm2, lm_BGS)
```
As we can see, the result indicate that the full or the more complex model has a relatively low p-value @ Pr(>F) (< .05); meaning the null hypothesis is weak, so we reject the null hypothesis. i.e the reduced model does not lead to better results, thus we retain our initial full model as it gives significantly improved fit.

## 95 % confidence interval for the difference ??2 ??? ??4
```{r}
t.test(BGS_Data$Ht2, BGS_Data$Ht9, paired = TRUE)
```
95% CI comes out to be (-48.79034, -46.94395)

## Is HT2 a significant variable after adjusting for WT2?
```{r}
Wt2_reduced<- lm(Ht18~Wt2, data= BGS_Data) #null hypothesis --> Ht2=0
summary(Wt2_reduced)
Wt2_Ht2_reduced<- lm(Ht18~Wt2+Ht2, data= BGS_Data)
summary(Wt2_Ht2_reduced)
anova(Wt2_reduced,Wt2_Ht2_reduced)
```  
As P value is very small (<0.05), we reject the null hypotheses, hence Htw is a significant variable after adjusting for Wt2 


# Question 2
## Reading the Data
```{r}
setwd("E:/Study Material/Masters/Studies/Semester 1/Regression Analysis - DATA.STAT.460/Take Home Task")
hosp <-scan("risk.txt",what=list(id=0,length=0,age=0,risk=0,cult=0, xray=0, beds=0,school=0,region=0,patient=0,nurse=0,service=0))
hosp <- data.frame(hosp); hosp <- hosp[ ,-1]
hosp$school <- factor(hosp$school) # school become 'factor' variables.
hosp$region <- factor(hosp$region)
hosp<- data.table(hosp)

head(hosp)
summary(hosp)
attach(hosp)
```  

## Fitting the Regression Model
```{r}
lm_hosp<- lm(length ~ age+risk+region+beds+patient+nurse,data=hosp)
summary(lm_hosp)
```

## EDA _ Histogram of All Predictor Variables
```{r}
hist(length, main="Average Duration of Hospital Stay", xlab="Days", col=1)
hist(age, main="Average Age of Patients", xlab="Years", col=2)
hist(risk, main="Averaged Infection Risk", xlab="Percent", col=3)
hist(beds, main="Number of Beds", xlab="Beds", col=5)
hist(patient, main="Average Number of Patients a Day", xlab="Average Patients", col=6)
hist(nurse, main="Number of Full-Employed, Trained Nurses", xlab="Nurses", col=7)
``` 

## EDA _ Scatterplot Matrix and Correlation Matrix
```{r}
par(mfrow = c(1, 1))
plot(hosp[,-1]) 
round(cor(hosp[,-c('school','region')]),2)
``` 
There's a high correlation between patient and beds, nurse and beds, and nurse and patient

##Combining highly correlated variables and making new model
```{r}
pat.beds<- patient/beds #Number of Patients per Bed
pat.nurse<- patient/nurse #Number of Patient per Nurse
nurse.bed<- nurse/beds #Number of Nurse per Beds
lm_new_hosp<- lm(length ~ age+risk+region+beds+patient+nurse+pat.beds+pat.nurse+nurse.bed,data=hosp)
summary(lm_new_hosp)
par(mfrow = c(2, 2))
plot(lm_new_hosp)
``` 

##Box Cox Transformation and Cook's Distance Computation
```{r}
boxcox(lm(age~length, data=hosp)) #will take log transformation
boxcox(lm(risk~length, data=hosp)) #unchanged
boxcox(lm(beds~length, data=hosp)) #will take log transformation
boxcox(lm(patient~length, data=hosp)) ##will take log transformation
boxcox(lm(nurse~length, data=hosp)) ##will take log transformation
boxcox(lm(pat.beds~length, data=hosp)) #unchanged
boxcox(lm(pat.nurse~length, data=hosp)) #will take -ve square root 
boxcox(lm(nurse.bed~length, data=hosp)) #unchanged

I = influence.measures(lm_new_hosp)
summary(I)
cook = I$infmat[,10]
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")
``` 

###Transformed Model
```{r}
lm_transformed_hosp<- lm(length ~ log(age)+risk+log(beds)+log(patient)+log(nurse)+pat.beds+1/sqrt(pat.nurse)+nurse.bed+ risk*region, data=hosp)
summary(lm_transformed_hosp)
``` 

## Stepwise Regression
```{r}
full<- lm_transformed_hosp
mini<- lm(length~risk+risk*region)
reduced<- lm(length~1)
step.lm <- step(mini, scope= list(lower=reduced, upper =full), direction= "both")
summary(step.lm)
``` 
From above results, it looks like with log transformation, the best model seems to be with 8 variables, length ~ risk + region + log(patient) + nurse.bed + log(age) +  pat.beds + log(beds) + risk*region

## Best Model Fit
```{r}
best_m<- lm(length ~ risk + region + log(patient) + nurse.bed + log(age) +  pat.beds + log(beds) + risk*region, data=hosp)
summary(best_m)
plot(best_m)
``` 
Our best  model comes with adjusted R squered of ~0.6. 
Issues with our current model seems to be outliers and non normality in QQ-Plot. So opting to solve those via Robust Regression below:

##Robust Regression
```{r}
robust_reg<- rlm(length ~ risk + region + log(patient) + nurse.bed + log(age) +  pat.beds + log(beds) + risk*region, data=hosp)
summary(robust_reg)
plot(robust_reg)
``` 
Our issues have been resolved via robust regression

##3D Plot _ Extra -- While playing around 3D plotting in R, I came up with this interesting plot
```{r}
m_lm <- lm(length~risk+region*risk, data=hosp)
predict3d(m_lm,radius=0.1)
```
