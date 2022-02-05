setwd("C:/Users/alans/Documents/613/A2")
library(tidyverse)
library(ggplot2)
#Exercise 1
#Calculate the correlation between Y and X: 0.1435
datind2009 <- read.csv("datind2009.csv")
datind2009=datind2009%>%filter(wage>0)
cor(datind2009$wage,datind2009$age)
#Calculate the coefficients on this regression: intercept: 14141.2, age: 231.0
X=as.matrix(cbind(1,datind2009$age))
Y=as.matrix(datind2009$wage)
beta=solve(t(X)%*%X)%*%t(X)%*%Y
beta
#Calculate the standard errors of ¦Â
#Using the standard formulas of the OLS: intercept: 645.2, age: 14.9
residuals=Y-X%*%beta
var=sum(residuals^2)/(nrow(X)-2)*solve(t(X)%*%X)
se_formula=sqrt(diag(var))
se_formula
#Using bootstrap with 49 and 499 replications respectively. Comment on the difference between the two strategies.
#49: intercept: 658.1, age: 15.2
#499: intercept: 658.7, age: 15.2
#The first strategy is based on one sample, while the second strategy is based on samples of the sample.
boot49=mat.or.vec(49,2)
set.seed(1)
for (i in 1:49){
  s=sample(1:nrow(X),replace=T)
  m=X[s,]
  residuals=Y-m%*%beta
  var=sum(residuals^2)/(nrow(m)-2)*solve(t(m)%*%m)
  boot49[i,]=sqrt(diag(var))
}
colMeans(boot49)
boot499=mat.or.vec(499,2)
set.seed(2)
for (i in 1:499){
  s=sample(1:nrow(X),replace=T)
  m=X[s,]
  residuals=Y-m%*%beta
  var=sum(residuals^2)/(nrow(m)-2)*solve(t(m)%*%m)
  boot499[i,]=sqrt(diag(var))
}
colMeans(boot499)
#Exercise 2
#Create a categorical variable ag, which bins the age variables into the following groups: ¡°18-25¡±, ¡°26-30¡±, ¡°31-35¡±, ¡°36-40¡±,¡°41-45¡±, ¡°46-50¡±,¡°51-55¡±, ¡°56-60¡±, and ¡°60+¡±.
library(readr)
ind05_09 <- do.call(rbind,list.files(pattern="datind200[5-9].csv")%>%lapply(read_csv))
ind10_18 <- do.call(rbind,list.files(pattern="datind201[0-8].csv")%>%lapply(read_csv))
ind05_18 <- rbind(ind05_09,ind10_18)
write.csv(ind05_18,"ind05_18.csv")
ind05_18=ind05_18%>%mutate(ag=case_when(age>=18&age<=25~"18-25",age>=26&age<=30~"26-30",age>=31&age<=35~"31-35",age>=36&age<=40~"36-40",age>=41&age<=45~"41-45",age>=46&age<=50~"46-50",age>=51&age<=55~"51-55",age>=56&age<=60~"56-60",age>60~"60+"))
#Plot the wage of each age group across years. Is there a trend?
#The wages have been increasing.
ggplot(ind05_18,aes(x=factor(year),y=wage,color=factor(ag)))+geom_boxplot()+facet_wrap(~ag,scales="free")
#Consider Yit = ¦ÂXit + ¦Ãt + eit. After including a time fixed effect, how do the estimated coefficients change?
#I am not sure whether I should create a dummy variable for each year. Through this assignment I just include the year variable for simplicity.
#Intercept: 9529.1, age: 306.0
#Compared to Exercise 1, the coefficient of intercept is smaller, and the coefficient of age is bigger.
ind05_18=ind05_18[complete.cases(ind05_18[,"age"]),]%>%filter(wage>0)
X=as.matrix(cbind(1,ind05_18$age,factor(ind05_18$year)))
Y=as.matrix(ind05_18$wage)
beta=solve(t(X)%*%X)%*%t(X)%*%Y
beta
#Exercise 3
#Exclude all individuals who are inactive.
datind2007 <- read.csv("datind2007.csv")
datind2007=datind2007%>%filter(empstat!="Inactive"&empstat!="Retired")
#Write a function that returns the likelihood of the probit of being employed.
datind2007$empstat[datind2007$empstat=="Unemployed"]=0
datind2007$empstat[datind2007$empstat=="Employed"]=1
y=as.numeric(datind2007$empstat)
x=datind2007$age
log_like=function(beta){
  xbeta=beta[1]+beta[2]*x
  p=pnorm(xbeta)
  p[p>0.999999]=0.999999
  p[p<0.000001]=0.000001
  -sum(y*log(p)+(1-y)*log(1-p))
}
#Optimize the model and interpret the coefficients.
#Intercept: 1.042, age: 0.007
#Age has a significantly positive effect on labor market participation.
#An increase in age leads to an increase in the predicted probability of being employed.
o=optim(c(0,0),log_like,method="BFGS",hessian=T)
o
sqrt(abs(diag(solve(-o$hessian))))
#Can you estimate the same model including wages as a determinant of labor market participation? Explain.
#Yes, as long as wage=0 is deleted.
#Intercept: -2.681, age: 0.0045, wage: 0.00066
#Age and wage both have a positive effect on labor market participation. Only wage is significant.
#An increase in age/wage leads to an increase in the predicted probability of being employed.
datind2007=datind2007%>%filter(wage>0)
y=as.numeric(datind2007$empstat)
x1=datind2007$age
x2=datind2007$wage
log_like2=function(beta){
  xbeta=beta[1]+beta[2]*x1+beta[3]*x2
  p=pnorm(xbeta)
  p[p>0.999999]=0.999999
  p[p<0.000001]=0.000001
  -sum(y*log(p)+(1-y)*log(1-p))
}
o2=optim(c(0,0,0),log_like2,method="BFGS",hessian=T)
o2
sqrt(abs(diag(solve(-o2$hessian))))
#Exercise 4
#Exclude all individuals who are inactive.
ind05_09 <- do.call(rbind,list.files(pattern="datind200[5-9].csv")%>%lapply(read_csv))
ind10_15 <- do.call(rbind,list.files(pattern="datind201[0-5].csv")%>%lapply(read_csv))
ind05_15 <- rbind(ind05_09,ind10_15)
write.csv(ind05_15,"ind05_15.csv")
ind05_15=ind05_15%>%filter(empstat!="Inactive"&empstat!="Retired")
#Write and optimize the probit, logit, and the linear probability models.
#I am not sure whether I should create a dummy variable for each year. Through this assignment I just include the year variable for simplicity.
ind05_15=ind05_15[complete.cases(ind05_15[,"age"]),]
ind05_15$empstat[ind05_15$empstat=="Unemployed"]=0
ind05_15$empstat[ind05_15$empstat=="Employed"]=1
#Probit: intercept: -2.14, age: -0.00865
y=as.numeric(ind05_15$empstat)
x1=ind05_15$age
x2=ind05_15$year
log_like3=function(beta){
  xbeta=beta[1]+beta[2]*x1+beta[3]*x2
  p=pnorm(xbeta)
  p[p>0.999999]=0.999999
  p[p<0.000001]=0.000001
  -sum(y*log(p)+(1-y)*log(1-p))
}
o3=optim(c(0,0,0),log_like3,method="BFGS",hessian=T)
o3
beta_p=o3$par
sqrt(abs(diag(solve(-o3$hessian))))
#Logit: intercept: -5.52, age: 0.0121
log_like4=function(beta){
  xbeta=beta[1]+beta[2]*x1+beta[3]*x2
  p=plogis(xbeta)
  p[p>0.999999]=0.999999
  p[p<0.000001]=0.000001
  -sum(y*log(p)+(1-y)*log(1-p))
}
o4=optim(c(0,0,0),log_like4,method="BFGS",hessian=T)
o4
beta_l=o4$par
sqrt(abs(diag(solve(-o4$hessian))))
#LPM: intercept: 4.37, age: 0.00233
X=as.matrix(cbind(1,x1,x2))
Y=as.matrix(y)
beta=solve(t(X)%*%X)%*%t(X)%*%Y
beta
residuals=Y-X%*%beta
var=sum(residuals^2)/(nrow(X)-2)*solve(t(X)%*%X)
se_formula=sqrt(diag(var))
se_formula
#Interpret and compare the estimated coefficients. How significant are they?
#Probit: An increase in age leads to a decrease in the predicted probability of being employed.
#Logit: An increase in age leads to an increase in the predicted probability of being employed.
#LPM: An increase of 1 in age leads to an increase of 0.00233 in the predicted probability of being employed.
#The coefficient of age is negative according to probit, while positive according to logit or LPM.
#All the coefficients are significant.
#Exercise 5
#Compute the marginal effect of the previous probit and logit models.
#Probit: -0.00149
X=as.matrix(cbind(1,x1,x2))
mean(dnorm(X%*%beta_p)*beta_p[2])
#Logit: 0.0011
mean(dlogis(X%*%beta_l)*beta_l[2])
#Construct the standard errors of the marginal effects.
#Probit: 0.00019
boot_p=mat.or.vec(49,1)
set.seed(3)
for (i in 1:49){
  s=sample(1:nrow(X),replace=T)
  m=X[s,]
  boot_p[i]=sd(dnorm(X%*%beta_p)*beta_p[2])
}
mean(boot_p)
#Logit: 0.00012
boot_l=mat.or.vec(49,1)
set.seed(4)
for (i in 1:49){
  s=sample(1:nrow(X),replace=T)
  m=X[s,]
  boot_l[i]=sd(dlogis(X%*%beta_l)*beta_l[2])
}
mean(boot_l)
