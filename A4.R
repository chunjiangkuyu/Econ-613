setwd("C:/Users/alans/Documents/613/A4")
library(tidyverse)
#Exercise 1
#Create additional variable for the age of the agent ¡±age¡±, total work experience measured in years ¡±work exp¡±.
dat <- read.csv("dat_A4.csv")
dat=dat%>%mutate(age=2019-KEY_BDATE_Y_1997)
dat=dat%>%mutate(work_exp=rowSums(dat[18:28],na.rm=T)/52)

#Create additional education variables indicating total years of schooling from all variables related to education (eg, ¡±BIOLOGICAL FATHERS HIGHEST GRADE COMPLETED¡±) in our dataset.
#I assume a PhD/professional degree takes five years.
dat$edu=recode(dat$YSCH.3113_2019,"1"=0,"2"=12,"3"=12,"4"=14,"5"=16,"6"=18,"7"=21,"8"=21)
dat$edu[is.na(dat$edu)]=0
dat$edu_biodad=dat$CV_HGC_BIO_DAD_1997
dat$edu_biodad[dat$edu_biodad==95|is.na(dat$edu_biodad)]=0
dat$edu_biomom=dat$CV_HGC_BIO_MOM_1997
dat$edu_biomom[dat$edu_biomom==95|is.na(dat$edu_biomom)]=0
dat$edu_resdad=dat$CV_HGC_RES_DAD_1997
dat$edu_resdad[dat$edu_resdad==95|is.na(dat$edu_resdad)]=0
dat$edu_resmom=dat$CV_HGC_RES_MOM_1997
dat$edu_resmom[dat$edu_resmom==95|is.na(dat$edu_resmom)]=0

#Provide the following visualizations.
#Plot the income data (where income is positive) by i) age groups, ii) gender groups and iii) number of children
dat%>%filter(YINC_1700_2019>0)%>%ggplot(aes(x=factor(age),y=YINC_1700_2019))+geom_boxplot()+xlab("Age")+ylab("Income")
dat%>%filter(YINC_1700_2019>0)%>%ggplot(aes(x=factor(KEY_SEX_1997),y=YINC_1700_2019))+geom_boxplot()+xlab("Gender£º1 = Male, 2 = Female")+ylab("Income")
dat%>%filter(YINC_1700_2019>0)%>%ggplot(aes(x=factor(CV_BIO_CHILD_HH_U18_2019),y=YINC_1700_2019))+geom_boxplot()+xlab("Number of Children")+ylab("Income")

#Table the share of ¡±0¡± in the income data by i) age groups, ii) gender groups, iii) number of children and marital status
age_income=as.data.frame.matrix(table(dat$age,dat$YINC_1700_2019))
age_income=age_income%>%mutate(share=`0`/rowSums(age_income))
write.csv(age_income$share,"share_age.csv")
gender_income=as.data.frame.matrix(table(dat$KEY_SEX_1997,dat$YINC_1700_2019))
gender_income=gender_income%>%mutate(share=`0`/rowSums(gender_income))
write.csv(gender_income$share,"share_gender.csv")
children_income=as.data.frame.matrix(table(dat$CV_BIO_CHILD_HH_U18_2019,dat$YINC_1700_2019))
children_income=children_income%>%mutate(share=`0`/rowSums(children_income))
write.csv(children_income$share,"share_children.csv")
marital_income=as.data.frame.matrix(table(dat$CV_MARSTAT_COLLAPSED_2019,dat$YINC_1700_2019))
marital_income=marital_income%>%mutate(share=`0`/rowSums(marital_income))
write.csv(marital_income$share,"share_marital.csv")

#interpret the visualizations from above
#Men have higher income than women. 
#Those having 1-3 children have higher income.
#Men are more likely to have 0 income. 
#Those having no child are more likely to have 0 income. 
#Separated people are more likely to have 0 income.

#Exercise 2
#Specify and estimate an OLS model to explain the income variable (where income is positive).
ols=lm(YINC_1700_2019~age+work_exp+edu+edu_biodad+edu_biomom+edu_resdad+edu_resmom,data=dat%>%filter(YINC_1700_2019>0))
summary(ols)

#Interpret the estimation results
#One more year of work experience leads to an increase of 1029 in predicted income.
#One more year of education leads to an increase of 1980 in predicted income.
#Dads' education has greater impact on income than moms' education.

#Explain why there might be a selection problem when estimating an OLS this way
#Only the incomes of those in the labor market are observed.
#Maybe someone with 0 income was offered a low-income job and rejected it. 
#The missingness isn't random, which leads to sample selection bias.

#Explain why the Heckman model can deal with the selection problem.
#The Heckman model estimates a Probit model for missingness and calculates the inverse Mills ratio, which is then included in the linear regression as an additional control variable to eliminate sample selection bias. 

#Estimate a Heckman selection model. Interpret the results from the Heckman selection model and compare the results to OLS results. Why does there exist a difference?
dat=dat%>%mutate(ob=(YINC_1700_2019>0))
dat$ob[is.na(dat$ob)]=F
#It seems to me that labor force participation depends on age, work experience, education and number of children.
dat$CV_BIO_CHILD_HH_U18_2019[is.na(dat$CV_BIO_CHILD_HH_U18_2019)]=0
heckman_probit=glm(ob~age+work_exp+edu+CV_BIO_CHILD_HH_U18_2019,family=binomial(link="probit"),data=dat)
summary(heckman_probit)
dat=dat%>%mutate(imr=dnorm(heckman_probit$linear.predictors)/pnorm(heckman_probit$linear.predictors))
heckman_ols=lm(YINC_1700_2019~age+work_exp+edu+edu_biodad+edu_biomom+edu_resdad+edu_resmom+imr,data=dat%>%filter(YINC_1700_2019>0))
summary(heckman_ols)
#Check my result.
install.packages("sampleSelection")
library(sampleSelection)
heckman=selection(selection=ob~age+work_exp+edu+CV_BIO_CHILD_HH_U18_2019,outcome=YINC_1700_2019~age+work_exp+edu+edu_biodad+edu_biomom+edu_resdad+edu_resmom,data=dat,method="2step")
summary(heckman)
#One more year of work experience leads to an increase of 795 in predicted income.
#One more year of education leads to an increase of 1648 in predicted income.
#The coefficients are smaller than those estimated by OLS.
#OLS overestimates the coefficients due to sample selection bias (those who could have low income are missing but not taken into account).

#Exercise 3
#Plot a histogram to check whether the distribution of the income variable. What might be the censored value here?
hist(dat$YINC_1700_2019)  
#The censored value is 100,000.

#Propose a model to deal with the censoring problem.
#The Tobit model.

#Estimate the appropriate model with the censored data
#Use the AER package to find the starting values of my optimization.
install.packages("AER")
library(AER)
tbt=tobit(YINC_1700_2019~age+work_exp+edu+edu_biodad+edu_biomom+edu_resdad+edu_resmom,left=-Inf,right=100000,data=dat%>%filter(YINC_1700_2019>0))
summary(tbt)
dat=dat%>%mutate(d=(YINC_1700_2019<100000))
d=(dat%>%filter(YINC_1700_2019>0))$d
y=(dat%>%filter(YINC_1700_2019>0))$YINC_1700_2019
x1=(dat%>%filter(YINC_1700_2019>0))$age
x2=(dat%>%filter(YINC_1700_2019>0))$work_exp
x3=(dat%>%filter(YINC_1700_2019>0))$edu
x4=(dat%>%filter(YINC_1700_2019>0))$edu_biodad
x5=(dat%>%filter(YINC_1700_2019>0))$edu_biomom
x6=(dat%>%filter(YINC_1700_2019>0))$edu_resdad
x7=(dat%>%filter(YINC_1700_2019>0))$edu_resmom
flike=function(par){
  xbeta=par[1]+par[2]*x1+par[3]*x2+par[4]*x3+par[5]*x4+par[6]*x5+par[7]*x6+par[8]*x7
  dn=dnorm((y-xbeta)/par[9])
  dn[dn>0.999999]=0.999999
  dn[dn<0.000001]=0.000001
  pn=pnorm((100000-xbeta)/par[9])
  pn[pn>0.999999]=0.999999
  pn[pn<0.000001]=0.000001
  -sum(d*log(dn/par[9])+(1-d)*log(1-pn))
}
o=optim(c(tbt$coefficients,28718),flike,method="BFGS")
o

#Interpret the results above and compare to those when not correcting for the censored data
#One more year of work experience leads to an increase of 1097 in predicted income.
#One more year of education leads to an increase of 2174 in predicted income.
#The coefficients are greater than those estimated by OLS.
#High wages have been decreased to 100,000 in the censored data.
#When censoring isn't corrected, the regression is flattened and the coefficients underestimated.

#Exercise 4
#Explain the potential ability bias when trying to explain to understand the determinants of wages
#Those who acquire more education may just be talented, so that they could have high income even without that much education.
#Likewise, talented people may be more likely to have more work experience and get married.

#Exploit the panel dimension of the data to propose a model to correct for the ability bias. Estimate the model using the following strategy.
pdat <- read.csv("dat_A4_panel.csv")
pdat=pdat%>%rename(CV_HIGHEST_DEGREE_EVER_EDT_1998=CV_HIGHEST_DEGREE_9899_1998)
pdat=pdat%>%rename(CV_HIGHEST_DEGREE_EVER_EDT_1999=CV_HIGHEST_DEGREE_9900_1999)
pdat=pdat%>%rename(CV_HIGHEST_DEGREE_EVER_EDT_2000=CV_HIGHEST_DEGREE_0001_2000)
pdat=pdat%>%rename(CV_HIGHEST_DEGREE_EVER_EDT_2001=CV_HIGHEST_DEGREE_0102_2001)
pdat=pdat%>%rename(CV_HIGHEST_DEGREE_EVER_EDT_2002=CV_HIGHEST_DEGREE_0203_2002)
pdat=pdat%>%rename(CV_HIGHEST_DEGREE_EVER_EDT_2003=CV_HIGHEST_DEGREE_0304_2003)
pdat=pdat%>%rename(CV_HIGHEST_DEGREE_EVER_EDT_2004=CV_HIGHEST_DEGREE_0405_2004)
pdat=pdat%>%rename(CV_HIGHEST_DEGREE_EVER_EDT_2005=CV_HIGHEST_DEGREE_0506_2005)
pdat=pdat%>%rename(CV_HIGHEST_DEGREE_EVER_EDT_2006=CV_HIGHEST_DEGREE_0607_2006)
pdat=pdat%>%rename(CV_HIGHEST_DEGREE_EVER_EDT_2007=CV_HIGHEST_DEGREE_0708_2007)
pdat=pdat%>%rename(CV_HIGHEST_DEGREE_EVER_EDT_2008=CV_HIGHEST_DEGREE_0809_2008)
pdat=pdat%>%rename(CV_HIGHEST_DEGREE_EVER_EDT_2009=CV_HIGHEST_DEGREE_0910_2009)
install.packages("panelr")
library(panelr)
long_pdat=long_panel(pdat,prefix="_",begin=1997,end=2019,label_location="end")
long_pdat=as.data.frame(long_pdat)
write.csv(long_pdat,"long_pdat.csv")
#I'll ignore 0 income, as those with 0 income are not in the labor market.
long_pdat$YINC.1700[long_pdat$YINC.1700==0]=NA
long_pdat=long_pdat%>%mutate(work_exp=(rowSums(long_pdat[10:16],na.rm=T)+rowSums(long_pdat[23:30],na.rm=T))/52)
#I assume a PhD/professional degree takes five years.
long_pdat$edu=recode(long_pdat$CV_HIGHEST_DEGREE_EVER_EDT,"0"=0,"1"=12,"2"=12,"3"=14,"4"=16,"5"=18,"6"=21,"7"=21)
long_pdat$edu[is.na(long_pdat$edu)]=0
long_pdat=long_pdat%>%mutate(MARRIED=(CV_MARSTAT_COLLAPSED==1))
long_pdat=long_pdat%>%mutate(SEPARATED=(CV_MARSTAT_COLLAPSED==2))
long_pdat=long_pdat%>%mutate(DIVORCED=(CV_MARSTAT_COLLAPSED==3))
long_pdat=long_pdat%>%mutate(WIDOWED=(CV_MARSTAT_COLLAPSED==4))

#Within Estimator.
mean_pdat=long_pdat%>%group_by(id)%>%summarise(mean_y=mean(YINC.1700,na.rm=T),mean_x1=mean(work_exp,na.rm=T),mean_x2=mean(edu,na.rm=T),mean_x3=mean(MARRIED,na.rm=T),mean_x4=mean(SEPARATED,na.rm=T),mean_x5=mean(DIVORCED,na.rm=T),mean_x6=mean(WIDOWED,na.rm=T))
long_pdat=merge(long_pdat,mean_pdat,by="id")
long_pdat=long_pdat%>%mutate(d_y=YINC.1700-mean_y,d_x1=work_exp-mean_x1,d_x2=edu-mean_x2,d_x3=MARRIED-mean_x3,d_x4=SEPARATED-mean_x4,d_x5=DIVORCED-mean_x5,d_x6=WIDOWED-mean_x6)
within_ols=lm(d_y~d_x1+d_x2+d_x3+d_x4+d_x5+d_x6,data=long_pdat)
summary(within_ols)

#Between Estimator
between_ols=lm(mean_y~mean_x1+mean_x2+mean_x3+mean_x4+mean_x5+mean_x6,data=mean_pdat)
summary(between_ols)

#Difference (any) Estimator
long_pdat=long_pdat%>%group_by(id)%>%mutate(fd_y=YINC.1700-lag(YINC.1700),fd_x1=work_exp-lag(work_exp),fd_x2=edu-lag(edu),fd_x3=MARRIED-lag(MARRIED),fd_x4=SEPARATED-lag(SEPARATED),fd_x5=DIVORCED-lag(DIVORCED),fd_x6=WIDOWED-lag(WIDOWED))
difference_ols=lm(fd_y~fd_x1+fd_x2+fd_x3+fd_x4+fd_x5+fd_x6,data=long_pdat)
summary(difference_ols)

#Interpret the results from each model and explain why different models yield different parameter estimates
#Within Estimator:
#One more year of work experience leads to an increase of 2296 in predicted income.
#One more year of education leads to an increase of 1055 in predicted income.
#Being married increases predicted income by 19825.
#Being separated increases predicted income by 15896.
#Being divorced increases predicted income by 20217.
#Being widowed increases predicted income by 12570.
#Between Estimator:
#One more year of work experience leads to an increase of 2805 in predicted income.
#One more year of education leads to an increase of 1067 in predicted income.
#Being married increases predicted income by 6940.
#First Difference Estimator:
#One more year of work experience leads to an increase of 502 in predicted income.
#One more year of education leads to a decrease of 71 in predicted income.
#Being married increases predicted income by 979.
#Being divorced increases predicted income by 2034.

#The estimates are different because the within estimator ignores the individual variation and the between estimator ignores the time variation.
