setwd("C:/Users/alans/Documents/613/A3")
library(tidyverse)
#Exercise 1
#Number of students, schools, programs: 151697, 689, 32
#I'm excluding all observations with NAs for simplicity.
datstu <- read.csv("datstu_v2.csv")
datstu=datstu[complete.cases(datstu),]
datjss <- read.csv("datjss.csv")
datjss=datjss[complete.cases(datjss),]
datsss <- read.csv("datsss.csv")
datsss=datsss[complete.cases(datsss),]
datsss=datsss%>%distinct(schoolcode,.keep_all=T)
nrow(datstu)
nrow(datsss)
length(unique(c(datstu$choicepgm1,datstu$choicepgm2,datstu$choicepgm3,datstu$choicepgm4,datstu$choicepgm5,datstu$choicepgm6)))
#Number of choices (school, program): 2843
long_datstu=datstu%>%gather(school,schoolcode,schoolcode1:schoolcode6)%>%gather(program,choicepgm,choicepgm1:choicepgm6)
long_datstu$school=substr(long_datstu$school,11,11)
long_datstu$program=substr(long_datstu$program,10,10)
long_datstu=long_datstu%>%filter(school==program)
nrow(distinct(long_datstu,schoolcode,choicepgm))
#Number of students applying to at least one senior high schools in the same district to home: 112106
datstu_sss=merge(long_datstu,datsss,by="schoolcode")
nrow(distinct(datstu_sss%>%filter(jssdistrict==sssdistrict),V1.x))
#Number of students each senior high school admitted
ad1=datstu_sss%>%filter(school==rankplace)
size1=as.data.frame(table(ad1$schoolcode))
size1=size1%>%rename(schoolcode=Var1,size=Freq)
write.csv(size1,"size1.csv")
#The cutoff of senior high schools (the lowest score to be admitted)
cutoff1=ad1%>%group_by(schoolcode)%>%summarise(cutoff=min(score))
write.csv(cutoff1,"cutoff1.csv")
#The quality of senior high schools (the average score of students admitted)
quality1=ad1%>%group_by(schoolcode)%>%summarise(quality=mean(score))
write.csv(quality1,"quality1.csv")
#Exercise 2
dat=distinct(long_datstu,schoolcode,choicepgm)
dat=dat%>%mutate(choice=paste(schoolcode,choicepgm,sep="/"))
dat=merge(dat,datsss,by="schoolcode")
dat=dat%>%select(-V1,-schoolname)
ad2=ad1%>%mutate(choice=paste(schoolcode,choicepgm,sep="/"))
cutoff2=ad2%>%group_by(choice)%>%summarise(cutoff=min(score))
dat=merge(dat,cutoff2,by="choice")
quality2=ad2%>%group_by(choice)%>%summarise(quality=mean(score))
dat=merge(dat,quality2,by="choice")
size2=as.data.frame(table(ad2$choice))
size2=size2%>%rename(choice=Var1,size=Freq)
dat=merge(dat,size2,by="choice")
write.csv(dat,"dat.csv")
#Exercise 3
datdis=merge(datstu_sss,datjss,by="jssdistrict")
datdis=datdis%>%mutate(dis=sqrt((69.172*(ssslong-point_x)*cos(point_y/57.3))^2+(69.172*(ssslat-point_y))^2))
datdis=datdis%>%select(V1.x,schoolcode,choicepgm,dis)
write.csv(datdis,"datdis.csv")
#Exercise 4
dat_rev=long_datstu%>%mutate(scode_rev=substr(schoolcode,1,3))%>%select(-schoolcode)
dat_rev=dat_rev%>%mutate(pgm_rev=case_when(choicepgm=="General Arts"|choicepgm=="Visual Arts"~"Arts",choicepgm=="Business"|choicepgm=="Home Economics"~"Economics",choicepgm=="General Science"~"Science",T~"Others"))%>%select(-choicepgm)
dat_rev=dat_rev%>%mutate(choice_rev=paste(scode_rev,pgm_rev,sep="/"))
ad3=dat_rev%>%filter(school==rankplace)
cutoff3=ad3%>%group_by(choice_rev)%>%summarise(cutoff=min(score))
dat_rev=merge(dat_rev,cutoff3,by="choice_rev")
quality3=ad3%>%group_by(choice_rev)%>%summarise(quality=mean(score))
dat_rev=merge(dat_rev,quality3,by="choice_rev")
dat_rev=dat_rev%>%filter(score>=355)
write.csv(dat_rev,"dat_rev.csv")
#Exercise 5
install.packages("mlogit")
library(mlogit)
install.packages("nnet")
library(nnet)
install.packages("survival")
library(survival)
install.packages("mclogit")
library(mclogit)
install.packages("margins")
library(margins)
dat_rev=dat_rev%>%filter(school==1)
model1=multinom(choice_rev~score,data=dat_rev)
margin1=margins(model1)
n=nrow(dat_rev)
dat_uni=distinct(dat_rev,choice_rev,.keep_all=T)
m=nrow(dat_uni)
#Beta contains m-1 parameters for intercepts and m-1 parameters for scores.
like1=function(beta){
  mat_prob=mat.or.vec(n,m)
  mat_prob[,1]=1
  for (j in 2:m){
      mat_prob[,j]=exp(beta[j-1]+dat_rev$score*beta[j+m-2])
  }
  for (i in 1:n){
    mat_prob[i,]=mat_prob[i,]/sum(mat_prob[i,])
  }
  mat_prob[mat_prob>0.999999]=0.999999
  mat_prob[mat_prob<0.000001]=0.000001
  for (i in 1:n){
    for (j in 1:m){
      mat_prob[i,j]=mat_prob[i,j]^(dat_rev$choice_rev[i]==dat_uni$choice_rev[j])
    }
  }
  sum(mat_prob)
}
log_like1=function(beta){
  -log(like1(beta))
}
o1=optim(mat.or.vec(2*(m-1),1),log_like1,method="BFGS",hessian=T)
#It's taking really too long, so I give up.
#For the marginal effect, I suppose beta is the result computed by the optimization.
mat_beta=mat.or.vec(2,m)
mat_beta[,1]=0
for (j in 2:m){
  mat_beta[1,j]=beta[j-1]
  mat_beta[2,j]=beta[j+m-2]
}
mat_prob=mat.or.vec(n,m)
for (j in 1:m){
  mat_prob[,j]=exp(mat_beta[1,j]+dat_rev$score*mat_beta[2,j])
}
for (i in 1:n){
  mat_prob[i,]=mat_prob[i,]/sum(mat_prob[i,])
}
mat_prob[mat_prob>0.999999]=0.999999
mat_prob[mat_prob<0.000001]=0.000001
m_margin=mat.or.vec(n,m)
#Let m_margin[i,j]=d(pij)/d(xi).
for (i in 1:n){
  for (j in 1:m){
    m_margin[i,j]=mat_prob[i,j]*(mat_beta[2,j]-sum(mat_prob[i,]*mat_beta[2,]))
  }
}
#Exercise 6
#Since quality varies across choices, the conditional logit should be applied.
#Gamma contains m-1 parameters for intercepts and 1 parameter for quality.
like2=function(gamma){
  mat_prob=mat.or.vec(n,m)
  mat_prob[,1]=1
  for (j in 2:m){
    mat_prob[,j]=exp(gamma[j-1]+dat_uni$quality[j]*gamma[m])
  }
  for (i in 1:n){
    mat_prob[i,]=mat_prob[i,]/sum(mat_prob[i,])
  }
  mat_prob[mat_prob>0.999999]=0.999999
  mat_prob[mat_prob<0.000001]=0.000001
  for (i in 1:n){
    for (j in 1:m){
      mat_prob[i,j]=mat_prob[i,j]^(dat_rev$choice_rev[i]==dat_uni$choice_rev[j])
    }
  }
  sum(mat_prob)
}
log_like2=function(gamma){
  -log(like2(gamma))
}
o2=optim(mat.or.vec(m,1),log_like2,method="BFGS",hessian=T)
#Again, it's taking really too long, so I give up.
#For the marginal effect, I suppose gamma is the result computed by the optimization.
mat_prob=mat.or.vec(n,m)
mat_prob[,1]=1
for (j in 2:m){
  mat_prob[,j]=exp(gamma[j-1]+dat_uni$quality[j]*gamma[m])
}
for (i in 1:n){
  mat_prob[i,]=mat_prob[i,]/sum(mat_prob[i,])
}
mat_prob[mat_prob>0.999999]=0.999999
mat_prob[mat_prob<0.000001]=0.000001
for (i in 1:n){
  for (j in 1:m){
    mat_prob[i,j]=mat_prob[i,j]^(dat_rev$choice_rev[i]==dat_uni$choice_rev[j])
  }
}
mat_margin=array(0,c(n,m,m))
#Let c_margin[i,j,k]=d(pij)/d(xik).
c_margin[i,j,k]=mat_prob[i,j]*((j==k)-mat_prob[i,k])*gamma[m]
#Exercise 7
#I think either of the models is appropriate. Maybe using the second model is more interesting, since the independent variable varies across choices.
dat_rev=dat_rev%>%filter(pgm_rev!="Others")
n=nrow(dat_rev)
dat_uni=distinct(dat_rev,choice_rev,.keep_all=T)
m=nrow(dat_uni)
like2=function(gamma){
  mat_prob=mat.or.vec(n,m)
  mat_prob[,1]=1
  for (j in 2:m){
    mat_prob[,j]=exp(gamma[j-1]+dat_uni$quality[j]*gamma[m])
  }
  for (i in 1:n){
    mat_prob[i,]=mat_prob[i,]/sum(mat_prob[i,])
  }
  mat_prob[mat_prob>0.999999]=0.999999
  mat_prob[mat_prob<0.000001]=0.000001
  for (i in 1:n){
    for (j in 1:m){
      mat_prob[i,j]=mat_prob[i,j]^(dat_rev$choice_rev[i]==dat_uni$choice_rev[j])
    }
  }
  sum(mat_prob)
}
log_like2=function(gamma){
  -log(like2(gamma))
}
o2=optim(mat.or.vec(m,1),log_like2,method="BFGS",hessian=T)
#Again, suppose gamma is the result computed by the optimization.
mat_prob=mat.or.vec(n,m)
mat_prob[,1]=1
for (j in 2:m){
  mat_prob[,j]=exp(gamma[j-1]+dat_uni$quality[j]*gamma[m])
}
for (i in 1:n){
  mat_prob[i,]=mat_prob[i,]/sum(mat_prob[i,])
}
mat_prob[mat_prob>0.999999]=0.999999
mat_prob[mat_prob<0.000001]=0.000001
#I expect that the relative probabilities of the remaining choices don't change.