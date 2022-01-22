setwd("C:/Users/alans/Documents/613/A1")
install.packages("tidyverse")
library(tidyverse)
#Exercise 1
#Number of households surveyed in 2007: 10498
dathh2007 <- read.csv("dathh2007.csv")
nrow(dathh2007)
#Number of households with marital status ¡°Couple with kids¡± in 2005: 3374
dathh2005 <- read.csv("dathh2005.csv")
nrow(dathh2005%>%filter(mstatus=="Couple, with Kids"))
#Number of individuals surveyed in 2008: 25510
datind2008 <- read.csv("datind2008.csv")
nrow(datind2008)
#Number of individuals aged between 25 and 35 in 2016: 2765
datind2016 <- read.csv("datind2016.csv")
nrow(datind2016%>%filter(age>=25&age<=35))
#Cross-table gender/profession in 2009
datind2009 <- read.csv("datind2009.csv")
install.packages("crosstable")
library(crosstable)
datind2009$profession = factor(datind2009$profession)
gen_prof=crosstable(datind2009,cols="profession",by="gender")
write.csv(gen_prof,"gen_prof.csv")
#Distribution of wages in 2005 and 2019. Report the mean, the standard deviation, the inter-decile ratio D9/D1 and the Gini coefficient:
#2005: mean=22443, sd=18077, inter-decile=8.90, gini=0.377
#2019: mean=27579, sd=25107, inter-decile=13.86, gini=0.399
datind2005 <- read.csv("datind2005.csv")
datind2019 <- read.csv("datind2019.csv")
wage2005=na.omit(datind2005$wage)
wage2019=na.omit(datind2019$wage)
plot(density(wage2005))
plot(density(wage2019))
#Omit wage=0.
wage2005=wage2005[wage2005!=0]
wage2019=wage2019[wage2019!=0]
mean(wage2005)
mean(wage2019)
sd(wage2005)
sd(wage2019)
inter_decile=function(x){
  q=quantile(x,c(0.1,0.9))
  q[[2]]/q[[1]]
}
inter_decile(wage2005)
inter_decile(wage2019)
gini=function(x){
  x=sort(x)
  1-2/(length(x)+1)*sum(cumsum(x))/sum(x)
}
gini(wage2005)
gini(wage2019)
#Distribution of age in 2010. Plot an histogram. Is there any difference between men and women?
#There are fewer men around 30 years old. There are more elder men.
datind2010 <- read.csv("datind2010.csv")
hist(datind2010$age)
male2010=datind2010%>%filter(gender=="Male")
female2010=datind2010%>%filter(gender=="Female")
hist(male2010$age)
hist(female2010$age)
#Number of individuals in Paris in 2011: 3514
dathh2011 <- read.csv("dathh2011.csv")
datind2011 <- read.csv("datind2011.csv")
nrow(merge(dathh2011,datind2011,by="idmen")%>%filter(location=="Paris"))
#Exercise 2
#Read all individual datasets from 2004 to 2019. Append all these datasets.
install.packages("readr")
library(readr)
all_ind <- do.call(rbind,list.files(pattern="ind")%>%lapply(read_csv))
write.csv(all_ind,"all_ind.csv")
#Read all household datasets from 2004 to 2019. Append all these datasets.
all_hh <- list.files(pattern="hh")%>%lapply(read_csv)%>%bind_rows
write.csv(all_hh,"all_hh.csv")
#List the variables that are simultaneously present in the individual and household datasets: idmen, year
intersect(names(all_ind),names(all_hh))
#Merge the appended individual and household datasets.
all=inner_join(all_ind,all_hh,by=c("idmen","year"))
write.csv(all,"all.csv")
#Number of households in which there are more than four family members: 12436
s=0
for (y in 2004:2019){
  temp1=all%>%filter(year==y)
  temp2=as.data.frame(table(temp1$idmen))
  s=s+nrow(temp2[temp2$Freq>4,])
}
s
#Number of households in which at least one member is unemployed: 17241
s=0
for (y in 2004:2019){
  temp=all%>%filter(year==y,empstat=="Unemployed")
  s=s+length(unique(temp$idmen))
}
s
#Number of households in which at least two members are of the same profession: 7586
s=0
for (y in 2004:2019){
  temp1=all%>%filter(year==y)
  temp2=as.data.frame(table(temp1$idmen,temp1$profession))
  temp3=temp2[temp2$Freq>=2,1]
  s=s+length(unique(temp3))
}
s
#Number of individuals in the panel that are from household-Couple with kids: 209382
nrow(all%>%filter(mstatus=="Couple, with Kids"))
#Number of individuals in the panel that are from Paris: 51904
nrow(all%>%filter(location=="Paris"))
#Find the household with the most number of family members. Report its idmen.
#2007: 2207811124040100 with 14 members; 2010: 2510263102990100 with 14 members.
maxf=function(y){
  temp1=all%>%filter(year==y)
  temp2=as.data.frame(table(temp1$idmen))
  temp2[which.max(temp2$Freq),]
}
lapply(2004:2019,maxf)
#Number of households present in 2010 and 2011: 8984
data2010=all%>%filter(year==2010)
data2011=all%>%filter(year==2011)
length(intersect(data2010$idmen,data2011$idmen))
#Exercise 3
#Find out the year each household enters and exit the panel. Report the distribution of the time spent in the survey for each household.
#Mean=4.31, sd=2.64.
getenter=function(i){
  temp=all_hh%>%filter(idmen==i)
  min(temp$year)
}
getexit=function(i){
  temp=all_hh%>%filter(idmen==i)
  max(temp$year)+1
}
time_spent=sapply(unique(all_hh$idmen),getexit)-sapply(unique(all_hh$idmen),getenter)
plot(density(time_spent))
mean(time_spent)
sd(time_spent)
#Based on datent, identify whether or not a household moved into its current dwelling at the year of survey. Report the first 10 rows of your result and plot the share of individuals in that situation across years.
all_hh$year==all_hh$datent
(all_hh$year==all_hh$datent)[1:10]
moved=function(y){
  nrow(all%>%filter(year==y,datent==y))/ nrow(all%>%filter(year==y))
}
ind_moved=sapply(2004:2019,moved)
barplot(ind_moved,names.arg=2004:2019)
#Based on myear and move, identify whether or not household migrated at the year of survey. Report the first 10 rows of your result and plot the share of individuals in that situation across years.
#Here I am using move==2 to check migration after 2014, which leads to overestimation.
#(One alternative is to only consider households that are also in the panel in the previous year, which would lead to underestimation.)
(!is.na(all_hh$myear)&all_hh$myear==all_hh$year)|(!is.na(all_hh$move)&all_hh$move==2)
((!is.na(all_hh$myear)&all_hh$myear==all_hh$year)|(!is.na(all_hh$move)&all_hh$move==2))[1:10]
migrated=function(y){
  if(y<=2014){
    nrow(all%>%filter(year==y,myear==y))/nrow(all%>%filter(year==y))
  }else{
    nrow(all%>%filter(year==y,move==2))/nrow(all%>%filter(year==y))
  }
}
ind_migrated=sapply(2004:2019,migrated)
barplot(ind_migrated,names.arg=2004:2019)
#Mix the two plots you created above in one graph, clearly label the graph. Do you prefer one method over the other? Justify.
#I prefer the first method, since the other one involves "myear" and "move", which creates inconsistency.
barplot(rbind(ind_moved,ind_migrated),names.arg=2004:2019,beside=T,xlab="Year",ylab="Migration",legend.text=c("datent","myear/move"),args.legend=list(x="top"))
#For households who migrate, find out how many households had at least one family member changed his/her profession or employment status: 3369
migratehh=all%>%filter(year==datent)
num_change=function(y){
  temp1=migratehh%>%filter(year==y)
  for (i in 1:nrow(temp1)){
    id=temp1$idind[i]
    temp2=all_ind%>%filter(idind==id,year==y-1)
    temp1$bl[i]=!temp1[1,"profession"]==temp2[1,"profession"]|!temp1[1,"empstat"]==temp2[1,"empstat"]
  }
  temp3=temp1%>%filter(bl==T)
  length(unique(temp3$idmen))
}
sum(sapply(2004:2019,num_change))
#Exercise 4
#Compute the attrition across each year, where attrition is defined as the reduction in the number of individuals staying in the data panel. Report your final result as a table in proportions.
enter_year=0
exit_year=0
for (i in unique(all_ind$idind)){
  enter_year=append(enter_year,min((all_ind%>%filter(idind==i))$year))
  exit_year=append(exit_year,max((all_ind%>%filter(idind==i))$year)+1)
}
attrition=function(y){
  num_enter=length(enter_year[enter_year==y])
  num_exit=length(exit_year[exit_year==y])
  num_prev=nrow(all_ind%>%filter(year==y-1))
  (num_exit-num_enter)/num_prev
}
Year=2005:2019
Attrition=sapply(Year,attrition)
library(scales)
attrition_table=data.frame(Year,percent(Attrition))
write.csv(attrition_table,"attrition_table.csv")
