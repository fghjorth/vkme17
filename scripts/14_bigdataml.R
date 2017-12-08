setwd("~/GitHub/vkme17")

require(tidyverse)
require(rpart) #regression trees
require(rpart.plot) #plot pænere regressionstræer
require(glmnet) #lasso

#hent det store datasæt fra midterm-opgaven
eb<-readRDS("data/14_eurobarometer.rds") #rds er et kompakt format til at gemme r-objekter

#skal først lave nyt data uden NA's
eb2<-eb %>% 
  select(membrshp,age,sex,educ,income,lrs,matpmat) %>% 
  na.omit() %>% 
  mutate(membrshp01=ifelse(membrshp>1,0,1)) #bytter rundt så 1=pro-EU

#baseline: en logit-model
ebformula<-as.formula(membrshp01~age+sex+educ+income+lrs+matpmat)
summary(logit<-glm(ebformula,data=eb2,family="binomial"))

#fit et regressionstræ
regtree<-rpart(ebformula,data=eb2,control=rpart.control(minsplit=30, cp=0.0001))
prp(regtree)

#til LASSO skal vi bruge en matrice kun med inputvariablene
ebinputmat<-as.matrix(dplyr::select(eb2,-membrshp,-membrshp01))

#fit en LASSO-regression
lasso<-glmnet(x=ebinputmat,y=eb2$membrshp01,family="binomial")

#plot lasso koefficienter som funktion af tuningparameteren lambda
plot(lasso,xvar="lambda",label=T)
lasso$beta@Dimnames[[1]] #navne der svarer til labels på plottet

#prediktion: lad os prøve at forudsige en holdningen for en typisk VKM-studerende
#dvs. ung, høj udd, semi-venstreorienteret
vkmdat<-as.matrix(data.frame(age=26,sex=1,educ=10,income=4,lrs=3,matpmat=4))
#predict() er ligesom normalt, men vi skal angive 's' for tuning-parameteren, dvs. lambda
#jeg vælger en lav værdi, for variablene er valgt teoretisk => ikke så bange for overfitting
#type="response" er det man angiver i predict() når man skal have forudsagt ssh fra en model
predict(lasso,newx=vkmdat,s=.001,type="response")
#70 pct. ssh for pro-EU... sounds about right 