setwd("~/GitHub/vkme17/scripts")

library(tidyverse)
library(stargazer)
library(cem)

#simuler data med mgl overlap
trueeffect<-0
simdata<-data_frame(x1=rnorm(1000),
                   x2=rnorm(1000),
                   treat=sample(0:1,1000,replace=T)) %>% 
  mutate(y=rnorm(1000)+trueeffect*treat)

simdatb<-data_frame(x1=rnorm(100),
                    x2=rnorm(100)+5,
                    treat=1) %>% 
  mutate(y=rnorm(100)+1.2+trueeffect*treat)
  
simdat<-bind_rows(simdata,simdatb)

#naiv ols
summary(ols1<-lm(y~treat,data=simdat))

#evaluer balance
imbalance(simdat$treat,data=as.data.frame(select(simdat,x1,x2)))

#match
mat<-cem(treatment="treat",data=simdat,cutpoints=list(x1=c(-2,-1,0,1,2),x2=c(-2,-1,0,1,2)))

#ols paa matchede data
summary(ols2<-lm(y~treat,data=simdat[mat$matched,]))

#sammenlign
stargazer(ols1,ols2,type="text")

#kunne man have set det paa andre maader?
ggplot(simdat,aes(x1,y,color=factor(treat))) +
  geom_point()
ggplot(simdat,aes(x2,y,color=factor(treat))) +
  geom_point()
