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
                    x2=rnorm(100)+8,
                    treat=1) %>% 
  mutate(y=rnorm(100)+1.2+trueeffect*treat)
  
simdat<-bind_rows(simdata,simdatb)

#visualisering af data
ggplot(simdat,aes(x1,y,color=factor(treat))) +
  geom_point()
ggplot(simdat,aes(x2,y,color=factor(treat))) +
  geom_point()

#naiv ols
summary(ols1<-lm(y~treat,data=simdat))

#evaluer balance
imbalance(simdat$treat,data=as.data.frame(select(simdat,x1,x2)))

#definer kategorierne for de variable vi matcher paa
cutpointslist<-list(x2=c(-4,4))

#match
mat<-cem(treatment="treat",data=simdat,drop="x1",cutpoints=cutpointslist)

#nyt, 'pruned' data kun med matchede obs
simdat_matched<-simdat[mat$matched,]

#kig paa fordelingen i de prunede data
ggplot(simdat_matched,aes(x2,y,color=factor(treat))) +
  geom_point()

#evaluer balance
imbalance(simdat_matched$treat,data=as.data.frame(select(simdat_matched,x1,x2)))

#ols paa matchede data
summary(ols2<-lm(y~treat,data=simdat_matched))

#sammenlign
stargazer(ols1,ols2,type="text")


