setwd("~/GitHub/vkme17")

require(tidyverse)
require(broom)
require(randomizr)

### BLOCKING

#simuler data hvor blocking er meget relevant
#eksempel: vi treater folk med HCA-eventyr og tester holdning til HCA (y), men et subset af samplen er fynboere med markant højere y
set.seed(1234)
samplesize<-80
simdat<-data_frame(fynbo=sample(0:1,samplesize,replace=T,prob=c(.9,.1)),
                   ypre=rnorm(samplesize,mean=5)+12*fynbo,
                   simpletreat=sample(0:1,samplesize,replace=T),
                   noise=rnorm(samplesize))

simdat<-simdat %>% 
  mutate(blocktreat=block_ra(fynbo),
         ypost_simple=ypre + simpletreat*1+noise,
         ypost_block=ypre + blocktreat*1+noise)

#tjek at der er skævhed i assignment
table(simdat$fynbo,simdat$simpletreat)

#se på modeller
summary(lm(ypost_simple~simpletreat+fynbo,data=simdat))
summary(lm(ypost_block~blocktreat+fynbo,data=simdat))

### NONCOMPLIANCE

#indlæs data
ggd<-read_csv("data/08_gg.csv")

#overblik over data
glimpse(ggd)

#begræns data til one-person households i kontrol eller canvas
ggd<-filter(ggd,onetreat==1 & mailings==0 & phongotv==0 & persons==1)

#ift. bogen:
# VOTED hedder her v98
# ASSIGNED hedder her persngrp
# TREATED hedder her cntany

#model for ITT
ittmodel<-lm(v98~persngrp,data=ggd)
summary(ittmodel)
itt<-tidy(ittmodel)[2,2]

#model for ITTD
ittdmodel<-lm(cntany~persngrp,data=ggd)
summary(ittdmodel)
ittd<-tidy(ittdmodel)[2,2]

#beregn CACE
cace <- itt / ittd
