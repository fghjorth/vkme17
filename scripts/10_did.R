setwd("~/GitHub/vkme17")

require(tidyverse)
require(stargazer)

elrcd<-readRDS("data/elrcd.rds")

#long form
elrcd_long<-elrcd %>% 
  select(komopaf,yshare14,yshare15,dfshare15,treat_05k,treat_10k,treat_15k) %>% 
  gather("year","share",2:3) %>% 
  arrange(komopaf) %>% 
  mutate(post=ifelse(year=="yshare15",1,0))

#calculate manually
treatpost<-mean(elrcd_long$share[elrcd_long$treat_05k==1 & elrcd_long$post==1],na.rm=T)
treatpre<-mean(elrcd_long$share[elrcd_long$treat_05k==1 & elrcd_long$post==0],na.rm=T)
ctrlpost<-mean(elrcd_long$share[elrcd_long$treat_05k==0 & elrcd_long$post==1],na.rm=T)
ctrlpre<-mean(elrcd_long$share[elrcd_long$treat_05k==0 & elrcd_long$post==0],na.rm=T)
diffindiffs<-(treatpost-treatpre) - (ctrlpost-ctrlpre)

#fit models
m1_05k_in<-lm(share~treat_05k*post,data=elrcd_long)
m1_05k_fe<-lm(share~treat_05k:post+factor(komopaf)+factor(year),data=elrcd_long)

#show results
stargazer(m1_05k_in,m1_05k_fe,type="text",omit="factor",digits=8)
