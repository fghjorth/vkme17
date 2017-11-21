setwd("~/GitHub/vkme17")

require(tidyverse)
require(readtext)
require(quanteda)


bhtxts<-readtext("data/12_bh/2011_12",encoding="windows-1251") #encoding kan estimeres med encoding()

bhtxts[,1]

tscores<-c(rep(NA,41),-1,rep(NA,16),1,rep(NA,33))

bhcorp<-corpus(bhtxts)

bhdfm<-dfm(bhcorp,remove="\\s+",valuetype="regex")

bh_ws<-textmodel_wordscores(bhdfm,tscores)

bh_ws_pred<-predict(bh_ws) 

