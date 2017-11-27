setwd("~/GitHub/vkme17")

require(tidyverse)
require(readtext)
require(quanteda)
require(stringr)

####
# REGNEEKSEMPEL FRA SLIDES
####

simpletexts<-c("velfærd velfærd velfærd",
               "velfærd velfærd vækst",
               "velfærd vækst vækst",
               "vækst vækst vækst")
simplecorpus<-corpus(simpletexts,docnames=c("el","s","v","la"))
simpledfm<-dfm(simpletexts)

#tf-idf
tfidf(simpledfm)

#regner efter i hånden
tf<-3
idf<- log10( 4/3 )
tf*idf

####
# BATURO & MIKHAYLOV
####

# 1: importer tekster
bhtxts<-readtext("data/12_bh/2011_12",encoding="windows-1251") #encoding kan estimeres med encoding()

bhtxts[1,2]
bhtxts[,1]

#definer referencevaerdier
tscores<-c(rep(NA,41),-1,rep(NA,16),1,rep(NA,33))

# 2: konstruer korpus
bhcorp<-corpus(bhtxts)

# 3: konstruer document feature matrice
bhdfm<-dfm(bhcorp,remove="\\s+",valuetype="regex")

# 4: estimer wordscores
bh_ws<-textmodel_wordscores(bhdfm,tscores)

#konstruer data frame med praedikerede wordscores
bh_ws_pred<-predict(bh_ws) 
bh_ws_df<-bind_cols(data.frame(text=bh_ws_pred@newdata@Dimnames$docs),bh_ws_pred@textscores) %>% 
  mutate(yr=as.numeric(str_extract(text,"[0-9]+")),
         province=str_extract(text,"[^_]+"))

#modeller positioner som fkt af tid
olsmodel<-lm(textscore_raw~yr+province,data=bh_ws_df)
summary(olsmodel)

