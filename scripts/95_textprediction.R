require(tidyverse)
require(quanteda)
require(glmnet)

simtweets<-data_frame(tweet=c("politikere er korrupte og uærlige",
                              "politik er et korrupt spil",
                              "politik handler om at få resultater",
                              "politik gør en forskel i verden",
                              "kun uærlige mennesker går ind i politik"),
                      cynic=c(1,1,0,0,1))

twdfm<-dfm(simtweets$tweet,stem=T) 
twdfm_df<-as.data.frame(twdfm)

twmodel<-glmnet(x=as.matrix(twdfm),
                y=simtweets$cynic,
                family="binomial",
                alpha=0)

newtweets<-c("korrupt og uærligt, sådan er politik, det er spil for galleriet",
             "politik skaber resultater for verden, det gør en forskel")

newdfm<-dfm(newtweets,stem=T) %>% 
  dfm_select(.,twdfm)

newdfm_df<-as.data.frame(newdfm)

predict(twmodel,newx=as.matrix(newdfm),type="response",s=.1)
