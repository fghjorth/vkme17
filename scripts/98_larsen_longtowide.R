setwd("~/GitHub/vkme17")

library(haven)
library(tidyverse)
library(reshape2)
library(data.table)

#import long form data
ll<-read_dta("../housing/data/raplidata.dta") %>% 
  select(year,valgstedid,hp_1yr,incsupport,unemprate) %>% 
  filter(!is.na(year) & !is.na(valgstedid))

#long to wide
lw<-ll %>% 
  as.data.frame() %>% 
  reshape(.,idvar="valgstedid",timevar="year",direction="wide",sep=".") %>% 
  as_tibble()

#save as RDS
saveRDS(lw,"data/04_larsen_wide.rds")
