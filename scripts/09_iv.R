setwd("~/GitHub/vkme17")

require(readr)
require(dplyr)
require(broom)
require(AER) #anvendt økonometri
require(sandwich) #robuste standardfejl

#indlæs data
ggd<-read_csv("data/08_gg.csv")

#opsamling fra forrige øvelse:
#ift. bogen:
# VOTED hedder her v98
# ASSIGNED hedder her persngrp
# TREATED hedder her cntany
ggd<-subset(ggd,onetreat==1 & mailings==0 & phongotv==0 & persons==1)
ittmodel<-lm(v98~persngrp,data=ggd)
itt<-tidy(ittmodel)[2,2]
ittdmodel<-lm(cntany~persngrp,data=ggd)
ittd<-tidy(ittdmodel)[2,2]
cace <- itt / ittd

#estimer model hvor assignment bruges som instrument for treatment
ivmodel<-ivreg(v98~cntany,~persngrp,data=ggd)
summary(ivmodel)
coeftest(ivmodel,vcovHC(ivmodel)) #obs - man skal bruge robuste standardfejl v iv-estimation, da 2nd stage ols ikke tager højde for usikkerheden i 1st stage
