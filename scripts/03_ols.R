#definer sti til mappen
setwd("~/GitHub/vkme17")

#indlæs pakker
library(tidyverse)

#indlæs data
sd <- readRDS("data/03_soltdata.rds") 

#definer modellen
ols<-lm(div_hhn~gini_cnty+
               income_cnty+black_cnty+perc_bush04+pop_cnty+income+educ+age+male+union+emp+partyid+ideo+attend,
             data=sd)

#se på estimaterne
summary(ols)

#den afhængige er dikotom, så vi kan også fitte en logit
sd_logit01<-glm(div_hhn~gini_cnty+
               income_cnty+black_cnty+perc_bush04+pop_cnty+income+educ+age+male+union+emp+partyid+ideo+attend,
             data=sd,family="binomial")

#se på estimaterne
summary(sd_logit01)

### ØVELSE: fit en model kun med fagforeningsmedlemskab ('union') som den afhængige. 
## - hvordan fortolkes koefficienten? 
## - hvad behøver vi antage for at den kan fortolkes kausalt?
## - tilføj ideologi ('ideo') til modellen. hvordan ændrer det resultaterne?

# tip: hent pakken "stargazer" og kør stargazer(model1,model2,type="text") for en nem måde at sammenligne resultater
