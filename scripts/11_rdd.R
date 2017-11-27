setwd("~/GitHub/vkme17")

require(tidyverse)
require(rdd)

#indlæs data
eh<-read_csv("data/11_eh.csv")

#overblik over data
glimpse(eh)

#relevante variable:
# ln.net: logget nettoformue
# margin: valgmargen
# party: parti

#estimater med alm. ols
olsrd_tory<-lm(ln.net~margin+I(margin>0),data=subset(eh,party=="tory" & margin >= -.15 & margin <= .15))
summary(olsrd_tory)

olsrd_labour<-lm(ln.net~margin+I(margin>0),data=subset(eh,party=="labour" & margin >= -.15 & margin <= .15))
summary(olsrd_labour)

#estimater med rdd
rd_tory<-RDestimate(ln.net~margin,data=subset(eh,party=="tory"),bw=.15,kernel="rectangular")
summary(rd_tory)

rd_labour<-RDestimate(ln.net~margin,data=subset(eh,party=="labour"),bw=.15,kernel="rectangular")
summary(rd_labour)

#mccrary test for sorting
mccrarytest<-DCdensity(eh$margin,bw=.15,ext.out=F)
#p-værdien skal helst ikke være sig (!)

#visuel RD
ggplot(eh,aes(margin,ln.net)) +
  geom_point(alpha=.3) + #punkter
  geom_vline(xintercept=0,linetype="dashed") + #lodret linje v cutoff
  theme_bw() + #enklere design
  facet_grid(party~.) + #opdelt på parti
  geom_smooth(data=subset(eh,margin<0),aes(x=margin,y=ln.net),method="lm",alpha=0.2,color="red") + #linje for valg-tabere
  geom_smooth(data=subset(eh,margin>0),aes(x=margin,y=ln.net),method="lm",alpha=0.2,color="red")   #linje for valg-vindere
