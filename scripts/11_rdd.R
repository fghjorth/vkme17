setwd("~/GitHub/vkme16")

require(readr)
require(dplyr)
require(rdd) #RD designs
require(ggplot2) #plots

#indlæs data
eh<-read_csv("data/10_eh.csv")

#overblik over data
glimpse(eh)

#relevante variable:
# ln.net: logget nettoformue
# margin: valgmargen
# party: parti

#estimater med 

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
