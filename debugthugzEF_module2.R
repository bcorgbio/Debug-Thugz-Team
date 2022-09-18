library(tidyverse)
library(features)
pseed <- read_csv("fin.amp.pumpkinseed.EF.csv")
pseed.bl <- read_csv("body_length_pumpkinseed_EF.csv")
speeds <- read_csv("swim_speed_pumpkinseed_EF.csv")

pseed.wide <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)%>%
  group_by(date,frame)%>%
  mutate(amp.sum=sum(amp.bl))%>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from=amp.bl)%>%
  mutate(amp.sum=L+R)%>%
  select(-m.s,-cm.s)

find.peaks <- function(x,y,mult=100){
  f <- fget(features(x=x,y=y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}

pseed.wide.max <- pseed.wide%>%
  group_by(fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)
  

s.e.mean <- function(x){sd(x)/sqrt(length(x))}

pseed.sum.max <- pseed.wide.max%>%
  group_by(fish,bl.s)%>%
  summarise(amp.sum=amp.sum,amp.sum.mean=mean(amp.sum),amp.sum.se=s.e.mean(amp.sum))

pseed.sum.max%>%
  ggplot(aes(x=amp.sum.mean,y=bl.s,col=fish))+geom_errorbar(aes(ymin=bl.s-amp.sum.se, ymax=bl.s+amp.sum.se),width=.002)+geom_point()

pseed.met <- read_csv("pseed.met.rate_EF.csv")
pseed.met <- pseed.met%>%
  select(fish,met.rate)

pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.met,by="fish")


pseed.sum.max%>%
  ggplot(aes(x=met.rate,y=amp.sum.mean,col=fish))+geom_point()
  
