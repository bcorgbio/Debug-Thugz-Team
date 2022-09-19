library(tidyverse)
library(features)
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

#establish pseed.wide data tibble
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)%>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print()

#computing mean maximum of all amp.sums for each speed for each fish
find.peaks <- function(x,y,mult=100){
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) 
}

pseed.max <- pseed.wide%>%
  group_by(fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)

pseed.sum.max<- pseed.max %>%
  group_by(fish,bl.s) %>%
  summarize(amp.sum.mean=mean(amp.sum))

#custom function to compute standard error of the mean (SE) and adding column
SE <- function(x){sd(x)/sqrt(length(x))}

pseed.sum.se <- pseed.max%>%
  group_by(fish,bl.s)%>%
  summarize(amp.sum.se=SE(amp.sum))

pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.sum.se, by=c("bl.s","fish"))

#plotting mean amp.sum vs. swimming speed with error bars for SE
pseed.sum.max%>%
  ggplot(aes(x=bl.s, y=amp.sum.mean, col=fish))+geom_point()+geom_smooth(method="lm")+geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width = 0.05, size = 0.5)+theme_classic()

#read as a tibble and merge with pseed.sum.max tibble
pseed.met.rate <- read_csv("pseed.met.rate.csv")
pseed.met.rate <- pseed.met.rate%>%
  select(fish,date,met.rate)

pseed.sum.max <- pseed.sum.max%>%
  left_join(pseed.met.rate,by="fish")

#plot metabolic power output of each fish vs. mean max of amp.sum
pseed.sum.max%>%
  ggplot(aes(x=met.rate,y=amp.sum.mean,col=fish))+geom_point()+geom_smooth(method="lm")+theme_classic()