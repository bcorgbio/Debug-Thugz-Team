library(tidyverse)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")


pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))

pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")
 
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)

library(features)

find.peaks <- function(x,y,mult=100){ 
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) 
}

pseed.max <- pseed.wide%>%
  group_by(fish,speed)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)
pseed.max$peak<-NULL

pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 
#end of given code for Mod 2

se = function(v) {
  n = length(v)
  return(sd(v) / sqrt(n))
}

pseed.sum.max <- pseed.max %>% 
  group_by(fish,speed) %>% 
  summarize(amp.sum.mean = mean(amp.sum), amp.sum.se = se(amp.sum), fish = fish, date = date)
pseed.sum.max <- as.data.frame(pseed.sum.max)
ggplot(pseed.sum.max, aes(speed, amp.sum.mean, col = fish)) + geom_point() + geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se))
print(pseed.sum.max)

pseed.met<-read_csv("pseed.met.rate.csv")

pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.met.rate, by <- "date") %>%
  print()
ggplot(pseed.sum.max, aes(met.rate, amp.sum.mean, col=fish.x)) + geom_point()+ geom_smooth(method="lm")