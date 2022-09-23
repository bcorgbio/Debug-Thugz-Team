library(tidyverse)
library(features)
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)

pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))

pseed.wide <- pseed2 %>% 
  select(-amp)%>% 
  pivot_wider(names_from = fin,values_from = amp.bl) %>% 
  mutate(amp.sum=L+R)%>% 
  print() 

#CPK: This ^ could have been much more concise. A la ...

pseed.wide <-pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl) %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl)) %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)

pseed.max <- pseed.wide %>%
  group_by(fish,speed) 

#CPK: Why not just add this ^ to this v. And we wanted to group by experiment (date), because fin-beat cycles are specific to each. [-1]. So . . 

pseed.max <- pseed.wide %>%
  group_by(fish,speed,date) 




#CPK: There's no function find.peaks define before you use it. [-1]. Here it is.

find.peaks <- function(x,y,mult=100){ 
  f <- fget(features(x = x,y=y*mult))[2:3]%>%
    as_tibble()%>%
    filter(curvature<0)%>%
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}

pseed.max = as.data.frame(pseed.max) %>%
  mutate(peak = frame %in% find.peaks(frame,amp.sum)) %>%
  filter(peak==T)


se = function(v) {
  n = length(v)
  return(sd(v) / sqrt(n))
}
pseed.sum.max <- pseed.max %>% 
  group_by(fish, speed) %>% 
  summarize(amp.sum.mean = mean(amp.sum), amp.sum.se = se(amp.sum), fish = fish, date = date)

pseed.sum.max = as.data.frame(pseed.sum.max)

#CPK: There's no need to keep defining your tibbles as a data.frame

ggplot(pseed.sum.max, aes(speed, amp.sum.mean, col=fish)) + geom_point() + geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se))
print(pseed.sum.max)

#CPK: Why the "print"? And why not pipe this ^ . . .

pseed.sum.max %>% 
  ggplot(aes(speed, amp.sum.mean, col=fish)) + geom_point() + geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se))
pseed.met <- read_csv("pseed.met.csv")

pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.met, by = "date") %>%
  print()


#CPK: pseed.met is never define, so this ^ won't run [-1]

ggplot(pseed.sum.max, aes(met.rate, amp.sum.mean, col=fish.x)) + geom_point() + geom_smooth(method="lm")

#CPK: Pretty good work, but be sure to include what you need so that the script runs.

