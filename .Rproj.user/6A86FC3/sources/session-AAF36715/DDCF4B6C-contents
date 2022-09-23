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


#CPK: This^^^ could have been done with one series of pipes. A la. . . 

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

library(features)

#CPK: best to put library calls at the top.

find.peaks <- function(x,y,mult=100){ 
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) 
}

#CPk: Notice that this would've have run bc you never define pseed.wide. I did above. [-1]
pseed.max <- pseed.wide%>%
  group_by(fish,speed)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)
pseed.max$peak<-NULL

#CPK: ^^ We wanted to group by experiment (date) bc cycles apply only to individual experiments. So. . .

pseed.max <- pseed.wide%>%
  group_by(fish,speed,date)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)
pseed.max$peak<-NULL

#CPK: ^^ Why do this again v? [-1]
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

#CPK: There's no need to define your tibbles as a data.frame. And we should use the pipe here vv

ggplot(pseed.sum.max, aes(speed, amp.sum.mean, col = fish)) + geom_point() + geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se))
print(pseed.sum.max)

pseed.met<-read_csv("pseed.met.rate.csv")

#CPK: There's no file "pseed.met.rate.csv" in your repo/cloned directory. How can I run in? [-1]


pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.met.rate, by <- "date") %>%
  print()
ggplot(pseed.sum.max, aes(met.rate, amp.sum.mean, col=fish.x)) + geom_point()+ geom_smooth(method="lm")

#CPK: Getting better! Just be sure to make things more concise and provide all the data you need and read it in properly.
