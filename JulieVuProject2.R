 
library(tidyverse)

library(features)
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()

pseed.bl%>%
  print()

pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()

pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

#USING CUSTOMIZED FUNCTION 
#install features package 
library(features)

#to use features() function 
#need to make a curvy relationship and vectors for x and y data
exp1 <- pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")

f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1
#made an exp1 data frame
#pass "frame" column to x parameter
#pass "amp.bl" column to y parameter
#to extract important stuff from features() function
fget(f1)
#there are 24 critical points 

#to get positive and negative curvatures for the critical points 
#multiply amplitude values by 100 and see results
f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
fget(f2)

#to make a tibble using tidyverse
#to pull out peaks column (where critical points are negative) use filter ()
f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()

#to find max amplitude for each fin during each oscillation use for loop function

find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, use fget() to retrieve important features, subset results to take the 2nd and 3rd and items, the critical points and curvature
    as_tibble()%>% #pass to a tibble
    filter(curvature<0)%>% #pass in through filter that returns curvatures <0
    mutate(peaks=round(crit.pts,0)) #add a column that rounds the critical point to an integer that represents the frame
  return(f$peaks) # return the peaks from tibble
}  

pseed2%>%
  filter(date%in%unique(date)[1:3])%>% 
  group_by(date,fin)%>% #added a column peak to data set
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%

pseed2
#to add column (say amp.sum) to the tibble that represents the sum of the amplitudes   
pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))
#to delete one of the rows for each frame in each experiment  
#each data value must have it’s own cell
pseed2 %>%
  filter(fin=="R")
#to give left and right fins their own amplitude column and sum the values of these to get sum of amplitude   
pseed.wide <- pseed2 %>% 
  select(-amp)%>% #pass pseed2 through select() to remove unneeded amp column 
  pivot_wider(names_from = fin,values_from = amp.bl) %>% #pulls names from "fin" column and data from "amp.bl" column
  mutate(amp.sum=L+R)%>% #new column named "amp.sum" with amplitude data from L and R
  print() 

# pseed.sum.max <-pseed.wide %>% 
#   mutate(amp.sum.mean=mean(amp.sum))
#   print(pseed.sum.max)
#   
# amp.sum.se <- pseed.wide %>%
#   mutate(pseed.sum.max=std.error(amp.sum.mean))
# print(amp.sum.se)
#  #need to find sd (by using fucntion sd) for each and then calculate se?


se = function(v) {
  n = length(v)
  return(sd(v) / sqrt(n))
}
pseed.sum.max = pseed.wide %>% 
  group_by(speed) %>% 
  summarize(amp.sum.mean = mean(amp.sum), amp.sum.se = se(amp.sum), fish = fish, date = date)
  #ggplot(aes(amp.sum.mean, speed, col = species))

pseed.sum.max = as.data.frame(pseed.sum.max)
ggplot(pseed.sum.max, aes(speed, amp.sum.mean, col=fish)) + geom_point() + geom_errorbar(aes(ymin = amp.sum.mean - amp.sum.se, ymax = amp.sum.mean + amp.sum.se))
print(pseed.sum.max)

pseed.met = read_csv("pseed.met.csv")

pseed.sum.max = pseed.sum.max %>%
  left_join(pseed.met, by = "date") %>%
  print()

ggplot(pseed.sum.max, aes(met.rate, amp.sum.mean, col=fish.x)) + geom_point()


met.rate %>%
  print()


