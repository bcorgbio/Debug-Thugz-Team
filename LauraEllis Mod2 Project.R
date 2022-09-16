#Mod2 project

library(tidyverse)
library(lokern)
library(features)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

#Joining Relational Data - adding m.s and cm.s from speeds to pseed
pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()

#Adding BL from pseed.bl to pseed
pseed.bl%>%
  print()

pseed2%>% #showing that above tibble has 3 values for fish but this only has 2
  select(fish)%>%
  unique()

#Joining pseed.bl with pseed2
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()

#Make a new column for cm.s/bl.s
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

#Plot specific fin amplitude (amp.bl) against specific swimming speed(bl.s)
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()
#adjust transparency of points to see relation better
pseed2%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point(alpha=0.01)
#looking at just left pelvic fin for only one experiment/date -> shows its oscillating
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()

#find max and min of curvy data with features() - finding max amplitude across speeds
exp1 <- pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")
f1 <-  features(x = exp1$frame,y=exp1$amp.bl)->f1
fget(f1) #extracts important stuff from results

#Plotting vertical lines corresponding to critical points given by fget() over original plot
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame,y=amp.bl))+geom_point()+geom_vline(xintercept = fget(f1)$crit.pts)
#multiplying amplitude values by 100 shows some curvatures are negative (peaks) and some are positive (troughs), rerun from above and see results
f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
fget(f2)
#isolate peaks by choosing only negative critical points
f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()
#creating new column called peaks with discrete values not estimated critical points from features()
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  mutate(peak=frame %in% f.tib$peaks)%>%
  ggplot(aes(x=frame,y=amp.bl,col=peak))+geom_point()

#showing number of experiments
pseed2%>%
  summarize(n=length(unique(date)))

#finding max amplitude for each fun during each oscillation with a custom function from the code we wrote for first experiment
find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much we want to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, wrap in fget() to retrieve the important features, subset the results to take the 2nd and 3rd and  items, the critical points and curvature, then pass it to a tibble
    as_tibble()%>% #pass in through a filter that returns curvatures <0
    filter(curvature<0)%>% #add a column that rounds the critical point to an integer that represents the frame
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) #return the peaks from tibble
}

#trying function on pseed2, restricted to first 3 unique values of the date column (the first 3 experiments)
pseed2%>%
  filter(date%in%unique(date)[1:3])%>%
  group_by(date,fin)%>% #grouped data by date(experiment) and fin
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>% #added a column peak to data set
  ggplot(aes(x=frame,y=amp.bl,alpha=peak,col=peak))+geom_point()+facet_grid(date~fin) #plotted amplitude over frame, controlling for transparency and color of the point according to whether it's a peak or not

#finding peaks for all data (100 individual groups to apply function to, producing tibble of 3300 amplitudes for peak of oscillation)
pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) #new filter

#how does amplitude change with speed? plotting specific amplitude vs. specific swimming speed and add line to represent linear regression model
pseed.max%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()+geom_smooth(method="lm")
#shows amplitude decreases with speed

#statistical test to see if significant
amp.aov <-  aov(amp.bl~bl.s,pseed.max)
summary(amp.aov)
#p value less than 0.05 means it is significant

#compute means for each speed and for each fish, and plot means vs. speed with linear regression line for each fish
pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl)) %>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")

#tidy data are a data set where 1. each variable must have its own column, 2. each observation must have its own row, 3. each value must have its own cell - these data sets are tidy
#pivoting operations to make it tidy if it wasn't or if asking a different question
#new question: what is the sum of amplitude of both funs over our range of speeds?
#difficult bc data for each fin is in a different row for each frame (each frame has 2 rows)
#compute sum of L and R amps for each frame by adding amp.sum column
pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))
#but this violates the tidy principle (each data value must have its own cell - they span 2 cells)
#fix this by deleting one of the rows for each frame in each experiment
pseed2 %>%
  filter(fin=="R")
#but now we lose half our data! and amp.sum appears to only apply to right fin
#avoid this problem by making data wider, give L and R fins their own amplitude column and then sum the values of these to get the sum of amp
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 




#For project, we are answering question 'How does the sum of amplitude of both fins vary over our range of speeds?' compared to the other question of 'How does amplitude change with speed?'



#2. find mean maximum of all amp.sums for each swimming speed for each fish

#compute means for each speed and for each fish, and plot means vs. speed with linear regression line for each fish
pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl)) %>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")


pseed.wide %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.sum))



#3 standard error of mean and add column to summary table from #2

library(plotrix)

std.error(data())
#or
stderror <- function(data) sd(data)/sqrt(length(data))

#Make a new column for cm.s/bl.s
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()

#compute sum of L and R amps for each frame by adding amp.sum column
pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))




#4 plot mean amp.sum vs. specific swimming speed and add error bars, color points by fish




#5 download file, read as tibble, merge it with new pseed.sum.max tibble



#6 plot metabolic power output of each fish vs. mean maximum of amp.sum


