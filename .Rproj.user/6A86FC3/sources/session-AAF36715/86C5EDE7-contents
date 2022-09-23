library(ggplot2)
library(tidyverse)

#CPK: No need to set the wd if you're working in an R project

setwd("~/Desktop/Exp Methods/Files")

#CPK: This won't run bc there's no file of that name in the directory. I had to add it [-1]

dat <- read.csv("scales.csv")
dim(dat)
sapply(dat,class)
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
species
species.n<- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

#CPK: These two for loops are unneeded. The second is a partial repeat of what's required [-2]

#CPK: This is repeated below, so it's not needed [-1]

for(i in 1:10) print(i)
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}

#CPK: I needed a pdf file of a unique name (i.e., the filename includes your name). [-1]

pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

#CPK: This is unneeded. [-1]
list.files(pattern=".pdf")

#CPK: Pretty solid, but try to only include what's needed and leverage the economy of using an R project and git. That is, don't set the wd, store data in the project dir/repo, and git (I noticed you uploaded rather than pushed your script [-1]).




