library(ggplot2)
library(tidyverse)
#CPK: This won't run bc there's no file of that name in the directory. I had to add it [-1]
dat <- read.csv("scales.csv")
sapply(dat,class)
dim(dat)
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
species
species.n <- dat%>%
  group_by(species)%>%
  summarise(n=n())
species.n
dat%>%
  count(species,specimen)%>%
  print()%>%
  count(species,name="n.specimens")


#CPK: This is repeated below, so it's not needed [-1]

for(i in species){
  p <- dat%>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
pdf("EF_MODULE1_species.quadrant.pdf")
for (i in species){
  p <- dat%>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()

#CPK: This next line isn't needed.

list.files(pattern=".pdf")

#CPK: Pretty solid work, but just be sure to include only what you need and no less to answer the prompts.