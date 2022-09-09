library(ggplot2)
library(tidyverse)
dat <- read.csv("scales (1).csv")
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
list.files(pattern=".pdf")