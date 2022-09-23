library(ggplot2)
library(tidyverse)
setwd("~/Desktop/ExpMethods/Debug-Thugz-Team")

#CPK: No need to set the wd if you're working in an R project

#Creating a 'dat' variable containing the scales dataset
#CPK: This won't run bc there's no directories of these names on my machine. Commented and changed to work (note I had to add the scales data to the directory/repo) [-1]

#dat <- read.csv("/Users/lauraellis/Desktop/ExpMethods/scales.csv")

dat <- read.csv("scales.csv")

#Line of code reporting class of each column in dataset
sapply(dat,class)

#Line of code reporting dimensions of dataset
dim(dat)

#Convert species column to factor to inspect the levels and number of species
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
species

#Code producing summary of number of scales punctured for each species
species.n<- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n

#Code producing summary of number of specimens for each species
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

#Making and saving boxplots of puncture force vs quadrant for each species in a PDF file

#CPK: This loop is repeated [-1]
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()
list.files(pattern=".pdf")

#CPK: Pretty solid. Try to only include what's needed and leverage the economy of using an R project and git. That is, don't set the wd, store data in the project dir/repo, and git (I noticed you uploaded rather than pushed your script [-1]).