library(tidyverse)
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)

anole <- read_csv("anole.dat.EF.csv")
anole.eco <- read_csv("anole.eco.EF.csv")
anole.tree <- read.tree("anole.EF.tre")

#1 establishing data tibble 
anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>%
  print()

anole.log <- anole2%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)

#2 two linear models 

anole.log.PD.lm <- lm(HTotal~SVL+ArbPD,anole.log)
  
anole.log.PH.lm <- lm(HTotal~SVL+PH,anole.log)


#3 how perch diameter and height effect the hindlimb-SVL 

anole.log.PD.res <- anole.log%>%
  mutate(res.PD=residuals(anole.log.PD.lm))%>%
  ggplot(aes(x=Ecomorph2,y=res.PD))+geom_boxplot()+stat_summary(fun=mean,geom="point",size=3)
print(anole.log.PD.res)

anole.log.PH.res <- anole.log%>%
  mutate(res.PH=residuals(anole.log.PH.lm))%>%
  ggplot(aes(x=Ecomorph2,y=res.PH))+geom_boxplot()+stat_summary(fun=mean,geom="point",size=3)
print(anole.log.PH.res)


#4 PGLS relationships


pgls.ph <- gls(HTotal~SVL+PH,correlation=corBrownian(1,phy=anole.tree,form=~Species),data=anole.log,method="ML")
pgls.pd <- gls(HTotal~SVL+ArbPD,correlation=corBrownian(1,phy=anole.tree,form=~Species),data=anole.log,method="ML")
pgls.pd.ph <- gls(HTotal~SVL+ArbPD+PH,correlation=corBrownian(1,phy=anole.tree,form=~Species),data=anole.log,method="ML")

#5 Assessment of fit 

anole.ph.pd.phylo.aic <- AICc(pgls.ph,pgls.pd,pgls.pd.ph)
aicw(anole.ph.pd.phylo.aic$AICc)

#based off of the fit, it looks like both of the covariates combined are significant predictors of hindlimb length in a phylogenetic context


#6 Plot visualizing covariate effect in best fit PGLS

#new linear model with both covariates 

anole.log.PHPD.lm <- lm(HTotal~SVL+ArbPD+PH,anole.log)

anole.log <- anole.log%>%
  mutate(phylo.res.best=residuals(pgls.pd.ph),uncorrected.res=residuals(anole.log.PHPD.lm))
  
anole.log%>%
  dplyr::select(Ecomorph2,phylo.res.best,PH,ArbPD,uncorrected.res)%>%
  pivot_longer(cols=c("phylo.res.best","uncorrected.res"))%>%
  print()%>%
  ggplot(aes(x=Ecomorph2,y=value))+geom_boxplot()+stat_summary(fun=mean,geom="point",size=3)+facet_grid(name~.,scales="free_y")+ylab("Residuals")+xlab("Ecomorph")

#CPK: Awesome work Debugthugz!!!!