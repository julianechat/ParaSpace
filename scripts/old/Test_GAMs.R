## Script name : GA(M)Ms analysis tests

## Authors : Juliane Vigneault
## Date created : March 30, 2023

## Copyright (c) Juliane Vigneault, 2023
## Email: juliane.vigneault@umontreal.ca

# ---- Script setup ----
## R Setup ----

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"
to.carto <- "./carto/"

## Loading packages & functions ----

library(mgcv)
library(gratia)
library(gamlss)

## Loading data ----

mod.data <- read.csv(paste0(to.output, "ModelAnalysis_Df.csv"))

# ---- Method tests ----

mod.data$Lake <- as.factor(mod.data$Lake)
mod.data$Transect_ID <- as.factor(mod.data$Transect_ID)
mod.data$Watershed <- as.factor(mod.data$Watershed)

#Because we want to compare models with different fixed effect, we choose ML for method estimation
#Tests were made on nutrient data (TN:TP & TOC)
#Tests were made at transect scale

## GAMs ----

#TN_TP thin spline smooth
test1.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T), family = binomial, data = mod.data, method = "ML")
summary(test1.gam) #Significant
#Adj. R-sq = 0.107
#Deviance explained = 27.4%
plot(test1.gam)
appraise(test1.gam, method = "simulate")

#TN_TP cubic regression smooth
test2.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr"), family = binomial, data = mod.data, method = "ML")
summary(test2.gam) #Significant
#Adj. R-sq = 0.127
#Deviance explained = 28.8%
plot(test2.gam)
appraise(test2.gam, method = "simulate")
#Doesn't change much 

#TN_TP & TOC cubic regression smooth
test3.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(TOC.T, bs = "cr"), family = binomial, data = mod.data, method = "ML")
summary(test3.gam) #All variables significant
#Adj. R-sq = 0.631
#Deviance explained = 76.4%
plot(test3.gam)
appraise(test3.gam, method = "simulate") #Observed vs fitted values is better

#Quasibinomial distribution
test4.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(TOC.T, bs = "cr"), family = quasibinomial, data = mod.data, method = "ML")
summary(test4.gam) #All variables significant
#Adj. R-sq = 0.538
#Deviance explained = 57.6%
plot(test4.gam)
appraise(test4.gam, method = "simulate") #Corrects QQ plot

#Betabinomial distribution
test5.gam <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), family = BB, data = mod.data)
summary(test5.gam) #No variable significant
#Exit is weird
#AIC = 334.3
plot(test5.gam) #Validation is not necessarily better than Quasibinomial distribution

## GAMMs ----
#Random effects *********
#Not sure what I coded
test1.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T) + s(TOC.T), random = ~1, family = binomial, data = mod.data, method = "ML")
summary(test1.gamm) #All variable significant
#Adj. R-sq. = 0.589
#Deviance explained = 74.6%
appraise(test1.gamm, method = "simulate")

#Binomial distribution - thin spline
test2.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T) + s(TOC.T) + s(Lake, bs = "re"), family = binomial, data = mod.data, method = "ML")
summary(test2.gamm) #All variable significant
#Adj. R-sq. = 0.953
#Deviance explained = 97.4%
plot(test2.gamm)
appraise(test2.gamm, method = "simulate")

#Quasibinomial distribution - thin spline
test3.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T) + s(TOC.T) + s(Lake, bs = "re"), family = quasibinomial, data = mod.data, method = "ML")
summary(test3.gamm) #TN:TP is significant
#Adj. R-sq. = 0.835
#Deviance explained = 86.4%
plot(test3.gamm)
appraise(test3.gamm, method = "simulate") #QQ plot is little better

#Quasibinomial distribution - cubic regression
test4.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(TOC.T, bs = "cr") + s(Lake, bs = "re"), family = quasibinomial, data = mod.data, method = "ML")
summary(test4.gamm) #TN:TP is significant
#Adj. R-sq. = 0.864
#Deviance explained = 89.1%
plot(test4.gamm)
appraise(test4.gamm, method = "simulate") #Validation is actually worse than thin spline

#Quasibinomial distribution - double penalty
test5.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T) + s(TOC.T) + s(Lake, bs = "re"), family = quasibinomial, data = mod.data, method = "ML", select = TRUE)
summary(test5.gamm) #No variable significant
#Adj. R-sq. = 0.822
#Deviance explained = 85.1%
plot(test5.gamm)
appraise(test5.gamm, method = "simulate") #Validation is a little better than no penalty

#OLRE random effect
test5.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(TOC.T, bs = "cr") + s(Transect_ID, bs = "re"), family = quasibinomial, data = mod.data, method = "ML")
#Cannot use this method because is demands too many coefficients

#Cannot add a random slope with two independent variables (too many coefficients) so we only keep one
#Random slope & intercept with TN:TP
test6.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") +  s(Lake, bs = "re") + s(Lake, TN_TP.T, bs = "re"), family = quasibinomial, data = mod.data, method = "ML")
summary(test6.gamm) #All variable significant
#Adj. R-sq. = 0.86
#Deviance explained = 88.7%
plot(test6.gamm)
appraise(test6.gamm, method = "simulate")

#Random slope & intercept with TOC
test7.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TOC.T, bs = "cr") + s(Lake, bs = "re") + s(Lake, TOC.T, bs = "re"), family = quasibinomial, data = mod.data, method = "ML")
summary(test7.gamm) #All variable significant
#Adj. R-sq. = 0.603
#Deviance explained = 68.1%
plot(test7.gamm)
appraise(test7.gamm, method = "simulate")

#Random spline

#Based on R-sq, best model is with quasibinomial distribution, cubric regression spline & random intercept on Lake

