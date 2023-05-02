## Script name : GL(M)Ms analysis tests

## Authors : Juliane Vigneault
## Date created : March 23, 2023

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

library(performance)
library(lme4)
library(MASS)
library(glmmTMB)
library(aod)

## Loading data ----

mod.data <- read.csv(paste0(to.output, "ModelAnalysis_Df.csv"))

# ---- Method tests ----

#Tests were made on nutrient data (TN:TP & TOC)
#Tests were made at transect scale

## GLMs ----
test1.glm <- glm(cbind(inf_fish, tot_fish-inf_fish) ~ TOC.T * TN_TP.T, family = binomial, data = mod.data)
summary(test1.glm) #All variables & interaction are significant
#AIC = 2438.5
check_overdispersion(test1.glm) #Overdispersion detected
#phi = 68.247
plot(test1.glm) 

test2.glm <- glm(cbind(inf_fish, tot_fish-inf_fish) ~ TOC.T + TN_TP.T, family = binomial, data = mod.data)
summary(test2.glm) #All variables are significant
#AIC = 2441.6
check_overdispersion(test2.glm) #Overdispersion detected
#phi= 65.940
#Not considering the interaction reduce the overdispersion
plot(test2.glm)

## GLMMs ----
#In GLMMs, the random effect factor is the lake

#Random intercept
test1.RI.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1|Lake), family = binomial, data = mod.data)
summary(test1.RI.glmm) #All variables are significant
#Suggest rescaling values - large eigenvalue ratio
#AIC = 1073.3
check_overdispersion(test1.RI.glmm) #Overdispersion detected
#phi = 24.293
#Adding random effect helps a lot the overdispersion
plot(test1.RI.glmm)

#Random intercept - TN:TP scaled
test2.RI.glmm <- glmer(cbind(inf_fish, tot_fish-inf_fish) ~ TOC.T * scale(TN_TP.T) + (1|Lake), family = binomial, data = mod.data)
summary(test2.RI.glmm) #All variables significant
#AIC = 1073.3
#Scaling doesn't affect AIC value
check_overdispersion(test2.RI.glmm) #Overdispersion detected
#phi = 24.293
#Scaling doesn't affect phi value
plot(test2.RI.glmm)

#Random intercept - TN:TP log transformed
test3.RI.glmm <- glmer(cbind(inf_fish, tot_fish-inf_fish) ~ TOC.T * log(TN_TP.T) + (1|Lake), family = binomial, data = mod.data)
summary(test3.RI.glmm) #All variables significant
#AIC = 1043.7
#log transformation reduce AIC value
check_overdispersion(test3.RI.glmm) #Overdispersion detected
#phi = 23.548
#log transformation reduces phi value
plot(test3.RI.glmm)

#Random intercept - TOC & TN:TP log transformed
test4.RI.glmm <- glmer(cbind(inf_fish, tot_fish-inf_fish) ~ log(TOC.T) * log(TN_TP.T) + (1|Lake), family = binomial, data = mod.data)
summary(test4.RI.glmm) #All variables significant
#AIC = 1033.9
#Double log transformation reduces AIC value
check_overdispersion(test4.RI.glmm) #Overdispersion detected
#phi = 23.307
#Double log transformation reduces phi value a little
plot(test4.RI.glmm)

#Random intercept - Quasibinomial distribution
test5.RI.glmm <- glmmPQL(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T, random = ~1|Lake, data = mod.data, family = quasibinomial)
summary(test5.RI.glmm) #No variable significant
#Modeling quasi-families doesn't give an AIC value
plot(test5.RI.glmm)

#Random intercept - Lake nested in watershed
test6.RI.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1|Watershed/Lake), data = mod.data, family = binomial)
summary(test6.RI.glmm) #All variables significant
#Suggest rescaling values - large eigenvalue ratio
#AIC = 1074.0
#AIC similar to normal random effect meaning there is no significant effect of nesting lakes in their watershed
check_overdispersion(test6.RI.glmm) #Overdispersion detected
#phi = 25.090
#No effect of nesting random factor on overdispersion
plot(test6.RI.glmm)

#Random intercept - Transect nested in lake
test7.RI.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1|Lake/Transect_ID), family = binomial, data = mod.data)
summary(test7.RI.glmm) #No variable significant
#This type of correction for overdispersion might be to hard on data
#Suggest rescaling values - large eigenvalue ratio
#AIC = 338.6
check_overdispersion(test7.RI.glmm) #No overdispersion detected
#phi = 0.156
plot(test7.RI.glmm) #Very different trend than the others

#Random intercept - OLRE method 
test8.RI.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1|Lake) + (1|Transect_ID), family = binomial, data = mod.data)
summary(test8.RI.glmm) #No variable significant
#Suggest rescaling values - large eigenvalue ratio
#AIC = 338.6
check_overdispersion(test8.RI.glmm) #No overdispersion detected
#phi = 0.156
plot(test8.RI.glmm) #Very different trend than the others
#It seems that nesting transect in lake as the same effect as OLRE method

#Random intercept - betabinomial distribution
test9.RI.glmm <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ TN_TP.T * TOC.T + (1|Lake), family = betabinomial, data = mod.data)
summary(test9.RI.glmm) #No variable significant
#AIC = 334.0
#Best model fit so far
check_overdispersion(test9.RI.glmm) #Overdispersion detected... (but betabinomial distribution should correct for overdispersion)
#phi = 33.263

#Random slope & intercept on TOC
test1.RIS.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1 + TOC.T|Lake), family = binomial, data = mod.data)
summary(test1.RIS.glmm) #All variable significant
#Suggest rescaling values - large eigenvalue ratio
#AIC = 868.6
#Better than random intercept
check_overdispersion(test1.RIS.glmm) #Overdispersion detected
#phi = 17.153
plot(test1.RIS.glmm)

#Random slope & intercept on TN_TP
test2.RIS.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1 + TN_TP.T|Lake), family = binomial, data = mod.data)
summary(test2.RIS.glmm)#All variable significant
#Suggest rescaling values - large eigenvalue ratio
#AIC = 670.5
check_overdispersion(test2.RIS.glmm) #Overdispersion decteted
#phi = 10.801
#Random slope on TN:TP is better than random slope on TOC
plot(test2.RIS.glmm)

#Random slope & intercept on TOC & TN_TP
test3.RIS.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1 + TN_TP.T|Lake) + (1 + TOC.T|Lake), family = binomial, data = mod.data)
summary(test3.RIS.glmm) #No variable significant
#Might overpenalize
#Suggest rescaling values - large eigenvalue ratio
#AIC = 601.5 
check_overdispersion(test3.RIS.glmm) #Overdispersion decteted
#phi = 8.732
plot(test3.RIS.glmm)

#Random effect model
test.RE.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ 1 + (1|Lake), family = binomial, data = mod.data)
summary(test.RE.glmm) #Significant
#AIC = 1101.5
check_overdispersion(test.RE.glmm) #Overdispersion detected
#phi = 23.131
plot(test.RE.glmm)

## Betabinomial models ----
#No random effect
test1.betabin <- betabin(cbind(inf_fish, tot_fish - inf_fish) ~ TN_TP.T * TOC.T, ~1, data = mod.data, link = "logit")
summary(test1.betabin) #No variable significant
#AIC = 345
#Gives AICc too
#phi = 0.3807
#Corrects for overdispersion

#Lake as random effect
test2.betabin <- betabin(cbind(inf_fish, tot_fish - inf_fish) ~ TN_TP.T + TOC.T, ~Lake, data = mod.data, link = "logit")
summary(test2.betabin) #No variable significant
#AIC = 338.7 
#phi = all < 1, except Triton = 1
#Can't include interaction
