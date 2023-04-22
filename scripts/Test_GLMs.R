## Script name : GLMs analysis tests

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

## Loading data ----

ParaSpaceMod <- read.csv(paste0(to.output, "Transects_Lake_Data.csv"))

# ---- GLMs analysis ----

## Testing method on nutrient model ##

### Linear models ###
#Binomial glm
test.glm <- glm(cbind(inf_fish, tot_fish-inf_fish) ~ TOC.T * TN_TP.T, family = binomial, data = mod.data)
summary(test.glm)
#All variables significant
#AIC = 2438.5
check_overdispersion(test.glm)
#Overdispersion detected

#Binomial glmm (random = Lake) - Random intercept
test1.RI.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1|Lake), family = binomial, data = mod.data)
summary(test1.RI.glmm)
#Suggest rescaling values - large eigenvalue ratio
#All variables significative
#AIC = 1043.7
check_overdispersion(test1.RI.glmm)
overdisp_fun(test1.RI.glmm)
#Overdispersion detected

#Binomial glmm (random = Lake) - scaling TN_TP - Random intercept
test2.RI.glmm <- glmer(cbind(inf_fish, tot_fish-inf_fish) ~ TOC.T * scale(TN_TP.T) + (1|Lake), family = binomial, data = mod.data)
summary(test2.RI.glmm)
#All significative
#AIC = 1073.3
check_overdispersion(test2.RI.glmm)
overdisp_fun(test2.RI.glmm)
#Overdispersion detected

#Binomial glmm (random = Lake) - log TN_TP - Random intercept
test3.RI.glmm <- glmer(cbind(inf_fish, tot_fish-inf_fish) ~ TOC.T * log(TN_TP.T) + (1|Lake), family = binomial, data = mod.data)
summary(test3.RI.glmm)
#All significative
#AIC = 1043.7
check_overdispersion(test3.RI.glmm)
overdisp_fun(test3.RI.glmm)
#Overdispersion detected

#Quasibinomial glmm (random = Lake) - Random intercept
mod.data2 <- mod.data[-2,] #This model doesn't take NA values
test4.RI.glmm <- glmmPQL(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T, random = ~1|Lake, data = mod.data2, family = quasibinomial)
summary(test4.RI.glmm)
#No significative at all
#AIC = NA
#Ce genre de modèle prend en compte la sudispersion (pas besoin de regarder phi).

#Quasibinomial glmm (random = Lake nested in watershed) - Random intercept
test5.RI.glmm <- glmmPQL(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T, random = ~1|Watershed/Lake, data = mod.data2, family = binomial)
summary(test5.RI.glmm)
#No significative at all
#AIC = NA

#Binomial glmm (random = Lake nested in watershed) - Random intercept
test6.RI.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1|Watershed/Lake), family = binomial, data = mod.data)
summary(test6.RI.glmm)
#Suggest rescaling values - large eigenvalue ratio
#All variables significative
#AIC = 1074.0
check_overdispersion(test6.RI.glmm)
overdisp_fun(test6.RI.glmm)
#Overdispersion detected

#Binomial glmm (random = transect nested in lake) - Random intercept
test7.RI.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1|Lake/Transect_ID), family = binomial, data = mod.data)
summary(test7.RI.glmm)
#Suggest rescaling values - large eigenvalue ratio
#All variables unsignificatives
#AIC = 338.6
check_overdispersion(test7.RI.glmm)
overdisp_fun(test7.RI.glmm)
#Test non concluant
#Donne excatement le même AIC que betabin2 ~Lake....

#Binomial glmm (random = transect nested in lake nested in watershed) - Random intercept
test8.RI.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1|Watershed/Lake/Transect_ID), family = binomial, data = mod.data)
summary(test8.RI.glmm)
#All variables unsignificatives
#AIC = 340.6
check_overdispersion(test8.RI.glmm)
overdisp_fun(test8.RI.glmm)
#Test non concluant

#Binomial glmm (random = lake) - OLRE method - Random intercept
test9.RI.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1|Lake) + (1|Transect_ID), family = binomial, data = mod.data)
summary(test9.RI.glmm)
#All variables unsignificatives
#AIC = 351.0
check_overdispersion(test9.RI.glmm)
overdisp_fun(test9.RI.glmm)
#Test non concluant

#Betabinomial glmm (random - Lake) - Random intercept
test10.RI.glmm <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ TN_TP.T * TOC.T + (1|Lake), family = betabinomial, data = mod.data)
summary(test10.RI.glmm)
#AIC = 334
#Aucun significatif
#Que veut dire dispersion parameter?

#Binomial glmm (random = lake) - Random slope & intercept on TOC
test1.RIS.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1 + TOC.T|Lake), family = binomial, data = mod.data)
summary(test1.RIS.glmm)
#Significant
#Need to rescale
#AIC = 868.6
overdisp_fun(test1.RIS.glmm)
check_overdispersion(test1.RIS.glmm)
#Overdispersion detected

#Binomial glmm (random = lake) - Random slope & intercept on TN_TP
test2.RIS.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1 + TN_TP.T|Lake), family = binomial, data = mod.data)
summary(test2.RIS.glmm)
#Significant
#Need to rescale
#AIC = 670.5
overdisp_fun(test2.RIS.glmm)
check_overdispersion(test2.RIS.glmm)
#Overdispersion decteted

#Binomial glmm (random = lake) - Random slope & intercept on TOC & TN_TP
test3.RIS.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1 + TN_TP.T|Lake) + (1 + TOC.T|Lake), family = binomial, data = mod.data)
summary(test3.RIS.glmm)#No better
#Not significant 
#AIC = 602.0 
overdisp_fun(test3.RIS.glmm)
check_overdispersion(test3.RIS.glmm)
#Overdispersion decteted

#Binomial glmm (random - lake) - Random effect model
test.RE.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ 1 + (1|Lake), family = binomial, data = mod.data)
summary(test.RE.glmm)
#AIC = 1101.5
overdisp_fun(test.RE.glmm)
check_overdispersion(test.RE.glmm)
#Overdispersion detected

#Betabinomial model (no random effect)
test1.betabin <- betabin(cbind(inf_fish, tot_fish - inf_fish) ~ TN_TP.T + TOC.T, ~1, data = mod.data, link = "logit")
summary(test1.betabin)
#AIC = 343.1
#Aucun significatif
#Pas de surdispersion

#Betabinomial model (random = lake)
test2.betabin <- betabin(cbind(inf_fish, tot_fish - inf_fish) ~ TN_TP.T + TOC.T, ~Lake, data = mod.data, link = "logit")
summary(test2.betabin)
#AIC = 338.7 (mais AICc supérieur)
#Aucun significatif
#Pas de surdispersion