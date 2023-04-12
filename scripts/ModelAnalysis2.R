# ---- Script setup ----
## R Setup ----

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"
to.carto <- "./carto/"

## Loading packages & functions ----

library(dplyr)
library(vegan)
library(ggplot2)
library(cowplot)
library(patchwork)
library(gratia)

source(paste0(to.R, "rquery.cormat.R"))
source(paste0(to.R, "glmm_funs.R"))

## Loading data ----

ParaSpaceMod <- read.csv(paste0(to.output, "Transects_Lake_Data.csv"))

# ---- Data exploration ----
#Creating new subsrtate variables with PCA axis
substrate.vars <- ParaSpaceMod[c(7:10)]

substrate.rda <- rda(substrate.vars, scale = FALSE) 
summary(substrate.rda)
biplot(substrate.rda, scaling = 1)
biplot(substrate.rda, scaling = 2)

site.scores <- scores(substrate.rda, choices = c(1,2), display= "sites", tidy = FALSE) #Extracting axis 
sub1 <- site.scores[,1] #PCA1 - sub1 explains most of the variation in percentage of silt & rock.
sub2 <- site.scores[,2] #PCA2 - sub2 explains most of the variation in percentage of block.

#Adjusting data frame
mod.data <- ParaSpaceMod %>% 
  mutate(TN_TP.T = TN.T / TP.T) %>% relocate(TN_TP.T, .after = "TOC.T") %>% 
  mutate(TN_TP.L = TN.L /TP.L) %>% relocate(TN_TP.L, .after = "TOC.L") %>% 
  mutate(Area_Perimeter = (Lake_area*1000000/Perimeter)) %>% relocate(Area_Perimeter, .before = "Mean_depth") %>% 
  mutate(Sub1 = sub1) %>% relocate(Sub1, .before = "Macrophyte") %>% 
  mutate(Sub2 = sub2)  %>% relocate(Sub2, .before = "Macrophyte")

mod.data <- within(mod.data, rm("Silt", "Sand", "Rock", "Block"))

# ---- Data analysis ----

## Testing method on nutrient model ##
library(lme4)
library(performance)
library(glmmTMB)
library(MASS)
library(aod)
library(mgcv)
library(gamlss)
library(gamm4)
library(DHARMa)
library(mgcViz)

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

### General additive models ###
#Binomial gam - TN_TP smooth
test1.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T), family = binomial, data = mod.data, method = "REML")
summary(test1.gam)
#Donne adj. R-sq (1.114) et deviance explained (27.4%)
#Significatif
plot(test1.gam)
appraise(test1.gam, method = "simulate")

#Binomial gam - TN_TP cubic regression smooth
test2.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr"), family = binomial, data = mod.data, method = "REML")
summary(test2.gam)
#Adj. R-sq = 0.13
#Deviance explained = 28.4%
plot(test2.gam)
appraise(test2.gam, method = "simulate")

#Binomial gam - TN_TP & TOC cubic regression smooth
test3.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(TOC.T, bs = "cr"), family = binomial, data = mod.data, method = "REML")
summary(test3.gam)
#Adj. R-sq = 0.625
#Deviance explained = 75.4%
plot(test3.gam)
check_overdispersion(test3.gam)
appraise(test3.gam, method = "simulate")

#Quasibinomial gam - TN_TP & TOC cubic regression smooth
test4.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(TOC.T, bs = "cr"), family = quasibinomial, data = mod.data, method = "REML")
summary(test4.gam)
#Adj. R-sq = 0.536
#Deviance explained = 57.1%
#Est-ce qu'il faut regarder sur dispersion dans un gam ?
#REML bcp plus petit (mieux) que test3.gam
plot(test4.gam)
gam.check(test4.gam) #Interprétation ?

check_overdispersion(test4.gam) #Améliore un peu la sudispersion...
appraise(test4.gam, method = "simulate")

#Betabinomial gam - TN_TP & TOC cubic regression smooth
test5.gam <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), family = BB, data = mod.data2)
summary(test5.gam)
#Pas sur de coprendre la sortie
#AIC = 334.3 (comme betabin3)
plot(test5.gam)

#Binomial gamm (no random effect)
test1.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~1, family = binomial, data = mod.data, method = "REML")
summary(test1.gamm)
#All significatives
#Adj. R-sq. = 0.302
#Deviance explained = 29.8
appraise(test1.gamm, method = "simulate")

#Binomial gamm (random = Lake)
test2.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~Lake, family = binomial, data = mod.data, method = "REML")
summary(test2.gamm)
#All significatives
#Adj. R-sq. = 0.302
#Deviance explained = 29.8
#Exactement la même sortie...
check_overdispersion(test2.gamm)
appraise(test2.gamm, method = "simulate")

#Quasibinomial gamm (no random effect)
test3.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~1, family = quasibinomial, data = mod.data, method = "REML")
summary(test3.gamm)
#TOC significatif
#Adj. R-sq. = 0.302
#Deviance explained = 29.8%
#MAIIS REML beaucoup plus bas (donc better fit ?)
appraise(test3.gamm, method = "simulate")

#Quasibinomial gamm (random effect = lake)
test4.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(TOC.T, bs = "cr"), random = ~Lake, family = quasibinomial, data = mod.data2, method = "REML")
summary(test4.gamm)
#TOC significatif
#Adj. R-sq. = 0.302
#Deviance explained = 29.8
#Comme test3.gamm
check_overdispersion(test4.gamm)
#mmmh dispersion ratio encore plus haut que binomial sans effet aléatoire. Bizzare
gam.check(test4.gamm)
appraise(test4.gamm, method = "simulate")

#Betabinomial gamm (no random effect)
test5.gamm <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~Lake, family = BB, data = mod.data2)
summary(test5.gamm)
#No significatif
#AIC = 334.3
#Exactement même sortie que sans l'effet aléatoire (test5.gam)

#Binomial gamm (OLRE random effect)
test6.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~Lake + Transect_ID, family = quasibinomial, data = mod.data, method = "REML")
summary(test6.gamm)
#Adj. R-sq. = 0.309
#Deviance explained =29.8%
#Significatif
appraise(test6.gamm, method = "simulate")


### Unique var gamm ###
##Gratia functions##
#draw() - for visualizing smmooths
#gam.check() - for k test and edf values
#appraise(mod, method = "simulate") - for diagnostic plots ("simulate" method is better for GLM-like method)
mod.data$Lake <- as.factor(mod.data$Lake)
mod.data2 <- na.omit(mod.data)
mod.data2$Lake <- as.factor(mod.data2$Lake)

##tests with TNTP
#BetaBinomial gam
TNTP.GAMM <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T), random = ~Lake, 
                   family = BB, data = mod.data2, REML = TRUE)
summary(TNTP.GAMM)
#AIC = 342.88
#No significative

#Binomial gam 
TNTP.GAMM2 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Lake, bs = "re"),
                  family = binomial, data = mod.data2, method = "REML")
summary(TNTP.GAMM2)
appraise(TNTP.GAMM2, method = "simulate")
testDispersion(TNTP.GAMM2)
check_overdispersion(TNTP.GAMM2)
gam.check(TNTP.GAMM2)

#Quasibinomial gam - correcting for overdispersion
TNTP.GAMM3 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(TNTP.GAMM3)
appraise(TNTP.GAMM3, method = "simulate")
check_overdispersion(TNTP.GAMM3)
gam.check(TNTP.GAMM3)
#It appears to be the best model so far
anova(TNTP.GAMM2, TNTP.GAMM3)

#Quaisbionmial random slope doesnt really help
TNTP.GAMM4 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Lake, bs = "re") + s(Lake, TN_TP.T, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(TNTP.GAMM4)
appraise(TNTP.GAMM4)

TNTP.GAMM5 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Lake, TN_TP.T, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(TNTP.GAMM5)
appraise(TNTP.GAMM5)

anova(TNTP.GAMM3, TNTP.GAMM5, TNTP.GAMM4)
#Random slope and intercept seems to be the best model 

#Visualizaing with draw()
#Cant change y axis with plogis
draw.TNTP <- draw(TNTP.GAMM3, unconditional = TRUE, overall_uncertainty = TRUE)
draw.TNTP

#Visualizing with plot()
plot.TNTP <- plot(TNTP.GAMM3, trans = plogis, residuals = TRUE, 
                  shift = coef(TNTP.GAMM)[1], seWithMean = TRUE, 
                  pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                  ylab = "Prevalence", xlab = "TN:TP", 
                  select = 1)
plot.TNTP

gam.check(TNTP.GAMM3)
TNTP.GAMM3$sp
appraise(TNTP.GAMM3)

#lake
TNTP.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.L, bs = "cr") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(TNTP.GAMM.L) #unsignificative

#Nitrogen
TN.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN.T, bs = "cr") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(TN.GAMM) #unsignificative
appraise(TN.GAMM)
check_overdispersion(TN.GAMM)
gam.check(TN.GAMM)


plot.TN <- plot(TN.GAMM, trans = plogis, residuals = TRUE, 
                  shift = coef(TN.GAMM)[1], seWithMean = TRUE, 
                  pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                  ylab = "Prevalence", xlab = "TN", 
                  select = 1)
draw.TN <- draw(TN.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.TN

TN.GAMM2 <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN.T) + random(Lake), 
                    family = BB, data = mod.data2, REML = TRUE, method = mixed())
summary(TN.GAMM2)

#lake
TN.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN.L, bs = "cr") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data2, method = "REML")
summary(TN.GAMM.L) #unsignificative

#Phosphorus
TP.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TP.T, bs = "cr") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data2, method = "REML")
summary(TP.GAMM) #unsignificative
appraise(TP.GAMM)
check_overdispersion(TP.GAMM)
gam.check(TP.GAMM)

plot.TP <- plot(TP.GAMM, trans = plogis, residuals = TRUE, 
                shift = coef(TP.GAMM)[1], seWithMean = TRUE, 
                pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                ylab = "Prevalence", xlab = "TP", 
                select = 1)
draw.TP <- draw(TP.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.TP

#lake
TP.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TP.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "REML")
summary(TP.GAMM.L) #unsignificative

#Carbon
TOC.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TOC.T, bs = "cs") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data2, method = "REML")
summary(TOC.GAMM) #unsignificative
appraise(TOC.GAMM)
check_overdispersion(TOC.GAMM)
gam.check(TOC.GAMM) #TOC should be linear

plot.TOC <- plot(TOC.GAMM, trans = plogis, residuals = TRUE, 
                shift = coef(TOC.GAMM)[1], seWithMean = TRUE, 
                pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                ylab = "Prevalence", xlab = "TOC", 
                select =1)
draw.TOC <- draw(TOC.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.TOC

#lake
TOC.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TOC.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "REML")
summary(TOC.GAMM.L) #unsignificative

#Sub 1
SUB1.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub1, bs = "cs") + s(Lake, bs = "re"),
                family = quasibinomial, data = mod.data2, method = "REML")
summary(SUB1.GAMM) #unsignificative
appraise(SUB1.GAMM)
check_overdispersion(SUB1.GAMM)
gam.check(SUB1.GAMM)

plot.SUB1 <- plot(SUB1.GAMM, trans = plogis, residuals = TRUE, 
                 shift = coef(SUB1.GAMM)[1], seWithMean = TRUE, 
                 pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                 ylab = "Prevalence", xlab = "SUB1", 
                 select = 1)
draw.SUB1 <- draw(SUB1.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.SUB1

#Sub 2
SUB2.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub2, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "REML")
summary(SUB2.GAMM) #unsignificative
appraise(SUB2.GAMM)
check_overdispersion(SUB2.GAMM)
gam.check(SUB2.GAMM)

plot.SUB2 <- plot(SUB2.GAMM, trans = plogis, residuals = TRUE, 
                  shift = coef(SUB2.GAMM)[1], seWithMean = TRUE, 
                  pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                  ylab = "Prevalence", xlab = "SUB2", 
                  select = 1)
draw.SUB2 <- draw(SUB2.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.SUB2

#Macrophyte
MACRO.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(MACRO.GAMM) #significative
appraise(MACRO.GAMM)
check_overdispersion(MACRO.GAMM)
gam.check(MACRO.GAMM)

plot.MACRO <- plot(MACRO.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(MACRO.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "MACRO", 
                   select = 1)
draw.MACRO <- draw(MACRO.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.MACRO

#Depth
DEPTH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Depth, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(DEPTH.GAMM) #unsignificative
appraise(DEPTH.GAMM)
check_overdispersion(DEPTH.GAMM)
gam.check(DEPTH.GAMM)

plot.DEPTH <- plot(DEPTH.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(DEPTH.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "DEPTH", 
                   select = 1)
draw.DEPTH <- draw(DEPTH.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.DEPTH

#Trunk
TRUNK.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Trunk, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(TRUNK.GAMM) #unsignificative
appraise(TRUNK.GAMM) #some residual patterns
check_overdispersion(TRUNK.GAMM)
gam.check(TRUNK.GAMM)

plot.TRUNK <- plot(TRUNK.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(TRUNK.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "TRUNK", 
                   select = 1)
draw.TRUNK <- draw(TRUNK.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.TRUNK

#Temperature
TEMP.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.T, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(TEMP.GAMM) #significative
appraise(TEMP.GAMM)
check_overdispersion(TEMP.GAMM)
gam.check(TEMP.GAMM)

plot.TEMP <- plot(TEMP.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(TEMP.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "TEMP", 
                   select = 1)
draw.TEMP <- draw(TEMP.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.TEMP

#lake
TEMP.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "REML")
summary(TEMP.GAMM.L) #unsignificative

#Turbidity
TURB.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(TURB.GAMM) #significative
appraise(TURB.GAMM) #pretty good
check_overdispersion(TURB.GAMM)
gam.check(TURB.GAMM)

plot.TURB <- plot(TURB.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(TURB.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "TURB", 
                   select = 1)
draw.TURB <- draw(TURB.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.TURB

#lake
TURB.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "REML")
summary(TURB.GAMM.L) #unsignificative

#pH
PH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(pH.T, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(PH.GAMM) #significative
appraise(PH.GAMM)
check_overdispersion(PH.GAMM)
gam.check(PH.GAMM) #almost linear

plot.PH <- plot(PH.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(PH.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "PH", 
                   select = 1)
draw.PH <- draw(PH.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.PH

#lake
PH.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(pH.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "REML")
summary(PH.GAMM.L) #significative

#Oxygen
DO.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(DO.T, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(DO.GAMM) #significative
appraise(DO.GAMM, method = "simulate")
check_overdispersion(DO.GAMM)
gam.check(DO.GAMM)

plot.DO <- plot(DO.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(DO.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "DO", 
                   select = 1)
draw.DO <- draw(DO.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.DO

#lake
DO.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(DO.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "REML")
summary(DO.GAMM.L) #unsignificative

#Conductivity
COND.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Cond.T, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(COND.GAMM) #significative (but not for Lake)
appraise(COND.GAMM) #meh
check_overdispersion(COND.GAMM)
gam.check(COND.GAMM)

plot.COND <- plot(COND.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(COND.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "COND", 
                   select = 1)
draw.COND <- draw(COND.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.COND

#lake
COND.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Cond.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "REML")
summary(COND.GAMM.L) #significative

#Area:Perimeter
AREAPERI.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Area_Perimeter, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(AREAPERI.GAMM) #significative (but not lake)
appraise(AREAPERI.GAMM)
check_overdispersion(AREAPERI.GAMM)
gam.check(AREAPERI.GAMM)

plot.AREAPERI <- plot(AREAPERI.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(AREAPERI.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "AREAPERI", 
                   select = 1)
draw.AREAPERI <- draw(AREAPERI.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.AREAPERI

#Area
AREA.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Lake_area, bs = "cs") + s(Lake, bs = "fs"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(AREA.GAMM) #unsignificative
appraise(AREA.GAMM)
check_overdispersion(AREA.GAMM)
gam.check(AREA.GAMM)

plot.AREA <- plot(AREA.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(AREA.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "AREA", 
                   select = 1)
draw.AREA <- draw(AREA.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.AREA

#Perimeter
PERI.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Perimeter, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(PERI.GAMM) #significative (but not lake)
appraise(PERI.GAMM)
check_overdispersion(PERI.GAMM)
gam.check(PERI.GAMM)

plot.PERI <- plot(PERI.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(PERI.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "PERI", 
                   select = 1)
draw.PERI <- draw(PERI.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.PERI

#Mean depth
MDEPTH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Mean_depth, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(MDEPTH.GAMM) #unsignificative
appraise(MDEPTH.GAMM)
check_overdispersion(MDEPTH.GAMM)
gam.check(MDEPTH.GAMM)

plot.MDEPTH <- plot(MDEPTH.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(MDEPTH.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "MDEPTH", 
                   select = 1)
draw.MDEPTH <- draw(MDEPTH.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.MDEPTH

#Water residence time
WRT.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(WRT, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(WRT.GAMM) #unsignificative
appraise(WRT.GAMM)
check_overdispersion(WRT.GAMM)
gam.check(WRT.GAMM)

plot.WRT <- plot(WRT.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(WRT.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "WRT", 
                   select = 1)
draw.WRT <- draw(WRT.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.WRT

#Drainage area
DRAIN.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Drainage_area, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(DRAIN.GAMM) #unsignificative
appraise(DRAIN.GAMM)
check_overdispersion(DRAIN.GAMM)
gam.check(DRAIN.GAMM)

plot.DRAIN <- plot(DRAIN.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(DRAIN.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "DRAIN", 
                   select = 1)
draw.DRAIN <- draw(DRAIN.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.DRAIN

#Elevation
ELEV.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Elevation, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(ELEV.GAMM) #unsignificative
appraise(ELEV.GAMM)
check_overdispersion(ELEV.GAMM)
gam.check(ELEV.GAMM)

plot.ELEV <- plot(ELEV.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(ELEV.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "ELEV", 
                   select = 1)
draw.ELEV <- draw(ELEV.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.ELEV

#Centrarchids
CENT.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Centrarchids.T, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(CENT.GAMM) #unsignificative
appraise(CENT.GAMM)
check_overdispersion(CENT.GAMM)
gam.check(CENT.GAMM)

plot.CENT <- plot(CENT.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(CENT.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "CENT", 
                   select = 1)
draw.CENT <- draw(CENT.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.CENT

#lake
CENT.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Centrarchids.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "REML")
summary(CENT.GAMM.L) #unsignificative

#Species richness
SP.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Species_richness.T, bs = "cs", k = 5) + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(SP.GAMM) #unsignificative
appraise(SP.GAMM)
check_overdispersion(SP.GAMM)
gam.check(SP.GAMM)

plot.SP <- plot(SP.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(SP.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "SP", 
                   select = 1)
draw.SP <- draw(SP.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.SP

#lake
SP.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Species_richness.L, bs = "cr", k = 4) + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "REML")
summary(SP.GAMM.L) #significative
draw(SP.GAMM.L, residuals = TRUE)

#Diversity
DIVERS.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Diversity.T, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(DIVERS.GAMM) #significative
appraise(DIVERS.GAMM)
check_overdispersion(DIVERS.GAMM)
gam.check(DIVERS.GAMM)

plot.DIVERS <- plot(DIVERS.GAMM, trans = plogis, residuals = TRUE, 
                   shift = coef(DIVERS.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence", xlab = "DIVERS", 
                   select = 1)
draw.DIVERS <- draw(DIVERS.GAMM, unconditional = TRUE, overall_uncertainty = TRUE)
draw.DIVERS

#lake
DIVERS.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Diversity.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "REML")
summary(DIVERS.GAMM.L) #unsignificative

#All model plots
#Setting colors for lakes
color_pallete_function <- colorRampPalette(
  colors = c("red", "orange", "blue", "green"),
  space = "Lab")

num_colors <- nlevels(mod.data2$Lake)
lake_colors <- color_pallete_function(num_colors)

#Plot grid
pdf(paste0(to.figs, "GAM_plots.pdf"), width = 20, height = 15)
par(mfrow = c(4, 6), mar = c(2,2,2,1), xpd = TRUE)
plot.gam(TNTP.GAMM3, trans = plogis,
                  shift = coef(TNTP.GAMM)[1], seWithMean = TRUE, 
                  pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                  ylab = "Prevalence",
                  select = 1, main = "TN:TP", col = "red",  ylim = c(0,1))
points(x = mod.data2$TN_TP.T, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(TN.GAMM, trans = plogis,
                shift = coef(TN.GAMM)[1], seWithMean = TRUE, 
                pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                select = 1, main = "TN", ylim = c(0,1))
points(x = mod.data2$TN.T, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(TP.GAMM, trans = plogis,
                shift = coef(TP.GAMM)[1], seWithMean = TRUE, 
                pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                select = 1, main = "TP",  ylim = c(0,1))
points(x = mod.data2$TP.T, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(TOC.GAMM, trans = plogis,
                 shift = coef(TOC.GAMM)[1], seWithMean = TRUE, 
                 pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                 select  =1, main = "TOC", ylim = c(0,1))
points(x = mod.data2$TOC.T, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(SUB1.GAMM, trans = plogis,
                  shift = coef(SUB1.GAMM)[1], seWithMean = TRUE, 
                  pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                  select = 1, main = "SUB1",  ylim = c(0,1))
points(x = mod.data2$Sub1, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(SUB2.GAMM, trans = plogis,
                  shift = coef(SUB2.GAMM)[1], seWithMean = TRUE, 
                  pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                  select = 1, main = "SUB2",  ylim = c(0,1))
points(x = mod.data2$Sub2, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(MACRO.GAMM, trans = plogis,
                   shift = coef(MACRO.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   ylab = "Prevalence",
                   select = 1, main = "MACROPHYTE", col = "red",  ylim = c(0,1))
points(x = mod.data2$Macrophyte, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(DEPTH.GAMM, trans = plogis,
                   shift = coef(DEPTH.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   select = 1, main = "DEPTH",  ylim = c(0,1))
points(x = mod.data2$Depth, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(TRUNK.GAMM, trans = plogis, 
                   shift = coef(TRUNK.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   select = 1, main = "TRUNK",  ylim = c(0,1))
points(x = mod.data2$Trunk, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(TEMP.GAMM, trans = plogis,
                  shift = coef(TEMP.GAMM)[1], seWithMean = TRUE, 
                  pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                  select = 1, main = "TEMPERATURE", col = "red",  ylim = c(0,1))
points(x = mod.data2$Temp.T, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(TURB.GAMM, trans = plogis, 
                  shift = coef(TURB.GAMM)[1], seWithMean = TRUE, 
                  pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                  select = 1, main = "TURBIDITY", col = "red",  ylim = c(0,1))
points(x = mod.data2$Turb.T, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(PH.GAMM, trans = plogis,
                shift = coef(PH.GAMM)[1], seWithMean = TRUE, 
                pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                select = 1, main = "PH", col = "red",  ylim = c(0,1))
points(x = mod.data2$pH.T, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(DO.GAMM, trans = plogis,
                shift = coef(DO.GAMM)[1], seWithMean = TRUE, 
                pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                ylab = "Prevalence", 
                select = 1, main = "DO", col = "red",  ylim = c(0,1))
points(x = mod.data2$DO.T, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(COND.GAMM, trans = plogis,
                  shift = coef(COND.GAMM)[1], seWithMean = TRUE, 
                  pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                  select = 1, main = "CONDUCTIVITY", col = "red",  ylim = c(0,1))
points(x = mod.data2$Cond.T, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(AREAPERI.GAMM, trans = plogis,
                      shift = coef(AREAPERI.GAMM)[1], seWithMean = TRUE, 
                      pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                      select = 1, main = "AREA:PERIMETER", col = "red",  ylim = c(0,1))
points(x = mod.data2$Area_Perimeter, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(AREA.GAMM, trans = plogis,
                  shift = coef(AREA.GAMM)[1], seWithMean = TRUE, 
                  pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                  select = 1, main = "AREA",  ylim = c(0,1))
points(x = mod.data2$Lake_area, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(PERI.GAMM, trans = plogis,
                  shift = coef(PERI.GAMM)[1], seWithMean = TRUE, 
                  pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                  select = 1, main = "PERIMETER", col = "red",  ylim = c(0,1))
points(x = mod.data2$Perimeter, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(MDEPTH.GAMM, trans = plogis,
                    shift = coef(MDEPTH.GAMM)[1], seWithMean = TRUE, 
                    pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                    select = 1, main = "MEAN_DEPTH",  ylim = c(0,1))
points(x = mod.data2$Mean_depth, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(WRT.GAMM, trans = plogis,
                 shift = coef(WRT.GAMM)[1], seWithMean = TRUE, 
                 pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                 ylab = "Prevalence",
                 select = 1, main = "WTR",  ylim = c(0,1))
points(x = mod.data2$WRT, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(DRAIN.GAMM, trans = plogis,
                   shift = coef(DRAIN.GAMM)[1], seWithMean = TRUE, 
                   pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                   select = 1, main = "DRAINAGE_AREA",  ylim = c(0,1))
points(x = mod.data2$Drainage_area, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(ELEV.GAMM, trans = plogis,
                  shift = coef(ELEV.GAMM)[1], seWithMean = TRUE, 
                  pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                  select = 1, main = "ELEVATION",  ylim = c(0,1))
points(x = mod.data2$Elevation, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(CENT.GAMM, trans = plogis,
                  shift = coef(CENT.GAMM)[1], seWithMean = TRUE, 
                  pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                  select = 1, main = "CENTRARCHIDS",  ylim = c(0,1))
points(x = mod.data2$Centrarchids.T, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(SP.GAMM, trans = plogis,
                shift = coef(SP.GAMM)[1], seWithMean = TRUE, 
                pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                select = 1, main = "SP_RICHNESS", ylim = c(0,1))
points(x = mod.data2$Species_richness.T, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])

plot(DIVERS.GAMM, trans = plogis,
                    shift = coef(DIVERS.GAMM)[1], seWithMean = TRUE, 
                    pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                    select = 1, main = "DIVERSITY", col = "red",  ylim = c(0,1))
points(x = mod.data2$Diversity.T, y = mod.data2$prev_fish, pch = 1, col = lake_colors[mod.data2$Lake])
#legend("topright",
       #legend = levels(mod.data2$Lake),
       #inset = c(5, 0),
       #col = lake_colors,
       #pch = 1,
       #cex = .7)
#mtext("Significative smooths are in red\n Non significative smooths are in black\n Colors represent different lakes", side = 3, line = -4, cex = 0.75, col = "black", outer = TRUE)
dev.off()

# ---- Lake scale GAMs ---- 
#La plupart du temps, prendre les moyennes déperissent les modèles
#pH, Cond et Species Richness sont signfificatif avec les données à l'échelle du lac

#morpho
lake.morpho.gam1 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Area_Perimeter, bs = "cr") + s(Mean_depth, bs = "cr") + s(Lake, bs = "re"), 
                family = quasibinomial, data = mod.data2, method = "REML")
summary(lake.morpho.gam1) #both significative

#space
lake.space.gam1 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Elevation, bs = "cr") + s(Drainage_area, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "REML")
summary(lake.space.gam1)#nope

lake.space.gam2 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Elevation, bs = "cr") + s(WRT, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "REML")
summary(lake.space.gam2)#nope
       