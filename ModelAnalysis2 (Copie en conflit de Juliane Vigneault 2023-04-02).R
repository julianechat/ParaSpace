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
TNTP.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(TNTP.GAMM$mer)
summary(TNTP.GAMM$gam)
plot.TNTP <- draw(TNTP.GAMM$gam, residuals = TRUE)

gam.check(TNTP.GAMM$gam) #suggest that k is too low... 
k.check(TNTP.GAMM$gam) #smaller the betterz
appraise(TNTP.GAMM$gam)

res <- resid(TNTP.GAMM$gam, type = "deviance")
res_model <- gam(res ~ s(TN_TP.T, bs = "cr", k = 10),
                 method = "ML",
                 family = quasi(link = "logit", variance = "constant"), 
                 data = mod.data)
edf(res_model)

TN.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN.T, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(TN.GAMM$mer)
summary(TN.GAMM$gam)
plot.TN <- draw(TN.GAMM$gam, residuals = TRUE)
plot.TN
plot(TN.GAMM$gam, residuals = TRUE, pch = 1, shift = coef(TN.GAMM$gam)[1], 
     seWithMean = TRUE, shade = TRUE, xlab = "TN")

fv <- fitted_values(TN.GAMM$gam, data = mod.data, scale = "response")
ggplot(fv, aes(x = TN.T, y = fitted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  geom_smooth(alpha = 0.4, method = "gam") + 
  geom_point(aes(TN.T, prev_fish), data = mod.data)

ggplot(mod.data, # Data frame that has our data
       aes(x = TN.T, # Our x variable
           y = prev_fish)) + # Our y variable
  geom_point() + # Choosing to plot our data in the form of points
  geom_smooth(method = "gam", # geom_smooth allows for GAM modeling in plot
              formula = y ~ s(x, # Specifies formula of our model
                              k = 10), # Number of basis functions
              se = TRUE, # Whether we want the gray 95% CI or not (we do!)
              method.args = list(family = "binomial")) + # Error distribution
  labs(x = "TN", # Customizing x and y axis labels
       y = "Prev_fish") + # Text between ~~ will be subscript (markdown syntax)
  theme_classic() + # Removes gray grid in background and adds lines to axes
  theme(axis.title = element_text(size = 12), # Font size for all axis titles
        axis.text = element_text(size = 10)) # Font size for all axis text
#______________________________________________________________________________


TP.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(TP.T, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(TP.GAMM$mer)
summary(TP.GAMM$gam)
plot.TP <- draw(TP.GAMM$gam, residuals = TRUE)

TOC.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(TOC.T, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(TOC.GAMM$mer)
summary(TOC.GAMM$gam)
plot.TOC <- draw(TOC.GAMM$gam, residuals = TRUE)

SUB1.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub1, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(SUB1.GAMM$mer)
summary(SUB1.GAMM$gam)
plot.SUB1 <- draw(SUB1.GAMM$gam, residuals = TRUE)

SUB2.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub2, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(SUB2.GAMM$mer)
summary(SUB2.GAMM$gam)
plot.SUB2 <- draw(SUB2.GAMM$gam, residuals = TRUE)

MACRO.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(MACRO.GAMM$mer)
summary(MACRO.GAMM$gam)
plot.MACRO <- draw(MACRO.GAMM$gam, residuals = TRUE)

DEPTH.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Depth, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(DEPTH.GAMM$mer)
summary(DEPTH.GAMM$gam)
plot.DEPTH <- draw(DEPTH.GAMM$gam, residuals = TRUE)

TRUNK.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Trunk, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(TRUNK.GAMM$mer)
summary(TRUNK.GAMM$gam)
plot.TRUNK <- draw(TRUNK.GAMM$gam, residuals = TRUE)

TEMP.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.T, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(TEMP.GAMM$mer)
summary(TEMP.GAMM$gam)
plot.TEMP <- draw(TEMP.GAMM$gam, residuals = TRUE)

TURB.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(TURB.GAMM$mer)
summary(TURB.GAMM$gam)
plot.TURB <- draw(TURB.GAMM$gam, residuals = TRUE)

PH.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(pH.T, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(PH.GAMM$mer)
summary(PH.GAMM$gam)
plot.PH <- draw(PH.GAMM$gam, residuals = TRUE)

DO.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(DO.T, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(DO.GAMM$mer)
summary(DO.GAMM$gam)
plot.DO <- draw(DO.GAMM$gam, residuals = TRUE)

COND.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Cond.T, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(COND.GAMM$mer)
summary(COND.GAMM$gam)
plot.COND <- draw(COND.GAMM$gam, residuals = TRUE)

AREAPER.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Area_Perimeter, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(AREAPER.GAMM$mer)
summary(AREAPER.GAMM$gam)
plot.AREAPER <- draw(AREAPER.GAMM$gam, residuals = TRUE)

AREA.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Lake_area, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(AREA.GAMM$mer)
summary(AREA.GAMM$gam)
plot.AREA <- draw(AREA.GAMM$gam, residuals = TRUE)

PERI.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Perimeter, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(PERI.GAMM$mer)
summary(PERI.GAMM$gam)
plot.PERI <- draw(PERI.GAMM$gam, residuals = TRUE)

MDEPTH.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Mean_depth, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(MDEPTH.GAMM$mer)
summary(MDEPTH.GAMM$gam)
plot.MDEPTH <- draw(MDEPTH.GAMM$gam, residuals = TRUE)

WRT.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(WRT, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(WRT.GAMM$mer)
summary(WRT.GAMM$gam)
plot.WRT <- draw(WRT.GAMM$gam, residuals = TRUE)

DRAIN.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Drainage_area, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(DRAIN.GAMM$mer)
summary(DRAIN.GAMM$gam)
plot.DRAIN <-draw(DRAIN.GAMM$gam, residuals = TRUE)

ELEV.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Elevation, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(ELEV.GAMM$mer)
summary(ELEV.GAMM$gam)
plot.ELEV <- draw(ELEV.GAMM$gam, residuals = TRUE)

CENT.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Centrarchids.T, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(CENT.GAMM$mer)
summary(CENT.GAMM$gam)
plot.CENT <- draw(CENT.GAMM$gam, residuals = TRUE)

SP.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Species_richness.T, bs = "cr", k = 6), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(SP.GAMM$mer)
summary(SP.GAMM$gam)
plot.SP <- draw(SP.GAMM$gam, residuals = TRUE)

DIVERS.GAMM <- gamm4(cbind(inf_fish, tot_fish - inf_fish) ~ s(Diversity.T, bs = "cr"), random = ~(1|Lake), family = binomial, data = mod.data, method = "REML")
summary(DIVERS.GAMM$mer)
summary(DIVERS.GAMM$gam)
plot.DIVERS <- draw(DIVERS.GAMM$gam, residuals = TRUE)

#geom_smooth(data = mod.data, aes(Diversity.T, prev_fish), col = 'red') +
  geom_smooth(alpha = 0.4)

gamm.resids_wrap <- plot.TN + plot.TP + plot.TNTP + plot.TOC + 
  plot.TEMP + plot.TURB + plot.PH + plot.DO + plot.COND + 
  plot.WRT + plot.ELEV + plot.DRAIN + 
  plot.PERI + plot.AREA +  plot.AREAPER + plot.MDEPTH + 
  plot.TRUNK + plot.SUB1 + plot.SUB2 + plot.MACRO + plot.DEPTH + 
  plot.SP + plot.CENT + plot.DIVERS + plot_layout(ncol = 4, nrow = 6)
ggsave(paste0(to.figs, "GAMM_residuals_wrap.png"), plot = gamm.resids_wrap, dpi = 500, width = 20, height = 10)

### without resids ###

plot.TNTP <- draw(TNTP.GAMM$gam)
plot.TN <- draw(TN.GAMM$gam)
plot.TP <- draw(TP.GAMM$gam)
plot.TOC <- draw(TOC.GAMM$gam)
plot.SUB1 <- draw(SUB1.GAMM$gam)
plot.SUB2 <- draw(SUB2.GAMM$gam)
plot.MACRO <- draw(MACRO.GAMM$gam)
plot.DEPTH <- draw(DEPTH.GAMM$gam)
plot.TRUNK <- draw(TRUNK.GAMM$gam)
plot.TEMP <- draw(TEMP.GAMM$gam)
plot.TURB <- draw(TURB.GAMM$gam)
plot.PH <- draw(PH.GAMM$gam)
plot.DO <- draw(DO.GAMM$gam)
plot.COND <- draw(COND.GAMM$gam)
plot.AREAPER <- draw(AREAPER.GAMM$gam)
plot.AREA <- draw(AREA.GAMM$gam)
plot.PERI <- draw(PERI.GAMM$gam)
plot.MDEPTH <- draw(MDEPTH.GAMM$gam)
plot.WRT <- draw(WRT.GAMM$gam)
plot.DRAIN <-draw(DRAIN.GAMM$gam)
plot.ELEV <- draw(ELEV.GAMM$gam)
plot.CENT <- draw(CENT.GAMM$gam)
plot.SP <- draw(SP.GAMM$gam)
plot.DIVERS <- draw(DIVERS.GAMM$gam)

gamm_wrap <- plot.TN + plot.TP + plot.TNTP + plot.TOC + 
  plot.TEMP + plot.TURB + plot.PH + plot.DO + plot.COND + 
  plot.WRT + plot.ELEV + plot.DRAIN + 
  plot.PERI + plot.AREA +  plot.AREAPER + plot.MDEPTH + 
  plot.TRUNK + plot.SUB1 + plot.SUB2 + plot.MACRO + plot.DEPTH + 
  plot.SP + plot.CENT + plot.DIVERS + plot_layout(ncol = 4, nrow = 6)
ggsave(paste0(to.figs, "GAMM_wrap.png"), plot = gamm_wrap, dpi = 500, width = 20, height = 10)


#
fv <- fitted_values(DIVERS.GAMM$gam, data = mod.data, scale = "response")
ggplot(fv, aes(x = Diversity.T, y = fitted)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  geom_smooth(alpha = 0.4, method = "gam") + 
  geom_point(aes(Diversity.T, prev_fish), data = mod.data)

ggplot(data = mod.data, aes(Diversity.T, prev_fish)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", random = ~Lake, k = 10))


