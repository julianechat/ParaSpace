## Script name : Model analysis

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

library(dplyr)
library(vegan)
library(ggplot2)
library(cowplot)
library(patchwork)
library(gratia)
library(mgcv)
library(gamlss)
library(itsadug)

source(paste0(to.R, "rquery.cormat.R"))
source(paste0(to.R, "inverse_logit_trans.R"))

## Loading data ----

mod.data <- read.csv(paste0(to.output, "ModelAnalysis_Df.csv"))

# ---- Data analysis ----

mod.data$Lake <- as.factor(mod.data$Lake)
mod.data$Watershed <- as.factor(mod.data$Watershed)
mod.data$Transect_ID <- as.factor(mod.data$Transect_ID)

## One predictor GAMMs ----

### Null ----
#### Model
#Our null model is a random effects model
NULL.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(NULL.GAMM) #Variable significant
#Adj. R-sq. = 0.62
#Deviance explained = 68.6%

#### Model validation
appraise(NULL.GAMM)
gam.check(NULL.GAMM)
#Model validation not so good. Probably due to missing predictors.

### TN:TP ----
#### Model
TNTP.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(TNTP.GAMM) #All variable significant
#Adj. R-sq. = 0.845
#Deviance explained = 87.1%

TNTP.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + random(Lake), 
                     family = BB, data = mod.data, REML = TRUE)
#Cannot converge

#### Model validation
appraise(TNTP.GAMM, method = "simulate")
gam.check(TNTP.GAMM)
TNTP.GAMM$scale
#Model validation is OK

#### Visualizing partial effects
draw.TNTP <- draw(TNTP.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TNTP

#### Visualizing summed effects
plot_smooth(TNTP.GAMM, view = "TN_TP.T", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(25,55), xlab = "TN:TP",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

plot_smooth(TNTP.GAMM, view = "TN_TP.T", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(25,55), xlab = "TN:TP",
            rug = FALSE, 
            hide.label = TRUE) 

#### Lake mean model
TNTP.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.L, bs = "cr") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(TNTP.GAMM.L) #unsignificative

### Nitrogen ----
#### Model
TN.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN.T, bs = "cr") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data, method = "ML")
summary(TN.GAMM) #TN unsignificative
#Adj. R-sq. = 0.65
#Deviance explained = 72.6%

TN.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN.T) + random(Lake), 
                     family = BB, data = mod.data, REML = TRUE, method = mixed())
summary(TN.GAMM.BB)

#### Model validation
appraise(TN.GAMM)
gam.check(TN.GAMM)
TN.GAMM$scale
#Model validation shows some residual patterns

#### Visualizing partial effects
draw.TN <- draw(TN.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TN

#### Lake mean model
TN.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(TN.GAMM.L) #unsignificative

### Phosphorus ----
#### Model
TP.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TP.T, bs = "cr") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data, method = "ML")
summary(TP.GAMM) #TP not significative
#Adj. R-sq. = 0.667
#Deviance explained = 73.5%

TP.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TP.T) + random(Lake), 
                     family = BB, data = mod.data, REML = TRUE, method = mixed())
#Cannot converge

#### Model validation
appraise(TP.GAMM)
gam.check(TP.GAMM)
TP.GAMM$scale
#Model validation shows some residual patterns

#### Visualizing partial effects
draw.TP <- draw(TP.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TP

#### Lake mean model
TP.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TP.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(TP.GAMM.L) #unsignificative

### Carbon ----
#### Model
TOC.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TOC.T, bs = "cs") + s(Lake, bs = "re"),
                family = quasibinomial, data = mod.data, method = "ML")
summary(TOC.GAMM) #TOC not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

TOC.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TOC.T) + random(Lake), 
                      family = BB, data = mod.data, REML = TRUE, method = mixed())
#Cannot converge

#### Model validation
appraise(TOC.GAMM)
gam.check(TOC.GAMM)
TOC.GAMM$scale
#Model validation shows some residual patterns

#### Visualizing partial effects
draw.TOC <- draw(TOC.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) +
  scale_y_continuous(trans = inverse_logit_trans)
draw.TOC

#### Lake mean model
TOC.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TOC.L, bs = "cr") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(TOC.GAMM.L) #unsignificative

### Sub 1 ----
#### Model
SUB1.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub1, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(SUB1.GAMM) #Sub1 is not significant
#Adj. R-sq. = 0.62
#Deviance explained  = 69.6%

SUB1.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Sub1) + random(Lake), 
                      family = BB, data = mod.data, REML = TRUE, method = mixed())
#Cannot converge

#### Model validation
appraise(SUB1.GAMM)
gam.check(SUB1.GAMM)
SUB1.GAMM$scale
#Model validation shows some residual patterns

#### Visualizing partial effects
draw.SUB1 <- draw(SUB1.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) +
  scale_y_continuous(trans = inverse_logit_trans)
draw.SUB1

### Sub 2 ----
#### Model
SUB2.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub2, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(SUB2.GAMM) #Sub2 is not significant
#Adj. R-sq. = 0.62
#Deviance explaine = 69.6%

SUB2.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Sub2) + random(Lake), 
                       family = BB, data = mod.data, REML = TRUE, method = mixed())
#Cannot converge

#### Model validation
appraise(SUB2.GAMM)
gam.check(SUB2.GAMM)
SUB2.GAMM$scale
#Model validation shows some residual patterns

#### Visualizing partial effects
draw.SUB2 <- draw(SUB2.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.SUB2

### Macrophyte ----
MACRO.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(MACRO.GAMM) #All variables are significant
#Adj. R-sq. = 0.813
#Deviance explained = 84.2%

MACRO.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Macrophyte) + random(Lake), 
                       family = BB, data = mod.data, REML = TRUE, method = mixed())
#Cannot converge

#### Model validation
appraise(MACRO.GAMM)
gam.check(MACRO.GAMM)
MACRO.GAMM$scale
#Could be better...

#### Visualizing partial effects
draw.MACRO <- draw(MACRO.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.MACRO
#Signiticativity OK

#### Visualizing summed effects
plot_smooth(MACRO.GAMM, view = "Macrophyte", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,80),
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

plot_smooth(MACRO.GAMM, view = "Macrophyte", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,80),
            rug = FALSE, 
            hide.label = TRUE) 

### Transect depth ----
#### Model
DEPTH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Depth, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(DEPTH.GAMM) #Depth is not significant
#Adj. R- sq. = 0.62
#Deviance explained = 69.6%

DEPTH.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Depth) + random(Lake), 
                        family = BB, data = mod.data, REML = TRUE, method = mixed())
summary(DEPTH.GAMM.BB) #Not significant

#### Model validation
appraise(DEPTH.GAMM)
gam.check(DEPTH.GAMM)
DEPTH.GAMM$scale
#Model validation shows some residual patterns

#### Visualizing partial effects
draw.DEPTH <- draw(DEPTH.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.DEPTH

### Trunk ----
#### Model
TRUNK.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Trunk, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(TRUNK.GAMM) #Trunk is not significant
# Adj. R-sq. = 0.62
#Deviance explained = 69.6%

TRUNK.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Trunk) + random(Lake), 
                        family = BB, data = mod.data, REML = TRUE, method = mixed())
summary(TRUNK.GAMM.BB) #Not significant

#### Model validation
appraise(TRUNK.GAMM)
gam.check(TRUNK.GAMM)
TRUNK.GAMM$scale
#Model validation shows some residual patterns

#### Visualizing partial effects
draw.TRUNK <- draw(TRUNK.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TRUNK

### Temperature ----
#### Model
TEMP.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.T, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(TEMP.GAMM) #All variables are significant
#Adj. R-sq. = 0.745
#Deviance explained = 79.5%

TEMP.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Temp.T) + random(Lake), 
                        family = BB, data = mod.data, REML = TRUE, method = mixed())
summary(TEMP.GAMM.BB) #Not significant

#### Model validation
appraise(TEMP.GAMM)
gam.check(TEMP.GAMM)
TEMP.GAMM$scale
#Validation is fine

#### Visualizing partial effects
draw.TEMP <- draw(TEMP.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TEMP

#### Visualizing summed effect
plot_smooth(TEMP.GAMM, view = "Temp.T", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(19,26), xlab = "Temperature",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

plot_smooth(TEMP.GAMM, view = "Temp.T", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(19,26), xlab = "Temperature",
            rug = FALSE, 
            hide.label = TRUE) 

#### Lake mean effect
TEMP.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.L, bs = "cr") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(TEMP.GAMM.L) #unsignificative

### Turbidity ----
#### Model
TURB.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(TURB.GAMM) #All variables are significant
#Adj. R-sq. = 0.853
#Deviance explained = 88.7%

TURB.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Turb.T) + random(Lake), 
                       family = BB, data = mod.data, REML = TRUE, method = mixed())
#Cannot converge

#### Model validation
appraise(TURB.GAMM) #pretty good
gam.check(TURB.GAMM)
TURB.GAMM$scale
#Model validation is fine

#### Visualizing partial effects
draw.TURB <- draw(TURB.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TURB

#### Visualizing summed effects
plot_smooth(TURB.GAMM, view = "Turb.T", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,5), xlab = "Turbidity",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

plot_smooth(TURB.GAMM, view = "Turb.T", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,5), xlab = "Turbidity",
            rug = FALSE, 
            hide.label = TRUE) 

#### Lake mean model
TURB.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.L, bs = "cr") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(TURB.GAMM.L) #unsignificative

### pH ----
#### Model
PH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(pH.T, bs = "cs") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data, method = "ML")
summary(PH.GAMM) #All variables are significant
#Adj. R-sq. = 0.649
#Deviance explained = 70.5%

PH.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(pH.T) + random(Lake), 
                       family = BB, data = mod.data, REML = TRUE, method = mixed())
summary(PH.GAMM.BB) #All variable significant

#### Model validation
appraise(PH.GAMM)
gam.check(PH.GAMM)
PH.GAMM$scale
#Model validation is fine

#### Visualizing partial effects
draw.PH <- draw(PH.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) +
  scale_y_continuous(trans = inverse_logit_trans)
draw.PH

#### Visualizing summed effects
plot_smooth(PH.GAMM, view = "pH.T", rm.ranef = FALSE, 
            transform = plogis,
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(5,8.5), xlab = "pH",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

plot_smooth(PH.GAMM, view = "pH.T", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(5,8.5), xlab = "pH",
            rug = FALSE, 
            hide.label = TRUE) 

#### Lake mean model
PH.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(pH.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(PH.GAMM.L) #significative

### Oxygen ----
#### Model
DO.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(DO.T, bs = "cs") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data, method = "ML")
summary(DO.GAMM) #All variables are significant
#Adj. R-sq. = 0.683
#Deviance explained = 75.3%

DO.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(DO.T) + random(Lake), 
                     family = BB, data = mod.data, REML = TRUE, method = mixed())
#Connot converge

#### Model validation
appraise(DO.GAMM, method = "simulate")
gam.check(DO.GAMM)
DO.GAMM$scale
#Some residual patterns, but OK

#### Visualizing partial effects
draw.DO <- draw(DO.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.DO

#### Visualizing summed effects
plot_smooth(DO.GAMM, view = "DO.T", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(4,10), xlab = "DO",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

plot_smooth(DO.GAMM, view = "DO.T", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(5,8.5), xlab = "DO",
            rug = FALSE, 
            hide.label = TRUE) 

#### Lake mean model
DO.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(DO.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(DO.GAMM.L) #unsignificative

### Conductivity ----
#### Model
COND.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Cond.T, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(COND.GAMM) #Conductivity is significant (but not Lake)
#Adj. R-sq. = 0.536
#Deviance explained = 56.4%

COND.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Cond.T) + random(Lake), 
                     family = BB, data = mod.data, REML = TRUE, method = mixed())
summary(COND.GAMM.BB) #All variables are significant
plot(COND.GAMM.BB)

#### Model validation
appraise(COND.GAMM)
gam.check(COND.GAMM)
COND.GAMM$scale
#Model validation is not good

#### Visualizing partial effects
draw.COND <- draw(COND.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.COND

#### Visualizing summed effect
plot_smooth(COND.GAMM, view = "Cond.T", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,200), xlab = "Conductivity",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

plot_smooth(COND.GAMM, view = "Cond.T", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,200), xlab = "Conductivity",
            rug = FALSE, 
            hide.label = TRUE) 

#### Lake mean model
COND.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Cond.L, bs = "cr") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(COND.GAMM.L) #significative

### Area:Perimeter ----
#### Model
AREAPERI.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Area_Perimeter, bs = "cs") + s(Lake, bs = "re"),
                     family = quasibinomial, data = mod.data, method = "ML")
summary(AREAPERI.GAMM) #Area:Perimeter is significant (but not lake)
#Adj. R-sq. = 0.63
#Deviance explained = 67.8%

AREAPERI.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Area_Perimeter) + random(Lake), 
                       family = BB, data = mod.data, REML = TRUE, method = mixed())
summary(AREAPERI.GAMM.BB) #Area:Perimeter is significant

#### Model validation
appraise(AREAPERI.GAMM)
gam.check(AREAPERI.GAMM)
AREAPERI.GAMM$scale
#Model validation shows some residual patterns

#### Visualizing partial effects
draw.AREAPERI <- draw(AREAPERI.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.AREAPERI

#### Visualizng summed effect
plot_smooth(AREAPERI.GAMM, view = "Area_Perimeter", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 250), xlab = "Area:Perimeter",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

plot_smooth(AREAPERI.GAMM, view = "Area_Perimeter", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 250), xlab = "Area:Perimeter",
            rug = FALSE, 
            hide.label = TRUE) 

### Area ----
#### Model
AREA.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Lake_area, bs = "cs") + s(Lake, bs = "fs"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(AREA.GAMM) #Lake area is not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

AREA.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Lake_area) + random(Lake), 
                           family = BB, data = mod.data, REML = TRUE, method = mixed())
#Model cannot converge

#### Model validation
appraise(AREA.GAMM)
gam.check(AREA.GAMM)
AREA.GAMM$scale
#Model validation show some residual patterns

#### Visualizing partial effects
draw.AREA <- draw(AREA.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.AREA

### Perimeter ----
#### Model 
PERI.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Perimeter, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(PERI.GAMM) #Perimeter is significant (but not lake)
#Adj. R-sq. = 0.63
#Deviance explained = 69.9%

PERI.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Perimeter) + random(Lake), 
                       family = BB, data = mod.data, REML = TRUE, method = mixed())
#Model cannot converge

#### Mode validation
appraise(PERI.GAMM)
gam.check(PERI.GAMM)
PERI.GAMM$scale
#Model validation shows some residual patterns

#### Visualizing partial effects
draw.PERI <- draw(PERI.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.PERI

#### Visualizing summed effect
plot_smooth(PERI.GAMM, view = "Perimeter", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 25000), xlab = "Perimeter",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

plot_smooth(PERI.GAMM, view = "Perimeter", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 25000), xlab = "Perimeter",
            rug = FALSE, 
            hide.label = TRUE) 

### Mean depth ----
MDEPTH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Mean_depth, bs = "cs") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(MDEPTH.GAMM) #Mean depth is not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

MDEPTH.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Mean_depth) + random(Lake), 
                       family = BB, data = mod.data, REML = TRUE, method = mixed())
summary(MDEPTH.GAMM.BB) #Model is not significant

#### Model validation
appraise(MDEPTH.GAMM)
gam.check(MDEPTH.GAMM)
MDEPTH.GAMM$scale
#Model validation show some residual patterns

#### Visualizing partial effects
draw.MDEPTH <- draw(MDEPTH.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.MDEPTH

### Maximum depth ----
XDEPTH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Max_depth, bs = "cs") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(XDEPTH.GAMM) #Maximum depth is not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

XDEPTH.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Max_depth) + random(Lake), 
                         family = BB, data = mod.data, REML = TRUE, method = mixed())
summary(XDEPTH.GAMM.BB) #Model is not significant

#### Model validation
appraise(XDEPTH.GAMM)
gam.check(XDEPTH.GAMM)
XDEPTH.GAMM$scale
#Model validation show some residual patterns

#### Visualizing partial effects
draw.XDEPTH <- draw(XDEPTH.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.XDEPTH

### Water residence time ----
#### Model
WRT.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(WRT, bs = "cs") + s(Lake, bs = "re"),
                family = quasibinomial, data = mod.data, method = "ML") 
summary(WRT.GAMM) #WRT is not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

WRT.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(WRT) + random(Lake), 
                         family = BB, data = mod.data, REML = TRUE, method = mixed())
#Model cannot converge

#### Model validation
appraise(WRT.GAMM)
gam.check(WRT.GAMM)
WRT.GAMM$scale
#Model validation show some residual patterns

#### Visualizing partial effects
draw.WRT <- draw(WRT.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.WRT

### Drainage area ----
#### Model
DRAIN.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Drainage_area, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(DRAIN.GAMM) #Drainage area is not significant
#Adj. R-sq. = 0.621
#Deviance explained = 69.7%

DRAIN.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Drainage_area) + random(Lake), 
                      family = BB, data = mod.data, REML = TRUE, method = mixed())
#Model cannot converge

#### Model validation
appraise(DRAIN.GAMM)
gam.check(DRAIN.GAMM)
DRAIN.GAMM$scale
#Model validation shows some residual patterns

#### Visualizing partial effects
draw.DRAIN <- draw(DRAIN.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.DRAIN

### Elevation ----
#### Model
ELEV.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Elevation, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(ELEV.GAMM) #Elevation is not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

ELEV.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Elevation) + random(Lake), 
                        family = BB, data = mod.data, REML = TRUE, method = mixed())
#Model cannot converge

#### Model validation
appraise(ELEV.GAMM)
gam.check(ELEV.GAMM)
ELEV.GAMM$scale
#Model validation shows some residual patterns

#### Visualizing partial effects
draw.ELEV <- draw(ELEV.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.ELEV

### Centrarchids ----
#### Model
CENT.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Centrarchids.T, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(CENT.GAMM) #Centrarchids is not significant

CENT.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Centrarchids.T) + random(Lake), 
                        family = BB, data = mod.data, REML = TRUE, method = mixed())
#Model cannot converge

#### Model validation
appraise(CENT.GAMM)
gam.check(CENT.GAMM)
CENT.GAMM$scale
#Model validation shows some residual patterns

#### Visualizing partial effects
draw.CENT <- draw(CENT.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.CENT

#### Lake mean model
CENT.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Centrarchids.L, bs = "cr") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(CENT.GAMM.L) #unsignificative

### Species richness ----
#### Model
SP.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Species_richness.T, bs = "cs", k = 5) + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data, method = "ML")
summary(SP.GAMM) #Species richness is not significant
#Adj. R-sq. = 0.664
#Deviance explained = 73.6%

SP.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Species_richness.T) + random(Lake), 
                      family = BB, data = mod.data, REML = TRUE, method = mixed())
summary(SP.GAMM.BB) #Model is not significant

#### Model validation
appraise(SP.GAMM)
gam.check(SP.GAMM)
SP.GAMM$scale
#Model validation shows some patterns

#### Visualizing partial effects
draw.SP <- draw(SP.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.SP

#### Lake mean model
SP.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Species_richness.L, bs = "cr", k = 4) + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(SP.GAMM.L) #Species richness is significant

### Diversity ----
DIVERS.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Diversity.T, bs = "cs") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(DIVERS.GAMM) #All variables are significant
#Adj. R-sq. = 0.743
#Deviance explained = 79.7%

DIVERS.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(Diversity.T) + random(Lake), 
                     family = BB, data = mod.data, REML = TRUE, method = mixed())
#Model cannot converge

#### Model validation
appraise(DIVERS.GAMM)
gam.check(DIVERS.GAMM)
DIVERS.GAMM$scale
#Model validation shows some residual patterns, but OK. 

#### Visualizing partial effects
draw.DIVERS <- draw(DIVERS.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.DIVERS

#### Visualizing summed effects
DIVERS.fv <- fitted_values(DIVERS.GAMM, data = mod.data, scale = "response")
ggplot(DIVERS.fv) +
  geom_point(aes(x = Diversity.T, y = fitted, color = Lake)) + 
  geom_smooth(aes(x = Diversity.T, y = fitted), color = "black", method = "gam", formula = y~s(x, bs = "cr"))

plot_smooth(DIVERS.GAMM, view = "Diversity.T", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 0.7), xlab = "Diversity",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

plot_smooth(DIVERS.GAMM, view = "Diversity.T", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 0.7), xlab = "Diversity",
            rug = FALSE, 
            hide.label = TRUE) 

#### Lake mean model
DIVERS.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Diversity.L, bs = "cr") + s(Lake, bs = "re"),
                     family = quasibinomial, data = mod.data, method = "ML")
summary(DIVERS.GAMM.L) #unsignificative

# ---- Figures ----
## Significant one variable GAMMs summed plots ----
pdf(paste0(to.figs, "GAM_SummedEffects.pdf"), width = 30, height = 15)

par(mfrow = c(2, 5), mar = c(5,5,3,1))

plot_smooth(TNTP.GAMM, view = "TN_TP.T", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(25,55), xlab = "TN:TP",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(MACRO.GAMM, view = "Macrophyte", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,80),
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(TEMP.GAMM, view = "Temp.T", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(19,26), xlab = "Temperature",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(TURB.GAMM, view = "Turb.T", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,5), xlab = "Turbidity",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(PH.GAMM, view = "pH.T", rm.ranef = FALSE, 
            transform = plogis,
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(5,8.5), xlab = "pH",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(DO.GAMM, view = "DO.T", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(4,10), xlab = "DO",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(COND.GAMM, view = "Cond.T", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,200), xlab = "Conductivity",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(AREAPERI.GAMM, view = "Area_Perimeter", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 250), xlab = "Area:Perimeter",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(PERI.GAMM, view = "Perimeter", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 25000), xlab = "Perimeter",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(DIVERS.GAMM, view = "Diversity.T", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 0.7), xlab = "Diversity",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

dev.off()

## Correlation plot of significant variables ----
significant.data <- mod.data %>% 
  select_("TN_TP.T", "Macrophyte", "Temp.T", "Turb.T", "pH.T", "DO.T", "Cond.T", "Area_Perimeter", "Perimeter", "Diversity.T")

par(mfrow = c(1, 1), mar = c(3,3,3,1))

pdf(paste0(to.figs, "GAM_Corrplot.pdf"), width = 10, height = 10)

significant.corrplot <- rquery.cormat(significant.data, type = "full")

dev.off()
