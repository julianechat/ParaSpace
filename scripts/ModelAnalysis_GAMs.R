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

## ---- One predictor GAMMs ----

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
            ylim = c(0,1)) 

plot_smooth(TNTP.GAMM, view = "TN_TP.T", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1)) 

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
library(DHARMa)
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
            ylim = c(0,1)) 

plot_smooth(MACRO.GAMM, view = "Macrophyte", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1)) 

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
            ylim = c(0,1)) 

plot_smooth(TEMP.GAMM, view = "Temp.T", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1)) 

#### Lake mean effect
TEMP.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.L, bs = "cr") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data2, method = "ML")
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
            ylim = c(0,1)) 

plot_smooth(TURB.GAMM, view = "Turb.T", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1)) 

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
            ylim = c(0,1)) 

plot_smooth(PH.GAMM, view = "pH.T", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1)) 

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
            ylim = c(0,1)) 

plot_smooth(DO.GAMM, view = "DO.T", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1)) 

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
            ylim = c(0,1)) 

plot_smooth(COND.GAMM, view = "Cond.T", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1)) 

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
            ylim = c(0,1)) 

plot_smooth(AREAPERI.GAMM, view = "Area_Perimeter", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1)) 

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
            ylim = c(0,1)) 

plot_smooth(PERI.GAMM, view = "Perimeter", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1)) 

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
            ylim = c(0,1)) 

plot_smooth(DIVERS.GAMM, view = "Diversity.T", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1)) 

#### Lake mean model
DIVERS.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Diversity.L, bs = "cr") + s(Lake, bs = "re"),
                     family = quasibinomial, data = mod.data, method = "ML")
summary(DIVERS.GAMM.L) #unsignificative

#### ---- HERE ---- ####
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
                        family = quasibinomial, data = mod.data2, method = "ML")
summary(lake.morpho.gam1) #both significative
draw(lake.morpho.gam1)
#space
lake.space.gam1 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Elevation, bs = "cr") + s(Drainage_area, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(lake.space.gam1)

lake.space.gam2 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Elevation, bs = "cr") + s(WRT, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(lake.space.gam2)#nope

# ---- Transect scale GAMs ----
#nutrients
trans.nutrient.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(TOC.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "REML")
summary(trans.nutrient.gam) #TN_TP significative

#physico
trans.physico.gam1 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cr") + s(Temp.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam1) #Turb > Temp significative

trans.physico.gam2 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cr") + s(pH.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam2) #Turb > pH significative

trans.physico.gam3 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.T, bs = "cr") + s(pH.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam3) #Temp significative
draw(trans.physico.gam3)
appraise(trans.physico.gam3)

trans.physico.gam4 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cr") + s(DO.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam4) #Turb significative

trans.physico.gam5 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cr") + s(Cond.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam5) #Turb significative

trans.physico.gam6 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.T, bs = "cr") + s(DO.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam6) #Temp significative

trans.physico.gam7 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.T, bs = "cr") + s(Cond.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam7) #Temp significative

trans.physico.gam8 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Cond.T, bs = "cr") + s(DO.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam8) #DO significative

#Habitat
trans.habitat.gam1 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Sub1, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam1) #Macrophyte & Sub1 significative

trans.habitat.gam2 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Sub2, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam2) #Macrophyte & Sub2 significative

trans.habitat.gam3 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Depth, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam3) #Macrophyte significative

trans.habitat.gam4 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Trunk, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam4) #Macrophyte significative

trans.habitat.gam5 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub1, bs = "cr") + s(Depth, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam5) #Non significative

trans.habitat.gam6 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub1, bs = "cr") + s(Trunk, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam6) #Non significative

trans.habitat.gam7 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub2, bs = "cr") + s(Depth, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam7) #Sub2 & Depth lightly significative

trans.habitat.gam8 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub2, bs = "cr") + s(Trunk, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam8) #Non significative

trans.habitat.gam9 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Depth, bs = "cr") + s(Trunk, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam9) #Depth significative

#biotic
trans.biotic.gam1 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Diversity.T, bs = "cr") + s(Species_richness.T, bs = "cr", k = 5) + s(Lake, bs = "re"), 
                         family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.biotic.gam1) #Non significative

trans.biotic.gam2 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Diversity.T, bs = "cr") + s(Centrarchids.T, bs = "cr") + s(Lake, bs = "re"), 
                         family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.biotic.gam2) #Diversity & Centrarchids significative

trans.biotic.gam3 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Species_richness.T, bs = "cr", k = 5) + s(Centrarchids.T, bs = "cr") + s(Lake, bs = "re"), 
                         family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.biotic.gam3) #Non significative



## Two predictor GAMs ----
# Mixed variables models ----
trans.mix.gam1 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Macrophyte, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam1)  

trans.mix.gam2 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Temp.T, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam2)  

trans.mix.gam3 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Turb.T, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam3)
draw(trans.mix.gam3)
appraise(trans.mix.gam3, method = "simulate")

trans.mix.gam4 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(pH.T, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam4)  

trans.mix.gam5 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(DO.T, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam5)  

trans.mix.gam6 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Cond.T, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam6)  

trans.mix.gam7 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Area_Perimeter, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam7)  

trans.mix.gam8 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam8)  

trans.mix.gam9 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Temp.T, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam9)  

trans.mix.gam10 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Turb.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam10)  

trans.mix.gam11 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(pH.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam11)  

trans.mix.gam12 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(DO.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam12)  

trans.mix.gam13 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Cond.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam13)  

trans.mix.gam14 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Area_Perimeter, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam14)  

trans.mix.gam15 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam15)  

trans.mix.gam16 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.T, bs = "cr") + s(Area_Perimeter, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam16)  

trans.mix.gam17 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.T, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam17)  

trans.mix.gam18 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cr") + s(Area_Perimeter, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam18)  

trans.mix.gam19 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam19)  

trans.mix.gam20 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(pH.T, bs = "cr") + s(Area_Perimeter, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam20)  

trans.mix.gam21 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(pH.T, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam21)  

trans.mix.gam22 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(DO.T, bs = "cr") + s(Area_Perimeter, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam22)  

trans.mix.gam23 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(DO.T, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam23)  

trans.mix.gam24 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Cond.T, bs = "cr") + s(Area_Perimeter, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam24)  

trans.mix.gam25 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Cond.T, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam25)  

trans.mix.gam26 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Area_Perimeter, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam26)  


#--- Model selection with QAICc
#library(MuMIn)
#chat1 = deviance(trans.biotic.gam1) / df.residual(trans.biotic.gam1)
#chat2 = deviance(trans.biotic.gam2) / df.residual(trans.biotic.gam2)
#chat3 = deviance(trans.biotic.gam1) / df.residual(trans.biotic.gam3)
#options(na.action = "na.fail")



#as.data.frame(model.sel(trans.biotic.gam1, trans.biotic.gam2, trans.biotic.gam3, 
#                        rank = "QAICc", 
 #                       rank.args = alist(chat = chat)))

#options(na.action = "na.omit")
#QAICc(trans.biotic.gam1, chat = chat1)

#trans.biotic.gam1 <- update(trans.biotic.gam1,family="quasibinomial", na.action=na.fail) 

#dredge(trans.biotic.gam1, rank="QAICc", chat=chat1)
#gg
#QAICc(trans.biotic.gam1, chat = chat1, k = 2, REML = FALSE)

##works !
#sei.gam0.1 <- update(trans.biotic.gam1, family=binomial(link="logit"))
#sei.gam0.2 <- update(trans.physico.gam2, family=binomial(link="logit"))
#dredge(sei.gam0.1, rank="QAICc", chat=chat1)


#fitted plot avec significatifs


#corr plot avec significatifs