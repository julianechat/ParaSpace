## Script name : Model analysis GAMMs

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
to.rédaction <- "./rédaction/"

## Loading packages & functions ----

library(vegan)
library(ggplot2)
library(cowplot)
library(patchwork)
library(gratia)
library(mgcv)
library(itsadug)
library(colorspace)
library(broom)
library(gt)
library(dplyr)

source(paste0(to.R, "rquery.cormat.R"))
source(paste0(to.R, "inverse_logit_trans.R"))

## Loading data ----

mod.data <- read.csv(paste0(to.output, "ModelAnalysis_Corrected.csv"))

# ---- Data analysis ----

mod.data$Lake <- as.factor(mod.data$Lake)
mod.data$Watershed <- as.factor(mod.data$Watershed)
mod.data$Transect_ID <- as.factor(mod.data$Sampling_ID)

sequential_hcl(palette = "YlOrBr", 5)
col.pal <- c("#682714", "#C75C00", "#F2A400", "#FAE094", "#FEFEE3")

## Null ----
### Site scale model ----

#Our null model is a random effects model

NULL.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(NULL.GAMM) 
#Variable significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

### Model validation ----

appraise(NULL.GAMM)
NULL.GAMM$scale
#Model validation not so good. Evidence of residual patterns. Probably due to missing predictors.

## TN:TP ----
### Site scale model ----

TNTP.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP, bs = "cr") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(TNTP.GAMM) 
#All variable significant
#Adj. R-sq. = 0.845
#Deviance explained = 87.1%

### Model validation ----

appraise(TNTP.GAMM, method = "simulate")
TNTP.GAMM$scale
#Model validation is OK

### Visualizing partial effects ----

#Simple visualization
draw.TNTP <- draw(TNTP.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TNTP #Signiticativity OK

#Extracting model estimates and setting graph aesthetics 
TNTP.sm <- smooth_estimates(TNTP.GAMM) %>%
  add_confint()
TNTP.pr <- mod.data %>%
  add_partial_residuals(TNTP.GAMM)
TNTP.pe <- TNTP.sm %>%
  filter(.smooth == "s(TN_TP)") %>%
  ggplot( unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = TN_TP),
           data = TNTP.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = TN_TP), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = TN_TP, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "TN:TP", y = "Partial effect (prevalence)", tag = "F") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank())
TNTP.pe

### Visualizing summed effects ----

#Combined smooth
plot_smooth(TNTP.GAMM, view = "TN_TP", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(25,55), xlab = "TN:TP",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

#Lake's smooths
plot_smooth(TNTP.GAMM, view = "TN_TP", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(25,55), xlab = "TN:TP",
            rug = FALSE, 
            hide.label = TRUE) 

### Lake scale model ----

TNTP.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.L, bs = "cr") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(TNTP.GAMM.L) 
#Unsignificative

## Nitrogen ----
### Site scale model ----

TN.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN, bs = "cr") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data, method = "ML")
summary(TN.GAMM) 
#TN unsignificative
#Adj. R-sq. = 0.65
#Deviance explained = 72.6%

### Model validation ----

appraise(TN.GAMM)
TN.GAMM$scale
#Model validation shows some residual patterns

### Visualizing partial effects ----

#Simple visualization
draw.TN <- draw(TN.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TN

### Lake mean model ----

TN.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(TN.GAMM.L) 
#Unsignificative

## Phosphorus ----
### Site scale model ----

TP.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TP, bs = "cr") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data, method = "ML")
summary(TP.GAMM) 
#TP not significative
#Adj. R-sq. = 0.667
#Deviance explained = 73.5%

### Model validation ----

appraise(TP.GAMM)
TP.GAMM$scale
#Model validation shows some residual patterns

### Visualizing partial effects ----

#Simple visualization
draw.TP <- draw(TP.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TP

### Lake scale model ----

TP.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TP.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(TP.GAMM.L) 
#Unsignificative

## Carbon ----
### Site scale model ----

TOC.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TOC, bs = "cs") + s(Lake, bs = "re"),
                family = quasibinomial, data = mod.data, method = "ML")
summary(TOC.GAMM) 
#TOC not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

### Model validation ----

appraise(TOC.GAMM)
TOC.GAMM$scale
#Model validation shows some residual patterns

### Visualizing partial effects ----

#Simple visualization
draw.TOC <- draw(TOC.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) +
  scale_y_continuous(trans = inverse_logit_trans)
draw.TOC

### Lake scale model ----
TOC.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TOC.L, bs = "cr") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(TOC.GAMM.L) #unsignificative

## Silt ----

### Site scale model ----

SILT.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Silt, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(SILT.GAMM) 
#Unsignificative
#Adj. R-Sq. = 0.62
#Deviance explained = 69.6%

### Model validation ----

appraise(SILT.GAMM)
SILT.GAMM$scale
#Model validation shows some residual patterns

### Visualizing partial effects ----

#Simple visualization
draw.SILT <- draw(SILT.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.SILT
#Signiticativity not OK

## Sand ----

### Site scale model ----

SAND.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sand, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(SAND.GAMM) 
#Unsignificative
#Adj. R-Sq. = 0.663
#Deviance explained = 74.2%

### Model validation ----

appraise(SAND.GAMM)
SAND.GAMM$scale
#Model validation shows some residual patterns, scale better than Null model

### Visualizing partial effects ----

#Simple visualization
draw.SAND <- draw(SAND.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.SAND
#Signiticativity not OK, evidence a negative trend

## Rock ----
### Site scale model ----

ROCK.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Rock, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(ROCK.GAMM) 
#Unsignificative
#Adj. R-Sq. = 0.62
#Deviance explained = 69.6%

### Model validation ----

appraise(ROCK.GAMM)
ROCK.GAMM$scale
#Model validation shows some residual patterns

### Visualizing partial effects ----

#Simple visualization
draw.ROCK <- draw(ROCK.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.ROCK
#Signiticativity not OK

## Boulder ----
### Site scale model ----

BOULD.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Boulder, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(BOULD.GAMM) 
#Unsignificative
#Adj. R-Sq. = 0.62
#Deviance explained = 69.6%

### Model validation ----

appraise(BOULD.GAMM)
BOULD.GAMM$scale
#Model validation shows some residual patterns

### Visualizing partial effects ----

#Simple visualization
draw.BOULD <- draw(BOULD.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.BOULD
#Signiticativity not OK

## Macrophyte ----

### Site scale model ----

MACRO.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(MACRO.GAMM) 
#All variables are significant
#Adj. R-sq. = 0.813
#Deviance explained = 84.2%

### Model validation ----

appraise(MACRO.GAMM)
MACRO.GAMM$scale
#Some residual patterns, but scale is better than random effect only

### Visualizing partial effects ----

#Simple visualization
draw.MACRO <- draw(MACRO.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.MACRO
#Signiticativity OK

#Extracting model estimates and setting graph aesthetics 
MACRO.sm <- smooth_estimates(MACRO.GAMM) %>%
  add_confint()
MACRO.pr <- mod.data %>%
  add_partial_residuals(MACRO.GAMM)
MACRO.pe <- MACRO.sm %>%
  filter(.smooth == "s(Macrophyte)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Macrophyte),
           data = MACRO.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Macrophyte), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Macrophyte, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Macrophyte coverage (%)", y = "Partial effect (prevalence)", tag = "G") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank())
MACRO.pe

### Visualizing summed effects ----

#Combined smooth
plot_smooth(MACRO.GAMM, view = "Macrophyte", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,80),
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

#Lake's smooths
plot_smooth(MACRO.GAMM, view = "Macrophyte", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,80),
            rug = FALSE, 
            hide.label = TRUE) 

## Transect depth ----

### Site scale model ----

DEPTH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Site_depth, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(DEPTH.GAMM) 
#Depth is not significant
#Adj. R- sq. = 0.62
#Deviance explained = 69.6%

### Model validation ----

appraise(DEPTH.GAMM)
DEPTH.GAMM$scale
#Model validation shows some residual patterns

### Visualizing partial effects ----

#Simple visualization
draw.DEPTH <- draw(DEPTH.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.DEPTH

## Trunk ----

### Site scale model ----

TRUNK.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Trunk, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(TRUNK.GAMM) 
#Trunk is not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

### Model validation ----

appraise(TRUNK.GAMM)
TRUNK.GAMM$scale
#Model validation shows some residual patterns

### Visualizing partial effects ----

#Simple visualization
draw.TRUNK <- draw(TRUNK.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TRUNK

## Temperature ----

### Site scale model ----

TEMP.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temperature, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(TEMP.GAMM) 
#All variables are significant
#Adj. R-sq. = 0.745
#Deviance explained = 79.5%

### Model validation ----

appraise(TEMP.GAMM)
TEMP.GAMM$scale
#Validation is OK

### Visualizing partial effects ----

#Simple visualization
draw.TEMP <- draw(TEMP.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TEMP
#Significativity OK.

#Extracting model estimates and setting graph aesthetics 
TEMP.sm <- smooth_estimates(TEMP.GAMM) %>%
  add_confint()
TEMP.pr <- mod.data %>%
  add_partial_residuals(TEMP.GAMM)
TEMP.pe <- TEMP.sm %>%
  filter(.smooth == "s(Temperature)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Temperature),
           data = TEMP.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Temperature), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Temperature, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Temperature (°C)", y = "Partial effect (prevalence)", tag = "B") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank())
TEMP.pe

### Visualizing summed effect ----

#Combined smooth
plot_smooth(TEMP.GAMM, view = "Temperature", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(19,26), xlab = "Temperature",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

#Lake's smooths
plot_smooth(TEMP.GAMM, view = "Temperature", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(19,26), xlab = "Temperature",
            rug = FALSE, 
            hide.label = TRUE) 

### Lake scale model ----

TEMP.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.L, bs = "cr") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(TEMP.GAMM.L) 
#Unsignificative

## Turbidity ----

### Site scale model ----

TURB.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turbidity, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(TURB.GAMM) 
#All variables are significant
#Adj. R-sq. = 0.853
#Deviance explained = 88.7%

### Model validation ----

appraise(TURB.GAMM)
TURB.GAMM$scale
#Model validation is OK

### Visualizing partial effects ----

#Simple visualization
draw.TURB <- draw(TURB.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TURB
#Significativity OK

#Extracting model estimates and setting graph aesthetics 
TURB.sm <- smooth_estimates(TURB.GAMM) %>%
  add_confint()
TURB.pr <- mod.data %>%
  add_partial_residuals(TURB.GAMM)
TURB.pe <- TURB.sm %>%
  filter(.smooth == "s(Turbidity)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Turbidity),
           data = TURB.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Turbidity), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Turbidity, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Turbidity (NTU)", y = "Partial effect (prevalence)", tag = "A") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank())
TURB.pe

### Visualizing summed effects ----

#Combined smooth
plot_smooth(TURB.GAMM, view = "Turbidity", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,5), xlab = "Turbidity",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

#Lake's smooths
plot_smooth(TURB.GAMM, view = "Turbidity", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,5), xlab = "Turbidity",
            rug = FALSE, 
            hide.label = TRUE) 

### Lake scale model ----

TURB.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.L, bs = "cr") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(TURB.GAMM.L) 
#Unsignificative

## pH ----

### Site scale model ----

PH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(pH, bs = "cs") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data, method = "ML")
summary(PH.GAMM) 
#All variables are significant
#Adj. R-sq. = 0.649
#Deviance explained = 70.5%

### Model validation ----

appraise(PH.GAMM)
PH.GAMM$scale
#Model validation is OK

### Visualizing partial effects ----

#Simple visualization
draw.PH <- draw(PH.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) +
  scale_y_continuous(trans = inverse_logit_trans) 
draw.PH
#Significativity OK

#Extracting model estimates and setting graph aesthetics 
PH.sm <- smooth_estimates(PH.GAMM) %>%
  add_confint()
PH.pr <- mod.data %>%
  add_partial_residuals(PH.GAMM)
PH.pe <- PH.sm %>%
  filter(.smooth == "s(pH)") %>%
  ggplot( unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = pH),
           data = PH.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = pH), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = pH, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "pH", y = "Partial effect(prevalence)", tag = "C") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank())
PH.pe

### Visualizing summed effects ----

#Combined smooth
plot_smooth(PH.GAMM, view = "pH", rm.ranef = FALSE, 
            transform = plogis,
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(5,8.5), xlab = "pH",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

#Lake's smooths
plot_smooth(PH.GAMM, view = "pH", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(5,8.5), xlab = "pH",
            rug = FALSE, 
            hide.label = TRUE) 

### Lake scale model ----

PH.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(pH.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(PH.GAMM.L) 
#Significative

## Dissolved oxygen ----

### Site scale model ----

DO.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(DO, bs = "cs") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data, method = "ML")
summary(DO.GAMM) 
#All variables are significant
#Adj. R-sq. = 0.683
#Deviance explained = 75.3%

### Model validation ----
appraise(DO.GAMM, method = "simulate")
DO.GAMM$scale
#Some residual patterns. Scale slightly better than Null model

### Visualizing partial effects ----

#Simple visualization
draw.DO <- draw(DO.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans) 
draw.DO
#Significativity not OK

#Extracting model estimates and setting graph aesthetics 
DO.sm <- smooth_estimates(DO.GAMM) %>%
  add_confint()
DO.pr <- mod.data %>%
  add_partial_residuals(DO.GAMM)
DO.pe <- DO.sm %>%
  filter(.smooth == "s(DO)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = DO),
           data = DO.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = DO), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = DO, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Dissolved oxygen (mg/L)", y = "Partial effect (prevalence)", tag = "E") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank())
DO.pe

### Visualizing summed effects ----

#Combined smooth
plot_smooth(DO.GAMM, view = "DO", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(4,10), xlab = "DO",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

#Lake's smooths
plot_smooth(DO.GAMM, view = "DO", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(5,8.5), xlab = "DO",
            rug = FALSE, 
            hide.label = TRUE) 

### Lake scale model ----

DO.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(DO.L, bs = "cr") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(DO.GAMM.L) 
#Unsignificative

## Conductivity ----

### Site scale model ----

COND.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Conductivity, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(COND.GAMM) 
#Conductivity is significant (but not Lake). I tested without the random effect from the lake and it didn't change the model estimates.
#Adj. R-sq. = 0.536
#Deviance explained = 56.4%

### Model validation ----
appraise(COND.GAMM)
COND.GAMM$scale
#Model validation is not good. Big gap in data

### Visualizing partial effects ----

#Simple visualization
draw.COND <- draw(COND.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans) 
draw.COND
#Significativity OK

#Extracting model estimates and setting graph aesthetics 
COND.sm <- smooth_estimates(COND.GAMM) %>%
  add_confint()
COND.pr <- mod.data %>%
  add_partial_residuals(COND.GAMM)
COND.pe <- COND.sm %>%
  filter(.smooth == "s(Conductivity)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Conductivity),
           data = COND.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Conductivity), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Conductivity, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Conductivity (μS/cm)", y = "Partial effect (prevalence)", tag = "D") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank())
COND.pe

### Visualizing summed effect ----

#Combined smooth
plot_smooth(COND.GAMM, view = "Conductivity", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,200), xlab = "Conductivity",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

#Lake's smooths
plot_smooth(COND.GAMM, view = "Conductivity", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,200), xlab = "Conductivity",
            rug = FALSE, 
            hide.label = TRUE) 

### Lake scale model ----

COND.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Conductivity, bs = "cr") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(COND.GAMM.L) 
#Significative (but not lake)

## Area:Perimeter ----
### Site scale model ----

AREAPERI.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Area_Perimeter, bs = "cs") + s(Lake, bs = "re"),
                     family = quasibinomial, data = mod.data, method = "ML")
summary(AREAPERI.GAMM) 
#Area:Perimeter is significant (but not lake)
#Adj. R-sq. = 0.63
#Deviance explained = 67.8%

### Model validation ----

appraise(AREAPERI.GAMM)
AREAPERI.GAMM$scale
#Model validation shows some residual patterns, but scale is better than Null model

### Visualizing partial effects ----

#Simple visualization
draw.AREAPERI <- draw(AREAPERI.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.AREAPERI
#Significativity OK. 

#Extracting model estimates and setting graph aesthetics 
AREAPERI.sm <- smooth_estimates(AREAPERI.GAMM) %>%
  add_confint()
AREAPERI.pr <- mod.data %>%
  add_partial_residuals(AREAPERI.GAMM)
AREAPERI.pe <- AREAPERI.sm %>%
  filter(.smooth == "s(Area_Perimeter)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Area_Perimeter),
           data = AREAPERI.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Area_Perimeter), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Area_Perimeter, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Area:Perimeter (m)", y = "Partial effect (prevalence)", tag = "H") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank())
AREAPERI.pe

### Visualizng summed effect ----

#Combined smooth
plot_smooth(AREAPERI.GAMM, view = "Area_Perimeter", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 250), xlab = "Area:Perimeter",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

#Lake's smooth
plot_smooth(AREAPERI.GAMM, view = "Area_Perimeter", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 250), xlab = "Area:Perimeter",
            rug = FALSE, 
            hide.label = TRUE) 

## Surface area ----

### Site scale model ----

AREA.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Lake_area, bs = "cs") + s(Lake, bs = "fs"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(AREA.GAMM) 
#Lake surface area is not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

### Model validation ----

appraise(AREA.GAMM)
AREA.GAMM$scale
#Model validation show some residual patterns

### Visualizing partial effects ----

#Simple visualization
draw.AREA <- draw(AREA.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.AREA
#Significativity not OK

## Perimeter ----

### Site scale model ----

PERI.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Perimeter, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(PERI.GAMM) 
#Perimeter is significant (but not lake)
#Adj. R-sq. = 0.63
#Deviance explained = 69.9%

### Model validation ----

appraise(PERI.GAMM)
PERI.GAMM$scale
#Model validation shows some residual patterns, but scale is better than Null model

### Visualizing partial effects ----

#Simple visualization
draw.PERI <- draw(PERI.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans) 
draw.PERI
#Significativity OK

#Extracting model estimates and setting graph aesthetics 
PERI.sm <- smooth_estimates(PERI.GAMM) %>%
  add_confint()
PERI.pr <- mod.data %>%
  add_partial_residuals(PERI.GAMM)
PERI.pe <- PERI.sm %>%
  filter(.smooth == "s(Perimeter)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Perimeter),
           data = PERI.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Perimeter), fill ="#682714", alpha = 0.5) +
  geom_line(aes(x = Perimeter, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Perimeter (m)", y = "Partial effect (prevalence)", tag = "I") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank())
PERI.pe

### Visualizing summed effect ----

#Combined smooth
plot_smooth(PERI.GAMM, view = "Perimeter", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 25000), xlab = "Perimeter",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

#Lake's smooths
plot_smooth(PERI.GAMM, view = "Perimeter", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 25000), xlab = "Perimeter",
            rug = FALSE, 
            hide.label = TRUE) 

## Mean lake depth ----

### Site scale model ----

MDEPTH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Mean_depth, bs = "cs") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(MDEPTH.GAMM) 
#Mean depth is not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

### Model validation ----

appraise(MDEPTH.GAMM)
MDEPTH.GAMM$scale
#Model validation show some residual patterns

### Visualizing partial effects ----

#Simple visualization
draw.MDEPTH <- draw(MDEPTH.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.MDEPTH
#Significativity not OK

## Maximum lake depth ----

### Site scale model ----

XDEPTH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Max_depth, bs = "cs") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(XDEPTH.GAMM) 
#Maximum depth is not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

### Model validation ----

appraise(XDEPTH.GAMM)
XDEPTH.GAMM$scale
#Model validation show some residual patterns

### Visualizing partial effects ----

#Simple visualization
draw.XDEPTH <- draw(XDEPTH.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.XDEPTH
#Significativity not OK

## Nearest lake ----

### Site scale model ----

NEAR.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Connectivity, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(NEAR.GAMM) 
#Distance to nearest lake is not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

### Model validation ----
appraise(NEAR.GAMM)
NEAR.GAMM$scale
#Model validation shows some residual patterns

### Visualizing partial effects

#Simple visualization
draw.NEAR <- draw(NEAR.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.NEAR
#Significativity not OK.

## Water residence time ----

### Site scale model ----

WRT.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(WRT, bs = "cs") + s(Lake, bs = "re"),
                family = quasibinomial, data = mod.data, method = "ML") 
summary(WRT.GAMM) 
#WRT is not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

### Model validation ----
appraise(WRT.GAMM)
WRT.GAMM$scale
#Model validation show some residual patterns

### Visualizing partial effects ----

#Simple visualization
draw.WRT <- draw(WRT.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.WRT
#Significativity not OK

## Drainage area ----

### Site scale model ----

DRAIN.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Drainage_area, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(DRAIN.GAMM) 
#Drainage area is not significant
#Adj. R-sq. = 0.621
#Deviance explained = 69.7%

### Model validation ----
appraise(DRAIN.GAMM)
DRAIN.GAMM$scale
#Model validation shows some residual patterns

### Visualizing partial effects

#Simple visualization
draw.DRAIN <- draw(DRAIN.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.DRAIN
#Significativity not OK. Might be something there but not enough large drainage area in our data

## Elevation ----
### Site scale model ----

ELEV.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Elevation, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(ELEV.GAMM) 
#Elevation is not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

### Model validation ----

appraise(ELEV.GAMM)
ELEV.GAMM$scale
#Model validation shows some residual patterns

### Visualizing partial effects ----

#Simple visualization
draw.ELEV <- draw(ELEV.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.ELEV
#Significativity not OK

## Fish abundance ----
### Site scale model ----

FISH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(tot_fish, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(FISH.GAMM) 
#All variables are significant
#Adj. R-sq. = 0.747
#Deviance explained = 80.8%

### Model validation ----

appraise(FISH.GAMM)
FISH.GAMM$scale
#Some residual patterns, but scale is better than Null model

### Visualizing partial effects ----

#Simple visualization
draw.FISH <- draw(FISH.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.FISH
#Significativity is OK

#Extracting model estimates and setting graph aesthetics 
FISH.sm <- smooth_estimates(FISH.GAMM) %>%
  add_confint()
FISH.pr <- mod.data %>%
  add_partial_residuals(FISH.GAMM)
FISH.pe <- FISH.sm %>%
  filter(.smooth == "s(tot_fish)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = tot_fish),
           data = FISH.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = tot_fish), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = tot_fish, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Fish abundance", y = "Partial effect (prevalence)", tag = "J") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank()) 
FISH.pe

### Visualizing summed effects ----

#Combined smooth
plot_smooth(FISH.GAMM, view = "tot_fish", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 0.7), xlab = "Fish abundance",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

#Lake's smooths
plot_smooth(FISH.GAMM, view = "tot_fish", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 0.7), xlab = "Fish abundance",
            rug = FALSE, 
            hide.label = TRUE) 

## Non-host abundance ----

### Site scale model ----

NONHOST.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(tot_Cyprinidae, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(NONHOST.GAMM)
#All variables are significant
#Adj. R-sq. = 0.856
#Deviance explained = 87.5%

### Model validation ----

appraise(NONHOST.GAMM)
NONHOST.GAMM$scale
#Some residual patterns but OK. Scale better than Null model

### Visualizing partial effects ----

#Simple visualization
draw.NONHOST<- draw(NONHOST.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.NONHOST
#Significativity is OK

#Extracting model estimates and setting graph aesthetics 
NONHOST.sm <- smooth_estimates(NONHOST.GAMM) %>%
  add_confint()
NONHOST.pr <- mod.data %>%
  add_partial_residuals(NONHOST.GAMM)
NONHOST.pe <- NONHOST.sm %>%
  filter(.smooth == "s(tot_Cyprinidae)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = tot_Cyprinidae),
           data = NONHOST.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = tot_Cyprinidae), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = tot_Cyprinidae, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Non-host abundance", y = "Partial effect (prevalence)", tag = "K") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank()) 
NONHOST.pe

### Visualizing summed effects ----

#Combined smooth
plot_smooth(NONHOST.GAMM, view = "tot_Cyprinidae", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 0.7), xlab = "Non-host abundance",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

#Lake's smooths
plot_smooth(NONHOST.GAMM, view = "tot_Cyprinidae", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 0.7), xlab = "Non-host abundance",
            rug = FALSE, 
            hide.label = TRUE) 

## Species richness ----

### Site scale model ----

SP.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Species_richness, bs = "cs", k = 5) + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data, method = "ML")
summary(SP.GAMM) 
#Species richness is not significant
#Adj. R-sq. = 0.664
#Deviance explained = 73.6%

### Model validation ----

appraise(SP.GAMM)
SP.GAMM$scale
#Model validation shows some residual patterns

### Visualizing partial effects ----

#Simple visualization
draw.SP <- draw(SP.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.SP
#Significativity not OK

### Lake scale model ----

SP.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(SpR.L, bs = "cr", k = 4) + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(SP.GAMM.L) #Species richness is significant

## Diversity Index ----

### Site scale model ----

DIVERS.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Diversity, bs = "cs") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(DIVERS.GAMM) 
#All variables are significant
#Adj. R-sq. = 0.743
#Deviance explained = 79.7%

### Model validation ----

appraise(DIVERS.GAMM)
DIVERS.GAMM$scale
#Model validation shows some residual patterns, but OK. Scale is better than Null model

### Visualizing partial effects ----

#Simple visualization
draw.DIVERS <- draw(DIVERS.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans) 
draw.DIVERS
#Significativity not OK

#Extracting model estimates and setting graph aesthetics 
DIVERS.sm <- smooth_estimates(DIVERS.GAMM) %>%
  add_confint()
DIVERS.pr <- mod.data %>%
  add_partial_residuals(DIVERS.GAMM)
DIVERS.pe <- DIVERS.sm %>%
  filter(.smooth == "s(Diversity)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Diversity),
           data = DIVERS.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Diversity), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Diversity, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Simpson's Diversity Index", y = "Partial effect (prevalence)", tag = "L") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank()) 
DIVERS.pe

### Visualizing summed effects ----

#Combined smooth
plot_smooth(DIVERS.GAMM, view = "Diversity", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 0.7), xlab = "Diversity",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 

#Lake's smooths
plot_smooth(DIVERS.GAMM, view = "Diversity", rm.ranef = FALSE, plot_all = "Lake",
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 0.7), xlab = "Diversity",
            rug = FALSE, 
            hide.label = TRUE) 

### Lake scale model ----

DIVERS.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Diversity.L, bs = "cr") + s(Lake, bs = "re"),
                     family = quasibinomial, data = mod.data, method = "ML")
summary(DIVERS.GAMM.L) 
#Unsignificative

## Evenness ----

### Site scale model ----

EVEN.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Evenness, bs = "cs") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(EVEN.GAMM) 
#All variables are significant
#Adj. R-sq. = 0.625
#Deviance explained = 69.9%

### Model validation ----

appraise(EVEN.GAMM)
EVEN.GAMM$scale
#Model validation shows some residual patterns.

### Visualizing partial effects ----

#Simple visualization
draw.EVEN <- draw(EVEN.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans) 
draw.EVEN
#Significativity not OK

### Lake scale model ----

EVEN.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Evenness.L, bs = "cr") + s(Lake, bs = "re"),
                     family = quasibinomial, data = mod.data, method = "ML")
summary(EVEN.GAMM.L) 
#Unsignificative

# ---- Figures ----

## Significant GAMMs summed plots summary ----

pdf(paste0(to.figs, "GAMMs_SummedEffects.pdf"), width = 30, height = 15)

par(mfrow = c(3, 4), mar = c(5,5,3,1))

plot_smooth(TURB.GAMM, view = "Turbidity", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,5), xlab = "Turbidity",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(TEMP.GAMM, view = "Temperature", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(19,26), xlab = "Temperature",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(PH.GAMM, view = "pH", rm.ranef = FALSE, 
            transform = plogis,
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(5,8.5), xlab = "pH",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(COND.GAMM, view = "Conductivity", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0,200), xlab = "Conductivity",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(DO.GAMM, view = "DO", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(4,10), xlab = "DO",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(TNTP.GAMM, view = "TN_TP", rm.ranef = FALSE, 
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
plot_smooth(FISH.GAMM, view = "tot_fish", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 0.7), xlab = "Fish abundance",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(NONHOST.GAMM, view = "tot_Cyprinidae", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 0.7), xlab = "Non-host abundance",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE) 
plot_smooth(DIVERS.GAMM, view = "Diversity", rm.ranef = FALSE, 
            transform = plogis, 
            ylim = c(0,1), ylab = "Infection prevalence",
            xlim = c(0, 0.7), xlab = "Diversity",
            col = "orange",
            rug = FALSE, 
            hide.label = TRUE)
dev.off()

## Correlation plot of significant variables ----

significant.data <- mod.data %>% 
  select_("TN_TP", "Macrophyte", "Temperature", "Turbidity", "pH", "DO", "Conductivity", "Area_Perimeter", "Perimeter", "Diversity", "tot_fish", "tot_Cyprinidae")

par(mfrow = c(1, 1), mar = c(3,3,3,1))
pdf(paste0(to.figs, "GAMMs_Corrplot.pdf"), width = 10, height = 10)

significant.corrplot <- rquery.cormat(significant.data, type = "full")

dev.off()

## Significant GAMMs partial plots summary ----

Summary.plot <- TURB.pe + TEMP.pe + PH.pe + COND.pe + DO.pe + TNTP.pe + MACRO.pe + AREAPERI.pe + PERI.pe + FISH.pe + NONHOST.pe + DIVERS.pe &
  theme(text = element_text(family = "Calibri Light", size = 40, color = "black"))

ggsave(paste0(to.figs, "GAMMs_PartialEffects.png"), plot = Summary.plot, dpi = 300, width = 45, height = 28)
ggsave(paste0(to.rédaction, "Figures/Figure7_GAMMs.png"), plot = Summary.plot, dpi = 300, width = 45, height = 28)

## Model parametric coefficient and smooth terms summary table ----

#Extracting deviance explained, parametric coefficient and smooth terms for all models
tab.NULL.par <- tidy(NULL.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Null", .before = "term",
         Deviance = summary(NULL.GAMM)$dev.expl)
tab.NULL.smooth <- tidy(NULL.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Null", .before = "term",
         Deviance = summary(NULL.GAMM)$dev.expl)
tab.NULL <- merge(tab.NULL.par, tab.NULL.smooth, all = TRUE)

tab.TNTP.par <- tidy(TNTP.GAMM, parametric = TRUE) %>% 
  mutate(Model = "TN:TP", .before = "term",
         Deviance = summary(TNTP.GAMM)$dev.expl)
tab.TNTP.smooth <- tidy(TNTP.GAMM, parametric = FALSE) %>% 
  mutate(Model = "TN:TP", .before = "term",
         Deviance = summary(TNTP.GAMM)$dev.expl)
tab.TNTP <- merge(tab.TNTP.par, tab.TNTP.smooth, all = TRUE)

tab.TN.par <- tidy(TN.GAMM, parametric = TRUE) %>% 
  mutate(Model = "TN", .before = "term",
         Deviance = summary(TN.GAMM)$dev.expl)
tab.TN.smooth <- tidy(TN.GAMM, parametric = FALSE) %>% 
  mutate(Model = "TN", .before = "term",
         Deviance = summary(TN.GAMM)$dev.expl)
tab.TN <- merge(tab.TN.par, tab.TN.smooth, all = TRUE)

tab.TP.par <- tidy(TP.GAMM, parametric = TRUE) %>% 
  mutate(Model = "TP", .before = "term",
         Deviance = summary(TP.GAMM)$dev.expl)
tab.TP.smooth <- tidy(TP.GAMM, parametric = FALSE) %>% 
  mutate(Model = "TP", .before = "term",
         Deviance = summary(TP.GAMM)$dev.expl)
tab.TP <- merge(tab.TP.par, tab.TP.smooth, all = TRUE)

tab.TOC.par <- tidy(TOC.GAMM, parametric = TRUE) %>% 
  mutate(Model = "TOC", .before = "term",
         Deviance = summary(TOC.GAMM)$dev.expl)
tab.TOC.smooth <- tidy(TOC.GAMM, parametric = FALSE) %>% 
  mutate(Model = "TOC", .before = "term",
         Deviance = summary(TOC.GAMM)$dev.expl)
tab.TOC <- merge(tab.TOC.par, tab.TOC.smooth, all = TRUE)

tab.SILT.par <- tidy(SILT.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Silt", .before = "term",
         Deviance = summary(SILT.GAMM)$dev.expl)
tab.SILT.smooth <- tidy(SILT.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Silt", .before = "term",
         Deviance = summary(SILT.GAMM)$dev.expl)
tab.SILT <- merge(tab.SILT.par, tab.SILT.smooth, all = TRUE)

tab.SAND.par <- tidy(SAND.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Sand", .before = "term",
         Deviance = summary(SAND.GAMM)$dev.expl)
tab.SAND.smooth <- tidy(SAND.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Sand", .before = "term",
         Deviance = summary(SAND.GAMM)$dev.expl)
tab.SAND <- merge(tab.SAND.par, tab.SAND.smooth, all = TRUE)

tab.ROCK.par <- tidy(ROCK.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Rock", .before = "term",
         Deviance = summary(ROCK.GAMM)$dev.expl)
tab.ROCK.smooth <- tidy(ROCK.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Rock", .before = "term",
         Deviance = summary(ROCK.GAMM)$dev.expl)
tab.ROCK <- merge(tab.ROCK.par, tab.ROCK.smooth, all = TRUE)

tab.BOULD.par <- tidy(BOULD.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Boulder", .before = "term",
         Deviance = summary(BOULD.GAMM)$dev.expl)
tab.BOULD.smooth <- tidy(BOULD.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Boulder", .before = "term",
         Deviance = summary(BOULD.GAMM)$dev.expl)
tab.BOULD <- merge(tab.BOULD.par, tab.BOULD.smooth, all = TRUE)

tab.MACRO.par <- tidy(MACRO.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Macrophyte cover", .before = "term",
         Deviance = summary(MACRO.GAMM)$dev.expl)
tab.MACRO.smooth <- tidy(MACRO.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Macrophyte cover", .before = "term",
         Deviance = summary(MACRO.GAMM)$dev.expl)
tab.MACRO <- merge(tab.MACRO.par, tab.MACRO.smooth, all = TRUE)

tab.DEPTH.par <- tidy(DEPTH.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Transect depth", .before = "term",
         Deviance = summary(DEPTH.GAMM)$dev.expl)
tab.DEPTH.smooth <- tidy(DEPTH.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Transect depth", .before = "term",
         Deviance = summary(DEPTH.GAMM)$dev.expl)
tab.DEPTH <- merge(tab.DEPTH.par, tab.DEPTH.smooth, all = TRUE)

tab.TRUNK.par <- tidy(TRUNK.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Trunk", .before = "term",
         Deviance = summary(TRUNK.GAMM)$dev.expl)
tab.TRUNK.smooth <- tidy(TRUNK.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Trunk", .before = "term",
         Deviance = summary(TRUNK.GAMM)$dev.expl)
tab.TRUNK <- merge(tab.TRUNK.par, tab.TRUNK.smooth, all = TRUE)

tab.TEMP.par <- tidy(TEMP.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Temperature", .before = "term",
         Deviance = summary(TEMP.GAMM)$dev.expl)
tab.TEMP.smooth <- tidy(TEMP.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Temperature", .before = "term",
         Deviance = summary(TEMP.GAMM)$dev.expl)
tab.TEMP <- merge(tab.TEMP.par, tab.TEMP.smooth, all = TRUE)

tab.TURB.par <- tidy(TURB.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Turbidity", .before = "term",
         Deviance = summary(TURB.GAMM)$dev.expl)
tab.TURB.smooth <- tidy(TURB.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Turbidity", .before = "term",
         Deviance = summary(TURB.GAMM)$dev.expl)
tab.TURB <- merge(tab.TURB.par, tab.TURB.smooth, all = TRUE)

tab.PH.par <- tidy(PH.GAMM, parametric = TRUE) %>% 
  mutate(Model = "pH", .before = "term",
         Deviance = summary(PH.GAMM)$dev.expl)
tab.PH.smooth <- tidy(PH.GAMM, parametric = FALSE) %>% 
  mutate(Model = "pH", .before = "term",
         Deviance = summary(PH.GAMM)$dev.expl)
tab.PH <- merge(tab.PH.par, tab.PH.smooth, all = TRUE)

tab.DO.par <- tidy(DO.GAMM, parametric = TRUE) %>% 
  mutate(Model = "DO", .before = "term",
         Deviance = summary(DO.GAMM)$dev.expl)
tab.DO.smooth <- tidy(DO.GAMM, parametric = FALSE) %>% 
  mutate(Model = "DO", .before = "term",
         Deviance = summary(DO.GAMM)$dev.expl)
tab.DO <- merge(tab.DO.par, tab.DO.smooth, all = TRUE)

tab.COND.par <- tidy(COND.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Conductivity", .before = "term",
         Deviance = summary(COND.GAMM)$dev.expl)
tab.COND.smooth <- tidy(COND.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Conductivity", .before = "term",
         Deviance = summary(COND.GAMM)$dev.expl)
tab.COND <- merge(tab.COND.par, tab.COND.smooth, all = TRUE)

tab.AREAPERI.par <- tidy(AREAPERI.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Area:Perimeter", .before = "term",
         Deviance = summary(AREAPERI.GAMM)$dev.expl)
tab.AREAPERI.smooth <- tidy(AREAPERI.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Area:Perimeter", .before = "term",
         Deviance = summary(AREAPERI.GAMM)$dev.expl)
tab.AREAPERI <- merge(tab.AREAPERI.par, tab.AREAPERI.smooth, all = TRUE)

tab.AREA.par <- tidy(AREA.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Area", .before = "term",
         Deviance = summary(AREA.GAMM)$dev.expl)
tab.AREA.smooth <- tidy(AREA.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Area", .before = "term",
         Deviance = summary(AREA.GAMM)$dev.expl)
tab.AREA <- merge(tab.AREA.par, tab.AREA.smooth, all = TRUE)

tab.PERI.par <- tidy(PERI.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Perimeter", .before = "term",
         Deviance = summary(PERI.GAMM)$dev.expl)
tab.PERI.smooth <- tidy(PERI.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Perimeter", .before = "term",
         Deviance = summary(PERI.GAMM)$dev.expl)
tab.PERI <- merge(tab.PERI.par, tab.PERI.smooth, all = TRUE)

tab.MDEPTH.par <- tidy(MDEPTH.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Lake mean depth", .before = "term",
         Deviance = summary(MDEPTH.GAMM)$dev.expl)
tab.MDEPTH.smooth <- tidy(MDEPTH.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Lake mean depth", .before = "term",
         Deviance = summary(MDEPTH.GAMM)$dev.expl)
tab.MDEPTH <- merge(tab.MDEPTH.par, tab.MDEPTH.smooth, all = TRUE)

tab.XDEPTH.par <- tidy(XDEPTH.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Lake maximum depth", .before = "term",
         Deviance = summary(XDEPTH.GAMM)$dev.expl)
tab.XDEPTH.smooth <- tidy(XDEPTH.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Lake maximum depth", .before = "term",
         Deviance = summary(XDEPTH.GAMM)$dev.expl)
tab.XDEPTH <- merge(tab.XDEPTH.par, tab.XDEPTH.smooth, all = TRUE)

tab.NEAR.par <- tidy(NEAR.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Distance to nearest lake", .before = "term",
         Deviance = summary(NEAR.GAMM)$dev.expl)
tab.NEAR.smooth <- tidy(NEAR.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Distance to nearest lake", .before = "term",
         Deviance = summary(NEAR.GAMM)$dev.expl)
tab.NEAR <- merge(tab.NEAR.par, tab.NEAR.smooth, all = TRUE)

tab.WRT.par <- tidy(WRT.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Water residence time", .before = "term",
         Deviance = summary(WRT.GAMM)$dev.expl)
tab.WRT.smooth <- tidy(WRT.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Water residence time", .before = "term",
         Deviance = summary(WRT.GAMM)$dev.expl)
tab.WRT <- merge(tab.WRT.par, tab.WRT.smooth, all = TRUE)

tab.DRAIN.par <- tidy(DRAIN.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Drainage area", .before = "term",
         Deviance = summary(DRAIN.GAMM)$dev.expl)
tab.DRAIN.smooth <- tidy(DRAIN.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Drainage area", .before = "term",
         Deviance = summary(DRAIN.GAMM)$dev.expl)
tab.DRAIN <- merge(tab.DRAIN.par, tab.DRAIN.smooth, all = TRUE)

tab.ELEV.par <- tidy(ELEV.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Elevation", .before = "term",
         Deviance = summary(ELEV.GAMM)$dev.expl)
tab.ELEV.smooth <- tidy(ELEV.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Elevation", .before = "term",
         Deviance = summary(ELEV.GAMM)$dev.expl)
tab.ELEV <- merge(tab.ELEV.par, tab.ELEV.smooth, all = TRUE)

tab.SP.par <- tidy(SP.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Species richness", .before = "term",
         Deviance = summary(SP.GAMM)$dev.expl)
tab.SP.smooth <- tidy(SP.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Species richness", .before = "term",
         Deviance = summary(SP.GAMM)$dev.expl)
tab.SP <- merge(tab.SP.par, tab.SP.smooth, all = TRUE)

tab.DIVERS.par <- tidy(DIVERS.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Diversity index", .before = "term",
         Deviance = summary(DIVERS.GAMM)$dev.expl)
tab.DIVERS.smooth <- tidy(DIVERS.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Diversity index", .before = "term",
         Deviance = summary(DIVERS.GAMM)$dev.expl)
tab.DIVERS <- merge(tab.DIVERS.par, tab.DIVERS.smooth, all = TRUE)

tab.FISH.par <- tidy(FISH.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Fish abundance", .before = "term",
         Deviance = summary(FISH.GAMM)$dev.expl)
tab.FISH.smooth <- tidy(FISH.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Fish abundance", .before = "term",
         Deviance = summary(FISH.GAMM)$dev.expl)
tab.FISH <- merge(tab.FISH.par, tab.FISH.smooth, all = TRUE)

tab.NONHOST.par <- tidy(NONHOST.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Non-host abundance", .before = "term",
         Deviance = summary(NONHOST.GAMM)$dev.expl)
tab.NONHOST.smooth <- tidy(NONHOST.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Non-host abundance", .before = "term",
         Deviance = summary(NONHOST.GAMM)$dev.expl)
tab.NONHOST <- merge(tab.NONHOST.par, tab.NONHOST.smooth, all = TRUE)

tab.EVEN.par <- tidy(EVEN.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Evenness", .before = "term",
         Deviance = summary(EVEN.GAMM)$dev.expl)
tab.EVEN.smooth <- tidy(EVEN.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Evenness", .before = "term",
         Deviance = summary(EVEN.GAMM)$dev.expl)
tab.EVEN <- merge(tab.EVEN.par, tab.EVEN.smooth, all = TRUE)

#Creating summary table
Tab.summary.GAMMs <- rbind(tab.NULL, tab.TNTP, tab.TN, tab.TP, tab.TOC,
                           tab.SILT, tab.SAND, tab.ROCK, tab.BOULD, tab.MACRO, tab.DEPTH, tab.TRUNK, 
                           tab.TEMP, tab.TURB, tab.PH, tab.DO, tab.COND, 
                           tab.AREAPERI, tab.AREA, tab.PERI, tab.MDEPTH, tab.XDEPTH, 
                           tab.NEAR, tab.WRT, tab.DRAIN, tab.ELEV, 
                           tab.FISH, tab.NONHOST, tab.SP, tab.DIVERS, tab.EVEN) %>%
  group_by(Model) %>% 
  gt() %>% 
  tab_row_group(
    label = md("<p>Null<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(1,2)) %>% 
  tab_row_group(
    label = md("<p>TN:TP<br>(D<sup>2</sup> = 87.07%)</p>"),
    rows = c(3:5)) %>% 
  tab_row_group(
    label = md("<p>TN<br>(D<sup>2</sup> = 72.62%)</p>"),
    rows = c(6:8)) %>% 
  tab_row_group(
    label = md("<p>TP<br>(D<sup>2</sup> = 73.50%)</p>"),
    rows = c(9:11)) %>% 
  tab_row_group(
    label = md("<p>TOC<br>(D<sup>2</sup> = 69.63%)</p>"),
    rows = c(12:14)) %>% 
  tab_row_group(
   label = md("<p>Silt<br>(D<sup>2</sup> = 69.64%)</p>"),
   rows = c(15:17)) %>% 
  tab_row_group(
   label = md("<p>Sand<br>(D<sup>2</sup> = 74.18%)</p>"),
   rows = c(18:20)) %>% 
  tab_row_group(
    label = md("<p>Rock<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(21:23)) %>% 
  tab_row_group(
    label = md("<p>Boulder<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(24:26)) %>% 
  tab_row_group(
    label = md("<p>Macrophyte cover<br>(D<sup>2</sup> = 84.167%)</p>"),
    rows = c(27:29)) %>% 
  tab_row_group(
    label = md("<p>Transect depth<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(30:32)) %>% 
  tab_row_group(
    label = md("<p>Trunk<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(33:35)) %>% 
  tab_row_group(
    label = md("<p>Temperature<br>(D<sup>2</sup> = 79.54%)</p>"),
    rows = c(36:38)) %>% 
  tab_row_group(
    label = md("<p>Turbidity<br>(D<sup>2</sup> = 88.71%)</p>"),
    rows = c(39:41)) %>% 
  tab_row_group(
    label = md("<p>pH<br>(D<sup>2</sup> = 70.52%)</p>"),
    rows = c(42:44)) %>% 
  tab_row_group(
    label = md("<p>DO<br>(D<sup>2</sup> = 75.28%)</p>"),
    rows = c(45:47)) %>% 
  tab_row_group(
    label = md("<p>Conductivity<br>(D<sup>2</sup> = 56.41%)</p>"),
    rows = c(48:50)) %>% 
  tab_row_group(
    label = md("<p>Area:Perimeter<br>(D<sup>2</sup> = 67.77%)</p>"),
    rows = c(51:53)) %>% 
  tab_row_group(
    label = md("<p>Surface area<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(54:56)) %>% 
  tab_row_group(
    label = md("<p>Perimeter<br>(D<sup>2</sup> = 69.90%)</p>"),
    rows = c(57:59)) %>% 
  tab_row_group(
    label = md("<p>Lake mean depth<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(60:62)) %>% 
  tab_row_group(
    label = md("<p>Lake maximum depth<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(63:65)) %>% 
  tab_row_group(
    label = md("<p>Distance to nearest lake<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(66:68)) %>% 
  tab_row_group(
    label = md("<p>Water residence time<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(69:71)) %>% 
  tab_row_group(
    label = md("<p>Drainage area<br>(D<sup>2</sup> = 69.73%)</p>"),
    rows = c(72:74)) %>% 
  tab_row_group(
    label = md("<p>Elevation<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(75:77)) %>% 
  tab_row_group(
    label = md("<p>Fish abundance<br>(D<sup>2</sup> = 80.8%)</p>"),
    rows = c(78:80)) %>% 
  tab_row_group(
    label = md("<p>Non-host abundance<br>(D<sup>2</sup> = 87.5%)</p>"),
    rows = c(81:83)) %>% 
  tab_row_group(
    label = md("<p>Species richness<br>(D<sup>2</sup> = 73.61%)</p>"),
    rows = c(84:86)) %>% 
  tab_row_group(
    label = md("<p>Diversity index<br>(D<sup>2</sup> = 79.69%)</p>"),
    rows = c(87:89)) %>% 
  tab_row_group(
    label = md("<p>Evenness<br>(D<sup>2</sup> = 69.93%)</p>"),
    rows = c(90:92)) %>% 
  cols_hide(c("ref.df", "Deviance")) %>% 
  cols_label(term = md("**Term**"), statistic = md("**Statistic**"), p.value = md("**p-value**"), estimate = md("**Estimate**"), std.error = md("**Standard error**"), edf = md("**edf**")) %>% 
  tab_header(md("**TABLE S16.** Estimated parameteric coefficients and approximate significance of smooth terms of the fine-scale prevalence community GAMMs. The deviance explained (D<sup>2</sup>) is given for every model as a measure of the model fit.")) %>% 
  tab_spanner(label = "Parametric coefficient", columns = c("estimate", "std.error", "statistic", "p.value")) %>% 
  tab_spanner(label = "Smooth terms", columns = c("statistic", "p.value", "edf")) %>% 
  tab_footnote(footnote = "Effective degrees of freedom", 
               locations = cells_column_labels(columns = "edf")) %>% 
  tab_footnote(footnote = "F-value", 
               locations = cells_body(columns = "statistic", rows = c(2,4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50,52,53,55,56,58,59,61,62,64,65,67,68,70,71,73,74,76,77,79,80,82,83,85,86,88,89,91,92))) %>% 
  tab_footnote(footnote = "t-value", 
             locations = cells_body(columns = "statistic", rows = c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90))) %>% 
  fmt_number(decimals = 3) %>% 
  sub_values(columns = "term", values = "(Intercept)", replacement = "Intercept") %>% 
  tab_options(row_group.as_column = TRUE,
              row.striping.include_table_body = TRUE,
              table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              table.border.bottom.style = "hidden") %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_row_groups()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_spanners()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(side = "top", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "<p>Evenness<br>(D<sup>2</sup> = 69.93%)</p>")) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "black"),
            locations =  cells_body(rows = 2)) %>% 
  tab_style(style = cell_borders(side = "bottom", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "<p>Null<br>(D<sup>2</sup> = 69.64%)</p>")) 
  
Tab.summary.GAMMs %>% #Saving gt tab
  gtsave("Tab_GAMMs_summary.png", paste0(to.figs))
Tab.summary.GAMMs %>% 
  gtsave("Table_S16.png", paste0(to.rédaction, "./Support_information/"))  
