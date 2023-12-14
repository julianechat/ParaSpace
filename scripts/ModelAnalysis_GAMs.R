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

library(dplyr)
library(vegan)
library(ggplot2)
library(cowplot)
library(patchwork)
library(gratia)
library(mgcv)
library(gamlss)
library(itsadug)
library(colorspace)
library(broom)
library(gt)

source(paste0(to.R, "rquery.cormat.R"))
source(paste0(to.R, "inverse_logit_trans.R"))

## Loading data ----

mod.data <- read.csv(paste0(to.output, "ModelAnalysis_Df.csv"))

# ---- Data analysis ----

mod.data$Lake <- as.factor(mod.data$Lake)
mod.data$Watershed <- as.factor(mod.data$Watershed)
mod.data$Transect_ID <- as.factor(mod.data$Transect_ID)

sequential_hcl(palette = "YlOrBr", 5)
col.pal <- c("#682714", "#C75C00", "#F2A400", "#FAE094", "#FEFEE3")

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
sequential_hcl(5, palette ="YlOrBr")

draw.TNTP <- draw(TNTP.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "TN:TP", y = "Prevalence", tag = "A") +
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

draw.TNTP

TNTP.sm <- smooth_estimates(TNTP.GAMM) %>%
  add_confint()
TNTP.pr <- mod.data %>%
  add_partial_residuals(TNTP.GAMM)
TNTP.pe <- TNTP.sm %>%
  filter(smooth == "s(TN_TP.T)") %>%
  ggplot( unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = TN_TP.T),
           data = TNTP.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = TN_TP.T), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = TN_TP.T, y = est), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "TN:TP", y = "Partial effect (prevalence)", tag = "A") +
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
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(y = "Prevalence", tag = "B") + 
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
draw.MACRO
#Signiticativity OK

MACRO.sm <- smooth_estimates(MACRO.GAMM) %>%
  add_confint()
MACRO.pr <- mod.data %>%
  add_partial_residuals(MACRO.GAMM)
MACRO.pe <- MACRO.sm %>%
  filter(smooth == "s(Macrophyte)") %>%
  ggplot( unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Macrophyte),
           data = MACRO.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = Macrophyte), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Macrophyte, y = est), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Macrophyte coverage (%)", y = "Partial effect (prevalence)", tag = "B") +
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
DEPTH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(MeanDepth.site, bs = "cs") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data, method = "ML")
summary(DEPTH.GAMM) #Depth is not significant
#Adj. R- sq. = 0.62
#Deviance explained = 69.6%

DEPTH.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(MeanDepth.site) + random(Lake), 
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
  scale_y_continuous(trans = inverse_logit_trans) + 
  labs(x = "Temperature", y = "Prevalence", tag = "C") + 
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
draw.TEMP

TEMP.sm <- smooth_estimates(TEMP.GAMM) %>%
  add_confint()
TEMP.pr <- mod.data %>%
  add_partial_residuals(TEMP.GAMM)
TEMP.pe <- TEMP.sm %>%
  filter(smooth == "s(Temp.T)") %>%
  ggplot( unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Temp.T),
           data = TEMP.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = Temp.T), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Temp.T, y = est), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Temperature (°C)", y = "Partial effect (prevalence)", tag = "C") +
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
  scale_y_continuous(trans = inverse_logit_trans) + 
  labs(x = "Turbidity", y = "Prevalence", tag = "D") +
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
draw.TURB

TURB.sm <- smooth_estimates(TURB.GAMM) %>%
  add_confint()
TURB.pr <- mod.data %>%
  add_partial_residuals(TURB.GAMM)
TURB.pe <- TURB.sm %>%
  filter(smooth == "s(Turb.T)") %>%
  ggplot( unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Turb.T),
           data = TURB.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = Turb.T), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Turb.T, y = est), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Turbidity (NTU)", y = "Partial effect (prevalence)", tag = "D") +
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
  scale_y_continuous(trans = inverse_logit_trans) + 
  labs(x = "pH", y = "Prevalence", tag = "E") +
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
draw.PH

PH.sm <- smooth_estimates(PH.GAMM) %>%
  add_confint()
PH.pr <- mod.data %>%
  add_partial_residuals(PH.GAMM)
PH.pe <- PH.sm %>%
  filter(smooth == "s(pH.T)") %>%
  ggplot( unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = pH.T),
           data = PH.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = pH.T), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = pH.T, y = est), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "pH", y = "Partial effect(prevalence)", tag = "E") +
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
  scale_y_continuous(trans = inverse_logit_trans) + 
  labs(x = "DO", y = "Prevalence", tag = "F") +
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
draw.DO

DO.sm <- smooth_estimates(DO.GAMM) %>%
  add_confint()
DO.pr <- mod.data %>%
  add_partial_residuals(DO.GAMM)
DO.pe <- DO.sm %>%
  filter(smooth == "s(DO.T)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = DO.T),
           data = DO.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = DO.T), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = DO.T, y = est), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Dissolved oxygen (mg/L)", y = "Partial effect (prevalence)", tag = "F") +
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
  scale_y_continuous(trans = inverse_logit_trans) + 
  labs(x = "Conductivity", y = "Prevalence", tag = "G") +
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
draw.COND

COND.sm <- smooth_estimates(COND.GAMM) %>%
  add_confint()
COND.pr <- mod.data %>%
  add_partial_residuals(COND.GAMM)
COND.pe <- COND.sm %>%
  filter(smooth == "s(Cond.T)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Cond.T),
           data = COND.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = Cond.T), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Cond.T, y = est), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Conductivity (μS/cm)", y = "Partial effect (prevalence)", tag = "G") +
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
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Area:Perimeter", y = "Prevalence", tag = "I") +
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
draw.AREAPERI

AREAPERI.sm <- smooth_estimates(AREAPERI.GAMM) %>%
  add_confint()
AREAPERI.pr <- mod.data %>%
  add_partial_residuals(AREAPERI.GAMM)
AREAPERI.pe <- AREAPERI.sm %>%
  filter(smooth == "s(Area_Perimeter)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Area_Perimeter),
           data = AREAPERI.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = Area_Perimeter), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Area_Perimeter, y = est), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Area:Perimeter (m)", y = "Partial effect (prevalence)", tag = "I") +
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
  scale_y_continuous(trans = inverse_logit_trans) + 
  labs(x = "Perimeter", y = "Prevalence", tag = "I") +
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
draw.PERI

PERI.sm <- smooth_estimates(PERI.GAMM) %>%
  add_confint()
PERI.pr <- mod.data %>%
  add_partial_residuals(PERI.GAMM)
PERI.pe <- PERI.sm %>%
  filter(smooth == "s(Perimeter)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Perimeter),
           data = PERI.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = Perimeter), fill ="#682714", alpha = 0.5) +
  geom_line(aes(x = Perimeter, y = est), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Perimeter (m)", y = "Partial effect (prevalence)") +
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
MDEPTH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(MeanDepth.lake, bs = "cs") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data, method = "ML")
summary(MDEPTH.GAMM) #Mean depth is not significant
#Adj. R-sq. = 0.62
#Deviance explained = 69.6%

MDEPTH.GAMM.BB <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(MeanDepth.lake) + random(Lake), 
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

### Fish abundance ----
FISH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(tot_fish, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(FISH.GAMM) #Centrarchids is not significant

#### Model validation
appraise(FISH.GAMM)
gam.check(FISH.GAMM)
FISH.GAMM$scale

#### Visualizing partial effects
draw.FISH <- draw(FISH.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.FISH

FISH.sm <- smooth_estimates(FISH.GAMM) %>%
  add_confint()
FISH.pr <- mod.data %>%
  add_partial_residuals(FISH.GAMM)
FISH.pe <- FISH.sm %>%
  filter(smooth == "s(tot_fish)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = tot_fish),
           data = FISH.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = tot_fish), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = tot_fish, y = est), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "fish abundance", y = "Partial effect (prevalence)", tag = "K") +
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

### Non-host abundance ----
NONHOST.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(tot_Cyprinidae, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data, method = "ML")
summary(NONHOST.GAMM) #Centrarchids is not significant

#### Model validation
appraise(NONHOST.GAMM)
gam.check(NONHOST.GAMM)
NONHOST.GAMM$scale

#### Visualizing partial effects
draw.NONHOST<- draw(NONHOST.GAMM, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.NONHOST

NONHOST.sm <- smooth_estimates(NONHOST.GAMM) %>%
  add_confint()
NONHOST.pr <- mod.data %>%
  add_partial_residuals(NONHOST.GAMM)
NONHOST.pe <- NONHOST.sm %>%
  filter(smooth == "s(tot_Cyprinidae)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = tot_Cyprinidae),
           data = NONHOST.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = tot_Cyprinidae), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = tot_Cyprinidae, y = est), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "non-host abundance", y = "Partial effect (prevalence)", tag = "L") +
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
  scale_y_continuous(trans = inverse_logit_trans) + 
  labs(x = "Simpson iversity", y = "Prevalence", tag = "H") +
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
draw.DIVERS

DIVERS.sm <- smooth_estimates(DIVERS.GAMM) %>%
  add_confint()
DIVERS.pr <- mod.data %>%
  add_partial_residuals(DIVERS.GAMM)
DIVERS.pe <- DIVERS.sm %>%
  filter(smooth == "s(Diversity.T)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Diversity.T),
           data = DIVERS.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = Diversity.T), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Diversity.T, y = est), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Simpson's Diversity Index", y = "Partial effect (prevalence)", tag = "H") +
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

## Summary figure ----

Summary.plot <- TNTP.pe + MACRO.pe + TEMP.pe + TURB.pe + PH.pe + DO.pe + COND.pe + DIVERS.pe + FISH.pe + NONHOST.pe + PERI.pe + AREAPERI.pe &
  theme(text = element_text(family = "Calibri Light", size = 40, color = "black"))


ggsave(paste0(to.figs, "GAMMs_summary.png"), plot = Summary.plot, dpi = 300, width = 45, height = 30)
ggsave(paste0(to.rédaction, "Figures/Figure5_GAMMs.png"), plot = Summary.plot, dpi = 300, width = 35, height = 30)

## Perimeter figure ----

PERI.pe

ggsave(paste0(to.figs, "GAMMs_Perimeter.png"), plot = PERI.pe, dpi = 300, width = 10, height = 10)  
ggsave(paste0(to.rédaction, "./Support_information/Figure_S1.png"), plot = PERI.pe, dpi = 300, width = 10, height = 10)  

## Summary table ----

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

tab.SUB1.par <- tidy(SUB1.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Substrate 1", .before = "term",
         Deviance = summary(SUB1.GAMM)$dev.expl)
tab.SUB1.smooth <- tidy(SUB1.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Substrate 1", .before = "term",
         Deviance = summary(SUB1.GAMM)$dev.expl)
tab.SUB1 <- merge(tab.SUB1.par, tab.SUB1.smooth, all = TRUE)

tab.SUB2.par <- tidy(SUB2.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Substrate 2", .before = "term",
         Deviance = summary(SUB2.GAMM)$dev.expl)
tab.SUB2.smooth <- tidy(SUB2.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Substrate 2", .before = "term",
         Deviance = summary(SUB2.GAMM)$dev.expl)
tab.SUB2 <- merge(tab.SUB2.par, tab.SUB2.smooth, all = TRUE)

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

tab.CENT.par <- tidy(CENT.GAMM, parametric = TRUE) %>% 
  mutate(Model = "Centrarchids", .before = "term",
         Deviance = summary(CENT.GAMM)$dev.expl)
tab.CENT.smooth <- tidy(CENT.GAMM, parametric = FALSE) %>% 
  mutate(Model = "Centrarchids", .before = "term",
         Deviance = summary(CENT.GAMM)$dev.expl)
tab.CENT <- merge(tab.CENT.par, tab.CENT.smooth, all = TRUE)

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

Tab.summary.GAMMs <- rbind(tab.NULL, tab.TNTP, tab.TN, tab.TP, tab.TOC, tab.SUB1, tab.SUB2, tab.MACRO, tab.DEPTH, tab.TRUNK, tab.TEMP, tab.TURB, tab.PH, tab.DO, tab.COND, tab.AREAPERI, tab.AREA, tab.PERI, tab.MDEPTH, tab.XDEPTH, tab.WRT, tab.DRAIN, tab.ELEV, tab.CENT, tab.SP, tab.DIVERS) %>%
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
    label = md("<p>Subrate 1<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(15:17)) %>% 
  tab_row_group(
    label = md("<p>Substrate 2<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(18:20)) %>% 
  tab_row_group(
    label = md("<p>Macrophyte cover<br>(D<sup>2</sup> = 84.167%)</p>"),
    rows = c(21:23)) %>% 
  tab_row_group(
    label = md("<p>Transect depth<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(24:26)) %>% 
  tab_row_group(
    label = md("<p>Trunk<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(27:29)) %>% 
  tab_row_group(
    label = md("<p>Temperature<br>(D<sup>2</sup> = 79.54%)</p>"),
    rows = c(30:32)) %>% 
  tab_row_group(
    label = md("<p>Turbidity<br>(D<sup>2</sup> = 88.71%)</p>"),
    rows = c(33:35)) %>% 
  tab_row_group(
    label = md("<p>pH<br>(D<sup>2</sup> = 70.52%)</p>"),
    rows = c(36:38)) %>% 
  tab_row_group(
    label = md("<p>DO<br>(D<sup>2</sup> = 75.28%)</p>"),
    rows = c(39:41)) %>% 
  tab_row_group(
    label = md("<p>Conductivity<br>(D<sup>2</sup> = 56.41%)</p>"),
    rows = c(42:44)) %>% 
  tab_row_group(
    label = md("<p>Area:Perimeter<br>(D<sup>2</sup> = 67.77%)</p>"),
    rows = c(45:47)) %>% 
  tab_row_group(
    label = md("<p>Area<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(48:50)) %>% 
  tab_row_group(
    label = md("<p>Perimeter<br>(D<sup>2</sup> = 69.90%)</p>"),
    rows = c(51:53)) %>% 
  tab_row_group(
    label = md("<p>Lake mean depth<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(54:56)) %>% 
  tab_row_group(
    label = md("<p>Lake maximum depth<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(57:59)) %>% 
  tab_row_group(
    label = md("<p>Water residence time<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(60:62)) %>% 
  tab_row_group(
    label = md("<p>Drainage area<br>(D<sup>2</sup> = 69.73%)</p>"),
    rows = c(63:65)) %>% 
  tab_row_group(
    label = md("<p>Elevation<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(66:68)) %>% 
  tab_row_group(
    label = md("<p>Centrarchids<br>(D<sup>2</sup> = 69.64%)</p>"),
    rows = c(69:71)) %>% 
  tab_row_group(
    label = md("<p>Species richness<br>(D<sup>2</sup> = 73.61%)</p>"),
    rows = c(72:74)) %>% 
  tab_row_group(
    label = md("<p>Diversity index<br>(D<sup>2</sup> = 79.69%)</p>"),
    rows = c(75:77)) %>% 
  cols_hide(c("ref.df", "Deviance")) %>% 
  cols_label(term = md("**Term**"), statistic = md("**Statistic**"), p.value = md("**p-value**"), estimate = md("**Estimate**"), std.error = md("**Standard error**"), edf = md("**edf**")) %>% 
  tab_header(md("**TABLE S17.** Estimated parameteric coefficients and approximate significance of smooth terms of the fine-scale prevalence community GAMMs.The deviance explained (D<sup>2</sup>) is given for every model.")) %>% 
  tab_spanner(label = "Parametric coefficient", columns = c("estimate", "std.error", "statistic", "p.value")) %>% 
  tab_spanner(label = "Smooth terms", columns = c("statistic", "p.value", "edf")) %>% 
  tab_footnote(footnote = "Effective degrees of freedom", 
               locations = cells_column_labels(columns = "edf")) %>% 
  tab_footnote(footnote = "F-value", 
               locations = cells_body(columns = "statistic", rows = c(2,4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50,52,53,55,56,58,59,61,62,64,65,67,68,70,71,73,74,76,77))) %>% 
  tab_footnote(footnote = "t-value", 
             locations = cells_body(columns = "statistic", rows = c(1,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69,72,75))) %>% 
  fmt_number(decimals = 3) %>% 
  sub_values(columns = "term", values = "(Intercept)", replacement = "Intercept") %>% 
  sub_values(columns = "term", values = "s(Diversity.T)", replacement = "s(Diversity)") %>% 
  sub_values(columns = "term", values = "s(Species_richness.T)", replacement = "s(Species_richness)") %>% 
  sub_values(columns = "term", values = "s(Centrarchids.T)", replacement = "s(Centrarchids)") %>% 
  sub_values(columns = "term", values = "s(Cond.T)", replacement = "s(Cond)") %>% 
  sub_values(columns = "term", values = "s(DO.T)", replacement = "s(DO)") %>% 
  sub_values(columns = "term", values = "s(pH.T)", replacement = "s(pH)") %>% 
  sub_values(columns = "term", values = "s(Turb.T)", replacement = "s(Turb)") %>% 
  sub_values(columns = "term", values = "s(Temp.T)", replacement = "s(Temp)") %>% 
  sub_values(columns = "term", values = "s(TOC.T)", replacement = "s(TOC)") %>% 
  sub_values(columns = "term", values = "s(TN.T)", replacement = "s(TN)") %>% 
  sub_values(columns = "term", values = "s(TP.T)", replacement = "s(TP)") %>% 
  sub_values(columns = "term", values = "s(TN_TP.T)", replacement = "s(TN_TP)") %>% 
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
            locations = cells_row_groups(groups = "<p>Diversity index<br>(D<sup>2</sup> = 79.69%)</p>")) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "black"),
            locations =  cells_body(rows = 2)) %>% 
  tab_style(style = cell_borders(side = "bottom", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "<p>Null<br>(D<sup>2</sup> = 69.64%)</p>")) 
  
Tab.summary.GAMMs %>% #Saving gt tab
  gtsave("Tab_GAMMs_summary.png", paste0(to.figs))
Tab.summary.GAMMs %>% 
  gtsave("Table_S17.png", paste0(to.rédaction, "./Support_information/"))  
  
