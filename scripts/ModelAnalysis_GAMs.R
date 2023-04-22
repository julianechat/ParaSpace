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

source(paste0(to.R, "rquery.cormat.R"))

## Loading data ----

ParaSpaceMod <- read.csv(paste0(to.output, "Transects_Lake_Data.csv"))

# ---- Data analysis ----
mod.data$Lake <- as.factor(mod.data$Lake)
mod.data2 <- na.omit(mod.data)
mod.data2$Lake <- as.factor(mod.data2$Lake)

## ---- One predictor GAMs ----
TNTP.GAMM3 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "ML")
summary(TNTP.GAMM3)
appraise(TNTP.GAMM3, method = "simulate")

draw.TNTP <- draw(TNTP.GAMM3, unconditional = TRUE, overall_uncertainty = TRUE)
draw.TNTP
#Visualizing with plot()
plot.TNTP <- plot(TNTP.GAMM3, trans = plogis, residuals = TRUE, 
                  shift = coef(TNTP.GAMM)[1], seWithMean = TRUE, 
                  pch = 1, shade = TRUE, shade.col = "azure3", rug = FALSE, 
                  ylab = "Prevalence", xlab = "TN:TP", 
                  select = 1)
plot.TNTP


#lake
TNTP.GAMM.L <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.L, bs = "cr") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data2, method = "ML")
summary(TNTP.GAMM.L) #unsignificative

#Null
NULL.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "ML")
summary(NULL.GAMM)
appraise(NULL.GAMM)

#Nitrogen
TN.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN.T, bs = "cr") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data2, method = "ML")
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
                 family = quasibinomial, data = mod.data2, method = "ML")
summary(TN.GAMM.L) #unsignificative

#Phosphorus
TP.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TP.T, bs = "cr") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data2, method = "ML")
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
                 family = quasibinomial, data = mod.data2, method = "ML")
summary(TP.GAMM.L) #unsignificative

#Carbon
TOC.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TOC.T, bs = "cs") + s(Lake, bs = "re"),
                family = quasibinomial, data = mod.data2, method = "ML")
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
                  family = quasibinomial, data = mod.data2, method = "ML")
summary(TOC.GAMM.L) #unsignificative

#Sub 1
SUB1.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub1, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "ML")
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
                 family = quasibinomial, data = mod.data2, method = "ML")
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
                  family = quasibinomial, data = mod.data2, method = "ML")
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
                  family = quasibinomial, data = mod.data2, method = "ML")
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
                  family = quasibinomial, data = mod.data2, method = "ML")
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
                 family = quasibinomial, data = mod.data2, method = "ML")
summary(TEMP.GAMM) #significative
appraise(TEMP.GAMM)
gam.check(TEMP.GAMM)
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
                   family = quasibinomial, data = mod.data2, method = "ML")
summary(TEMP.GAMM.L) #unsignificative

#Turbidity
TURB.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "ML")
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
                   family = quasibinomial, data = mod.data2, method = "ML")
summary(TURB.GAMM.L) #unsignificative

#pH
PH.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(pH.T, bs = "cs") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data2, method = "ML")
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
                 family = quasibinomial, data = mod.data2, method = "ML")
summary(PH.GAMM.L) #significative
draw(PH.GAMM.L)

#Oxygen
DO.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(DO.T, bs = "cs") + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data2, method = "ML")
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
                 family = quasibinomial, data = mod.data2, method = "ML")
summary(DO.GAMM.L) #unsignificative

#Conductivity
COND.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Cond.T, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "ML")
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
                   family = quasibinomial, data = mod.data2, method = "ML")
summary(COND.GAMM.L) #significative

#Area:Perimeter
AREAPERI.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Area_Perimeter, bs = "cs") + s(Lake, bs = "re"),
                     family = quasibinomial, data = mod.data2, method = "ML")
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
                 family = quasibinomial, data = mod.data2, method = "ML")
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
                 family = quasibinomial, data = mod.data2, method = "ML")
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
                   family = quasibinomial, data = mod.data2, method = "ML")
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
                family = quasibinomial, data = mod.data2, method = "ML")
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
                  family = quasibinomial, data = mod.data2, method = "ML")
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

#without achigan
mod.data3 <- mod.data2 %>% filter(!(Lake == "Achigan"))
DRAIN.GAMM2 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Drainage_area, bs = "cs") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data3, method = "ML")
summary(DRAIN.GAMM2) #unsignificative
appraise(DRAIN.GAMM2)
draw(DRAIN.GAMM2)

#Elevation
ELEV.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Elevation, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "ML")
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
                 family = quasibinomial, data = mod.data2, method = "ML")
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
                   family = quasibinomial, data = mod.data2, method = "ML")
summary(CENT.GAMM.L) #unsignificative

#Species richness
SP.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Species_richness.T, bs = "cs", k = 5) + s(Lake, bs = "re"),
               family = quasibinomial, data = mod.data2, method = "ML")
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
                 family = quasibinomial, data = mod.data2, method = "ML")
summary(SP.GAMM.L) #significative
draw(SP.GAMM.L, residuals = TRUE)

#Diversity
DIVERS.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Diversity.T, bs = "cs") + s(Lake, bs = "re"),
                   family = quasibinomial, data = mod.data2, method = "ML")
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
                     family = quasibinomial, data = mod.data2, method = "ML")
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