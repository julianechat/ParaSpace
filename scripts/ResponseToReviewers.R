## Script name : Response to reviewers

## Authors : Juliane Vigneault
## Date created : Dcember 12, 2024

## Copyright (c) Juliane Vigneault, 2024
## Email: Juliane.Vigneault@uqar.ca

# ---- Script setup ----

## R Setup ----

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"
to.carto <- "./carto/"
to.rédaction <- "./rédaction/"

## Loading packages ----

library(dplyr)
library(ggplot2)
library(splitstackshape)
library(patchwork)
library(mgcv)
library(gratia)

source(paste0(to.R, "inverse_logit_trans.R"))

## Loading data ----

FishingData <- read.csv(paste0(to.data, "Fishing_RawData.csv"), sep = ";")
CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))
mod.data <- read.csv(paste0(to.output, "ModelAnalysis_DataFrame.csv"))

# ----  Frequency distribution ----

ParasiteData <- FishingData %>% 
  select(Lake, Intensity_class, Abundance) %>% 
  na.omit()

# We want to estimate the mean parasite intensity of lake-scale fish communities
# Meaning - what is the mean parasite intensity in each lake ?

ParasiteData <- expandRows(ParasiteData, "Abundance")

ParasiteIntensity <- ParasiteData %>% 
  filter(!(Intensity_class == "0")) %>% 
  group_by(Lake) %>% 
  summarise(Intensity = mean(Intensity_class))

Intensity.hist <- ggplot(ParasiteIntensity, aes(Intensity)) + 
  geom_histogram(bins = 6, fill = "#7E7E7E", color = "black", alpha = 0.8) +
  labs(x = "Intensity", y = "Frequency") + 
  theme(text = element_text(size = 32, family = "Calibri Light", color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))

Intensity.hist

ggsave(paste0(to.figs, "FrequencyIntensity.png"), plot = Intensity.hist, dpi = 500, width = 5, height = 5) #Saving plot

# ---- Excluding uninfected species ----

CombinedData.ex <- CombinedData %>% #Muskellunge and brown bullhead individuals are excluded from the prevalence calculus since they are not host of the black spot disease
  select(!(c(tot_EsMa, inf_EsMa, tot_AmNe, inf_AmNe, tot_CaCo, inf_CaCo, tot_FuDi, inf_FuDi, tot_UmLi, inf_UmLi, tot_LuCo, inf_LuCo, tot_RhAt, inf_RhAt, tot_ChrosomusSpp., inf_ChrosomusSpp.)))

## Combined methods ----

Cdata <- CombinedData.ex %>% 
  select(Lake, starts_with(c("inf", "tot"))) %>% #Select total and infected community matrix
  na.omit()

Cdata.inf <- Cdata %>% 
  select(starts_with("inf")) #Select infected community matrix

Cdata.tot <- Cdata %>% 
  select(starts_with("tot")) #Select total community matrix

#Infection prevalence data
Infected <- rowSums(Cdata.inf)
Total <- rowSums(Cdata.tot)
Prevalence <- Infected/Total
Lake <- Cdata$Lake

Cdata <- data.frame(Lake, Infected, Total, Prevalence) %>% 
  mutate(Method = "Combined", .after = "Lake")

## Minnow trap ----

MTdata <- CombinedData.ex %>% 
  filter(Sampling_method == "Minnow_trap") %>% #Select minnow trap method
  select("Lake", starts_with(c("inf", "tot"))) %>% #Select total and infected community matrix
  na.omit()

MTdata.inf <- MTdata %>% 
  select(starts_with("inf"))  #Select infected community matrix

MTdata.tot <- MTdata %>% 
  select(starts_with("tot")) #Select total community matrix

#Infection prevalence data
Infected <- rowSums(MTdata.inf)
Total <- rowSums(MTdata.tot)
Prevalence <- Infected/Total
Lake <- MTdata$Lake

MTdata <- data.frame(Lake, Infected, Prevalence, Total) %>% 
  mutate(Method = "Minnow trap", .after = "Lake")

## Seine net ----

Sdata <- CombinedData.ex %>% 
  filter(Sampling_method == "Seine") %>% #Select seine method
  select("Lake", starts_with(c("inf", "tot"))) %>% #Select total and infected community matrix
  na.omit()

Sdata.inf <- Sdata %>% 
  select(starts_with("inf")) #Select infected community matrix

Sdata.tot <- Sdata %>% 
  select(starts_with("tot")) #Select total community matrix

#Infection prevalence data
Infected <- rowSums(Sdata.inf)
Total <- rowSums(Sdata.tot)
Prevalence <- Infected/Total
Lake <- Sdata$Lake

Sdata <- data.frame(Lake, Infected, Total, Prevalence) %>% 
  mutate(Method = "Seine net", .after = "Lake")

## Transect ----

Tdata <- CombinedData.ex %>% 
  filter(Sampling_method == "Transect") %>% #Select transect method
  select("Lake", starts_with(c("inf", "tot"))) %>% #Select total and infected community matrix
  na.omit()

Tdata.inf <- Tdata %>% 
  select(starts_with("inf")) #Select infected community matrix

Tdata.tot <- Tdata %>% 
  select(starts_with("tot")) #Select total community matrix

#Infection prevalence data
Infected <- rowSums(Tdata.inf)
Total <- rowSums(Tdata.tot)
Prevalence <- Infected/Total
Lake <- Tdata$Lake

Tdata <- data.frame(Lake, Infected, Total, Prevalence) %>% 
  mutate(Method = "Transect", .after = "Lake")

N <- 35 #Maximum number of samples (i)
Resampling <- 999 #Number of times each i is repeated 

C.prev <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    line <- sample(1:nrow(Cdata), i) #sample i lines randomly
    prop.samp <- Cdata[line, "Total"]/sum(Cdata[line, "Total"])
    w_prev <- Cdata[line, "Prevalence"]*prop.samp
    Prevalence <- sum(na.omit(w_prev))
    output <- data.frame(N = i, Resampling = j, Prevalence) #save output in temporary data.frame (changed at each iterations)
    C.prev <- rbind(C.prev, output)
  }
}

C.prev <- C.prev %>% 
  mutate(Method = "Combined", .before = "N")

boxplot.prev.C <- boxplot(Prevalence ~ N, data = C.prev)

MT.prev <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    line <- sample(1:nrow(MTdata), i) #sample i lines randomly
    prop.samp <- MTdata[line, "Total"]/sum(MTdata[line, "Total"])
    w_prev <- MTdata[line, "Prevalence"]*prop.samp
    Prevalence <- sum(na.omit(w_prev))
    output <- data.frame(N = i, Resampling = j, Prevalence) #save output in temporary data.frame (changed at each iterations)
    MT.prev <- rbind(MT.prev, output)
  }
}

MT.prev <- MT.prev %>% 
  mutate(Method = "Minnow trap", .before = "N")

boxplot.prev.MT <- boxplot(Prevalence ~ N, data = MT.prev)

S.prev <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    line <- sample(1:nrow(Sdata), i) #sample i lines randomly
    prop.samp <- Sdata[line, "Total"]/sum(Sdata[line, "Total"])
    w_prev <- Sdata[line, "Prevalence"]*prop.samp
    Prevalence <- sum(na.omit(w_prev))
    output <- data.frame(N = i, Resampling = j, Prevalence) #save output in temporary data.frame (changed at each iterations)
    S.prev <- rbind(S.prev, output)
  }
}

S.prev <- S.prev %>% 
  mutate(Method = "Seine net", .before = "N")

boxplot.prev.S <- boxplot(Prevalence ~ N, data = S.prev)

T.prev <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    line <- sample(1:nrow(Tdata), i) #sample i lines randomly
    prop.samp <- Tdata[line, "Total"]/sum(Tdata[line, "Total"])
    w_prev <- Tdata[line, "Prevalence"]*prop.samp
    Prevalence <- sum(na.omit(w_prev))
    output <- data.frame(N = i, Resampling = j, Prevalence) #save output in temporary data.frame (changed at each iterations)
    T.prev <- rbind(T.prev, output)
  }
}

T.prev <- T.prev %>% 
  mutate(Method = "Transect", .before = "N")

boxplot.prev.T <- boxplot(Prevalence ~ N, data = T.prev)

df.prev <- rbind(C.prev, MT.prev, S.prev, T.prev)
df.prev$N <- as.factor(df.prev$N)
df.prev$Method <- as.factor(df.prev$Method)

prev.acc.plot <- ggplot(df.prev) + 
  stat_summary(aes(x = N, y = Prevalence, group = Method, color = Method, shape = Method), fun = "mean", size = 1) +
  stat_smooth(aes(x= N, y = Prevalence, group = Method, color = Method, fill = Method), method = "loess", se = FALSE, level = 0.95, lineend = "round", alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Number of samples", y = "Mean infection prevalence") +
  scale_color_manual(values = c("#7E7E7E", "#2A5676", "#999600", "#966F1E"),
                     aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(0, 5, 2, 1)) +
  guides(fill = guide_legend(override.aes = list(fill = NA, linetype = 0))) +
  theme(text = element_text(size = 20, family = "Calibri Light", color = "black"),
        plot.background = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.key = element_rect(fill = NA, colour = NA),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line.x = element_line(color = "black", lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))

prev.acc.plot

#Histograms

C.prev <- CombinedData %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% #Select total and infected community matrix
  na.omit()

C.prev <- C.prev %>% 
  group_by(Lake) %>% #Summarize abundance data by lake
  summarise(across(.cols = everything(), sum))

C.prev <- C.prev %>%  #Sum total and infected fish abundance within lakes
  mutate(tot_fish = tot_AmRu  + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_PiNo + tot_SeAt, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_PiNo + inf_SeAt, .keep = "unused") 

C.prev <- C.prev %>% #Calculate community level prevalence for each lake
  mutate(prev_fish = (inf_fish/tot_fish)*100)

## Transect ----

T.prev <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% #Select transect method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Select total and infected community matrix
  na.omit()

T.prev <- T.prev %>% 
  group_by(Lake) %>% #Summarize abundance data by lake
  summarise(across(.cols = everything(), sum))

T.prev <- T.prev %>% #Sum total and infected fish abundance within lakes
  mutate(tot_fish = tot_AmRu  + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_PiNo + tot_SeAt, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_PiNo + inf_SeAt, .keep = "unused") 


T.prev <- T.prev %>% #Calculate community level prevalence for each lake
  mutate(prev_fish = (inf_fish/tot_fish)*100)

## Seine net ----

S.prev <- CombinedData %>% 
  filter(Sampling_method == "Seine") %>% #Select seine method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Select total and infected community matrix
  na.omit()

S.prev <- S.prev %>% 
  group_by(Lake) %>% #Summarize abundance data by lake
  summarise(across(.cols = everything(), sum))

S.prev <- S.prev %>% #Sum total and infected fish abundance within lakes
  mutate(tot_fish = tot_AmRu  + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_PiNo + tot_SeAt, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_PiNo + inf_SeAt, .keep = "unused") 


S.prev <- S.prev %>% #Calculate community level prevalence for each lake
  mutate(prev_fish = (inf_fish/tot_fish)*100)

## Minnow trap ----

MT.prev <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") %>% #Select minnow trap method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Select total and infected community matrix
  na.omit()

MT.prev <- MT.prev %>% 
  group_by(Lake) %>% #Summarize abundance data by lake
  summarise(across(.cols = everything(), sum))

MT.prev <- MT.prev %>%  #Sum total and infected fish abundance within lakes
  mutate(tot_fish = tot_AmRu  + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_PiNo + tot_SeAt, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_PiNo + inf_SeAt, .keep = "unused") 

MT.prev <- MT.prev %>% #Calculate community level prevalence for each lake
  mutate(prev_fish = (inf_fish/tot_fish)*100)

C.hist <- ggplot(C.prev, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#7E7E7E", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency", title = "Combined") + 
  theme(text = element_text(size = 32, family = "Calibri Light", color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"),
        plot.title = element_text(hjust = 0.5, vjust = 1))

C.hist

T.plain.hist <- hist(T.prev$prev_fish)

T.hist <- ggplot(T.prev, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#966F1E", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency", title = "Transect") + 
  theme(text = element_text(size = 32, family = "Calibri Light", color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"),
        plot.title = element_text(hjust = 0.5, vjust = 1))

T.hist

S.plain.hist <- hist(S.prev$prev_fish)

S.hist <- ggplot(S.prev, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#999600", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency", title = "Seine net") + 
  theme(text = element_text(size = 32, family = "Calibri Light", color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"),
        plot.title = element_text(hjust = 0.5, vjust = 1))

S.hist

MT.hist <- ggplot(MT.prev, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#2A5676", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency", title = "Minnow trap") +
  theme(text = element_text(size = 32, 
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
        plot.title = element_text(hjust = 0.5, vjust = 1))

MT.hist

Summary.plot <- C.hist + MT.hist + S.hist + T.hist + 
  plot_layout(ncol = 2,
              nrow = 2, 
              tag_level = "new") +
  plot_annotation(tag_levels = "A") &
  theme(text = element_text(size = 32, 
                            family = "Calibri Light", 
                            color = "black"),
        plot.margin=unit(c(10,5,10,5), 'mm'))

Summary.plot       
ggsave(paste0(to.figs, "FrequencyDistribution_Exclusion.png"), plot = Summary.plot, dpi = 300, width = 15, height = 17)


# ---- LeGi & hosts GAMMs ----

mod.data$Lake <- as.factor(mod.data$Lake)

## Null ----
NULL.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(NULL.GAMM.legi) 
#Variable significant
#Adj. R-sq. = 0.869
#Deviance explained = 87.2%

NULL.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(NULL.GAMM.host) 
#Variable significant
#Adj. R-sq. = 0.838
#Deviance explained = 85%

## TNTP ----
TNTP.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(TN_TP, bs = "cr") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(TNTP.GAMM.legi) 
#All variable significant
#Adj. R-sq. = 0.945
#Deviance explained = 94.9%

draw.TNTP.legi <- draw(TNTP.GAMM.legi, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans) +
  theme_void()
draw.TNTP.legi
#ok
TNTP.legi.sm <- smooth_estimates(TNTP.GAMM.legi) %>%
  add_confint()
TNTP.legi.pr <- mod.data %>%
  add_partial_residuals(TNTP.GAMM.legi)
TNTP.legi.pe <- TNTP.legi.sm %>%
  filter(.smooth == "s(TN_TP)") %>%
  ggplot( unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = TN_TP),
           data = TNTP.legi.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = TN_TP), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = TN_TP, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "TN:TP", y = "Partial effect (prevalence)", tag = "(d)") +
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
TNTP.legi.pe

TNTP.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(TN_TP, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(TNTP.GAMM.host) 
#Variable not significant
#Adj. R-sq. = 0.847
#Deviance explained = 85.8%

## Nitrogen ----

TN.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(TN, bs = "cs") + s(Lake, bs = "re"),
                    family = quasibinomial, data = mod.data, method = "ML")
summary(TN.GAMM.legi) 

#TN unsignificative
#Adj. R-sq. = 0.87
#Deviance explained = 88%

TN.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(TN, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(TN.GAMM.host) 
#Variable significant
#Adj. R-sq. = 0.838
#Deviance explained = 85%

## Phosphorus ----

TP.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(TP, bs = "cr") + s(Lake, bs = "re"),
                    family = quasibinomial, data = mod.data, method = "ML")
summary(TP.GAMM.legi) 
#TP slightly significative
#Adj. R-sq. = 0.888
#Deviance explained = 89.8%

draw.TP.legi <- draw(TP.GAMM.legi, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TP.legi
#ok
TP.legi.sm <- smooth_estimates(TP.GAMM.legi) %>%
  add_confint()
TP.legi.pr <- mod.data %>%
  add_partial_residuals(TP.GAMM.legi)
TP.legi.pe <- TP.legi.sm %>%
  filter(.smooth == "s(TP)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = TP),
           data = TP.legi.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = TP), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = TP, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "TP (mg/L)", y = "Partial effect (prevalence)", tag = "(e)") +
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
TP.legi.pe

TP.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(TP, bs = "cs") + s(Lake, bs = "re"),
                    family = quasibinomial, data = mod.data, method = "ML")
summary(TP.GAMM.host) 


## Carbon ----

TOC.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(TOC, bs = "cs") + s(Lake, bs = "re"),
                     family = quasibinomial, data = mod.data, method = "ML")
summary(TOC.GAMM.legi) 
#TOC not significant

TOC.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(TOC, bs = "cs") + s(Lake, bs = "re"),
                    family = quasibinomial, data = mod.data, method = "ML")
summary(TOC.GAMM.host) 

## Silt ----

SILT.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Silt, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(SILT.GAMM.legi) 
#slightly significative
#Adj. R-Sq. = 0.909
#Deviance explained = 90.9%

draw.SILT.legi <- draw(SILT.GAMM.legi, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.SILT.legi #Significativity not ok

SILT.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Silt, bs = "cs") + s(Lake, bs = "re"),
                    family = quasibinomial, data = mod.data, method = "ML")
summary(SILT.GAMM.host) 

draw.SILT.host <- draw(SILT.GAMM.host, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.SILT.host #Significativity not ok

## Sand ----

### Legi ----

SAND.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Sand, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(SAND.GAMM.legi) 
#Unsignificative

SAND.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Sand, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(SAND.GAMM.host) 


## Rock ----

ROCK.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Rock, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(ROCK.GAMM.legi) 
#significative
#Adj. R-Sq. = 0.902
#Deviance explained = 90.2%

draw.ROCK.legi <- draw(ROCK.GAMM.legi, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.ROCK.legi #Significativity not ok

ROCK.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Rock, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(ROCK.GAMM.host) 

draw.ROCK.host <- draw(ROCK.GAMM.host, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.ROCK.host #Significativity not ok

## Boulder ----

BOULD.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Boulder, bs = "cs") + s(Lake, bs = "re"),
                       family = quasibinomial, data = mod.data, method = "ML")
summary(BOULD.GAMM.legi) 
#Unsignificative

BOULD.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Boulder, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(BOULD.GAMM.host) 

draw.BOULD.host <- draw(BOULD.GAMM.host, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.BOULD.host #Significativity not ok

## Macrophyte cover ----

MACRO.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Macrophyte, bs = "cs") + s(Lake, bs = "re"),
                       family = quasibinomial, data = mod.data, method = "ML")
summary(MACRO.GAMM.legi) 
#Unsignificant

MACRO.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Macrophyte, bs = "cs") + s(Lake, bs = "re"),
                       family = quasibinomial, data = mod.data, method = "ML")
summary(MACRO.GAMM.host) 

## Transect depth ----

DEPTH.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Site_depth, bs = "cs") + s(Lake, bs = "re"),
                       family = quasibinomial, data = mod.data, method = "ML")
summary(DEPTH.GAMM.legi) 
#All variables are significant
#Adj. R- sq. = 0.913
#Deviance explained = 91.8%
draw.DEPTH.legi <- draw(DEPTH.GAMM.legi, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.DEPTH.legi #Significativity not ok

DEPTH.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Site_depth, bs = "cs") + s(Lake, bs = "re"),
                       family = quasibinomial, data = mod.data, method = "ML")
summary(DEPTH.GAMM.host) 

draw.DEPTH.host <- draw(DEPTH.GAMM.host, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.DEPTH.host #Significativity not ok

## Trunk ----

TRUNK.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Trunk, bs = "cs") + s(Lake, bs = "re"),
                       family = quasibinomial, data = mod.data, method = "ML")
summary(TRUNK.GAMM.legi) 
#Trunk is not significant

TRUNK.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Trunk, bs = "cs") + s(Lake, bs = "re"),
                       family = quasibinomial, data = mod.data, method = "ML")
summary(TRUNK.GAMM.host) 

## Temperature ----

TEMP.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Temperature, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(TEMP.GAMM.legi) 

#slightly significant
#Adj. R-sq. = 0.917
#Deviance explained = 92.1%
draw.TEMP.legi <- draw(TEMP.GAMM.legi, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TEMP.legi

Temp.legi.sm <- smooth_estimates(TEMP.GAMM.legi) %>%
  add_confint()
Temp.legi.pr <- mod.data %>%
  add_partial_residuals(TEMP.GAMM.legi)
Temp.legi.pe <- Temp.legi.sm %>%
  filter(.smooth == "s(Temperature)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Temperature),
           data = Temp.legi.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Temperature), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Temperature, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Temperature (°C)", y = "Partial effect (prevalence)", tag = "(b)") +
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
Temp.legi.pe

TEMP.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Temperature, bs = "cs") + s(Lake, bs = "re"),
                       family = quasibinomial, data = mod.data, method = "ML")
summary(TEMP.GAMM.host) 

draw.TEMP.host <- draw(TEMP.GAMM.host, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TEMP.host

## Turbidity ----

TURB.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Turbidity, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(TURB.GAMM.legi) 
#All variables are significant
#Adj. R-sq. = 0.939
#Deviance explained = 93.7%
draw.TURB.legi <- draw(TURB.GAMM.legi, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TURB.legi
#Significativity OK
TURB.legi.sm <- smooth_estimates(TURB.GAMM.legi) %>%
  add_confint()
TURB.legi.pr <- mod.data %>%
  add_partial_residuals(TURB.GAMM.legi)
TURB.legi.pe <- TURB.legi.sm %>%
  filter(.smooth == "s(Turbidity)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Turbidity),
           data = TURB.legi.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Turbidity), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Turbidity, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Turbidity (NTU)", y = "Partial effect (prevalence)", tag = "(a)") +
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
TURB.legi.pe

TURB.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Turbidity, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(TURB.GAMM.host) 

draw.TURB.host <- draw(TURB.GAMM.host, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.TURB.host

## PH ----

PH.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(pH, bs = "cs") + s(Lake, bs = "re"),
                    family = quasibinomial, data = mod.data, method = "ML")
summary(PH.GAMM.legi) 
#slightly significant
#Adj. R-sq. = 0.882
#Deviance explained = 88.5%

draw.PH.legi <- draw(PH.GAMM.legi, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) +
  scale_y_continuous(trans = inverse_logit_trans) 
draw.PH.legi
#Significativity not OK

PH.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(pH, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(PH.GAMM.host) 

draw.PH.host <- draw(PH.GAMM.host, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) +
  scale_y_continuous(trans = inverse_logit_trans) 
draw.PH.host

## Dissolved oxygen ----

DO.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(DO, bs = "cs") + s(Lake, bs = "re"),
                    family = quasibinomial, data = mod.data, method = "ML")
summary(DO.GAMM.legi) 
#All variables are significant
#Adj. R-sq. = 0.912
#Deviance explained = 91.9%
draw.DO.legi <- draw(DO.GAMM.legi, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans) 
draw.DO.legi
#ok
DO.legi.sm <- smooth_estimates(DO.GAMM.legi) %>%
  add_confint()
DO.legi.pr <- mod.data %>%
  add_partial_residuals(DO.GAMM.legi)
DO.legi.pe <- DO.legi.sm %>%
  filter(.smooth == "s(DO)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = DO),
           data = DO.legi.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = DO), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = DO, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Dissolved oxygen (mg/L)", y = "Partial effect (prevalence)", tag = "(c)") +
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
DO.legi.pe

DO.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(DO, bs = "cs") + s(Lake, bs = "re"),
                    family = quasibinomial, data = mod.data, method = "ML")
summary(DO.GAMM.host) 

draw.DO.host <- draw(DO.GAMM.host, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans) 
draw.DO.host

## Conductivity ----

COND.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Conductivity, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(COND.GAMM.legi) 
#Not significant

COND.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Conductivity, bs = "cs") + s(Lake, bs = "re"),
                    family = quasibinomial, data = mod.data, method = "ML")
summary(COND.GAMM.host) 

draw.COND.host <- draw(COND.GAMM.host, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans) 
draw.COND.host

## Area:Perimeter ratio ----

AREAPERI.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Area_Perimeter, bs = "cs") + s(Lake, bs = "re"),
                          family = quasibinomial, data = mod.data, method = "ML")
summary(AREAPERI.GAMM.legi) 
#Area:Perimeter is significant (but not lake)
#Adj. R-sq. = 0.874
#Deviance explained = 86.4%
draw.AREAPERI.legi <- draw(AREAPERI.GAMM.legi, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.AREAPERI.legi
#Significativity OK. 
AREAPERI.legi.sm <- smooth_estimates(AREAPERI.GAMM.legi) %>%
  add_confint()
AREAPERI.legi.pr <- mod.data %>%
  add_partial_residuals(AREAPERI.GAMM.legi)
AREAPERI.legi.pe <- AREAPERI.legi.sm %>%
  filter(.smooth == "s(Area_Perimeter)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Area_Perimeter),
           data = AREAPERI.legi.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Area_Perimeter), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Area_Perimeter, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Area:Perimeter (m)", y = "Partial effect (prevalence)", tag = "(f)") +
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
AREAPERI.legi.pe

AREAPERI.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Area_Perimeter, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(AREAPERI.GAMM.host) 

draw.AREAPERI.host <- draw(AREAPERI.GAMM.host, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.AREAPERI.host

## Surface area ----

AREA.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Lake_area, bs = "cs") + s(Lake, bs = "fs"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(AREA.GAMM.legi) 
#not significant

AREA.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Lake_area, bs = "cs") + s(Lake, bs = "re"),
                          family = quasibinomial, data = mod.data, method = "ML")
summary(AREA.GAMM.host) 

## Perimeter ----

PERI.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Perimeter, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(PERI.GAMM.legi) 
#Not significant

PERI.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Perimeter, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(PERI.GAMM.host) 

## Lake mean depth ----

MDEPTH.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Mean_depth, bs = "cs") + s(Lake, bs = "re"),
                        family = quasibinomial, data = mod.data, method = "ML")
summary(MDEPTH.GAMM.legi) 
#Mean depth is not significant

MDEPTH.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Mean_depth, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(MDEPTH.GAMM.host) 

## Maximum depth ----

XDEPTH.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Max_depth, bs = "cs") + s(Lake, bs = "re"),
                        family = quasibinomial, data = mod.data, method = "ML")
summary(XDEPTH.GAMM.legi) 
#Maximum depth is not significant

XDEPTH.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Max_depth, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(XDEPTH.GAMM.host) 

## Distance to nearest lake ----

NEAR.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Connectivity, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(NEAR.GAMM.legi) 
#Distance to nearest lake is not significant

NEAR.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Connectivity, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(NEAR.GAMM.host) 

## Water residence time ----

WRT.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(WRT, bs = "cs") + s(Lake, bs = "re"),
                     family = quasibinomial, data = mod.data, method = "ML") 
summary(WRT.GAMM.legi) 
#WRT is not significant

WRT.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(WRT, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(WRT.GAMM.host) 

## Drainage area ----

DRAIN.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Drainage_area, bs = "cs") + s(Lake, bs = "re"),
                       family = quasibinomial, data = mod.data, method = "ML")
summary(DRAIN.GAMM.legi) 
#Drainage area is not significant

DRAIN.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Drainage_area, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(DRAIN.GAMM.host) 

## Elevation ----

ELEV.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Elevation, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(ELEV.GAMM.legi) 
#Elevation is not significant

ELEV.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Elevation, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(ELEV.GAMM.host) 

draw.ELEV.host <- draw(ELEV.GAMM.host, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.ELEV.host

## Fish abundance ----

FISH.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(tot_fish, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(FISH.GAMM.legi) 
#All variables are significant
#Adj. R-sq. = 0.966
#Deviance explained = 96.8%
draw.FISH.legi <- draw(FISH.GAMM.legi, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.FISH.legi
#Significativity is OK
FISH.legi.sm <- smooth_estimates(FISH.GAMM.legi) %>%
  add_confint()
FISH.legi.pr <- mod.data %>%
  add_partial_residuals(FISH.GAMM.legi)
FISH.legi.pe <- FISH.legi.sm %>%
  filter(.smooth == "s(tot_fish)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = tot_fish),
           data = FISH.legi.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = tot_fish), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = tot_fish, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Fish abundance", y = "Partial effect (prevalence)", tag = "(g)") +
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
FISH.legi.pe

FISH.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(tot_fish, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(FISH.GAMM.host) 

draw.FISH.host <- draw(FISH.GAMM.host, unconditional = TRUE, overall_uncertainty = TRUE, select = 1) + 
  scale_y_continuous(trans = inverse_logit_trans)
draw.FISH.host

## Non-host abundance ----

NONHOST.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(tot_Cyprinidae, bs = "cs") + s(Lake, bs = "re"),
                         family = quasibinomial, data = mod.data, method = "ML")
summary(NONHOST.GAMM.legi)

NONHOST.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(tot_Cyprinidae, bs = "cs") + s(Lake, bs = "re"),
                         family = quasibinomial, data = mod.data, method = "ML")
summary(NONHOST.GAMM.host)

## Species richness ----

SP.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Species_richness, bs = "cs", k = 5) + s(Lake, bs = "re"),
                    family = quasibinomial, data = mod.data, method = "ML")
summary(SP.GAMM.legi) 
#Species richness is not significant

SP.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Species_richness, bs = "cs", k = 5) + s(Lake, bs = "re"),
                    family = quasibinomial, data = mod.data, method = "ML")
summary(SP.GAMM.host) 

## Diversity index ----

DIVERS.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Diversity, bs = "cs") + s(Lake, bs = "re"),
                        family = quasibinomial, data = mod.data, method = "ML")
summary(DIVERS.GAMM.legi) 
#Not significant

DIVERS.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Diversity, bs = "cs") + s(Lake, bs = "re"),
                        family = quasibinomial, data = mod.data, method = "ML")
summary(DIVERS.GAMM.host)

## Envenness ----

EVEN.GAMM.legi <- gam(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ s(Evenness, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(EVEN.GAMM.legi) 
#Diversity is not significant

EVEN.GAMM.host <- gam(cbind(inf_host, tot_host - inf_host) ~ s(Evenness, bs = "cs") + s(Lake, bs = "re"),
                      family = quasibinomial, data = mod.data, method = "ML")
summary(EVEN.GAMM.host) 

## Summary plot ----
Summary.plot.legi <- (TURB.legi.pe + Temp.legi.pe + DO.legi.pe + TNTP.legi.pe + TP.legi.pe + AREAPERI.legi.pe + FISH.legi.pe) +
  plot_layout(ncol =4, nrow = 2) &
  theme(text = element_text(family = "Calibri Light", size = 40, color = "black"))

ggsave(paste0(to.figs, "GAMMs_PartialEffects_LeGi.png"), plot = Summary.plot.legi, dpi = 500, width = 45, height = 20)

# ---- Accumulation legi ----

#LeGi
C.prev.legi <- data.frame()

for(i in 1:30) {
  for(j in 1:Resampling) {
    line <- sample(1:nrow(CLeGi), i) #sample i lines randomly
    prop.samp <- CLeGi[line, "tot_LeGi"]/sum(CLeGi[line, "tot_LeGi"])
    w_prev <- CLeGi[line, "inf_LeGi"]*prop.samp
    Prevalence <- sum(na.omit(w_prev))
    output <- data.frame(N = i, Resampling = j, Prevalence) #save output in temporary data.frame (changed at each iterations)
    C.prev.legi <- rbind(C.prev.legi, output)
  }
}

C.prev.legi <- C.prev.legi %>% 
  mutate(Method = "Combined", .before = "N")

#LeGi
MT.prev.legi <- data.frame()

for(i in 1:30) {
  for(j in 1:Resampling) {
    line <- sample(1:nrow(MTLeGi), i) #sample i lines randomly
    prop.samp <- MTLeGi[line, "tot_LeGi"]/sum(MTLeGi[line, "tot_LeGi"])
    w_prev <- MTLeGi[line, "inf_LeGi"]*prop.samp
    Prevalence <- sum(na.omit(w_prev))
    output <- data.frame(N = i, Resampling = j, Prevalence) #save output in temporary data.frame (changed at each iterations)
    MT.prev.legi <- rbind(MT.prev.legi, output)
  }
}

MT.prev.legi <- MT.prev.legi %>% 
  mutate(Method = "Minnow trap", .before = "N")

#LeGi
S.prev.legi <- data.frame()

for(i in 1:30) {
  for(j in 1:Resampling) {
    line <- sample(1:nrow(SLeGi), i) #sample i lines randomly
    prop.samp <- SLeGi[line, "tot_LeGi"]/sum(SLeGi[line, "tot_LeGi"])
    w_prev <- SLeGi[line, "inf_LeGi"]*prop.samp
    Prevalence <- sum(na.omit(w_prev))
    output <- data.frame(N = i, Resampling = j, Prevalence) #save output in temporary data.frame (changed at each iterations)
    S.prev.legi <- rbind(S.prev.legi, output)
  }
}

S.prev.legi <- S.prev.legi %>% 
  mutate(Method = "Seine net", .before = "N")

#LeGi
T.prev.legi <- data.frame()

for(i in 1:30) {
  for(j in 1:Resampling) {
    line <- sample(1:nrow(TLeGi), i) #sample i lines randomly
    prop.samp <- TLeGi[line, "tot_LeGi"]/sum(TLeGi[line, "tot_LeGi"])
    w_prev <- TLeGi[line, "inf_LeGi"]*prop.samp
    Prevalence <- sum(na.omit(w_prev))
    output <- data.frame(N = i, Resampling = j, Prevalence) #save output in temporary data.frame (changed at each iterations)
    T.prev.legi <- rbind(T.prev.legi, output)
  }
}

T.prev.legi <- T.prev.legi %>% 
  mutate(Method = "Transect", .before = "N")


#legi
df.prev.legi <- rbind(C.prev.legi, MT.prev.legi, S.prev.legi, T.prev.legi)
df.prev.legi$N <- as.factor(df.prev.legi$N)
df.prev.legi$Method <- as.factor(df.prev.legi$Method)

prev.acc.plot.legi <- ggplot(df.prev.legi) + 
  stat_summary(aes(x = N, y = Prevalence, group = Method, color = Method, shape = Method), fun = "mean", size = 1) +
  stat_smooth(aes(x= N, y = Prevalence, group = Method, color = Method, fill = Method), method = "loess", se = FALSE, level = 0.95, lineend = "round", alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Number of samples", y = "Mean infection prevalence") +
  scale_color_manual(values = c("#7E7E7E", "#2A5676", "#999600", "#966F1E"),
                     aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(0, 5, 2, 1)) +
  guides(fill = guide_legend(override.aes = list(fill = NA, linetype = 0))) +
  theme(text = element_text(size = 20, family = "Calibri Light", color = "black"),
        plot.background = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.key = element_rect(fill = NA, colour = NA),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line.x = element_line(color = "black", lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))


prev.acc.plot.legi

ggsave(paste0(to.figs, "AccumulationCurves_prevalence_LeGi.png"), plot = prev.acc.plot.legi, dpi = 300, width = 15, height = 10)  
