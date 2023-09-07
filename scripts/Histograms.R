## Script name : Frequency distributions

## Authors : Juliane Vigneault & Éric Harvey
## Date created : September 7, 2023

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

## Loading packages ----

library(dplyr)
library(ggplot2)
library(patchwork)

## Loading data ----

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))

# ---- Lake mean prevalence ----
## All methods ----

All.prev <- CombinedData %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

All.prev <- All.prev %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

All.prev <- All.prev %>% 
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_AmNe + tot_CaCo + tot_EsMa + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_AmNe + inf_CaCo + inf_EsMa + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae, .keep = "unused") 

All.prev <- All.prev %>% 
  mutate(prev_fish = inf_fish/tot_fish)

## Transect ----

Trans.prev <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Trans.prev <- Trans.prev %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Trans.prev <- Trans.prev %>% 
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_AmNe + tot_CaCo + tot_EsMa + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_AmNe + inf_CaCo + inf_EsMa + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae, .keep = "unused") 

Trans.prev <- Trans.prev %>% 
  mutate(prev_fish = inf_fish/tot_fish)

## Seine net ----

Seine.prev <- CombinedData %>% 
  filter(Sampling_method == "Seine") %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Seine.prev <- Seine.prev %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Seine.prev <- Seine.prev %>% 
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_AmNe + tot_CaCo + tot_EsMa + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_AmNe + inf_CaCo + inf_EsMa + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae, .keep = "unused") 

Seine.prev <- Seine.prev %>% 
  mutate(prev_fish = inf_fish/tot_fish)

## Minnow trap ----

Trap.prev <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Trap.prev <- Trap.prev %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Trap.prev <- Trap.prev %>% 
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_AmNe + tot_CaCo + tot_EsMa + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_AmNe + inf_CaCo + inf_EsMa + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae, .keep = "unused") 

Trap.prev <- Trap.prev %>% 
  mutate(prev_fish = inf_fish/tot_fish)

# ---- Histograms ----
diverging_hcl(7, palette = "Tofino")

## All methods ----

All.plain.hist <- hist(All.prev$prev_fish)

All.hist <- ggplot(All.prev, aes(prev_fish)) + 
  geom_histogram(bins = 15, fill = "lightgrey", color = "black") +
  geom_density() +
  labs(x = "Prevalence", y = "Frequency") + 
  theme(text = element_text(size = 20, family = "Calibri Light", color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))

## Transect ----

Trans.plain.hist <- hist(Trans.prev$prev_fish)

Trans.hist <- ggplot(Trans.prev, aes(prev_fish)) + 
  geom_histogram(bins = 15, fill = "#7A84B5", color = "black") +
  geom_density() +
  labs(x = "Prevalence", y = "Frequency") + 
  theme(text = element_text(size = 20, family = "Calibri Light", color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))

## Seine net ----

Seine.plain.hist <- hist(Seine.prev$prev_fish)

Seine.hist <- ggplot(Seine.prev, aes(prev_fish)) + 
  geom_histogram(bins = 15, fill = "#111111", color = "black") +
  geom_density(color = "red") +
  labs(x = "Prevalence", y = "Frequency") + 
  theme(text = element_text(size = 20, family = "Calibri Light", color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))

## Minnow trap ----

Trap.plain.hist <- hist(Trap.prev$prev_fish)

Trap.hist <- ggplot(Trap.prev, aes(prev_fish)) + 
  geom_histogram(bins = 15, fill = "#669157", color = "black") +
  geom_density() +
  labs(x = "Prevalence", y = "Frequency") + 
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
                                   lineend = "round"))

## Summary figure ----

Summary.plot <- All.hist + Trans.hist + Seine.hist + Trap.hist + 
  plot_layout(ncol = 2,
              nrow = 2, 
              tag_level = "new") +
  plot_annotation(tag_levels = "A", 
                  title = "Figure 2. Frequency distribution of lake prevalence. (A) All methods. (B) Transects. (C) Seine net (D) Minnow trap.",
                  theme = list(title = element_text(size = 16, 
                                                    family = "Calibri Light", 
                                                    color = "black"))) &
  theme(plot.title = element_text(hjust = 0,
                                  vjust = -290),
        plot.margin = unit(c(0,0,10,0), "mm"))
              
                  
ggsave(paste0(to.figs, "FrequencyDistribution_summary.png"), plot = Summary.plot, dpi = 300, width = 15, height = 15)




