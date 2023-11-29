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
to.rédaction <- "./rédaction/"

## Loading packages ----

library(dplyr)
library(ggplot2)
library(patchwork)
library(colorspace)

## Loading data ----

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))
FishingData <- read.csv(paste0(to.output, "Fishing_WideData.csv"))

# ---- Lake mean prevalence ----

CombinedData <- CombinedData %>% 
  filter(!(Lake == "Tracy"))

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
  mutate(prev_fish = (inf_fish/tot_fish)*100)

All.reg.prev <- mean(All.prev$prev_fish)

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
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Trans.reg.prev <- mean(Trans.prev$prev_fish)

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
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Seine.reg.prev <- mean(Seine.prev$prev_fish)

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
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Trap.reg.prev <- mean(Trap.prev$prev_fish)

### Large trap ----

Large.prev <- FishingData %>% 
  filter(Gear_ID %in% c("N6", "N7", "N8", "N9", "N10", "N11", "N16", "N17")) %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Large.prev <- Large.prev %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Large.prev <- Large.prev %>% 
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_Chrosomus.sp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_AmNe + tot_CaCo + tot_EsMa + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_Chrosomus.sp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_AmNe + inf_CaCo + inf_EsMa + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae, .keep = "unused") 

Large.prev <- Large.prev %>% 
  mutate(prev_fish = (inf_fish/tot_fish)*100)

### Small trap ----

Small.prev <- FishingData %>% 
  filter(Gear_ID %in% c("N1", "N2", "N3", "N4", "N5", "N12", "N13", "N14", "N15")) %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Small.prev <- Small.prev %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Small.prev <- Small.prev %>% 
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_Chrosomus.sp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_AmNe + tot_CaCo + tot_EsMa + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_Chrosomus.sp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_AmNe + inf_CaCo + inf_EsMa + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae, .keep = "unused") 

Small.prev <- Small.prev %>% 
  mutate(prev_fish = (inf_fish/tot_fish)*100)

# ---- Histograms ----

col.pal2 <- c("#7E7E7E", "#2A5676", "#999600", "#966F1E")

## All methods ----

All.plain.hist <- hist(All.prev$prev_fish)

All.hist <- ggplot(All.prev, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#7E7E7E", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency", title = "All") + 
  ylim(0,5) +
  theme(text = element_text(size = 32, family = "Calibri Light", color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"),
        plot.title = element_text(hjust = 0.5, vjust = 1))

ggsave(paste0(to.figs, "FrequencyDistribution_All.png"), plot = All.hist, dpi = 300, width = 5, height = 5)

## Transect ----

Trans.plain.hist <- hist(Trans.prev$prev_fish)

Trans.hist <- ggplot(Trans.prev, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#2A5676", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency", title = "Transect") + 
  ylim(0,5) +
  theme(text = element_text(size = 32, family = "Calibri Light", color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"),
        plot.title = element_text(hjust = 0.5, vjust = 1))

ggsave(paste0(to.figs, "FrequencyDistribution_Transect.png"), plot = Trans.hist, dpi = 300, width = 5, height = 5)

## Seine net ----

Seine.plain.hist <- hist(Seine.prev$prev_fish)

Seine.hist <- ggplot(Seine.prev, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#999600", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency", title = "Seine net") + 
  ylim(0,5) +
  theme(text = element_text(size = 32, family = "Calibri Light", color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"),
        plot.title = element_text(hjust = 0.5, vjust = 1))

ggsave(paste0(to.figs, "FrequencyDistribution_Seine.png"), plot = Seine.hist, dpi = 300, width = 5, height = 5)

## Minnow trap ----

Trap.plain.hist <- hist(Trap.prev$prev_fish)

Trap.hist <- ggplot(Trap.prev, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#966F1E", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency", title = "Minnow trap") + 
  ylim(0,5) +
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

ggsave(paste0(to.figs, "FrequencyDistribution_MinnowTrap.png"), plot = Trap.hist, dpi = 300, width = 5, height = 5)

### Small trap ----

Small.plain.hist <- hist(Small.prev$prev_fish)

Small.hist <- ggplot(Small.prev, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "grey", color = "black") +
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

### Large trap ----

Large.plain.hist <- hist(Large.prev$prev_fish)

Large.hist <- ggplot(Large.prev, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "grey", color = "black") +
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
  plot_annotation(tag_levels = "A") &
  theme(text = element_text(size = 32, 
                            family = "Calibri Light", 
                            color = "black"),
        plot.margin=unit(c(10,5,10,5), 'mm'))
              
                  
ggsave(paste0(to.figs, "FrequencyDistribution_summary.png"), plot = Summary.plot, dpi = 300, width = 15, height = 17)
ggsave(paste0(to.rédaction, "Figures/Figure4_FreqDistributions.png"), plot = Summary.plot, dpi = 300, width = 15, height = 17)

## Summary trap ----

Summary.trap <- Trap.hist + Small.hist + Large.hist +
  plot_layout(ncol = 3,
              nrow = 1, 
              tag_level = "new") +
  plot_annotation(tag_levels = "A", 
                  title = "Frequency distribution of lake prevalence. (A) All traps. (B) Small traps. (C) Large traps.",
                  theme = list(title = element_text(size = 20, 
                                                    family = "Calibri Light", 
                                                    color = "black"))) &
  theme(plot.title = element_text(hjust = 0,
                                  vjust = -160),
        plot.margin = unit(c(0,0,10,0), "mm")) &
  ylim(0, 5) &
  xlim(0, 100)


ggsave(paste0(to.figs, "FrequencyDistribution_TrapSummary.png"), plot = Summary.trap, dpi = 300, width = 30, height = 10)
