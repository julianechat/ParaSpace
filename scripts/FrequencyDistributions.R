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

# ---- Lake scale prevalence ----

CombinedData <- CombinedData %>% 
  filter(!(Lake == "Tracy")) #Deleting lake Tracy because of insufficient data

CombinedData <- CombinedData %>% #Muskellunge and brown bullhead individuals are excluded from the prevalence calculus since they are not host of the black spot disease
  select(!(c(inf_EsMa, tot_EsMa, inf_AmNe, tot_AmNe)))

## Combined methods ----

C.prev <- CombinedData %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% #Select total and infected community matrix
  na.omit()

C.prev <- C.prev %>% 
  group_by(Lake) %>% #Summarize abundance data by lake
  summarise(across(.cols = everything(), sum))

C.prev <- C.prev %>%  #Sum total and infected fish abundance within lakes
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSpp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_CaCo + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSpp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_CaCo + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae, .keep = "unused") 

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
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSpp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_CaCo + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSpp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_CaCo + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae, .keep = "unused") 

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
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSpp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_CaCo + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSpp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_CaCo + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae, .keep = "unused") 

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
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSpp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_CaCo + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSpp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_CaCo + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae, .keep = "unused") 

MT.prev <- MT.prev %>% #Calculate community level prevalence for each lake
  mutate(prev_fish = (inf_fish/tot_fish)*100)

### Large trap ----

Large.prev <- CombinedData %>% 
  filter(Gear_ID %in% c("N6", "N7", "N8", "N9", "N10", "N11", "N16", "N17")) %>% #Select gear ID of large round minnow traps
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Large.prev <- Large.prev %>% 
  group_by(Lake) %>% #Summarize abundance data by lakes
  summarise(across(.cols = everything(), sum))

Large.prev <- Large.prev %>% #Sum total and infected fish abundance within lakes
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSpp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_CaCo + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSpp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_CaCo + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae, .keep = "unused") 

Large.prev <- Large.prev %>% #Calculate community level prevalence for each lake
  mutate(prev_fish = (inf_fish/tot_fish)*100)

### Small trap ----

Small.prev <- CombinedData %>% 
  filter(Gear_ID %in% c("N1", "N2", "N3", "N4", "N5", "N12", "N13", "N14", "N15")) %>%  #Select gear ID of small square minnow traps
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Small.prev <- Small.prev %>% 
  group_by(Lake) %>% #Summarize abundance data by lakes
  summarise(across(.cols = everything(), sum))

Small.prev <- Small.prev %>% #Sum total and infected fish abundance within lakes
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSpp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_CaCo + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae, .keep = "unused") %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSpp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_CaCo + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae, .keep = "unused") 

Small.prev <- Small.prev %>% #Calculate community level prevalence for each lake
  mutate(prev_fish = (inf_fish/tot_fish)*100)

# ---- Frequency distributions ----

col.pal <- c("#7E7E7E", "#2A5676", "#999600", "#966F1E")

## Combined methods ----

C.plain.hist <- hist(C.prev$prev_fish)

C.hist <- ggplot(C.prev, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#7E7E7E", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency", title = "Combined") + 
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

C.hist

ggsave(paste0(to.figs, "FrequencyDistribution_Combined.png"), plot = C.hist, dpi = 300, width = 5, height = 5) #Saving plot

## Transect ----

T.plain.hist <- hist(T.prev$prev_fish)

T.hist <- ggplot(T.prev, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#966F1E", color = "black", alpha = 0.8) +
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

T.hist

ggsave(paste0(to.figs, "FrequencyDistribution_Transect.png"), plot = T.hist, dpi = 300, width = 5, height = 5) #Saving plot

## Seine net ----

S.plain.hist <- hist(S.prev$prev_fish)

S.hist <- ggplot(S.prev, aes(prev_fish)) + 
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

S.hist

ggsave(paste0(to.figs, "FrequencyDistribution_Seine.png"), plot = S.hist, dpi = 300, width = 5, height = 5) #Saving plot

## Minnow trap ----

MT.plain.hist <- hist(MT.prev$prev_fish)

MT.hist <- ggplot(MT.prev, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#2A5676", color = "black", alpha = 0.8) +
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

MT.hist

ggsave(paste0(to.figs, "FrequencyDistribution_MinnowTrap.png"), plot = MT.hist, dpi = 300, width = 5, height = 5) #Saving plot

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

Small.hist

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

Large.hist

## Summary methods figure ----

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
                  
ggsave(paste0(to.figs, "FrequencyDistribution_summary.png"), plot = Summary.plot, dpi = 300, width = 15, height = 17)
ggsave(paste0(to.rédaction, "Figures/Figure4_FreqDistributions.png"), plot = Summary.plot, dpi = 300, width = 15, height = 17)

## Summary trap figure ----

Summary.trap <- MT.hist + Small.hist + Large.hist +
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
        plot.margin = unit(c(0,0,10,0), "mm"))

Summary.trap

ggsave(paste0(to.figs, "FrequencyDistribution_TrapSummary.png"), plot = Summary.trap, dpi = 300, width = 30, height = 10)
