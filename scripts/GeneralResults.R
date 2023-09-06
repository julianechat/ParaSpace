## Script name : General patterns and host specificity

## Authors : Juliane Vigneault
## Date created : August 21, 2023

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
library(gt)
library(janitor)
library(webshot2)
library(tidyr)
library(splitstackshape)
library(stringr)
library(tibble)

## Loading data ----

#CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))
FishingData <- read.csv(paste0(to.output, "Fishing_WideData.csv"))
TransectData <- read.csv(paste0(to.output, "Transects_WideData.csv"))
FishingRaw <- read.csv(paste0(to.data, "Fishing_RawData.csv"), sep = ";")

# ---- Abundance data ----
## Captures ----

SpCaptured <- FishingData %>% #Selecting capture data
  select(Lake, starts_with("tot")) %>% 
  mutate(UnknownSp. = tot_Centrarchidae + tot_Cyprinidae, .keep = "unused")

SpCaptured <- SpCaptured %>% #Summarizing by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum)) 

SpCaptured <- SpCaptured %>% 
  adorn_totals(c("row", "col")) #Total by species and total by lake

SpCaptured_tab <- gt(SpCaptured) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), tot_AmRu = md("**AmRu**"), tot_FuDi = md("**FuDi**"), tot_MiDo = md("**MiDo**"), tot_LeGi = md("**LeGi**"), tot_PeFl = md("**PeFl**"), tot_PiPr = md("**PiPr**"), tot_Chrosomus.sp. = md("**Chrosomus sp.**"), tot_PiNo = md("**PiNo**"), tot_SeAt = md("**SeAt**"), tot_LuCo = md("**LuCo**"), tot_AmNe = md("**AmNe**"), tot_CaCo = md("**CaCo**"), tot_EsMa = md("**EsMa**"), tot_UmLi = md("**UmLi**"), tot_RhAt = md("**RhAt**"), UnknownSp. = md("**Unknown sp.**"), Total = md("**Total**")) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "lightgrey"),
            locations =  cells_body(rows = 15)) %>%
  tab_style(style = cell_borders(sides = "bottom", weight = px(2)),
            locations =  cells_body(rows = 16)) %>%
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(column = everything())) %>% 
  tab_style(style = cell_text(align = "center", v_align = "middle"), 
            locations = cells_column_labels()) %>% 
  tab_style(style = cell_borders(sides = "left", weight = px(2), color = "lightgrey"),
           locations =  cells_body(column = 18)) %>% 
  tab_style(style= cell_borders(sides = "left", weight = px(2), color = "lightgrey"), 
            location = list(cells_column_labels(column = 18))) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(column = 1)) %>% 
  sub_values(columns = 1, rows = 12, values = "Pin_rouge", replacement = "Pin rouge")

SpCaptured_tab %>% #Saving gt tab
gtsave("Summary_captures.png", paste0(to.figs))

## Observations ----

SpObserved <- TransectData %>% #Selecting observations
  select(Lake, starts_with("tot"))

SpObserved <- SpObserved %>% #Summarizing by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

SpObserved <- SpObserved %>% #Total by species and total by lake
  adorn_totals(c("row", "col"))

SpObserved_tab <- gt(SpObserved) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), tot_AmRu = md("**AmRu**"), tot_MiDo = md("**MiDo**"), tot_LeGi = md("**LeGi**"), tot_PeFl = md("**PeFl**"), tot_Cyprinidae = md("**Cyprinidae**"), Total = md("**Total**")) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "lightgrey"),
            locations =  cells_body(rows = 15)) %>%
  tab_style(style = cell_borders(sides = "bottom", weight = px(2)),
            locations =  cells_body(rows = 16)) %>%
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(column = everything())) %>% 
  tab_style(style = cell_text(align = "center", v_align = "middle"), 
            locations = cells_column_labels()) %>% 
  tab_style(style = cell_borders(sides = "left", weight = px(2), color = "lightgrey"),
            locations =  cells_body(column = 7)) %>% 
  tab_style(style= cell_borders(sides = "left", weight = px(2), color = "lightgrey"), 
            location = list(cells_column_labels(column = 7))) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(column = 1)) %>% 
  sub_values(columns = 1, rows = 12, values = "Pin_rouge", replacement = "Pin rouge")

SpObserved_tab %>% #Saving gt tab
  gtsave("Summary_observations.png", paste0(to.figs))

## All data ----

SpAll <- CombinedData %>% 
  select(Lake, starts_with("tot")) %>% 
  na.omit()
  
SpAll <- SpAll %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

SpAll <- SpAll %>% 
  adorn_totals(where = c("row", "col"), na.rm = TRUE)

SpAll_tab <- gt(SpAll) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), tot_AmRu = md("**AmRu**"), tot_FuDi = md("**FuDi**"), tot_MiDo = md("**MiDo**"), tot_LeGi = md("**LeGi**"), tot_PeFl = md("**PeFl**"), tot_PiPr = md("**PiPr**"), tot_ChrosomusSp. = md("**Chrosomus sp.**"), tot_PiNo = md("**PiNo**"), tot_SeAt = md("**SeAt**"), tot_LuCo = md("**LuCo**"), tot_AmNe = md("**AmNe**"), tot_CaCo = md("**CaCo**"), tot_EsMa = md("**EsMa**"), tot_UmLi = md("**UmLi**"), tot_RhAt = md("**RhAt**"), tot_Centrarchidae = md("**Centrarchidae**"), tot_Cyprinidae = md("**Cyprinidae**"), Total = md("**Total**")) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "lightgrey"),
            locations =  cells_body(rows = 15)) %>%
  tab_style(style = cell_borders(sides = "bottom", weight = px(2)),
            locations =  cells_body(rows = 16)) %>%
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(column = everything())) %>% 
  tab_style(style = cell_text(align = "center", v_align = "middle"), 
            locations = cells_column_labels()) %>% 
  tab_style(style = cell_borders(sides = "left", weight = px(2), color = "lightgrey"),
            locations =  cells_body(column = 19)) %>% 
  tab_style(style= cell_borders(sides = "left", weight = px(2), color = "lightgrey"), 
            location = list(cells_column_labels(column = 19))) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(column = 1)) %>% 
  sub_values(columns = 1, rows = 12, values = "Pin_rouge", replacement = "Pin rouge")

SpAll_tab %>% #Saving gt tab
  gtsave("Summary_AllFishes.png", paste0(to.figs))

# ---- Length data ----

FishLength <- FishingRaw %>% #Selecting data of interest
  select(Lake, Species_ID, Length, Abundance) %>% 
  na.omit() %>% 
  arrange(Lake, Species_ID) %>% 
  filter(!(Species_ID == "Centrarchidae")) %>% #Deleting Centrarchidae because not a species level
  filter(!(Species_ID == "Cyprinidae")) #Deleting Cyprinidae because not a species level
    
FishLength <- expandRows(FishLength, "Abundance") #Reshaping data frame for 1 row = 1 individual format

#Fishy1 <- FishLength %>% #Summarizing number of individuals, mean length and sd for each species within each lake
  #group_by(Lake, Species_ID, .add = TRUE) %>% 
  #summarise(Mean = mean(Length), sd = sd(Length), N = n())

#Fishy1 %>% #Saving gt tab
  #gtsave("Summary_Length.docx", paste0(to.figs))

#Fishy2 <- Fishy1 %>% 
  #pivot_wider(id_cols = Lake, 
              #names_from = Species_ID, 
              #values_from = c("Mean", "sd"))
  
#Fishy3 <- gt(Fishy1, rowname_col = "Species_ID",  groupname_col = "Lake")

#Fishy4 <- tbl_strata(FishLength,
                     #strata = Lake, 
                    #.tbl_fun = tbl_summary(FishLength, 
                                           #by = Species_ID,
                                           #statistic = list(all_continuous()~"{mean}({sd})")))

#Fishy5 <- FishLength %>% tabyl(Lake, Species_ID)
#xtabs(~Species_ID + Lake, data = FishLength)

#Fishy6 <- cross_mean_sd_n(
  #FishLength,
  #Length,
  #col_vars = Species_ID,
  #row_vars = Lake)


Length.TotMean <- FishLength %>% #All data summary statistics
  select(Length) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

Length.LakeMean <- FishLength %>% #Summary statistic by lake
  group_by(Lake) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

Length.SpeciesMean <- FishLength %>% #Summary statistic by species
  group_by(Species_ID) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

# ---- Host specificity ----
## Regional ----

HostSpec.regional <- CombinedData %>% #Selecting data of interest
  select(starts_with(c("tot_", "inf_")), -c("tot_Centrarchidae", "tot_Cyprinidae", "inf_Centrarchidae", "inf_Cyprinidae")) %>% 
  na.omit() 

#Hosty <- CombinedData %>% #Selecting data of interest
  #select(Lake, starts_with(c("tot_", "inf_")), -c("tot_Centrarchidae", "tot_Cyprinidae", "inf_Centrarchidae", "inf_Cyprinidae")) %>% 
  #na.omit() 

#Hosty <- Hosty %>% 
  #adorn_totals(where = "row")

#Hosty <- Hosty %>% filter(Lake == "Total")

#Hosty <- Hosty %>% #Calculating prevalence by species
 # mutate(prev_AmRu = inf_AmRu/tot_AmRu, .keep = "unused") %>% 
  #mutate(prev_FuDi = inf_FuDi/tot_FuDi, .keep = "unused") %>%
  #mutate(prev_MiDo = inf_MiDo/tot_MiDo, .keep = "unused") %>%
  #mutate(prev_LeGi = inf_LeGi/tot_LeGi, .keep = "unused") %>%
  #mutate(prev_PeFl = inf_PeFl/tot_PeFl, .keep = "unused") %>%
  #mutate(prev_PiPr = inf_PiPr/tot_PiPr, .keep = "unused") %>%
  #mutate(prev_ChrosomusSp. = inf_ChrosomusSp./tot_ChrosomusSp., .keep = "unused") %>%
  #mutate(prev_PiNo = inf_PiNo/tot_PiNo, .keep = "unused") %>%
  #mutate(prev_SeAt = inf_SeAt/tot_SeAt, .keep = "unused") %>%
  #mutate(prev_LuCo = inf_LuCo/tot_LuCo, .keep = "unused") %>%
  #mutate(prev_AmNe = inf_AmNe/tot_AmNe, .keep = "unused") %>%
  #mutate(prev_CaCo = inf_CaCo/tot_CaCo, .keep = "unused") %>%
  #mutate(prev_EsMa = inf_EsMa/tot_EsMa, .keep = "unused") %>%
  #mutate(prev_UmLi = inf_UmLi/tot_UmLi, .keep = "unused") %>%
  #mutate(prev_RhAt = inf_RhAt/tot_RhAt, .keep = "unused")

HostSpec.regional <- HostSpec.regional %>% #Calculating prevalence by species
  mutate(prev_AmRu = inf_AmRu/tot_AmRu, .keep = "unused") %>% 
  mutate(prev_FuDi = inf_FuDi/tot_FuDi, .keep = "unused") %>%
  mutate(prev_MiDo = inf_MiDo/tot_MiDo, .keep = "unused") %>%
  mutate(prev_LeGi = inf_LeGi/tot_LeGi, .keep = "unused") %>%
  mutate(prev_PeFl = inf_PeFl/tot_PeFl, .keep = "unused") %>%
  mutate(prev_PiPr = inf_PiPr/tot_PiPr, .keep = "unused") %>%
  mutate(prev_ChrosomusSp. = inf_ChrosomusSp./tot_ChrosomusSp., .keep = "unused") %>%
  mutate(prev_PiNo = inf_PiNo/tot_PiNo, .keep = "unused") %>%
  mutate(prev_SeAt = inf_SeAt/tot_SeAt, .keep = "unused") %>%
  mutate(prev_LuCo = inf_LuCo/tot_LuCo, .keep = "unused") %>%
  mutate(prev_AmNe = inf_AmNe/tot_AmNe, .keep = "unused") %>%
  mutate(prev_CaCo = inf_CaCo/tot_CaCo, .keep = "unused") %>%
  mutate(prev_EsMa = inf_EsMa/tot_EsMa, .keep = "unused") %>%
  mutate(prev_UmLi = inf_UmLi/tot_UmLi, .keep = "unused") %>%
  mutate(prev_RhAt = inf_RhAt/tot_RhAt, .keep = "unused")

regionalMeans <- HostSpec.regional %>% #Calculating mean prevalence by species
  apply(2, mean, na.rm = TRUE)
regionalSd <- HostSpec.regional %>% 
  apply(2, sd, na.rm=TRUE)
regionalN <- as.vector(SpAll[16, c(2:16)])

HostSpec.regional <- as.data.frame(regionalMeans, row.names = c("AmRu", "FuDi", "MiDo", "LeGi", "PeFl", "PiPr", "Chrosomus sp.", "PiNo", "SeAt", "LuCo", "AmNe", "CaCo", "EsMa", "UmLi", "RhAt")) %>% 
  mutate(sd = regionalSd) %>% 
  mutate(n = regionalN)

colnames(HostSpec.regional) <- HostSpec.regional %>% #Changing column name
  colnames() %>% 
  str_replace("regionalMeans", "mean")

HostSpec.regional <- HostSpec.regional %>% 
  rownames_to_column( var = "Species_ID")

HostSpec.regional <- HostSpec.regional %>% #Arranging species by numerical order of prevalence value
  arrange(desc(mean)) 

HostSpec.regional_tab <- gt(HostSpec.regional) %>% 
  cols_label(Species_ID = md("**Species_ID**"), mean = md("**Mean**"), sd = md("**sd**"), n = md("**n**")) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2)),
            locations = cells_body(rows = 15)) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = 1)) %>% 
  tab_style(style = cell_text(align = "center"),
            locations = cells_body()) %>% 
  tab_style(style = cell_text(align = "center"),
            locations = cells_column_labels()) %>% 
  fmt_number(decimals = 2)

HostSpec.regional_tab %>% #Saving gt tab
  gtsave("HostSpecificity_regional.png", paste0(to.figs))

## Local ----

HostSpec.local <- CombinedData %>% #Selecting data of interest
  select(Lake, starts_with(c("tot_", "inf_")), -c("tot_Centrarchidae", "tot_Cyprinidae", "inf_Centrarchidae", "inf_Cyprinidae")) %>% 
  na.omit() 

#HostSpec.local <- HostSpec.local %>% #Summarizing abundance data by lake
  #group_by(Lake) %>% 
  #summarise(across(.cols = everything(), sum))

HostSpec.local <- HostSpec.local %>% 
  group_by(Lake) %>% #Calculating prevalence by species and lake
  mutate(prev_AmRu = inf_AmRu/tot_AmRu) %>%
  mutate(prev_FuDi = inf_FuDi/tot_FuDi) %>% 
  mutate(prev_MiDo = inf_MiDo/tot_MiDo) %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi) %>% 
  mutate(prev_PeFl = inf_PeFl/tot_PeFl) %>% 
  mutate(prev_PiPr = inf_PiPr/tot_PiPr) %>% 
  mutate(prev_ChrosomusSp. = inf_ChrosomusSp./tot_ChrosomusSp.) %>% 
  mutate(prev_PiNo = inf_PiNo/tot_PiNo) %>% 
  mutate(prev_SeAt = inf_SeAt/tot_SeAt) %>% 
  mutate(prev_LuCo = inf_LuCo/tot_LuCo) %>% 
  mutate(prev_AmNe = inf_AmNe/tot_AmNe) %>% 
  mutate(prev_CaCo = inf_CaCo/tot_CaCo) %>% 
  mutate(prev_EsMa = inf_EsMa/tot_EsMa) %>% 
  mutate(prev_UmLi = inf_UmLi/tot_UmLi) %>% 
  mutate(prev_RhAt = inf_RhAt/tot_RhAt)

localMeans <- HostSpec.local %>% 
  select(Lake, starts_with("prev")) %>% 
  group_by(Lake) %>% 
  summarise_all(mean, na.rm = TRUE)
colnames(localMeans) <- localMeans %>% #Changing columns names
  colnames() %>% 
  str_remove_all("prev_")
localSd <- HostSpec.local %>% 
  select(Lake, starts_with("prev")) %>% 
  group_by(Lake) %>% 
  summarise_all(sd, na.rm = TRUE)
localN <- HostSpec.local %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum)) %>% 
  select(Lake, starts_with("tot"))
  
localMeans <- localMeans %>% 
  pivot_longer(2:16, names_to = "Species_ID", values_to = "Prevalence")
localSd <- localSd %>% 
  pivot_longer(2:16, names_to = "Species_ID", values_to = "sd")
localN <- localN %>% 
  pivot_longer(2:16, names_to = "Species_ID", values_to = "n")

HostSpec.local <- cbind(localMeans, localSd, localN) %>% 
  select(1, 2, Prevalence, sd, n)

#HostSpec.local <- HostSpec.local %>% 
    #select(!(starts_with("inf")))

#colnames(HostSpec.local) <- HostSpec.local %>% #Changing columns names
  #colnames() %>% 
  #str_remove_all("prev_") #%>% 
  #str_remove_all("tot_")

#HostSpec.local <- HostSpec.local %>% #Changing format
 # pivot_longer(cols = 27:31, names_to = "Species_ID", values_to = "Prevalence") %>% 
  #pivot_longer(cols = 9:16, names_to = "Species_ID", values_to = "n") %>% 
  #na.omit()

#HostSpec.local <- HostSpec.local %>% #Arranging species by numerical order of prevalence value within each lake
  #group_by(Lake) %>% 
  #arrange(Prevalence, .by_group = TRUE)
