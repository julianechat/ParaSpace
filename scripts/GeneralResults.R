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
library(gtsummary)

## Loading data ----

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))
FishingData <- read.csv(paste0(to.output, "Fishing_WideData.csv"))
TransectData <- read.csv(paste0(to.output, "Transects_WideData.csv"))
FishingRaw <- read.csv(paste0(to.data, "Fishing_RawData.csv"), sep = ";")

# ---- Abundance data ----
## Captures ----

SpCaptured <- FishingData %>% #Selecting capture data
  select(Lake, tot_AmRu, tot_FuDi, tot_MiDo, tot_LeGi, tot_PeFl, tot_PiPr, tot_Chrosomus.sp., tot_PiNo, tot_SeAt, tot_LuCo, tot_AmNe, tot_CaCo, tot_EsMa, tot_UmLi, tot_RhAt)

SpCaptured <- SpCaptured %>% #Summarizing by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum)) 

SpCaptured <- SpCaptured %>% 
  adorn_totals(c("row", "col")) #Total by species and total by lake

SpCaptured_tab <- gt(SpCaptured) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), tot_AmRu = md("**AmRu**"), tot_FuDi = md("**FuDi**"), tot_MiDo = md("**MiDo**"), tot_LeGi = md("**LeGi**"), tot_PeFl = md("**PeFl**"), tot_PiPr = md("**PiPr**"), tot_Chrosomus.sp. = md("**Chrosomus sp.**"), tot_PiNo = md("**PiNo**"), tot_SeAt = md("**SeAt**"), tot_LuCo = md("**LuCo**"), tot_AmNe = md("**AmNe**"), tot_CaCo = md("**CaCo**"), tot_EsMa = md("**EsMa**"), tot_UmLi = md("**UmLi**"), tot_RhAt = md("**RhAt**"), Total = md("**Total**")) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "lightgrey"),
            locations =  cells_body(rows = 15)) %>%
  tab_style(style = cell_borders(sides = "bottom", weight = px(2)),
            locations =  cells_body(rows = 16)) %>%
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(column = everything())) %>% 
  tab_style(style = cell_text(align = "center"), 
            locations = cells_column_labels()) %>% 
  tab_style(style = cell_borders(sides = "left", weight = px(2), color = "lightgrey"),
           locations =  cells_body(column = 17)) %>% 
  tab_style(style= cell_borders(sides = "left", weight = px(2), color = "lightgrey"), 
            location = list(cells_column_labels(column = 17))) %>% 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(column = 1)) %>% 
  sub_values(columns = 1, rows = 12, values = "Pin_rouge", replacement = "Pin rouge")

SpCaptured_tab %>% #Saving gt tab
gtsave("Summary_captures.png", paste0(to.figs))

## Observations ----

SpObserved <- TransectData %>% #Selecting observations
  select(Lake, tot_AmRu, tot_MiDo, tot_LeGi, tot_PeFl, tot_Cyprinidae)

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
  tab_style(style = cell_text(align = "center"), 
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

# ---- Length data ----

FishLength <- FishingRaw %>% #Selecting data of interest
  select(Lake, Species_ID, Length, Abundance) %>% 
  na.omit() %>% 
  arrange(Lake, Species_ID) %>% 
  filter(!(Species_ID == "Centrarchidae")) %>% #Deleting Centrarchidae because not a species level
  filter(!(Species_ID == "Cyprinidae")) #Deleting Cyprinidae because not a species level
    
FishLength <- expandRows(FishLength, "Abundance") #Reshaping data frame for 1 row = 1 individual format



Fishy1 <- FishLength %>% #Summarizing number of individuals, mean length and sd for each species within each lake
  group_by(Lake, Species_ID, .add = TRUE) %>% 
  summarise(N = n(), Mean = mean(Length), sd = sd(Length))

Fishy2 <- Fishy1 %>% 
  pivot_wider(id_cols = Lake, 
              names_from = Species_ID, 
              values_from = Mean) %>% gt()
  
Fishy3 <- gt(Fishy1, rowname_col = "Species_ID",  groupname_col = "Lake")

Fishy4 <- tbl_strata(FishLength,
                     strata = Lake, 
                    .tbl_fun = tbl_summary(FishLength, 
                                           by = Species_ID,
                                           statistic = list(all_continuous()~"{mean}({sd})")))
Fishy5 <- FishLength %>% tabyl(Lake, Species_ID)
xtabs(~Species_ID + Lake, data = FishLength, )

Fish6 <- cross_mean_sd_n(
  FishLength,
  Length,
  col_vars = Species_ID,
  row_vars = Lake)


