## Script name : Model analysis data frame building

## Authors : Juliane Vigneault
## Date created : March 17, 2023

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
library(lme4)

## Loading data ----

TransectData <- read.csv(paste0(to.output, "Transects_WideData.csv"))
LakesCaracteristics <- read.csv(paste0(to.data, "Lakes_Caracteristics.csv"), sep=";")
TransBiotic <- read.csv(paste0(to.output, "Trans_BioticData.csv"))
LakeBiotic <- read.csv(paste0(to.output, "Lake_Trans_BioticData.csv"))

# ---- Building data frame ----

trans.data <- merge(TransectData, LakesCaracteristics, by.x = "Lake") #Binding lake caractristics data
trans.data <- merge(trans.data, TransBiotic, by.x = "Transect_ID") #Binding biotic data

## Infection prevalence ----

trans.data$tot_fish <- trans.data %>% 
  select(starts_with("tot")) %>% #Selecting total abundances
  rowSums()

trans.data$inf_fish <- trans.data %>% 
  select(starts_with("inf")) %>% #Selecting infected abundances
  rowSums()

trans.data <- trans.data %>% 
  mutate(prev_fish = inf_fish/tot_fish) #Creating prevalence column

## Variable selection ----

trans.mod <- trans.data %>% #Keeping relevant variables
  select(Transect_ID, Lake, Watershed,
         inf_fish, tot_fish, prev_fish,
         Silt, Sand, Rock, Block, Macrophyte, Depth, Trunk,
         Temp, Cond, DO, Turb, pH, 
         TOC, TN, TP, 
         Lake_area, Perimeter, Mean_depth, Max_depth, WRT,
         Drainage_area, Elevation, Connectivity, 
         Centrarchids, Species_richness, Diversity)

colnames(trans.mod)[c(14:21, 30:32)] <- c("Temp.T", "Cond.T", "DO.T", "Turb.T", "pH.T", #Adjusting column names
                                   "TOC.T", "TN.T", "TP.T",
                                   "Centrarchids.T", "Species_richness.T", "Diversity.T")

## Creating lake scale mean variables ----

trans.mod <- trans.mod %>% #Filling NA's water parameters values by lake means
  group_by(Lake) %>% 
  mutate(Temp.L = mean(Temp.T)) %>% relocate("Temp.L", .after = "Temp.T") %>% 
  mutate(Cond.L = mean(Cond.T)) %>% relocate("Cond.L", .after = "Cond.T") %>% 
  mutate(DO.L = mean(DO.T)) %>% relocate("DO.L", .after = "DO.T") %>% 
  mutate(Turb.L = mean(Turb.T)) %>% relocate("Turb.L", .after = "Turb.T") %>% 
  mutate(pH.L = mean(pH.T)) %>% relocate("pH.L", .after = "pH.T") %>% 
  mutate(TOC.L = mean(TOC.T)) %>% relocate("TOC.L", .after = "TOC.T") %>% 
  mutate(TN.L = mean(TN.T)) %>% relocate("TN.L", .after = "TN.T") %>% 
  mutate(TP.L = mean(TP.T)) %>% relocate("TP.L", .after = "TP.T")

trans.mod <- merge(trans.mod, LakeBiotic, by.x = "Lake") #Binding biotic lake data
colnames(trans.mod)[c(41:43)] <- c("Centrarchids.L", "Species_richness.L", "Diversity.L") #Adjusting column names

trans.mod <- trans.mod %>% #Relocating columns
  relocate("Centrarchids.L", .after = "Centrarchids.T") %>% 
  relocate("Species_richness.L", .after = "Species_richness.T") %>% 
  relocate("Diversity.L", .after = "Diversity.T")

write.csv(trans.mod, paste0(to.output, "Transects_Lake_Data.csv"), row.names = FALSE) #Saving data set

# ---- Modelling test ----

test.trans <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TN.T + TOC.T + (1|Lake), family = binomial, data = trans.mod)
summary(test.trans)

test.lake <- glm(cbind(inf_fish, tot_fish - inf_fish) ~ TN.L + TOC.L + (1|Lake), family = binomial, data = trans.mod)
summary(test.lake)

