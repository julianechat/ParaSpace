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
to.rédaction <- "./rédaction/"

## Loading packages & functions ----

library(dplyr)

## Loading data ----

TransectData <- read.csv(paste0(to.output, "Transects_WideData.csv"))
LakesCharacteristics <- read.csv(paste0(to.data, "Lakes_Characteristics.csv"), sep=";")
TransBiotic <- read.csv(paste0(to.output, "Trans_BioticData.csv"))
LakeBiotic <- read.csv(paste0(to.output, "Lake_Trans_BioticData.csv"))

# ---- Building data frame ----

trans.data <- merge(TransectData, LakesCharacteristics, by = "Lake") #Binding lake charactristics data
trans.data <- trans.data %>% 
  rename_at("Latitude.x", ~"Latitude.site") %>% 
  rename_at("Longitude.x", ~"Longitude.site") %>% 
  rename_at("Latitude.y", ~"Latitude.lake") %>% 
  rename_at("Longitude.y", ~"Longitude.lake") %>% 
  rename_at("Mean_depth.x", ~"MeanDepth.site") %>% 
  rename_at("Mean_depth.y", ~"MeanDepth.lake")

trans.data <- merge(trans.data, TransBiotic, by = "Transect_ID") #Binding biotic data

## Infection prevalence ----

trans.data <- trans.data %>% 
  mutate(tot_fish = tot_AmRu + tot_MiDo + tot_LeGi + tot_PeFl + tot_Cyprinidae)

trans.data <- trans.data %>% 
  mutate(inf_fish = inf_AmRu + inf_MiDo + inf_LeGi + inf_PeFl + inf_Cyprinidae)

trans.data <- trans.data %>% 
  mutate(prev_fish = inf_fish/tot_fish) #Creating prevalence column

## Variable selection ----

trans.mod <- trans.data %>% 
  select(Transect_ID, Lake, Watershed,
         inf_fish, tot_fish, prev_fish, tot_Cyprinidae,
         Silt, Sand, Rock, Metric_block, Macrophyte, MeanDepth.site, Trunk,
         Temperature, Conductivity, DO, Turbidity, pH, 
         TOC, TN, TP, 
         Lake_area, Perimeter, MeanDepth.lake, Max_depth, WRT,
         Drainage_area, Elevation, Connectivity, 
         Centrarchids, Species_richness, Diversity)

colnames(trans.mod)[c(15:22, 31:33)] <- c("Temp.T", "Cond.T", "DO.T", "Turb.T", "pH.T", #Adjusting column names
                                   "TOC.T", "TN.T", "TP.T",
                                   "Centrarchids.T", "Species_richness.T", "Diversity.T")

## Creating new variables ----
  
#lake scale mean variables
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
colnames(trans.mod)[c(42:44)] <- c("Centrarchids.L", "Species_richness.L", "Diversity.L") #Adjusting column names

trans.mod <- trans.mod %>% #Relocating columns
  relocate("Centrarchids.L", .after = "Centrarchids.T") %>% 
  relocate("Species_richness.L", .after = "Species_richness.T") %>% 
  relocate("Diversity.L", .after = "Diversity.T")

trans.mod <- trans.mod %>% 
  mutate(TN_TP.T = TN.T / TP.T) %>% relocate(TN_TP.T, .after = "TOC.T") %>% #Creating TN:TP ratio for transect scale
  mutate(TN_TP.L = TN.L /TP.L) %>% relocate(TN_TP.L, .after = "TOC.L") %>%  #Creating TN:TP ratio for lake scale
  mutate(Area_Perimeter = (Lake_area*1000000/Perimeter)) %>% relocate(Area_Perimeter, .before = "MeanDepth.lake") #%>%  #Creating Area:Perimeter ratio

trans.mod$Lake <- as.factor(trans.mod$Lake)
trans.mod$Transect_ID <- as.factor(trans.mod$Transect_ID)
trans.mod$Watershed <- as.factor(trans.mod$Watershed)

write.csv(trans.mod, paste0(to.output, "Transects_Lake_Data.csv"), row.names = FALSE) #Saving data set
