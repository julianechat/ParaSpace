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
library(tidyr)

## Loading data ----

CombinedData <-  read.csv(paste0(to.output, "CombinedData.csv"))
SiteBiotic <-  read.csv(paste0(to.output, "Site_CommunityMetrics.csv"))
LakeBiotic <- read.csv(paste0(to.output, "Lake_CommunityMetrics.csv"))

# ---- Preparing data ----

## Community metrics ----

### Site-scale ----

Biotic.S <- SiteBiotic %>% 
  filter(Method == "Transect") #Select transect method because it is the only method needed for model analysis

Biotic.S <- Biotic.S %>% #Reshaping data frame in large format
  pivot_wider(names_from = Metric, values_from = Value) %>% 
  select(!Method) #Deleting method column

colnames(Biotic.S)[colnames(Biotic.S) == "Site"] <- "Sampling_ID" #Changing site column name
colnames(Biotic.S)[colnames(Biotic.S) == "Species richness"] <- "Species_richness" #Changing species richness column name

### Lake-scale ----

Biotic.L <- LakeBiotic %>% 
  filter(Method == "Transect") #Select transect method because it is the only method needed for model analysis

Biotic.L <- Biotic.L %>% #Reshaping data frame in large format
  pivot_wider(names_from = Metric, values_from = Value) %>% 
  select(!Method) #Deleting method column

colnames(Biotic.L)[colnames(Biotic.L) == "Species richness"] <- "SpR.L" #Changing species richness column name
colnames(Biotic.L)[colnames(Biotic.L) == "Diversity"] <- "Diversity.L" #Changing abundance column name
colnames(Biotic.L)[colnames(Biotic.L) == "Evenness"] <- "Evenness.L" #Changing abundance column name

## Combined data ----

Combined <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% #Select transect method because it is the only method needed for model analysis
  filter(!(Lake == "Beaver"| #Deleting lakes not sampled for transect method
           Lake == "Tracy"|
           Lake == "Montaubois"|
           Lake == "St-Onge"))

# ---- Building data frame ----   

mod.data <- merge(Combined, Biotic.S, by = "Sampling_ID") #Binding biotic data and combined data
mod.data <- merge(mod.data, Biotic.L, by = "Lake") 

## Infection prevalence ----

mod.data <- mod.data %>% 
  mutate(tot_fish = tot_AmRu + tot_MiDo + tot_LeGi + tot_PeFl + tot_Cyprinidae) %>% #Create total fish abundance column
  mutate(inf_fish = inf_AmRu + inf_MiDo + inf_LeGi + inf_PeFl + inf_Cyprinidae) #Create infected fish abundance column

mod.data2 <-   mod.data %>% 
  mutate(tot_fish = tot_AmRu + tot_MiDo + tot_LeGi + tot_PeFl + tot_Cyprinidae) %>% #Create total fish abundance column
  mutate(inf_fish = inf_AmRu + inf_MiDo + inf_LeGi + inf_PeFl + inf_Cyprinidae) #Create infected fish abundance column

mod.data <- mod.data %>% 
  mutate(prev_fish = inf_fish/tot_fish) %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi)#Create prevalence column

## Variable selection ----

mod.data <- mod.data %>% 
  select(Sampling_ID, Lake, Watershed,
         inf_fish, tot_fish, prev_fish, inf_LeGi, tot_LeGi, prev_LeGi, tot_Cyprinidae,
         Silt, Sand, Rock, Boulder, Macrophyte, Site_depth, Trunk,
         Temperature, Conductivity, DO, Turbidity, pH, 
         TOC, TN, TP, 
         Lake_area, Perimeter, Mean_depth, Max_depth, WRT,
         Drainage_area, Elevation, Connectivity, 
         Species_richness, Diversity, Evenness,
         SpR.L, Diversity.L, Evenness.L)

## Lake scale variables ----
  
#Lake scale mean variables
mod.data <- mod.data %>%  #Filling NA's water parameters values by lake means
  group_by(Lake) %>% 
  mutate(Temp.L = mean(Temperature)) %>% relocate("Temp.L", .after = "Temperature") %>% 
  mutate(Cond.L = mean(Conductivity)) %>% relocate("Cond.L", .after = "Conductivity") %>% 
  mutate(DO.L = mean(DO)) %>% relocate("DO.L", .after = "DO") %>% 
  mutate(Turb.L = mean(Turbidity)) %>% relocate("Turb.L", .after = "Turbidity") %>% 
  mutate(pH.L = mean(pH)) %>% relocate("pH.L", .after = "pH") %>% 
  mutate(TOC.L = mean(TOC)) %>% relocate("TOC.L", .after = "TOC") %>% 
  mutate(TN.L = mean(TN)) %>% relocate("TN.L", .after = "TN") %>% 
  mutate(TP.L = mean(TP)) %>% relocate("TP.L", .after = "TP")

#Relocate lake scale biotic data
mod.data <- mod.data %>% #Relocating columns
  relocate("SpR.L", .after = "Species_richness") %>% 
  relocate("Evenness.L", .after = "Evenness") %>% 
  relocate("Diversity.L", .after = "Diversity")

## New variables ----

mod.data <- mod.data %>%
  mutate(TN_TP = TN / TP) %>% relocate(TN_TP, .after = "TOC") %>% #Creating TN:TP ratio for transect scale
  mutate(TN_TP.L = TN.L /TP.L) %>% relocate(TN_TP.L, .after = "TOC.L") %>%  #Creating TN:TP ratio for lake scale
  mutate(Area_Perimeter = (Lake_area*1000000/Perimeter)) %>% relocate(Area_Perimeter, .before = "Mean_depth") #%>%  #Creating Area:Perimeter ratio

write.csv(mod.data, paste0(to.output, "ModelAnalysis_DataFrame.csv"), row.names = FALSE) #Saving data set
