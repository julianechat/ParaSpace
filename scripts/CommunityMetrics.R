## Script name : Community metrics

## Authors : Juliane Vigneault
## Date created : November 11, 2022

## Copyright (c) Juliane Vigneault, 2022
## Email: juliane.vigneault@umontreal.ca

# ---- Script setup ----

## R Setup ----- 

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"
to.carto <- "./carto/"
to.rédaction <- "./rédaction/"

## Loading packages ----

library(dplyr)
library(vegan)
library(tidyr)

## Loading data ----

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))

# ---- Site scale ----

site.data <- CombinedData %>% 
  select(Sampling_method, Sampling_ID, starts_with("tot")) %>% #Select data
  na.omit() #Delete lines with no abundance data

## Minnow trap ----

MT.site.data <- site.data %>% 
  filter(Sampling_method == "Minnow_trap") %>% #Select minnow trap method
  select(-c(tot_Centrarchidae, tot_Cyprinidae)) #Drop Centrarchidae and Cyprinidae columns as they would lead to bias metrics

### Species richness ----

MT.site.SR <- MT.site.data  %>% 
  select(starts_with("tot")) %>% #Select community matrix
  specnumber() #Calculate species richness

MT.site.SR <- data.frame(Method = "Minnow_trap", #Framing results
                         Metric = "Species richness", 
                         Site = MT.site.data$Sampling_ID ,
                         Value = MT.site.SR)

### Simpson diversity ----

MT.site.D <- MT.site.data %>% 
  select(starts_with("tot")) %>% #Select community matrix
  diversity(index = "simpson") #Calculate Simpson diversity

MT.site.D <- data.frame(Method = "Minnow_trap", #Framing results
                        Metric = "Diversity", 
                        Site = MT.site.data$Sampling_ID ,
                        Value = MT.site.D)
### Evenness ----

MT.site.E <- MT.site.data %>% 
  select(starts_with("tot")) %>% #Select community matrix
  diversity() #Calculate Shannon diversity

MT.site.E <- MT.site.E/log(MT.site.SR$Value) #Calculate Evenness

MT.site.E <- data.frame(Method = "Minnow_trap", #Framing results
                        Metric = "Evenness", 
                        Site = MT.site.data$Sampling_ID ,
                        Value = MT.site.E)
## Seine net ----

S.site.data <- site.data %>%
  filter(Sampling_method == "Seine") %>% #Select seine method
  select(-c(tot_Centrarchidae, tot_Cyprinidae)) #Drop Centrarchidae and Cyprinidae columns as they would lead to bias metrics

### Species richness ----

S.site.SR <- S.site.data %>% 
  select(starts_with("tot")) %>% #Select community matrix
  specnumber() #Calculate species richness

S.site.SR <- data.frame(Method = "Seine", #Framing results
                        Metric = "Species richness", 
                        Site = S.site.data$Sampling_ID,
                        Value = S.site.SR)

### Simpson diversity ----

S.site.D <- S.site.data %>% 
  select(starts_with("tot")) %>% #Select community matrix
  diversity(index = "simpson") #Calculate Simpson diversity

S.site.D <- data.frame(Method = "Seine", #Framing results
                       Metric = "Diversity", 
                       Site = S.site.data$Sampling_ID,
                       Value = S.site.D)

### Evenness ----

S.site.E <- S.site.data %>% 
  select(starts_with("tot")) %>% #Select community matrix
  diversity() #Calculate Shannon diversity

S.site.E <- S.site.E/log(S.site.SR$Value) #Calculate evenness

S.site.E <- data.frame(Method = "Seine", #Framing results
                       Metric = "Evenness", 
                       Site = S.site.data$Sampling_ID,
                       Value = S.site.E)

## Transect ----

T.site.data <- site.data %>%
  filter(Sampling_method == "Transect") #Select transect method

### Species richness ----

T.site.SR <- T.site.data %>% 
  select(starts_with("tot")) %>% #Select community matrix
  specnumber() #Calculate species richness

T.site.SR <- data.frame(Method = "Transect", #Framing results
                        Metric = "Species richness", 
                        Site = T.site.data$Sampling_ID,
                        Value = T.site.SR)

### Simpson diversity ----

T.site.D <- T.site.data %>% 
  select(starts_with("tot")) %>% #Select community matrix
  diversity(index = "simpson") #Calculate Simpson diversity

T.site.D <- data.frame(Method = "Transect", #Framing results
                       Metric = "Diversity", 
                       Site = T.site.data$Sampling_ID,
                       Value = T.site.D)

### Evenness ----

T.site.E <- T.site.data %>% 
  select(starts_with("tot")) %>% #Select community matrix
  diversity() #Calculate Shannon diversity

T.site.E <- T.site.E/log(T.site.SR$Value) #Calculate evenness

T.site.E <- data.frame(Method = "Transect", #Framing results
                       Metric = "Evenness", 
                       Site = T.site.data$Sampling_ID,
                       Value = T.site.E)

## Summary data frame ----

#Binding results of every metrics and methods
site.sum.df <- rbind(MT.site.SR, MT.site.D, MT.site.E, 
                     S.site.SR, S.site.D, S.site.E,
                     T.site.SR, T.site.D, T.site.E)

write.csv(site.sum.df, paste0(to.output, "Site_CommunityMetrics.csv"), row.names = FALSE) #Exporting data frame

# ---- Lake scale ----

lake.data <- CombinedData %>% 
  select(Lake, Sampling_method, starts_with("tot")) %>% #Select data
  na.omit() #Delete lines with no abundance data
  
lake.data <- lake.data %>% #Summarize by sampling method and lake
  group_by(Lake, Sampling_method) %>% 
  summarise(across(.cols = everything(), sum))

Lakes <- c("Achigan", "Beaver","Coeur", "Cornu", "Corriveau", "Croche", "Cromwell", "Echo", "Fournelle", "Montaubois", "Morency", "Pin_rouge", "St-Onge", "Tracy", "Triton") #Sampled lakes for minnow traps and seines
Lakes2 <- c("Achigan","Coeur", "Cornu", "Corriveau", "Croche", "Cromwell", "Echo", "Fournelle", "Morency", "Pin_rouge", "Triton") #Sampled lakes for transects

## Minnow trap ----

MT.lake.data <- lake.data %>%  #Select minnow trap method data and summarize abundance data by lake
  filter(Sampling_method == "Minnow_trap") %>% 
  select(Lake, starts_with("tot")) %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

MT.lake.matrix <- MT.lake.data %>% 
  select(-c(tot_Centrarchidae, tot_Cyprinidae)) #Deleting Centrarchidae & Cyprinidae columns as they would lead to overestimation of the number of species

### Species richness ----

MT.lake.SR <- MT.lake.matrix %>% 
  select(starts_with("tot")) %>% #Select community matrix
  specnumber() #Calculate species richness

MT.lake.SR <- data.frame(Method = "Minnow_trap", #Framing results
                        Metric = "Species richness", 
                        Lake = Lakes,
                        Value = MT.lake.SR)

### Simpson diversity ----

MT.lake.D <- MT.lake.matrix %>% 
  select(starts_with("tot")) %>% #Select community matrix
  diversity(index = "simpson") #Calculate Simpson diversity

MT.lake.D <- data.frame(Method = "Minnow_trap", #Framing results
                       Metric = "Diversity", 
                       Lake = Lakes,
                       Value = MT.lake.D)
### Evenness ----

MT.lake.E <- MT.lake.matrix %>% 
  select(starts_with("tot")) %>% #Select community matrix
  diversity() #Calculate Shannon diversity

MT.lake.E <- MT.lake.E/log(MT.lake.SR$Value) #Calculate evenness

MT.lake.E <- data.frame(Method = "Minnow_trap", #Framing results
                       Metric = "Evenness", 
                       Lake = Lakes,
                       Value = MT.lake.E)
## Seine net ----

S.lake.data <- lake.data %>% #Select seine method data and summarize abundance data by lake
  filter(Sampling_method == "Seine") %>% 
  select(Lake, starts_with("tot")) %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

S.lake.matrix <- S.lake.data %>% 
  select(-c(tot_Centrarchidae, tot_Cyprinidae)) #Deleting Centrarchidae & Cyprinidae columns as they would lead to overestimation of the number of species

### Species richness ----

S.lake.SR <- S.lake.matrix %>% 
  select(starts_with("tot")) %>% #Select community matrix
  specnumber() #Calculate species richness

S.lake.SR <- data.frame(Method = "Seine", #Framing results
                       Metric = "Species richness", 
                       Lake = Lakes,
                       Value = S.lake.SR)

### Simpson diversity ----

S.lake.D <- S.lake.matrix %>% 
  select(starts_with("tot")) %>% #Select community matrix
  diversity(index = "simpson") #Calculate Simpson diversity

S.lake.D <- data.frame(Method = "Seine", #Framing results
                       Metric = "Diversity", 
                       Lake = Lakes,
                       Value = S.lake.D)

### Evenness ----

S.lake.E <- S.lake.matrix %>% 
  select(starts_with("tot")) %>% #Select community matrix
  diversity() #Calculate Shannon diversity

S.lake.E <- S.lake.E/log(S.lake.SR$Value) #Calculate Evenness

S.lake.E <- data.frame(Method = "Seine", #Framing results
                       Metric = "Evenness", 
                       Lake = Lakes,
                       Value = S.lake.E)
## Transect ----

T.lake.data <- lake.data %>% #Select transect method data and summarize abundance data by lake
  filter(Sampling_method == "Transect") %>% 
  select(Lake, starts_with("tot")) %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

T.lake.matrix <- T.lake.data %>% 
  select(-c(tot_Centrarchidae)) #Deleting Centrarchidae column as it would lead to overestimation of the number of species

### Species richness ----

T.lake.SR <- T.lake.matrix %>% 
  group_by(Lake) %>% #Select community matrix
  specnumber() #Calculate species richness

T.lake.SR <- data.frame(Method = "Transect", #Framing results
                       Metric = "Species richness", 
                       Lake = Lakes2,
                       Value = T.lake.SR)

### Simpson diversity ----

T.lake.D <- T.lake.matrix %>% 
  select(starts_with("tot")) %>% #Select community matrix
  diversity(index = "simpson") #Calculate Simpson diversity

T.lake.D <- data.frame(Method = "Transect", #Framing results
                       Metric = "Diversity", 
                       Lake = Lakes2,
                       Value = T.lake.D)

### Evenness ----

T.lake.E <- T.lake.matrix %>% 
  select(starts_with("tot")) %>% #Select community matrix
  diversity() #Calculate Shannon diversity

T.lake.E <- T.lake.E/log(T.lake.SR$Value) #Calculate evenness

T.lake.E <- data.frame(Method = "Transect", #Framing results
                       Metric = "Evenness", 
                       Lake = Lakes2,
                       Value = T.lake.E)

## Summary data frame ----

#Binding results of every metrics and methods
lake.sum.df <- rbind(MT.lake.SR, MT.lake.D, MT.lake.E, 
                     S.lake.SR, S.lake.D, S.lake.E,
                     T.lake.SR, T.lake.D, T.lake.E)

write.csv(lake.sum.df, paste0(to.output, "Lake_CommunityMetrics.csv"), row.names = FALSE) #Exporting data frame
