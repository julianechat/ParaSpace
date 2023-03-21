## Biotic data transformation ##

# ----- R Setup ----- #
to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"

# ----- Loading packages ----- #

library(dplyr)
library(vegan)
library(writexl)

# ----- Loading data ----- #

Fishing.data <- read.csv(paste0(to.output, "Fishing_WideData.csv"))
TransectData <- read.csv(paste0(to.output, "Transects_WideData.csv"))
CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))

# ----------------------------- #

#### ----- lake scale biotic data ----- ####
lake.data <- na.omit(Fishing.data) #Deleting sampling with no capture

lake.data <- lake.data %>% 
  select(c(2, 10:26)) %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

## Centrarchids ##
lake.centrarchids <- lake.data %>% #Abundance of Centrarchids species (pumkinseeds excluded)
  select("tot_AmRu", "tot_MiDo", "tot_LeGi") %>% 
  rowSums()

## Species richness ##
lake.matrix <- lake.data[-c(5,11)] #Deleting Centrarchidae & Cyprinidae columns as they lead to overestimation of the number of species

lake.species_richness <- lake.matrix %>% 
  select(starts_with("tot")) %>% 
  specnumber()

## Simpson diversity ##
lake.diversity <- lake.matrix %>% 
  select(starts_with("tot")) %>% 
  diversity(index = "simpson")

## Evenness ## 
lake.diversity2 <- lake.matrix %>% 
  select(starts_with("tot")) %>% 
  diversity()

lake.evenness <- lake.diversity2/log(lake.species_richness)

## Creating data frame ##
LakesName <- c("Achigan", "Beaver","Coeur", "Cornu", "Corriveau", "Croche", "Cromwell", "Echo", "Fournelle", "Montaubois", "Morency", "Pin_rouge", "St-Onge", "Tracy", "Triton")
lake.biotic <- data.frame(Lake = LakesName, Centrarchids = lake.centrarchids, Species_richness = lake.species_richness, Diversity = lake.diversity) 

write.csv(lake.biotic, paste0(to.output, "Lake_BioticData.csv"), row.names = FALSE) #Exporting data frame

#### ----- Transect scale biotic data ----- ####
trans.data <- TransectData %>% slice(-c(7,8, 33:35, 43:46)) #Deleting sampling with no transect
trans.data <- trans.data %>% select(c(2, 9:13))

## Centrarchids ##
trans.centrarchids <- trans.data %>% #Abundance of Centrarchids species (pumkinseeds excluded)
  select("tot_AmRu", "tot_MiDo", "tot_LeGi") %>% 
  rowSums()

## Species richness ##
trans.species_richness <- trans.data %>% 
  select(starts_with("tot")) %>% 
  specnumber()

## Simpson diversity ##
trans.diversity <- trans.data %>% 
  select(starts_with("tot")) %>% 
  diversity(index = "simpson")

## Evenness ## 
trans.diversity2 <- trans.data %>% 
  select(starts_with("tot")) %>% 
  diversity()

trans.evenness <- trans.diversity2/log(trans.species_richness)

## Creating data frame ##
TransectsName <- trans.data$Transect_ID
trans.biotic <- data.frame(Transect_ID = TransectsName, Centrarchids = trans.centrarchids, Species_richness = trans.species_richness, Diversity = trans.diversity) 

write.csv(trans.biotic, paste0(to.output, "Trans_BioticData.csv"), row.names = FALSE) #Exporting data frame

#### ----- lake scale with transect biotic data ----- ####
lake.trans.data <- na.omit(TransectData) #Deleting sampling with no capture

lake.trans.data <- lake.trans.data %>% 
  select(c(1, 9:13)) %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

## Centrarchids ##
lake.trans.centrarchids <- lake.trans.data %>% #Abundance of Centrarchids species (pumkinseeds excluded)
  select("tot_AmRu", "tot_MiDo", "tot_LeGi") %>% 
  rowSums()

## Species richness ##
lake.trans.species_richness <- lake.trans.data %>% 
  select(starts_with("tot")) %>% 
  specnumber()

## Simpson diversity ##
lake.trans.diversity <- lake.trans.data %>% 
  select(starts_with("tot")) %>% 
  diversity(index = "simpson")

## Evenness ## 
lake.trans.diversity2 <- lake.trans.data %>% 
  select(starts_with("tot")) %>% 
  diversity()

lake.trans.evenness <- lake.diversity2/log(lake.species_richness)

##Faire data frame####
lake.trans.biotic <- data.frame(Lake = lake.trans.data$Lake, Centrarchids = lake.trans.centrarchids, Species_richness = lake.trans.species_richness, Diversity = lake.trans.diversity) 

write.csv(lake.trans.biotic, paste0(to.output, "Lake_Trans_BioticData.csv"), row.names = FALSE) #Exporting data frame
