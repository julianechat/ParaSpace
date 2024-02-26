## Script name : Data transformation

## Authors : Juliane Vigneault
## Date created : October 2, 2022

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

## Loading packages -----

library(tidyr)
library(dplyr)
library(writexl)
library(stringr)
library(tibble)
library(measurements)

## Loading data -----

Fishing_RawData <- read.csv(paste0(to.data,"Fishing_RawData.csv"), sep=";")
Transects_RawData <- read.csv(paste0(to.data, "Transects_RawData.csv"), sep=";")
Lakes_Characteristics <- read.csv(paste0(to.data, "Lakes_Characteristics.csv"), sep=";")
  
# ---- Fishing data -----

attach(Fishing_RawData)

## Long format ----

Abund_inf <- ifelse(Intensity_class > 0, Abundance, 0) #Creating new abundance column for infected fish
Abund_inf <- replace_na(Abund_inf, 0) #Replacing NA by 0's - Meaning there is no infected fish

CompleteData <- Fishing_RawData %>% 
  mutate(Abund_inf = Abund_inf) #Inserting the new column in the data frame
colnames(CompleteData)[colnames(CompleteData) == "Abundance"] <- "Abund_tot" #Changing abundance column name

#Summarize data
FishLong <- CompleteData %>% #Sum of abundances of species by fishing ID
  group_by(Fishing_ID, Species_ID, Lake, Date, Latitude, Longitude, Start_time, Gear_type, Gear_ID, Identifier) %>%
  summarise(across(.cols = Abund_inf | Abund_tot, sum)) %>%
  ungroup()

write_xlsx(FishLong, paste0(to.output, "Fishing_LongData.xlsx")) #Exporting data
write.csv(FishLong, paste0(to.output, "Fishing_LongData.csv"), row.names = FALSE)

## Wide format ----

ComMatrix <- pivot_wider(data = FishLong, names_from = "Species_ID", values_from = c("Abund_tot", "Abund_inf"), values_fill = 0) #Reshape data

ComMatrix <- ComMatrix %>% 
  select(-c(Abund_tot_NA, Abund_inf_NA)) #Deleting NA's abundance columns

ColNames <- str_remove_all(colnames(ComMatrix), "Abund_") #Changing species columns name
FishWide <- `colnames<-`(ComMatrix, ColNames)

write_xlsx(FishWide, paste0(to.output, "Fishing_WideData.xlsx")) #Exporting data
write.csv(FishWide, paste0(to.output, "Fishing_WideData.csv"), row.names = FALSE)

# ---- Transect data ----

## Summarizing data ----

#A row corresponds to the summary data of one transect

#Sum abundance data for each transect
Abund.summary <- Transects_RawData %>% 
  group_by(Transect_ID) %>% 
  summarise(across(.cols = c(LeGi_Health, LeGi_Infected, Cyp_Health, Cyp_Infected, MiDo_Health, MiDo_Infected, AmRu_Health, AmRu_Infected, PeFl_Health, PeFl_Infected), sum))

#Sum trunk abundance for each transect
Trunk.summary <- Transects_RawData %>% 
  group_by(Transect_ID) %>% 
  summarise(across(.cols = Trunk, sum))

#Mean habitat description (except Trunk) and water physico-chemistry parameters
Enviro.summary <- Transects_RawData %>% 
  group_by(Transect_ID) %>% 
  summarise(across(.cols = c(Silt, Sand, Rock, Metric_block, Macrophyte, Mean_depth, Temperature, Conductivity, DO, Turbidity, pH, TOC, TN, TP), mean))

#Binding summarized data
SummaryTrans <- merge(Abund.summary, Trunk.summary, by = "Transect_ID")
SummaryTrans <- merge(SummaryTrans, Enviro.summary, by = "Transect_ID")  

## Informative data ----

#For this part, we want only one informative row per Transect ID
#Abundance, habitat and water parameters have been previously summarize
#As Diver names and Start_time columns have varying values for a single transect. We will bind info of both divers and keep only the start value of the transect (0-10m segment)
#Position, Sampling_ID and Segment column can not be coerce at transect scale and have to be drop

InfoTrans <- Transects_RawData %>%
  select(c(Lake, Transect_ID, Date, Latitude, Longitude, Cloud_cover, Start_time))

InfoTrans <- InfoTrans %>% #Creating new data frame containing only the first row of each Transect_ID
  group_by(Transect_ID) %>%
  slice(1)

Identifier <- Transects_RawData %>% #Creating new column "Identifier" binding both divers for each Transect_ID
  group_by(Transect_ID) %>% 
  distinct(Diver) %>% 
  summarize(Diver = paste(Diver, collapse = "_")) %>% 
  select(Diver)
Identifier <- `colnames<-`(Identifier, "Identifier") #Changing column name

InfoTrans <- cbind(InfoTrans, Identifier) #Binding informative data and Info

#Binding summarise data and informative data
TransData <- merge(InfoTrans, SummaryTrans, by = "Transect_ID")

## Wide format ----

#Adjusting community matrix
TransWide <- TransData %>% #Creating total abundance columns
  mutate(tot_AmRu = AmRu_Health + AmRu_Infected, 
         tot_MiDo = MiDo_Health + MiDo_Infected, 
         tot_LeGi = LeGi_Health + LeGi_Infected, 
         tot_PeFl = PeFl_Health + PeFl_Infected, 
         tot_Cyprinidae = Cyp_Health + Cyp_Infected) %>% 
  select(-c("AmRu_Health", "MiDo_Health", "LeGi_Health", "PeFl_Health", "Cyp_Health")) #Deleting health abundance columns 

#Rename columns
colnames(TransWide)[colnames(TransWide) == "LeGi_Infected"] <- "inf_LeGi"
colnames(TransWide)[colnames(TransWide) == "Cyp_Infected"] <- "inf_Cyprinidae"
colnames(TransWide)[colnames(TransWide) == "MiDo_Infected"] <- "inf_MiDo"
colnames(TransWide)[colnames(TransWide) == "AmRu_Infected"] <- "inf_AmRu"
colnames(TransWide)[colnames(TransWide) == "PeFl_Infected"] <- "inf_PeFl"

#Adjusting columns order
TransWide <- TransWide %>% 
  relocate(c("tot_AmRu", "tot_MiDo", "tot_LeGi", "tot_PeFl", "tot_Cyprinidae"), .after = "Identifier") %>% #Relocating total abundance community matrix
  relocate("inf_PeFl", .before = "inf_Cyprinidae") %>% #Adjusting the columns order
  relocate("inf_MiDo", .before = "inf_LeGi") %>%
  relocate("inf_AmRu", .after = "tot_Cyprinidae")

write_xlsx(TransWide, paste0(to.output,"Transects_WideData.xlsx")) #Exporting data frame
write.csv(TransWide, paste0(to.output, "Transects_WideData.csv"), row.names = FALSE)

# ---- Combined data ----

## Transect data preparation ----

attach(TransWide)

#Adjusting date format
TransWide$Date <- format(as.Date(TransWide$Date, format = "%d/%m/%Y"), "%Y-%m-%d")

#Creating a new Sampling_ID column containing Transect_ID info + Method info
Sampling_ID <- str_replace(TransWide$Transect_ID, "1", "_T_01")
Sampling_ID <- str_replace(Sampling_ID, "2", "_T_02")
Sampling_ID <- str_replace(Sampling_ID, "3", "_T_03")
Sampling_ID <- str_replace(Sampling_ID, "4", "_T_04")
Sampling_ID <- str_replace(Sampling_ID, "5", "_T_05")
Sampling_ID <- str_replace(Sampling_ID, "6", "_T_06")

#Changing Transect_ID column by Sampling_ID column
TransWide$Transect_ID <- Sampling_ID
colnames(TransWide)[colnames(TransWide) == "Transect_ID"] <- "Sampling_ID"

TransWide <- TransWide %>% 
  relocate("Sampling_ID", .before = "Lake") #Relocating Sampling ID column

#Creating Sampling method column
TransPrep <- TransWide %>% 
  add_column(Sampling_method = "Transect") %>%
  relocate("Sampling_method", .after = "Start_time") 

#Creating abundance columns for missing fish species
TransPrep <- TransPrep %>% #Total abundance
  add_column(tot_FuDi = 0) %>% relocate("tot_FuDi", .after = "tot_AmRu") %>%
  add_column(tot_Centrarchidae = 0) %>% relocate("tot_Centrarchidae", .after = "tot_MiDo") %>%
  add_column(tot_PiPr = 0) %>% relocate("tot_PiPr", .after = "tot_PeFl") %>%
  add_column(tot_ChrosomusSpp. = 0) %>% relocate("tot_ChrosomusSpp.", .after = "tot_PiPr") %>%
  add_column(tot_PiNo = 0) %>% relocate("tot_PiNo", .after = "tot_ChrosomusSpp.") %>%
  add_column(tot_SeAt = 0) %>% relocate("tot_SeAt", .after = "tot_Cyprinidae") %>%
  add_column(tot_LuCo = 0) %>% relocate("tot_LuCo", .after = "tot_SeAt") %>%
  add_column(tot_AmNe = 0) %>% relocate("tot_AmNe", .after = "tot_LuCo") %>%
  add_column(tot_CaCo = 0) %>% relocate("tot_CaCo", .after = "tot_AmNe") %>%
  add_column(tot_EsMa = 0) %>% relocate("tot_EsMa", .after = "tot_CaCo") %>%
  add_column(tot_UmLi = 0) %>% relocate("tot_UmLi", .after = "tot_EsMa") %>%
  add_column(tot_RhAt = 0) %>% relocate("tot_RhAt", .after = "tot_UmLi")

TransPrep <- TransPrep %>% #Infected abundance
  add_column(inf_FuDi = 0) %>% relocate("inf_FuDi", .after = "inf_AmRu") %>%
  add_column(inf_Centrarchidae = 0) %>% relocate("inf_Centrarchidae", .after = "inf_MiDo") %>%
  add_column(inf_PiPr = 0) %>% relocate("inf_PiPr", .after = "inf_PeFl") %>%
  add_column(inf_ChrosomusSpp. = 0) %>% relocate("inf_ChrosomusSpp.", .after = "inf_PiPr") %>%
  add_column(inf_PiNo = 0) %>% relocate("inf_PiNo", .after = "inf_ChrosomusSpp.") %>%
  add_column(inf_SeAt = 0) %>% relocate("inf_SeAt", .after = "inf_Cyprinidae") %>%
  add_column(inf_LuCo = 0) %>% relocate("inf_LuCo", .after = "inf_SeAt") %>%
  add_column(inf_AmNe = 0) %>% relocate("inf_AmNe", .after = "inf_LuCo") %>%
  add_column(inf_CaCo = 0) %>% relocate("inf_CaCo", .after = "inf_AmNe") %>%
  add_column(inf_EsMa = 0) %>% relocate("inf_EsMa", .after = "inf_CaCo") %>%
  add_column(inf_UmLi = 0) %>% relocate("inf_UmLi", .after = "inf_EsMa") %>%
  add_column(inf_RhAt = 0) %>% relocate("inf_RhAt", .after = "inf_UmLi")

TransPrep[c(7,8,33:35,43:46), c(10:43)] <- NA #Replacing false 0 abundance created for lake that were not sampled with transect method

#Adding a Gear ID column
TransPrep <- TransPrep %>%
  add_column(Gear_ID = NA) %>% relocate("Gear_ID", .after = "Sampling_method")

#Adjusting column names
colnames(TransPrep)[colnames(TransPrep) == "Start_time"] <- "Sampling_time"
colnames(TransPrep)[colnames(TransPrep) == "Metric_block"] <- "Boulder"
colnames(TransPrep)[colnames(TransPrep) == "Mean_depth"] <- "Site_depth"
colnames(TransPrep)[colnames(TransPrep) == "Latitude"] <- "Site_latitude"
colnames(TransPrep)[colnames(TransPrep) == "Longitude"] <- "Site_longitude"

## Fishing data preparation ----

attach(FishWide)

#Adjusting column names
colnames(FishWide)[colnames(FishWide) == "Fishing_ID"] <- "Sampling_ID"
colnames(FishWide)[colnames(FishWide) == "Gear_type"] <- "Sampling_method"
colnames(FishWide)[colnames(FishWide) == "Start_time"] <- "Sampling_time"
colnames(FishWide)[colnames(FishWide) == "tot_Chrosomus sp."] <- "tot_ChrosomusSpp."
colnames(FishWide)[colnames(FishWide) == "inf_Chrosomus sp."] <- "inf_ChrosomusSpp."
colnames(FishWide)[colnames(FishWide) == "Latitude"] <- "Site_latitude"
colnames(FishWide)[colnames(FishWide) == "Longitude"] <- "Site_longitude"

#Creating empty columns for habitat and physico-chemistry variables
FishPrep <- FishWide %>% 
  add_column(Trunk = NA) %>%
  add_column(Silt = NA) %>%
  add_column(Sand = NA) %>% 
  add_column(Rock = NA) %>% 
  add_column(Boulder = NA) %>% 
  add_column(Macrophyte = NA) %>% 
  add_column(Site_depth = NA) %>% 
  add_column(Temperature = NA) %>% 
  add_column(Conductivity = NA) %>% 
  add_column(DO = NA) %>% 
  add_column(Turbidity = NA) %>% 
  add_column(pH = NA) %>% 
  add_column(TOC = NA) %>% 
  add_column(TN = NA) %>%
  add_column(TP = NA)

#Adding cloud cover column 
FishPrep <- FishPrep %>% 
  add_column(Cloud_cover = NA) %>% relocate("Cloud_cover", .after = "Site_longitude")

## Binding data sets ----

CombinedData <- rbind(TransPrep, FishPrep)

#Adding geogrpahical and morphological data
CombinedData <- merge(CombinedData, Lakes_Characteristics, by = "Lake") #Merging lake characteristic data to field data

colnames(CombinedData)[colnames(CombinedData) == "Latitude"] <- "Lake_latitude"
colnames(CombinedData)[colnames(CombinedData) == "Longitude"] <- "Lake_longitude"

### Coordinates conversion ----

#Latitude
CombinedData$Site_latitude <- str_replace(CombinedData$Site_latitude, "°", " ")
CombinedData$Site_latitude <- str_remove(CombinedData$Site_latitude, "'")
CombinedData$Site_latitude <- conv_unit(CombinedData$Site_latitude, from = "deg_dec_min", to = "dec_deg")
CombinedData$Site_latitude <- as.numeric(CombinedData$Site_latitude)

CombinedData$Lake_latitude <- str_replace(CombinedData$Lake_latitude, "°", " ")
CombinedData$Lake_latitude <- str_replace(CombinedData$Lake_latitude, "'", " ")
CombinedData$Lake_latitude <- str_remove(CombinedData$Lake_latitude, " N")
CombinedData$Lake_latitude <- str_remove(CombinedData$Lake_latitude, '"')
CombinedData$Lake_latitude <- conv_unit(CombinedData$Lake_latitude, from = "deg_min_sec", to = "dec_deg") #Coordinates conversion
CombinedData$Lake_latitude <- as.numeric(CombinedData$Lake_latitude)

#Longitude
CombinedData$Site_longitude <- str_replace(CombinedData$Site_longitude, "°", " ")
CombinedData$Site_longitude <- str_remove(CombinedData$Site_longitude, "'")
CombinedData$Site_longitude <- conv_unit(CombinedData$Site_longitude, from = "deg_dec_min", to = "dec_deg")
CombinedData$Site_longitude <- as.numeric(CombinedData$Site_longitude)*(-1) #Add negative sign as coordinates are from western hemisphere

CombinedData$Lake_longitude <- str_replace(CombinedData$Lake_longitude, "°", " ")
CombinedData$Lake_longitude <- str_replace(CombinedData$Lake_longitude, "'", " ")
CombinedData$Lake_longitude <- str_remove(CombinedData$Lake_longitude, " W")
CombinedData$Lake_longitude <- str_remove(CombinedData$Lake_longitude, '"')
CombinedData$Lake_longitude <- conv_unit(CombinedData$Lake_longitude, from = "deg_min_sec", to = "dec_deg") #Coordinates conversion
CombinedData$Lake_longitude <- as.numeric(CombinedData$Lake_longitude)*(-1)

#Creating new variable of interest

write_xlsx(CombinedData, paste0(to.output, "CombinedData.xlsx")) #Exporting data frame
write.csv(CombinedData, paste0(to.output, "CombinedData.csv"), row.names = FALSE)
