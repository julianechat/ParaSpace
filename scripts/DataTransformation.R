## Script name : Data transformation

## Authors : Juliane Vigneault
## Date created : October 11, 2022

## Copyright (c) Juliane Vigneault, 2022
## Email: juliane.vigneault@umontreal.ca

# ---- Script setup ----
## R Setup ----- 

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"

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
BioticData <- read.csv(paste0(to.output, "Lake_BioticData.csv"))
  
# ---- Fishing data -----

attach(Fishing_RawData)

## Long format ----

Abund_inf <- ifelse(Intensity_class > 0, Abundance, 0) #Creating new abundance column for infected fish
Abund_inf <- replace_na(Abund_inf, 0) #Replacing NA by 0's - Meaning there is no infected fish

CompleteData <- mutate(Fishing_RawData, Abund_inf) #Inserting the new column in the data frame
names(CompleteData)[names(CompleteData) == 'Abundance'] <- 'Abund_tot'

#Summarize data
FishLong <- CompleteData %>% #Sum of abundances of species by fishing ID
  group_by(Fishing_ID, Species_ID, Lake, Date, Latitude, Longitude, Start_time, Gear_type, Gear_ID, Identifier) %>%
  summarise(across(.cols = Abund_inf | Abund_tot, sum)) %>%
  ungroup()

write_xlsx(FishLong, paste0(to.output, "Fishing_LongData.xlsx")) #Exporting data
write.csv(FishLong, paste0(to.output, "Fishing_LongData.csv"), row.names = FALSE)

## Wide format ----

ComMatrix <- pivot_wider(data = FishLong, names_from = "Species_ID", values_from = c("Abund_tot", "Abund_inf"), values_fill = 0) #Reshape data
ComMatrix <- ComMatrix[,-c(10,28)] #Deleting NA's abundance columns

ColNames <- str_remove_all(colnames(ComMatrix), "Abund_") #Changing species columns name
FishWide <- `colnames<-`(ComMatrix, ColNames)

write_xlsx(FishWide, paste0(to.output, "Fishing_WideData.xlsx")) #Exporting data
write.csv(FishWide, paste0(to.output, "Fishing_WideData.csv"), row.names = FALSE)

# ---- Transect data ----

## Summarizing data ----

#A row corresponds to the summary data of one transect
Abund.summary <- Transects_RawData %>% 
  group_by(Transect_ID) %>% 
  summarise(across(.cols = 11:20, sum))

Trunk.summary <- Transects_RawData %>% 
  group_by(Transect_ID) %>% 
  summarise(across(.cols = 27, sum))

Enviro.summary <- Transects_RawData %>% 
  group_by(Transect_ID) %>% 
  summarise(across(.cols = c(21:26, 28:35), mean))

SummaryTrans <- merge(Abund.summary, Trunk.summary, by = "Transect_ID")
SummaryTrans <- merge(SummaryTrans, Enviro.summary, by = "Transect_ID")  

## Informative data ----

#For this part, we want only one informative row per Transect ID
#Abundance, habitat and water parameters have been previously summarize
#Diver and Start_time columns values change through the transect. We will need to bind info of both divers and keep the real start value of the transect (0-10m segment)
#Position, Sampling_ID, Segment can not be coerce at transect scale

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

Identifier <- `colnames<-`(Identifier, "Identifier") 

InfoTrans <- cbind(InfoTrans, Identifier)

#Binding summarise data and informative data
TransData <- merge(InfoTrans, SummaryTrans, by = "Transect_ID")

## Wide format ----

#Adjusting community matrix
TransWide <- TransData %>% #Creating total abundance columns
  mutate(tot_AmRu = AmRu_Health + AmRu_Infected, tot_MiDo = MiDo_Health + MiDo_Infected, tot_LeGi = LeGi_Health + LeGi_Infected, tot_PeFl = PeFl_Health + PeFl_Infected, tot_Cyprinidae = Cyp_Health + Cyp_Infected) %>% 
  select(-c("AmRu_Health", "MiDo_Health", "LeGi_Health", "PeFl_Health", "Cyp_Health")) #Deleting health abundance columns 

colnames(TransWide)[c(9:13)] <- c("inf_LeGi", "inf_Cyprinidae", "inf_MiDo", "inf_AmRu","inf_PeFl") 

TransWide <- TransWide %>% relocate(c("tot_AmRu", "tot_MiDo", "tot_LeGi", "tot_PeFl", "tot_Cyprinidae"), .after = "Identifier") %>% #Relocating total abundance community matrix
  relocate("inf_PeFl", .before = "inf_Cyprinidae") %>% #Adjusting the column order
  relocate("inf_MiDo", .before = "inf_LeGi") %>%
  relocate("inf_AmRu", .after = "tot_Cyprinidae")

write_xlsx(TransWide, paste0(to.output,"Transects_WideData.xlsx")) #Exporting data frame
write.csv(TransWide, paste0(to.output, "Transects_WideData.csv"), row.names = FALSE)

# ---- Combined data ---- #
## Transect data preparation ----

attach(TransWide)

TransWide$Date <- format(as.Date(TransWide$Date, format = "%d/%m/%Y"), "%Y-%m-%d") #Adjusting date format

Sampling_ID <- str_replace(TransWide$Transect_ID, "1", "_T_01") #Creating a new Sampling_ID column containing Transect_ID info + Method info
Sampling_ID <- str_replace(Sampling_ID, "2", "_T_02")
Sampling_ID <- str_replace(Sampling_ID, "3", "_T_03")
Sampling_ID <- str_replace(Sampling_ID, "4", "_T_04")
Sampling_ID <- str_replace(Sampling_ID, "5", "_T_05")
Sampling_ID <- str_replace(Sampling_ID, "6", "_T_06")

Sampling_ID <- data.frame(Sampling_ID)
colnames(TransWide)[1] <- "Sampling_ID"
TransWide[1] <- Sampling_ID

TransPrep <- TransWide %>% 
  add_column(Sampling_method = "Transect") %>% #Creating Sampling method column
  relocate("Sampling_method", .after = "Start_time") 

TransPrep <- TransPrep %>% 
  relocate("Sampling_ID", .before = "Lake") #Relocating Sampling ID column

TransPrep <- TransPrep %>% #Creating total abundance columns for missing fish
  add_column(tot_FuDi = 0) %>% relocate("tot_FuDi", .after = "tot_AmRu") %>%
  add_column(tot_Centrarchidae = 0) %>% relocate("tot_Centrarchidae", .after = "tot_MiDo") %>%
  add_column(tot_PiPr = 0) %>% relocate("tot_PiPr", .after = "tot_PeFl") %>%
  add_column(tot_ChrosomusSp. = 0) %>% relocate("tot_ChrosomusSp.", .after = "tot_PiPr") %>%
  add_column(tot_PiNo = 0) %>% relocate("tot_PiNo", .after = "tot_ChrosomusSp.") %>%
  add_column(tot_SeAt = 0) %>% relocate("tot_SeAt", .after = "tot_Cyprinidae") %>%
  add_column(tot_LuCo = 0) %>% relocate("tot_LuCo", .after = "tot_SeAt") %>%
  add_column(tot_AmNe = 0) %>% relocate("tot_AmNe", .after = "tot_LuCo") %>%
  add_column(tot_CaCo = 0) %>% relocate("tot_CaCo", .after = "tot_AmNe") %>%
  add_column(tot_EsMa = 0) %>% relocate("tot_EsMa", .after = "tot_CaCo") %>%
  add_column(tot_UmLi = 0) %>% relocate("tot_UmLi", .after = "tot_EsMa") %>%
  add_column(tot_RhAt = 0) %>% relocate("tot_RhAt", .after = "tot_UmLi")

TransPrep <- TransPrep %>% 
  add_column(inf_FuDi = 0) %>% relocate("inf_FuDi", .after = "inf_AmRu") %>%
  add_column(inf_Centrarchidae = 0) %>% relocate("inf_Centrarchidae", .after = "inf_MiDo") %>%
  add_column(inf_PiPr = 0) %>% relocate("inf_PiPr", .after = "inf_PeFl") %>%
  add_column(inf_ChrosomusSp. = 0) %>% relocate("inf_ChrosomusSp.", .after = "inf_PiPr") %>%
  add_column(inf_PiNo = 0) %>% relocate("inf_PiNo", .after = "inf_ChrosomusSp.") %>%
  add_column(inf_SeAt = 0) %>% relocate("inf_SeAt", .after = "inf_Cyprinidae") %>%
  add_column(inf_LuCo = 0) %>% relocate("inf_LuCo", .after = "inf_SeAt") %>%
  add_column(inf_AmNe = 0) %>% relocate("inf_AmNe", .after = "inf_LuCo") %>%
  add_column(inf_CaCo = 0) %>% relocate("inf_CaCo", .after = "inf_AmNe") %>%
  add_column(inf_EsMa = 0) %>% relocate("inf_EsMa", .after = "inf_CaCo") %>%
  add_column(inf_UmLi = 0) %>% relocate("inf_UmLi", .after = "inf_EsMa") %>%
  add_column(inf_RhAt = 0) %>% relocate("inf_RhAt", .after = "inf_UmLi")

TransPrep[c(7,8,33:35,43:46), c(9:43)] <- NA #Replacing false 0 created

TransPrep <- TransPrep[-c(6, 44:50)] #Deleting Cloud cover and habitat data columns 

TransPrep <- TransPrep %>% #Adjusting water parameters columns to mean values per lake
  group_by(Lake) %>%
  mutate(across(.cols = Temperature | Conductivity | DO | Turbidity | pH | TOC | TN | TP, mean)) %>% 
  ungroup()

## Fishing data preparation ----

attach(FishWide)

colnames(FishWide)[1] <- "Sampling_ID" #Changing the name of Fishing_ID column
colnames(FishWide)[7] <- "Sampling_method" #Changing the name of Gear_type column
colnames(FishWide)[c(17,34)] <- c("tot_ChrosomusSp.", "inf_ChrosomusSp.") #Changing the name of Chrosomus sp. columns

FishPrep <- FishWide[-8] #Deleting Gear_ID column

FishPrep <- FishPrep %>% 
  add_column(Temperature = NA) %>% relocate("Temperature", .after = "inf_RhAt") %>%
  add_column(Conductivity = NA) %>% relocate("Conductivity", .after = "Temperature") %>%
  add_column(DO = NA) %>% relocate("DO", .after = "Conductivity") %>%
  add_column(Turbidity = NA) %>% relocate("Turbidity", .after = "DO") %>%
  add_column(pH = NA) %>% relocate("pH", .after = "Turbidity") %>%
  add_column(TOC = NA) %>% relocate("TOC", .after = "pH") %>%
  add_column(TN = NA) %>% relocate("TN", .after = "TOC") %>%
  add_column(TP = NA) %>% relocate("TP", .after = "TN")

## Binding data sets ----

CombinedData <- rbind(TransPrep, FishPrep)

CombinedData <- CombinedData %>% #Filling NA's water parameters values by lake means
  group_by(Lake) %>% 
  mutate_each(funs(replace(., which(is.na(.)), mean(., na.rm=TRUE))), c("Temperature", "Conductivity", "DO", "Turbidity", "pH", "TOC", "TN", "TP"))

CombinedData <- merge(CombinedData, Lakes_Characteristics, by = "Lake") #Merging lake characteristic data to field data
CombinedData <- merge(CombinedData, BioticData, by = "Lake") #Merging biotic data

colnames(CombinedData)[c(4,5,43:50, 52, 53)] <- c("Lat.trans", "Long.trans", "Temp", "Cond", "DO", "Turb", "pH", "TOC", "TN", "TP", "Lat.lake", "Long.lake") #Changing column names

### Coordinates conversion ----

#Latitude
CombinedData$Lat.trans <- str_replace(CombinedData$Lat.trans, "°", " ")
CombinedData$Lat.trans <- str_remove(CombinedData$Lat.trans, "'")
CombinedData$Lat.trans <- conv_unit(CombinedData$Lat.trans, from = "deg_dec_min", to = "dec_deg")
CombinedData$Lat.trans <- as.numeric(CombinedData$Lat.trans)

CombinedData$Lat.lake <- str_replace(CombinedData$Lat.lake, "°", " ")
CombinedData$Lat.lake <- str_replace(CombinedData$Lat.lake, "'", " ")
CombinedData$Lat.lake <- str_remove(CombinedData$Lat.lake, " N")
CombinedData$Lat.lake <- str_remove(CombinedData$Lat.lake, '"')
CombinedData$Lat.lake <- conv_unit(CombinedData$Lat.lake, from = "deg_min_sec", to = "dec_deg") #Coordinates conversion
CombinedData$Lat.lake <- as.numeric(CombinedData$Lat.lake)

#Longitude
CombinedData$Long.trans <- str_replace(CombinedData$Long.trans, "°", " ")
CombinedData$Long.trans <- str_remove(CombinedData$Long.trans, "'")
CombinedData$Long.trans <- conv_unit(CombinedData$Long.trans, from = "deg_dec_min", to = "dec_deg")
CombinedData$Long.trans <- as.numeric(CombinedData$Long.trans)*(-1) #Add negative sign as coordinates are from western hemisphere

CombinedData$Long.lake <- str_replace(CombinedData$Long.lake, "°", " ")
CombinedData$Long.lake <- str_replace(CombinedData$Long.lake, "'", " ")
CombinedData$Long.lake <- str_remove(CombinedData$Long.lake, " W")
CombinedData$Long.lake <- str_remove(CombinedData$Long.lake, '"')
CombinedData$Long.lake <- conv_unit(CombinedData$Long.lake, from = "deg_min_sec", to = "dec_deg") #Coordinates conversion
CombinedData$Long.lake <- as.numeric(CombinedData$Long.lake)*(-1)

write_xlsx(CombinedData, paste0(to.output, "CombinedData.xlsx")) #Exporting data frame
write.csv(CombinedData, paste0(to.output, "CombinedData.csv"), row.names = FALSE)

# ----- Explicative data sets -----
## Transect data ----

Exp.Trans <- TransWide[-c(1, 3:5, 7:18)] #Deleting common variables between fishing and transect data

write_xlsx(Exp.Trans, paste0(to.output, "Transects_UniqueData.xlsx")) #Exporting data frame
write.csv(Exp.Trans, paste0(to.output, "Transects_UniqueData.csv"), row.names = FALSE)

## Fishing data ----

Exp.Fish <- FishWide[c(1,8)]

write_xlsx(Exp.Fish, paste0(to.output, "Fishing_UniqueData.xlsx")) #Exporting data frame
write.csv(Exp.Fish, paste0(to.output, "Fishing_UniqueData.csv"), row.names = FALSE)
  