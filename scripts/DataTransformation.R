## Field data transformation ##

# ----- R Setup ----- #

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"

# ----- Loading packages ----- #

library(tidyr)
library(dplyr)
library(writexl)
library(stringr)
library(tibble)

# ----- Loading data ----- #

Fishing_RawData <- read.csv(paste0(to.data,"Fishing_RawData.csv"), sep=";")
Transects_RawData <- read.csv(paste0(to.data, "Transects_RawData.csv"), sep=";")
Lakes_Caracteristics <- read.csv(paste0(to.data, "Lakes_Caracteristics.csv"), sep=";")
BioticData <- read.csv(paste0(to.output, "Lake_BioticData.csv"))

# ------------------------ #
  
#### ----- Fishing data ----- ####
CleanData <- Fishing_RawData[-c(596,613),] #Deleting lost data (NA Abundance)
attach(CleanData)

## Long format ##
Abund_inf <- ifelse(Intensity_class > 0, Abundance, 0) #Creating new abundance column for infected fish
Abund_inf <- replace_na(Abund_inf, 0) #Replacing NA by 0's - Meaning there is no infected fish

CompleteData <- mutate(CleanData, Abund_inf) #Inserting the new column in the data frame
names(CompleteData)[names(CompleteData) == 'Abundance'] <- 'Abund_tot'

# Summarize data #
Fishing_LongData <- CompleteData %>% #Sum of abundances of species by fishing ID
  group_by(Fishing_ID, Species_ID,Lake,Date,Latitude,Longitude,Start_time,Gear_type,Gear_ID,Identifier) %>%
  summarise(across(.cols = Abund_inf | Abund_tot, sum)) %>%
  ungroup()

write_xlsx(Fishing_LongData, paste0(to.output, "Fishing_LongData.xlsx")) #Exporting data
write.csv(Fishing_LongData, paste0(to.output, "Fishing_LongData.csv"), row.names = FALSE)

## Wide format ##
ComMatrix <- pivot_wider(data = Fishing_LongData, names_from = "Species_ID", values_from = c("Abund_tot", "Abund_inf"), values_fill = 0) #Reshape data
ComMatrix2 <- ComMatrix[,-c(10,28)] #Deleting NA's abundance columns

ColNames <- str_remove_all(colnames(ComMatrix2), "Abund_") #Changing species columns name
Fishing_WideData <- `colnames<-`(ComMatrix2, ColNames)

write_xlsx(Fishing_WideData, paste0(to.output, "Fishing_WideData.xlsx")) #Exporting data
write.csv(Fishing_WideData, paste0(to.output, "Fishing_WideData.csv"), row.names = FALSE)

#### ----- Transect data ----- ####
attach(Transects_RawData)

## Summarizing data at transect scale ##
# Abundance, habitat and water parameters data #

LeGi_H <- tapply(LeGi_Health, Transect_ID, sum)
LeGi_I <- tapply(LeGi_Infected, Transect_ID, sum)
Cyp_H <-  tapply(Cyp_Health, Transect_ID, sum)
Cyp_I <-  tapply(Cyp_Infected, Transect_ID, sum)
MiDo_H <- tapply(MiDo_Health, Transect_ID, sum)
MiDo_I <- tapply(MiDo_Infected, Transect_ID, sum)
AmRu_H <- tapply(AmRu_Health, Transect_ID, sum)
AmRu_I <- tapply(AmRu_Infected, Transect_ID, sum)
PeFl_H <- tapply(PeFl_Health, Transect_ID, sum)
PeFl_I <- tapply(PeFl_Infected, Transect_ID, sum)
Trunk <- tapply(Trunk, Transect_ID, sum)

Silt <- tapply(Silt, Transect_ID, mean)
Sand <- tapply(Sand, Transect_ID, mean)
Rock <- tapply(Rock, Transect_ID, mean)
Block <- tapply(Metric_block, Transect_ID, mean)
Macrophyte <- tapply(Macrophyte, Transect_ID, mean)
Depth <- tapply(Mean_depth, Transect_ID, mean)
Temp <- tapply(Temperature, Transect_ID, mean)
Cond <- tapply(Conductivity, Transect_ID, mean)
DO <- tapply(DO, Transect_ID, mean)
Turb <- tapply(Turbidity, Transect_ID, mean)
pH <- tapply(pH, Transect_ID, mean)
TOC <- tapply(TOC, Transect_ID, mean)
TN <- tapply(TN, Transect_ID, mean)
TP <- tapply(TP, Transect_ID, mean)

Abund.Hab.WP_Trans <- data.frame(cbind(LeGi_I, LeGi_H, Cyp_I, Cyp_H, PeFl_I, PeFl_H, MiDo_I, MiDo_H, AmRu_I, AmRu_H, Silt, Sand, Rock, Block, Macrophyte, Depth, Trunk, Temp, Cond, DO, Turb, pH, TOC, TN, TP), row.names = NULL)

# Informative data #
#For this part, we want only one informative row per Transect ID
#Abundance, habitat and water parameters have been previously summarize
#Diver and Start_time columns values change through the transect. We will need to bind info of both divers and keep the reel start value of the transect (0-10m segment)
#Position, Sampling_ID, Segment can not be coerce at transect scale

attach(Transects_RawData)
Clean_Trans <- Transects_RawData %>%
  select(c(Lake, Transect_ID, Date, Latitude, Longitude, Cloud_cover, Start_time))

ShortTrans <- Clean_Trans %>% #Creating new data frame containing only the first row of each Transect_ID #This fix the Start_time problem
  group_by(Transect_ID) %>%
  slice(1)

DiversTrans <- Transects_RawData %>% #Creating new column Identifier binding both divers for each Transect_ID
  group_by(Transect_ID) %>% 
  distinct(Diver) %>% 
  summarize(Diver=paste(Diver,collapse="_"))
DiversCol <- DiversTrans[2]
Identifier <- `colnames<-`(DiversCol, "Identifier") 

Info_Trans <- cbind(as.data.frame(ShortTrans), Identifier)

# Final data frame at transect scale #
df_TransScale <- cbind(Info_Trans, Abund.Hab.WP_Trans)

## Wide format ##
attach(df_TransScale)

# Adjusting community matrix # 
tot_Trans <- df_TransScale %>% #Creating total abundance columns
  mutate(tot_AmRu = AmRu_H + AmRu_I, tot_MiDo = MiDo_H + MiDo_I, tot_LeGi = LeGi_H + LeGi_I, tot_PeFl = PeFl_H + PeFl_I, tot_Cyprinidae = Cyp_H + Cyp_I) %>% 
  select(-c("AmRu_H", "MiDo_H", "LeGi_H", "PeFl_H", "Cyp_H")) #Deleting health abundance columns 

colnames(tot_Trans)[c(9:13)] <- c("inf_LeGi", "inf_Cyprinidae", "inf_PeFl", "inf_MiDo", "inf_AmRu") 

Transects_WideData <- tot_Trans %>% relocate(c("tot_AmRu", "tot_MiDo", "tot_LeGi", "tot_PeFl", "tot_Cyprinidae"), .after = "Identifier") %>% #Relocating total abundance community matrix
  relocate("inf_PeFl", .before = "inf_Cyprinidae") %>% #Adjusting the column order
  relocate("inf_MiDo", .before = "inf_LeGi") %>%
  relocate("inf_AmRu", .after = "tot_Cyprinidae")

write_xlsx(Transects_WideData, paste0(to.output,"Transects_WideData.xlsx")) #Exporting data frame
write.csv(Transects_WideData, paste0(to.output, "Transects_WideData.csv"), row.names = FALSE)

#### ----- Combined data ----- ####
## Prep of Transect data ##
attach(Transects_WideData)

Transects_WideData$Date <- format(as.Date(Transects_WideData$Date, format = "%d/%m/%Y"), "%Y-%m-%d") #Adjusting date format

Sampling_ID <- str_replace(Transects_WideData$Transect_ID, "1", "_T_01") #Creating a new Sampling_ID column containing Transect_ID info + Method info
Sampling_ID <- str_replace(Sampling_ID, "2", "_T_02")
Sampling_ID <- str_replace(Sampling_ID, "3", "_T_03")
Sampling_ID <- str_replace(Sampling_ID, "4", "_T_04")
Sampling_ID <- str_replace(Sampling_ID, "5", "_T_05")
Sampling_ID <- str_replace(Sampling_ID, "6", "_T_06")

Sampling_ID <- data.frame(Sampling_ID)
colnames(Transects_WideData)[2] <- "Sampling_ID"
Transects_WideData[2] <- Sampling_ID

T.Prep1 <- Transects_WideData %>% add_column(Sampling_method = "Transect") %>% #Creating Sampling method column
  relocate("Sampling_method", .after = "Start_time") 

T.Prep2 <- T.Prep1 %>%relocate("Sampling_ID", .before = "Lake") #Relocating Sampling ID column

T.Prep3 <- T.Prep2 %>% #Creating total abundance columns for missing fish
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

T.Prep4 <- T.Prep3 %>% 
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

T.Prep4[c(7,8,33:35,43:46), c(9:43)] <- NA #Replacing false 0 created

T.Prep5 <- T.Prep4[-c(6, 44:50)] #Deleting Cloud cover and habitat data columns 

T.Prep6 <- T.Prep5 %>% #Adjusting water parameters columns to mean values per lake
  group_by(Lake) %>%
  mutate(across(.cols = Temp | Cond | DO | Turb | pH | TOC | TN | TP, mean))

## Prep of Fishing data ## 
attach(Fishing_WideData)
colnames(Fishing_WideData)[1] <- "Sampling_ID" #Changing the name of Fishing_ID column
colnames(Fishing_WideData)[7] <- "Sampling_method" #Changing the name of Gear_type column
colnames(Fishing_WideData)[c(17,34)] <- c("tot_ChrosomusSp.", "inf_ChrosomusSp.") #Changing the name of Chrosomus sp. columns

F.Prep1 <- Fishing_WideData[-8] #Deleting Gear_ID column

F.Prep2 <- F.Prep1 %>% 
  add_column(Temp = NA) %>% relocate("Temp", .after = "inf_RhAt") %>%
  add_column(Cond = NA) %>% relocate("Cond", .after = "Temp") %>%
  add_column(DO = NA) %>% relocate("DO", .after = "Cond") %>%
  add_column(Turb = NA) %>% relocate("Turb", .after = "DO") %>%
  add_column(pH = NA) %>% relocate("pH", .after = "Turb") %>%
  add_column(TOC = NA) %>% relocate("TOC", .after = "pH") %>%
  add_column(TN = NA) %>% relocate("TN", .after = "TOC") %>%
  add_column(TP = NA) %>% relocate("TP", .after = "TN")

## Binding both data set ##
df.Comb <- rbind.data.frame(T.Prep6, F.Prep2)

df.Comb2 <- df.Comb %>% #Filling NA's water parameters values by lake means
  group_by(Lake) %>% 
  mutate_each(funs(replace(., which(is.na(.)), mean(., na.rm=TRUE))), c("Temp", "Cond", "DO", "Turb", "pH", "TOC", "TN", "TP"))

CombinedData <- merge(df.Comb2, Lakes_Caracteristics, by.x = "Lake") #Merging lake characteristic data to field data
CombinedData <- merge(CombinedData, BioticData, by.x = "Lake") #Merging biotic data

colnames(CombinedData)[c(43:50)] <- c("Temp", "Cond", "DO", "Turb", "pH", "TOC", "TN", "TP") #Changing column names

write_xlsx(CombinedData, paste0(to.output, "CombinedData.xlsx")) #Exporting data frame
write.csv(CombinedData, paste0(to.output, "CombinedData.csv"), row.names = FALSE)

#### ----- Explicative data sets ----- #### 
## Transect data ##
Exp.Trans <- Transects_WideData[-c(1, 3:5, 7:18)] #Deleting common variables between fishing and transect data

write_xlsx(Exp.Trans, paste0(to.output, "Transects_UniqueData.xlsx")) #Exporting data frame
write.csv(Exp.Trans, paste0(to.output, "Transects_UniqueData.csv"), row.names = FALSE)

## Fishing data ##
Exp.Fish <- Fishing_WideData[c(1,8)]

write_xlsx(Exp.Fish, paste0(to.output, "Fishing_UniqueData.xlsx")) #Exporting data frame
write.csv(Exp.Fish, paste0(to.output, "Fishing_UniqueData.csv"), row.names = FALSE)
  
  
  
  