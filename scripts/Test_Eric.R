#---- r_setup ----

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"

#---- load package ----
library(tidyverse)


#---- load data ----
raw.d <- read.csv(paste0(to.data,"Fishing_RawData.csv"), sep=",")
clean.d <- raw.d[-c(596,613),] #Deleting lost data (NA Abundance)

#---- convert to long format ----

## Create new column for number of infected fishes 
Abund_inf <- ifelse(clean.d$Intensity_class > 0, clean.d$Abundance, 0) #Creating new abundance column for infected fish
Abund_inf <- replace_na(Abund_inf, 0) #Replacing NA by 0's - Meaning there is no infected fish
CompleteData <- mutate(clean.d, Abund_inf) #Inserting the new column in the data frame
names(CompleteData)[names(CompleteData) == 'Abundance'] <- 'Abund_tot'

## Summarize data

Long.d <- CompleteData %>% #Sum of abundances of species by fishing ID
  group_by(Fishing_ID, Species_ID,Lake,Date,Latitude,Longitude,Start_time,Gear_type,Gear_ID,Identifier) %>%
  summarise(across(.cols = Abund_inf | Abund_tot, sum)) %>%
  ungroup()



