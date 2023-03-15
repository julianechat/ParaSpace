# ---- Script setup ----
## R Setup ----

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"
to.carto <- "./carto/"

## Loading packages ----

library(dplyr)
source(paste0(to.R, "rquery.cormat.R"))

## Loading data ----
CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))
TransectData <- read.csv(paste0(to.output, "Transects_WideData.csv"))
Lakes_Caracteristics <- read.csv(paste0(to.data, "Lakes_Caracteristics.csv"), sep=";")
Trans_Biotic <- read.csv(paste0(to.output, "Trans_BioticData.csv"))


# ---- Lake scale ----

## Data exploration ----

### Cleaning data ----
lake.fish.data <- CombinedData %>% filter(!Sampling_method == "Transect")

#Summarizing explicative data at lake scale
lake.exp.data <- lake.fish.data %>% 
  group_by(Lake) %>% 
  select(c(1, 43:50, 51:62)) %>% 
  distinct() 

#Summarizing community data at lake scale
lake.comm.matrix <- lake.fish.data %>% 
  group_by(Lake) %>% 
  select(starts_with("tot") | starts_with("inf")) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))
lake.comm.matrix <- lake.comm.matrix[c(2:35)]

#All of lake scale data
lake.fish.data <- data.frame(lake.exp.data, lake.comm.matrix)

# Creating response variables
lake.fish.data$tot_fish <- lake.fish.data %>% select(starts_with("tot")) %>% rowSums()
lake.fish.data$inf_fish <- lake.fish.data %>% select(starts_with("inf")) %>% rowSums()
lake.fish.data <- lake.fish.data %>% mutate(prev_fish = inf_fish/tot_fish)
lake.fish.data <- lake.fish.mod %>% mutate(prev_LeGi = inf_LeGi/tot_LeGi)

#Data frame for modelling
lake.fish.mod <- lake.fish.data %>% 
  select(Lake, Watershed, 
         inf_LeGi, tot_LeGi, inf_fish, tot_fish,
         Temp, Cond, DO, Turb, pH, 
         TOC, TN, TP, 
         Lake_area, Perimeter, Mean_depth, Max_depth, WRT,
         Drainage_area, Elevation, Connectivity, 
         Centrarchids, Species_richness, Diversity)

### Outliers ----

### Collinearity ----

### Relationships ----

## Data analysis ----

# ---- Transect scale ----
## Data exploration ----
### Cleaning data -----
#Binding explicative data
trans.data <- merge(TransectData, Lakes_Caracteristics, by.x = "Lake")
trans.data <- merge(trans.data, Trans_Biotic, by.x = "Transect_ID") 

#Creating response variables
trans.data$tot_fish <- trans.data %>% 
  select(starts_with("tot")) %>% 
  rowSums()

trans.data$inf_fish <- trans.data %>% 
  select(starts_with("inf")) %>% 
  rowSums()

trans.data <- trans.data %>% mutate(prev_fish = inf_fish/tot_fish)
trans.data <- trans.data %>% mutate(prev_LeGi = inf_LeGi/tot_LeGi)

#Data frame for modelling
trans.mod <- trans.data %>% 
  select(Transect_ID, Lake, 
         inf_LeGi, tot_LeGi, prev_LeGi, inf_fish, tot_fish, prev_fish,
         Temp, Cond, DO, Turb, pH, 
         TOC, TN, TP, 
         Lake_area, Perimeter, Mean_depth, Max_depth, WRT,
         Drainage_area, Elevation, Connectivity, 
         Centrarchids, Species_richness, Diversity)

### Outliers ----
out.response <- par(mfrow = c(1, 2), mar = c(3, 3, 3, 1))
dotchart(trans.mod$prev_LeGi, main = "PREV_LEGI", group = trans.mod$Lake)
dotchart(trans.mod$prev_fish, main = "PREV_FISH", group = trans.mod$Lake)

out.physico <- par(mfrow = c(3, 2), mar = c(3, 3, 3, 1))
dotchart(trans.mod$Temp, main = "TEMP", group = trans.mod$Lake)
dotchart(trans.mod$Cond, main = "COND", group = trans.mod$Lake)
dotchart(trans.mod$DO, main = "DO", group = trans.mod$Lake)
dotchart(trans.mod$Turb, main = "TURB", group = trans.mod$Lake)
dotchart(trans.mod$pH, main = "PH", group = trans.mod$Lake)

out.nutrients <- par(mfrow = c(3, 1), mar = c(3, 3, 3, 1))
dotchart(trans.mod$TOC, main = "TOC", group = trans.mod$Lake)
dotchart(trans.mod$TN, main = "TN", group = trans.mod$Lake)
dotchart(trans.mod$TP, main = "TP", group = trans.mod$Lake)

out.morpho <- par(mfrow = c(3, 2), mar = c(3, 3, 3, 1))
dotchart(trans.mod$Lake_area, main = "LAKE AREA", group = trans.mod$Lake)
dotchart(trans.mod$Perimeter, main = "PERIMETER", group = trans.mod$Lake)
dotchart(trans.mod$Mean_depth, main = "MEAN DEPTH", group = trans.mod$Lake)
dotchart(trans.mod$Max_depth, main = "MAX DEPTH", group = trans.mod$Lake)

out.space <-  par(mfrow = c(3, 1), mar = c(3, 3, 3, 1))
dotchart(trans.mod$Drainage_area, main = "DRAINAGE AREA", group = trans.mod$Lake)
dotchart(trans.mod$Elevation, main = "ELEVATION", group = trans.mod$Lake)
dotchart(trans.mod$Connectivity, main = "CONNECTIVITY", group = trans.mod$Lake)

out.biotic <-  par(mfrow = c(3, 1), mar = c(3, 3, 3, 1))
dotchart(trans.mod$Centrarchids, main = "CENTRARCHIDS", group = trans.mod$Lake)
dotchart(trans.mod$Species_richness, main = "RICHNESS", group = trans.mod$Lake)
dotchart(trans.mod$Diversity, main = "DIVERSITY", group = trans.mod$Lake)

out.all <- par(mfrow = c(4, 5), mar = c(3, 3, 3, 1))
dotchart(trans.mod$prev_LeGi, main = "PREV_LEGI", group = trans.mod$Lake)
dotchart(trans.mod$prev_fish, main = "PREV_FISH", group = trans.mod$Lake)
dotchart(trans.mod$Temp, main = "TEMP", group = trans.mod$Lake)
dotchart(trans.mod$Cond, main = "COND", group = trans.mod$Lake)
dotchart(trans.mod$DO, main = "DO", group = trans.mod$Lake)
dotchart(trans.mod$Turb, main = "TURB", group = trans.mod$Lake)
dotchart(trans.mod$pH, main = "PH", group = trans.mod$Lake)
dotchart(trans.mod$TOC, main = "TOC", group = trans.mod$Lake)
dotchart(trans.mod$TN, main = "TN", group = trans.mod$Lake)
dotchart(trans.mod$TP, main = "TP", group = trans.mod$Lake)
dotchart(trans.mod$Lake_area, main = "LAKE AREA", group = trans.mod$Lake)
dotchart(trans.mod$Perimeter, main = "PERIMETER", group = trans.mod$Lake)
dotchart(trans.mod$Mean_depth, main = "MEAN DEPTH", group = trans.mod$Lake)
dotchart(trans.mod$Max_depth, main = "MAX DEPTH", group = trans.mod$Lake)
dotchart(trans.mod$Drainage_area, main = "DRAINAGE AREA", group = trans.mod$Lake)
dotchart(trans.mod$Elevation, main = "ELEVATION", group = trans.mod$Lake)
dotchart(trans.mod$Connectivity, main = "CONNECTIVITY", group = trans.mod$Lake)
dotchart(trans.mod$Centrarchids, main = "CENTRARCHIDS", group = trans.mod$Lake)
dotchart(trans.mod$Species_richness, main = "RICHNESS", group = trans.mod$Lake)
dotchart(trans.mod$Diversity, main = "DIVERSITY", group = trans.mod$Lake)

### Collinearity ----
out <- par(mfrow = c(1, 1), mar = c(3, 3, 3, 1))
trans.corr <- trans.mod[c(5, 8:27)]
rquery.cormat(trans.corr, type = "full")

#Where does it cause problem ? 
#Elevation & Conductivity; Not in the same candidate model.
#TN & TP; We could do TN:TP ration instead.
#Drainage area & Lake_area; Not in the same model.
#Lake_area & Perimeter; We could do Area:Perimeter ratio instead.
#Mean_depth & Max_depth; We will keep Mean_depth as it is more important for littoral fish communities than max_depth.
#Cond & pH + pH & DO; We will keep only keep pH as it has a potentially strong effect on parasite or snail population.

#Adjusting data frame
trans.mod2 <- trans.mod %>% 
  mutate(TN_TP = TN / TP, .keep = "unused") %>% 
  mutate(Area_Perimeter = (Lake_area*1000000/Perimeter), .keep = "unused")

trans.mod2 <- within(trans.mod2, rm("DO", "Max_depth", "Cond"))

#Rerunning corr matrix
trans.corr2 <- trans.mod2[c(5, 8:22)]
rquery.cormat(trans.corr2, type = "full") #No more collinearity problems detected

out.all <- par(mfrow = c(3, 5), mar = c(3, 3, 3, 1))
dotchart(trans.mod2$prev_LeGi, main = "PREV_LEGI", group = trans.mod$Lake)
dotchart(trans.mod2$prev_fish, main = "PREV_FISH", group = trans.mod$Lake)
dotchart(trans.mod2$Temp, main = "TEMP", group = trans.mod$Lake)
dotchart(trans.mod2$Turb, main = "TURB", group = trans.mod$Lake)
dotchart(trans.mod2$pH, main = "PH", group = trans.mod$Lake)
dotchart(trans.mod2$TOC, main = "TOC", group = trans.mod$Lake)
dotchart(trans.mod2$TN_TP, main = "TN:TP", group = trans.mod$Lake)
dotchart(trans.mod2$Area_Perimeter, main = "AREA:PERIMETER", group = trans.mod$Lake)
dotchart(trans.mod2$Mean_depth, main = "MEAN DEPTH", group = trans.mod$Lake)
dotchart(trans.mod2$Drainage_area, main = "DRAINAGE AREA", group = trans.mod$Lake)
dotchart(trans.mod2$Elevation, main = "ELEVATION", group = trans.mod$Lake)
dotchart(trans.mod2$Connectivity, main = "CONNECTIVITY", group = trans.mod$Lake)
dotchart(trans.mod2$Centrarchids, main = "CENTRARCHIDS", group = trans.mod$Lake)
dotchart(trans.mod2$Species_richness, main = "RICHNESS", group = trans.mod$Lake)
dotchart(trans.mod2$Diversity, main = "DIVERSITY", group = trans.mod$Lake)
#Manipulations doesn't correct the outliers quite well...

### Relationships ----

## Data analysis ----



