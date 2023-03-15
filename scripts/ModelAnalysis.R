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
library(vegan)
library(ggplot2)
library(cowplot)

source(paste0(to.R, "rquery.cormat.R"))

## Loading data ----
CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))
TransectData <- read.csv(paste0(to.output, "Transects_WideData.csv"))
LakesCaracteristics <- read.csv(paste0(to.data, "Lakes_Caracteristics.csv"), sep=";")
TransBiotic <- read.csv(paste0(to.output, "Trans_BioticData.csv"))

# ---- Lake scale ----

## Preparing data ----
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
lake.fish.data <- lake.fish.data %>% mutate(prev_LeGi = inf_LeGi/tot_LeGi)

#Data frame for modelling
lake.fish.mod <- lake.fish.data %>% 
  select(Lake, Watershed, 
         inf_LeGi, tot_LeGi, inf_fish, tot_fish,
         Temp, Cond, DO, Turb, pH, 
         TOC, TN, TP, 
         Lake_area, Perimeter, Mean_depth, Max_depth, WRT,
         Drainage_area, Elevation, Connectivity, 
         Centrarchids, Species_richness, Diversity)
## Data exploration ----

### Outliers ----

### Collinearity ----

### Relationships ----

## Data analysis ----


# ---- Transect scale ----

## Preparing data -----
#Binding explicative data
trans.data <- merge(TransectData, LakesCaracteristics, by.x = "Lake")
trans.data <- merge(trans.data, TransBiotic, by.x = "Transect_ID") 

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
trans.mod.all <- trans.data %>% 
  select(Transect_ID, Lake, 
         inf_LeGi, tot_LeGi, prev_LeGi, inf_fish, tot_fish, prev_fish,
         Silt, Sand, Rock, Block, Macrophyte, Depth, Trunk,
         Temp, Cond, DO, Turb, pH, 
         TOC, TN, TP, 
         Lake_area, Perimeter, Mean_depth, Max_depth, WRT,
         Drainage_area, Elevation, Connectivity, 
         Centrarchids, Species_richness, Diversity)

## Data exploration ----
### Outliers ----
#Prevalences
par(mfrow = c(2, 1), mar = c(3, 3, 3, 1))
dotchart(trans.mod.all$prev_LeGi, main = "PREV_LEGI", group = trans.mod.all$Lake)
dotchart(trans.mod.all$prev_fish, main = "PREV_FISH", group = trans.mod.all$Lake)

#PhysicoChemistry
par(mfrow = c(3, 2), mar = c(3, 3, 3, 1))
dotchart(trans.mod.all$Temp, main = "TEMP", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Cond, main = "COND", group = trans.mod.all$Lake)
dotchart(trans.mod.all$DO, main = "DO", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Turb, main = "TURB", group = trans.mod.all$Lake)
dotchart(trans.mod.all$pH, main = "PH", group = trans.mod.all$Lake)

#Nutrients
par(mfrow = c(3, 1), mar = c(3, 3, 3, 1))
dotchart(trans.mod.all$TOC, main = "TOC", group = trans.mod.all$Lake)
dotchart(trans.mod.all$TN, main = "TN", group = trans.mod.all$Lake)
dotchart(trans.mod.all$TP, main = "TP", group = trans.mod.all$Lake)

#Morphometry
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1))
dotchart(trans.mod.all$Lake_area, main = "LAKE AREA", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Perimeter, main = "PERIMETER", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Mean_depth, main = "MEAN DEPTH", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Max_depth, main = "MAX DEPTH", group = trans.mod.all$Lake)

#Space
par(mfrow = c(3, 1), mar = c(3, 3, 3, 1))
dotchart(trans.mod.all$Drainage_area, main = "DRAINAGE AREA", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Elevation, main = "ELEVATION", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Connectivity, main = "CONNECTIVITY", group = trans.mod.all$Lake)

#Biotic
par(mfrow = c(3, 1), mar = c(3, 3, 3, 1))
dotchart(trans.mod.all$Centrarchids, main = "CENTRARCHIDS", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Species_richness, main = "RICHNESS", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Diversity, main = "DIVERSITY", group = trans.mod.all$Lake)

#Habitat
par(mfrow = c(4, 2), mar = c(3, 3, 3, 1))
dotchart(trans.mod.all$Silt, main = "SILT", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Sand, main = "SAND", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Rock, main = "ROCK", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Block, main = "BLOCK", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Macrophyte, main = "MACROPHYTE", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Depth, main = "DEPTH", group = trans.mod.all$Lake)
dotchart(trans.mod.all$Trunk, main = "TRUNK", group = trans.mod.all$Lake)

#All
pdf(paste0(to.figs, "Outliers_trans.pdf"), width = 20, height = 15)

par(mfrow = c(4, 7), mar = c(3, 3, 3, 1))
dotchart(trans.mod.all$prev_LeGi, main = "PREV_LEGI")
dotchart(trans.mod.all$prev_fish, main = "PREV_FISH")
dotchart(trans.mod.all$Temp, main = "TEMP")
dotchart(trans.mod.all$Cond, main = "COND")
dotchart(trans.mod.all$DO, main = "DO")
dotchart(trans.mod.all$Turb, main = "TURB")
dotchart(trans.mod.all$pH, main = "PH")
dotchart(trans.mod.all$TOC, main = "TOC")
dotchart(trans.mod.all$TN, main = "TN")
dotchart(trans.mod.all$TP, main = "TP")
dotchart(trans.mod.all$Lake_area, main = "LAKE AREA")
dotchart(trans.mod.all$Perimeter, main = "PERIMETER")
dotchart(trans.mod.all$Mean_depth, main = "MEAN DEPTH")
dotchart(trans.mod.all$Max_depth, main = "MAX DEPTH")
dotchart(trans.mod.all$Drainage_area, main = "DRAINAGE AREA")
dotchart(trans.mod.all$Elevation, main = "ELEVATION")
dotchart(trans.mod.all$Connectivity, main = "CONNECTIVITY")
dotchart(trans.mod.all$Centrarchids, main = "CENTRARCHIDS")
dotchart(trans.mod.all$Species_richness, main = "RICHNESS")
dotchart(trans.mod.all$Diversity, main = "DIVERSITY")
dotchart(trans.mod.all$Silt, main = "SILT")
dotchart(trans.mod.all$Sand, main = "SAND")
dotchart(trans.mod.all$Rock, main = "ROCK")
dotchart(trans.mod.all$Block, main = "BLOCK")
dotchart(trans.mod.all$Macrophyte, main = "MACROPHYTE")
dotchart(trans.mod.all$Depth, main = "DEPTH")
dotchart(trans.mod.all$Trunk, main = "TRUNK")

dev.off()

### Collinearity ----
#### Correlation matrix ----
par(mfrow = c(1, 1), mar = c(3, 3, 3, 1))
trans.corr <- trans.mod.all[c(5, 8:34)]
rquery.cormat(trans.corr, type = "full")

#Where does it cause problem ? 
#Elevation & Conductivity; Not in the same candidate model.
#TN & TP; We could do TN:TP ratio instead.
#Drainage area & Lake_area; Not in the same model.
#Lake_area & Perimeter; We could do Area:Perimeter ratio instead.
#Mean_depth & Max_depth; We will keep Mean_depth as it is more important for littoral fish communities than max_depth.
#Cond & pH + pH & DO; We will keep only keep pH as it has a potentially strong effect on parasite or snail population.
#Silt & Rock; We will do a PCA and extract axis as all substrate variables are a fraction of 100%.
#Lake_area & Sand; Not in the same model.
#Depth & Trunk; We will keep Trunk has we already have a depth variable in the morphometry model.

#Creating new subsrtate variables with PCA axis
substrate.vars <- trans.corr[c(3:6)]

substrate.rda <- rda(substrate.vars, scale = FALSE) 
summary(substrate.rda)
biplot(substrate.rda, scaling = 1)
biplot(substrate.rda, scaling = 2)

site.scores <- scores(substrate.rda, choices = c(1,2), display= "sites", tidy = FALSE) #Extracting axis 
sub1 <- site.scores[,1] #PCA1
sub2 <- site.scores[,2] #PCA2

#Adjusting data frame
trans.mod <- trans.mod.all %>% 
  mutate(TN_TP = TN / TP, .keep = "unused") %>% 
  mutate(Area_Perimeter = (Lake_area*1000000/Perimeter), .keep = "unused") %>% 
  mutate(Sub1 = sub1) %>% 
  mutate(Sub2 = sub2)

trans.mod <- within(trans.mod, rm("DO", "Max_depth", "Cond", "Depth", "Silt", "Sand", "Rock", "Block"))

#Rerunning corr matrix
trans.corr2 <- trans.mod[c(9:26)]
rquery.cormat(trans.corr2, type = "full") #No more collinearity problems detected

#Rerunning outliers
pdf(paste0(to.figs, "Outliers_trans_2.pdf"), width = 20, height = 15)

par(mfrow = c(4, 5), mar = c(3, 3, 3, 1))
dotchart(trans.mod$prev_LeGi, main = "PREV_LEGI")
dotchart(trans.mod$prev_fish, main = "PREV_FISH")
dotchart(trans.mod$Temp, main = "TEMP")
dotchart(trans.mod$Turb, main = "TURB")
dotchart(trans.mod$pH, main = "PH")
dotchart(trans.mod$TOC, main = "TOC")
dotchart(trans.mod$TN_TP, main = "TN_TP")
dotchart(trans.mod$Area_Perimeter, main = "AREA:PERIMETER")
dotchart(trans.mod$Mean_depth, main = "MEAN DEPTH")
dotchart(trans.mod$Drainage_area, main = "DRAINAGE AREA")
dotchart(trans.mod$Elevation, main = "ELEVATION")
dotchart(trans.mod$Connectivity, main = "CONNECTIVITY")
dotchart(trans.mod$Centrarchids, main = "CENTRARCHIDS")
dotchart(trans.mod$Species_richness, main = "RICHNESS")
dotchart(trans.mod$Diversity, main = "DIVERSITY")
dotchart(trans.mod$Sub1, main = "SUB1")
dotchart(trans.mod$Sub2, main = "SUB2")
dotchart(trans.mod$Macrophyte, main = "MACROPHYTE")
dotchart(trans.mod$Trunk, main = "TRUNK")

dev.off()
#Manipulations doesn't correct all of the ouliers

#### Ordination ----
trans.rda.data <- data.frame(trans.corr2, row.names = trans.mod.all$Transect_ID)

trans.rda <- rda(trans.rda.data, scale = TRUE) #Data must be scaled because variables have different units
summary(trans.rda)
par(mfrow = c(1, 1), mar = c(3, 3, 3, 1))
biplot(trans.rda, scaling = 1)
biplot(trans.rda, scaling = 2)

#Incorporating k-means groups to rda
trans.groups <- cascadeKM(dist(scale(trans.rda.data)), 2, 4, criterion = "ssi")
plot(trans.groups)

trans.grKM <- as.vector(trans.groups$partition[,3]) #Extracting groups to add on rda plot
trans.grKM <- as.factor(trans.grKM)
col.groups <- c("yellowgreen","forestgreen","orange","dodgerblue")

plot(trans.rda, scaling = 1, type = "n", main = "PCA + k-means cluster")
with(trans.rda.data, points(trans.rda, display = "sites", col = col.groups[trans.grKM], scaling = 1, pch = 21, bg = col.groups[trans.grKM]))
arrows(0, 0, scores(trans.rda, scaling = 1 )$species[,1], scores(trans.rda, scaling = 1)$species[,2], col="black", code = 2, length = 0.05)
ordiellipse(trans.rda, groups = trans.grKM, display = "sites", conf = 0.95, scaling = 1, col = col.groups, lwd = 2)
text(scores(trans.rda, scaling = 1)$sites, row.names(trans.rda.data), cex=0.5,pos=3, col="black")
text(trans.rda, display = "species", scaling = 1, cex = 0.5, col = "black", pos = 2)

### Relationships ----
#With LeGi prevalence
trans.LeGi.temp <- ggplot(data = trans.mod) + 
  geom_point(aes(Temp, prev_LeGi))
trans.LeGi.turb <- ggplot(data = trans.mod) + 
  geom_point(aes(Turb, prev_LeGi))
trans.LeGi.pH <- ggplot(data = trans.mod) + 
  geom_point(aes(pH, prev_LeGi))
trans.LeGi.TOC <- ggplot(data = trans.mod) + 
  geom_point(aes(TOC, prev_LeGi))
trans.LeGi.TNTP <- ggplot(data = trans.mod) + 
  geom_point(aes(TN_TP, prev_LeGi))
trans.LeGi.Mdepth <- ggplot(data = trans.mod) + 
  geom_point(aes(Mean_depth, prev_LeGi))
trans.LeGi.AP <- ggplot(data = trans.mod) + 
  geom_point(aes(Area_Perimeter, prev_LeGi))
trans.LeGi.WRT <- ggplot(data = trans.mod) + 
  geom_point(aes(WRT, prev_LeGi))
trans.LeGi.DA <- ggplot(data = trans.mod) + 
  geom_point(aes(Drainage_area, prev_LeGi))
trans.LeGi.elevation <- ggplot(data = trans.mod) + 
  geom_point(aes(Elevation, prev_LeGi))
trans.LeGi.connect <- ggplot(data = trans.mod) + 
  geom_point(aes(Connectivity, prev_LeGi))
trans.LeGi.centrar <- ggplot(data = trans.mod) + 
  geom_point(aes(Centrarchids, prev_LeGi))
trans.LeGi.SR <- ggplot(data = trans.mod) + 
  geom_point(aes(Species_richness, prev_LeGi))
trans.LeGi.diversity <- ggplot(data = trans.mod) + 
  geom_point(aes(Diversity, prev_LeGi))
trans.LeGi.sub1 <- ggplot(data = trans.mod) + 
  geom_point(aes(Sub1, prev_LeGi))
trans.LeGi.sub2 <- ggplot(data = trans.mod) + 
  geom_point(aes(sub2, prev_LeGi))
trans.LeGi.macro <- ggplot(data = trans.mod) + 
  geom_point(aes(Macrophyte, prev_LeGi))
trans.LeGi.trunk <- ggplot(data = trans.mod) + 
  geom_point(aes(Trunk, prev_LeGi))

trans.LeGi.plots <- plot_grid(trans.LeGi.temp, trans.LeGi.turb, trans.LeGi.pH, trans.LeGi.TOC, trans.LeGi.Mdepth, trans.LeGi.TNTP, trans.LeGi.AP, trans.LeGi.WRT, trans.LeGi.DA, trans.LeGi.elevation, trans.LeGi.connect, trans.LeGi.centrar, trans.LeGi.SR, trans.LeGi.diversity, trans.LeGi.macro, trans.LeGi.sub1, trans.LeGi.sub2, trans.LeGi.trunk,
          ncol = 4, nrow = 5)
ggsave(paste0(to.figs, "Relationships_trans_LeGi.png"), plot = trans.LeGi.plots, dpi = 500, width = 20, height = 10)

#With fish prevalence
trans.fish.temp <- ggplot(data = trans.mod) + 
  geom_point(aes(Temp, prev_fish))
trans.fish.turb <- ggplot(data = trans.mod) + 
  geom_point(aes(Turb, prev_fish))
trans.fish.pH <- ggplot(data = trans.mod) + 
  geom_point(aes(pH, prev_fish))
trans.fish.TOC <- ggplot(data = trans.mod) + 
  geom_point(aes(TOC, prev_fish))
trans.fish.TNTP <- ggplot(data = trans.mod) + 
  geom_point(aes(TN_TP, prev_fish))
trans.fish.Mdepth <- ggplot(data = trans.mod) + 
  geom_point(aes(Mean_depth, prev_fish))
trans.fish.AP <- ggplot(data = trans.mod) + 
  geom_point(aes(Area_Perimeter, prev_fish))
trans.fish.WRT <- ggplot(data = trans.mod) + 
  geom_point(aes(WRT, prev_fish))
trans.fish.DA <- ggplot(data = trans.mod) + 
  geom_point(aes(Drainage_area, prev_fish))
trans.fish.elevation <- ggplot(data = trans.mod) + 
  geom_point(aes(Elevation, prev_fish))
trans.fish.connect <- ggplot(data = trans.mod) + 
  geom_point(aes(Connectivity, prev_fish))
trans.fish.centrar <- ggplot(data = trans.mod) + 
  geom_point(aes(Centrarchids, prev_fish))
trans.fish.SR <- ggplot(data = trans.mod) + 
  geom_point(aes(Species_richness, prev_fish))
trans.fish.diversity <- ggplot(data = trans.mod) + 
  geom_point(aes(Diversity, prev_fish))

trans.fish.plots <- plot_grid(trans.fish.temp, trans.fish.turb, trans.fish.pH, trans.fish.TOC, trans.fish.Mdepth, trans.fish.TNTP, trans.fish.AP, trans.fish.WRT, trans.fish.DA, trans.fish.elevation, trans.fish.connect, trans.fish.centrar, trans.fish.SR, trans.fish.diversity,
                              ncol = 3, nrow = 5)
ggsave(paste0(to.figs, "Relationships_trans_fish.png"), plot = trans.fish.plots, dpi = 500, width = 20, height = 10)
# Relationships don't always suggest linear patterns...

## Data analysis ----



