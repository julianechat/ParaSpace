## Script name : Data exploration for model analysis

## Authors : Juliane Vigneault
## Date created : March 23, 2023

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

## Loading packages & functions ----

library(dplyr)
library(ggplot2)
library(patchwork)

source(paste0(to.R, "rquery.cormat.R"))

## Loading data ----

ParaSpaceMod <- read.csv(paste0(to.output, "Transects_Lake_Data.csv"))

# ---- Data exploration ----
## Transect scale ----

### Outliers ----

#Prevalence
par(mfrow = c(1, 1), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$prev_fish, main = "PREV_FISH", group = as.factor(ParaSpaceMod$Lake))
#No outliers

#PhysicoChemistry
par(mfrow = c(3, 2), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Temp.T, main = "TEMP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Cond.T, main = "COND", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$DO.T, main = "DO", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Turb.T, main = "TURB", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$pH.T, main = "PH", group = as.factor(ParaSpaceMod$Lake))
#Cromwell outlier compared to other lakes, but variance is constant

#Nutrients
par(mfrow = c(3, 1), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$TOC.T, main = "TOC", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TN.T, main = "TN", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TP.T, main = "TP", group = as.factor(ParaSpaceMod$Lake))
#No outliers

#Morphometry
par(mfrow = c(3, 2), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Lake_area, main = "LAKE AREA", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Perimeter, main = "PERIMETER", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Mean_depth, main = "MEAN DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Max_depth, main = "MAX DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$WRT, main = "WRT", group = as.factor(ParaSpaceMod$Lake))
#Achigan is an outlier for Lake area & Perimeter

#Space
par(mfrow = c(3, 1), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Drainage_area, main = "DRAINAGE AREA", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Elevation, main = "ELEVATION", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Connectivity, main = "CONNECTIVITY", group = as.factor(ParaSpaceMod$Lake))
#Achigan is an outlier for Drainage area

#Biotic
par(mfrow = c(3, 1), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Centrarchids.T, main = "CENTRARCHIDS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Species_richness.T, main = "RICHNESS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Diversity.T, main = "DIVERSITY", group = as.factor(ParaSpaceMod$Lake))
#TRIT1 & MORE3 are outliers for Centrarchids abundance
#ACHI2 is an outlier for Diversity - This data should be excluded as diversity based one fish absence is unrelevant

#Habitat
par(mfrow = c(4, 2), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Silt, main = "SILT", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Sand, main = "SAND", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Rock, main = "ROCK", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Block, main = "BLOCK", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Macrophyte, main = "MACROPHYTE", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Depth, main = "DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Trunk, main = "TRUNK", group = as.factor(ParaSpaceMod$Lake))
#ACHI1 is an outlier for Sand
#CORN4 is an oulier for Depth & Trunk

#All
pdf(paste0(to.figs, "Outliers_trans.pdf"), width = 20, height = 15)

par(mfrow = c(4, 7), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$prev_fish, main = "PREV_FISH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Temp.T, main = "TEMP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Cond.T, main = "COND", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$DO.T, main = "DO", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Turb.T, main = "TURB", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$pH.T, main = "PH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TOC.T, main = "TOC", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TN.T, main = "TN", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TP.T, main = "TP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Lake_area, main = "LAKE AREA", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Perimeter, main = "PERIMETER", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Mean_depth, main = "MEAN DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Max_depth, main = "MAX DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Drainage_area, main = "DRAINAGE AREA", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$WRT, main = "WRT", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Elevation, main = "ELEVATION", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Connectivity, main = "CONNECTIVITY", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Centrarchids.T, main = "CENTRARCHIDS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Species_richness.T, main = "RICHNESS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Diversity.T, main = "DIVERSITY", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Silt, main = "SILT", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Sand, main = "SAND", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Rock, main = "ROCK", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Block, main = "BLOCK", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Macrophyte, main = "MACROPHYTE", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Depth, main = "DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Trunk, main = "TRUNK", group = as.factor(ParaSpaceMod$Lake))

dev.off()

### Collinearity ----
#### Correlation matrix ----

par(mfrow = c(1, 1), mar = c(3, 3, 3, 1))
trans.corr.all <- ParaSpaceMod %>% 
  select_("Silt", "Sand", "Rock", "Block", "Macrophyte", "Depth", "Trunk", 
          "Temp.T", "Cond.T", "DO.T", "Turb.T", "pH.T", 
          "TOC.T", "TN.T", "TP.T",
          "Lake_area", "Perimeter", "Mean_depth", "Max_depth", "WRT", 
          "Drainage_area", "Elevation", "Connectivity", 
          "Centrarchids.T", "Species_richness.T", "Diversity.T")

rquery.cormat(trans.corr.all, type = "full")

#Where does it cause problem ? 
#TN & TP; We could do TN:TP ratio instead.
#Drainage area & Perimeter + Drainage area & Lake_area; Not in the same model.
#Lake_area & Perimeter; We could do Area:Perimeter ratio instead.
#Mean_depth & Max_depth; We will keep Mean_depth as it is more important for littoral fish communities than max_depth.
#Cond & pH + pH & DO; We will keep only keep pH as it has a potentially strong effect on parasite or snail population.
#Silt & Rock; We will do a PCA and extract axis as all substrate variables are a fraction of 100%.

#### Ordination ----

trans.rda.data <- data.frame(trans.corr.all, row.names = ParaSpaceMod$Transect_ID)

trans.rda <- rda(trans.rda.data, scale = TRUE) #Data must be scaled because variables have different units
summary(trans.rda)

par(mfrow = c(1, 1), mar = c(3, 3, 3, 1)) #Visualization of rda plots
biplot(trans.rda, scaling = 1)
biplot(trans.rda, scaling = 2)

#Incorporating k-means groups to rda
trans.groups <- cascadeKM(dist(scale(trans.rda.data)), 2, 4, criterion = "ssi")
plot(trans.groups)

trans.grKM <- as.vector(trans.groups$partition[,3]) #Extracting groups to add on rda plot
trans.grKM <- as.factor(trans.grKM)
col.groups <- c("yellowgreen","forestgreen","orange","dodgerblue")

pdf(paste0(to.figs, "KmeansPCA_trans.pdf"), width = 20, height = 15)

plot(trans.rda, scaling = 1, type = "n", main = "PCA + k-means cluster") #Visualization of k-means groups
with(trans.rda.data, points(trans.rda, display = "sites", col = col.groups[trans.grKM], scaling = 1, pch = 21, bg = col.groups[trans.grKM]))
arrows(0, 0, scores(trans.rda, scaling = 1 )$species[,1], scores(trans.rda, scaling = 1)$species[,2], col="black", code = 2, length = 0.05)
ordiellipse(trans.rda, groups = trans.grKM, display = "sites", conf = 0.95, scaling = 1, col = col.groups, lwd = 2)
text(scores(trans.rda, scaling = 1)$sites, row.names(trans.rda.data), cex=0.5,pos=3, col="black")
text(trans.rda, display = "species", scaling = 1, cex = 0.5, col = "black", pos = 2)

dev.off()

### Relationships ----

trans.temp <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Temp.T, prev_fish))
trans.turb <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Turb.T, prev_fish))
trans.DO <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(DO.T, prev_fish))
trans.cond <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Cond.T, prev_fish))
trans.pH <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(pH.T, prev_fish))
trans.TOC <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(TOC.T, prev_fish))
trans.TN <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(TN.T, prev_fish))
trans.TP <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(TP.T, prev_fish))
trans.Mdepth <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Mean_depth, prev_fish))
trans.Xdepth <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Max_depth, prev_fish))
trans.Area <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Lake_area, prev_fish))
trans.Peri <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Perimeter, prev_fish))
trans.WRT <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(WRT, prev_fish))
trans.DA <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Drainage_area, prev_fish))
trans.elevation <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Elevation, prev_fish))
trans.connect <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Connectivity, prev_fish))
trans.centrar <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Centrarchids.T, prev_fish))
trans.SR <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Species_richness.T, prev_fish))
trans.diversity <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Diversity.T, prev_fish))
trans.silt <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Silt, prev_fish))
trans.sand <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Sand, prev_fish))
trans.rock <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Rock, prev_fish))
trans.block <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Block, prev_fish))
trans.macro <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Macrophyte, prev_fish))
trans.trunk <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Trunk, prev_fish))
trans.depth <- ggplot(data = ParaSpaceMod) +
  geom_point(aes(Depth, prev_fish))

relationships.trans <- plot_grid(trans.temp, trans.turb, trans.pH, trans.DO, trans.cond, trans.TN, trans.TP, trans.TOC, trans.Mdepth, trans.Area, trans.Peri, trans.WRT, trans.DA, trans.elevation, trans.connect, trans.centrar, trans.SR, trans.diversity, trans.macro, trans.sand, trans.silt, trans.rock, trans.block, trans.trunk, trans.depth,
          ncol = 5, nrow = 5)
#Relationships don't always suggest linear patterns

ggsave(paste0(to.figs, "Relationships_trans.pdf"), plot = relationships.trans, dpi = 500, width = 20, height = 10) #Saving plot grid

## Lake scale ----
### Outliers ----

#We only present variables we reduced to lake mean as they might differ from transect scale data exploration

#PhysicoChemistry
par(mfrow = c(3, 2), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Temp.L, main = "TEMP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Cond.L, main = "COND", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$DO.L, main = "DO", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Turb.L, main = "TURB", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$pH.L, main = "PH", group = as.factor(ParaSpaceMod$Lake))
#Cromwell is an outlier for DO

#Nutrients
par(mfrow = c(3, 1), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$TOC.L, main = "TOC", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TN.L, main = "TN", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TP.L, main = "TP", group = as.factor(ParaSpaceMod$Lake))
#No outliers

#Biotic
par(mfrow = c(3, 1), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Centrarchids.L, main = "CENTRARCHIDS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Species_richness.L, main = "RICHNESS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Diversity.L, main = "DIVERSITY", group = as.factor(ParaSpaceMod$Lake))
#Triton is an outlier for Diversity

#All
pdf(paste0(to.figs, "Outliers_lake.pdf"), width = 20, height = 15)

par(mfrow = c(7, 3), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$prev_fish, main = "PREV_FISH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Temp.L, main = "TEMP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Cond.L, main = "COND", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$DO.L, main = "DO", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Turb.L, main = "TURB", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$pH.L, main = "PH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TOC.L, main = "TOC", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TN.L, main = "TN", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TP.L, main = "TP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Centrarchids.L, main = "CENTRARCHIDS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Species_richness.L, main = "RICHNESS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Diversity.L, main = "DIVERSITY", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Lake_area, main = "LAKE AREA", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Perimeter, main = "PERIMETER", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Mean_depth, main = "MEAN DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Max_depth, main = "MAX DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Drainage_area, main = "DRAINAGE AREA", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$WRT, main = "WRT", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Elevation, main = "ELEVATION", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Connectivity, main = "CONNECTIVITY", group = as.factor(ParaSpaceMod$Lake))

dev.off()

### Collinearity ----

#### Correlation matrix ----
par(mfrow = c(1, 1), mar = c(3, 3, 3, 1))

lake.corr.all <- ParaSpaceMod %>% 
  select_( "Temp.L", "Cond.L", "DO.L", "Turb.L", "pH.L", 
          "TOC.L", "TN.L", "TP.L",
          "Lake_area", "Perimeter", "Mean_depth", "Max_depth", "WRT", 
          "Drainage_area", "Elevation", "Connectivity", 
          "Centrarchids.L", "Species_richness.L", "Diversity.L")

rquery.cormat(lake.corr.all, type = "full")

#Where does it cause problem ?
#We discriminate at r >= 0.8
#TN & TP; We will use TN:TP ratio instead.
#Perimeter & Drainage area + Lake area & Drainage area; Not in the same model.
#Lake area & Perimeter; We will use Area:Perimeter ratio instead.
#Mean depth & Max depth ; We will keep Mean depth as it has a potentially more meaningful impact on host-parasite ecology.
#DO & pH + Cond & pH; We will keep pH as it has a potentially more meaningful impact on host-parasite ecology.

### Relationships ----

lake.temp <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Temp.L, prev_fish))
lake.turb <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Turb.L, prev_fish))
lake.pH <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(pH.L, prev_fish))
lake.DO <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(DO.L, prev_fish))
lake.cond <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Cond.L, prev_fish))
lake.TOC <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(TOC.L, prev_fish))
lake.TN <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(TN.L, prev_fish))
lake.TP <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(TP.L, prev_fish))
lake.centrar <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Centrarchids.L, prev_fish))
lake.SR <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Species_richness.L, prev_fish))
lake.diversity <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Diversity.L, prev_fish))
lake.Mdepth <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Mean_depth, prev_fish))
lake.Xdepth <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Max_depth, prev_fish))
lake.Area <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Lake_area, prev_fish))
lake.Peri <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Perimeter, prev_fish))
lake.WRT <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(WRT, prev_fish))
lake.DA <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Drainage_area, prev_fish))
lake.elevation <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Elevation, prev_fish))
lake.connect <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Connectivity, prev_fish))

relationships.lake <- plot_grid(lake.temp, lake.turb, lake.pH, lake.DO, lake.cond, lake.TOC, lake.TN, lake.TP, lake.centrar, lake.SR, lake.diversity, lake.Mdepth, lake.Xdepth, lake.Area, lake.Peri, lake.WRT, lake.DA, lake.elevation, lake.connect,
                                 ncol = 4, nrow = 5)
#Relationships don't necessarly suggest linear patterns...

ggsave(paste0(to.figs, "Relationships_lake.pdf"), plot = relationships.lake, dpi = 500, width = 20, height = 10) #Saving plot grid

# ---- Variables corrections ----
## Data manipulations ----

#Creating new subsrtate variables with PCA axis
substrate.vars <- trans.corr.all %>% 
  select_("Silt", "Sand", "Rock", "Block")

substrate.rda <- rda(substrate.vars, scale = FALSE) 
summary(substrate.rda)
biplot(substrate.rda, scaling = 1)
biplot(substrate.rda, scaling = 2)

site.scores <- scores(substrate.rda, choices = c(1,2), display= "sites", tidy = FALSE) #Extracting axis 
sub1 <- site.scores[,1] #PCA1 - sub1 explains most of the variation in percentage of silt & rock
sub2 <- site.scores[,2] #PCA2 - sub2 explains most of the variation in percentage of block

#Ajusting data frame
mod.data <- ParaSpaceMod %>% 
  mutate(TN_TP.T = TN.T / TP.T) %>% relocate(TN_TP.T, .after = "TOC.T") %>% #Creating TN:TP ratio for transect scale
  mutate(TN_TP.L = TN.L /TP.L) %>% relocate(TN_TP.L, .after = "TOC.L") %>%  #Creating TN:TP ratio for lake scale
  mutate(Area_Perimeter = (Lake_area*1000000/Perimeter)) %>% relocate(Area_Perimeter, .before = "Mean_depth") %>%  #Creating Area:Perimeter ratio
  mutate(Sub1 = sub1) %>% relocate(Sub1, .before = "Macrophyte") %>% #Adding new substrate variables
  mutate(Sub2 = sub2)  %>% relocate(Sub2, .before = "Macrophyte")

mod.data <- within(mod.data, rm("Silt", "Sand", "Rock", "Block")) #Deleting original substrate variables
mod.data <- mod.data[-2,] #Deleting ACHI2 as prevalence & Diversity index cannot be calculated on absence of fish

## Verifications ----
#Rerunning correlation matrix
trans.corr.adj <- mod.data %>% #Transect scale
  select_("Sub1", "Sub2", "Macrophyte", "Depth", "Trunk", 
          "Temp.T", "Cond.T", "DO.T", "Turb.T", "pH.T", 
          "TOC.T", "TN.T", "TP.T", "TN_TP.T",
          "Lake_area", "Perimeter", "Mean_depth", "Max_depth", "WRT", "Area_Perimeter",
          "Drainage_area", "Elevation", "Connectivity", 
          "Centrarchids.T", "Species_richness.T", "Diversity.T")

rquery.cormat(trans.corr.adj, type = "full")

lake.corr.adj <- mod.data %>% #Lake scale
  select_("Temp.L", "Cond.L", "DO.L", "Turb.L", "pH.L", 
          "TOC.L", "TN.L", "TP.L", "TN_TP.L",
          "Lake_area", "Perimeter", "Mean_depth", "Max_depth", "WRT", "Area_Perimeter",
          "Drainage_area", "Elevation", "Connectivity", 
          "Centrarchids.L", "Species_richness.L", "Diversity.L")

rquery.cormat(lake.corr.adj, type = "full")
#No new collinearity problems detected
#Defenitly less collinearity for data at transect scale

#Running outliers for new variables
pdf(paste0(to.figs, "Outliers_newvars.pdf"), width = 20, height = 15) #Transect scale

par(mfrow = c(3, 2), mar = c(3, 3, 3, 1))
dotchart(mod.data$TN_TP.T, main = "TN_TP.T", group = as.factor(mod.data$Lake))
dotchart(mod.data$TN_TP.L, main = "TN_TP.L", group = as.factor(ParaSpaceMod$Lake))
dotchart(mod.data$Area_Perimeter, main = "AREA:PERIMETER", group = as.factor(mod.data$Lake))
dotchart(mod.data$Sub1, main = "SUB1", group = as.factor(mod.data$Lake))
dotchart(mod.data$Sub2, main = "SUB2", group = as.factor(mod.data$Lake))

dev.off()
#Manipulations doesn't correct all of the outliers
#Achigan is still an outlier in Area:Perimeter ratio

#Running relationships for new variables
trans.TNTP <- ggplot(data = mod.data) + 
  geom_point(aes(TN_TP.T, prev_fish))
lake.TNTP <- ggplot(data = mod.data) + 
  geom_point(aes(TN_TP.L, prev_fish))
lake.AREAPERI <- ggplot(data = mod.data) + 
  geom_point(aes(Area_Perimeter, prev_fish))
trans.SUB1 <- ggplot(data = mod.data) + 
  geom_point(aes(Sub1, prev_fish))
trans.SUB2 <- ggplot(data = mod.data) + 
  geom_point(aes(Sub2, prev_fish))

relationships.newvars <- plot_grid(trans.TNTP, lake.TNTP, lake.AREAPERI, trans.SUB1, trans.SUB2,
                                ncol = 2, nrow = 3)

ggsave(paste0(to.figs, "Relationships_newvars.pdf"), plot = relationships.newvars, dpi = 500, width = 20, height = 10) #Saving plot grid

## Saving new data set ----

write.csv(mod.data, paste0(to.output, "ModelAnalysis_Df.csv"), row.names = FALSE) #This data frame is ready and cleaned for model analysis

