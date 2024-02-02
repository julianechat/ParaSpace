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
to.rédaction <- "./rédaction/"

## Loading packages & functions ----

library(dplyr)
library(ggplot2)
library(vegan)
library(cowplot)

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
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$TOC.T, main = "TOC", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TN.T, main = "TN", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TP.T, main = "TP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TN_TP.T, main = "TN:TP", group = as.factor(ParaSpaceMod$Lake))

#No outliers

#Morphometry
par(mfrow = c(3, 2), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Lake_area, main = "LAKE AREA", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Perimeter, main = "PERIMETER", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Area_Perimeter, main = "AREA:PERIMETER", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$MeanDepth.lake, main = "MEAN DEPTH", group = as.factor(ParaSpaceMod$Lake)) #nned to correct the .x
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
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Species_richness.T, main = "RICHNESS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Diversity.T, main = "DIVERSITY", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$tot_Cyprinidae, main = "NON HOST ABUNDANCE", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$tot_fish, main = "COMMUNITY ABUNDANCE", group = as.factor(ParaSpaceMod$Lake))
#ACHI2 is an outlier for Diversity - This data should be excluded as diversity based one fish absence is unrelevant

#Habitat
par(mfrow = c(4, 2), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Silt, main = "SILT", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Sand, main = "SAND", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Rock, main = "ROCK", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Metric_block, main = "BLOCK", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Macrophyte, main = "MACROPHYTE", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$MeanDepth.site, main = "DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Trunk, main = "TRUNK", group = as.factor(ParaSpaceMod$Lake))
#ACHI1 is an outlier for Sand
#CORN4 is an oulier for Depth & Trunk

#All
pdf(paste0(to.figs, "Outliers_Trans.pdf"), width = 20, height = 15)

par(mfrow = c(5, 6), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$prev_fish, main = "PREV_FISH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Temp.T, main = "TEMP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Cond.T, main = "COND", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$DO.T, main = "DO", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Turb.T, main = "TURB", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$pH.T, main = "PH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TOC.T, main = "TOC", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TN.T, main = "TN", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TP.T, main = "TP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TN_TP.T, main = "TN:TP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Lake_area, main = "LAKE AREA", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Perimeter, main = "PERIMETER", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Area_Perimeter, main = "AREA:PERIMETER", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$MeanDepth.lake, main = "MEAN DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Max_depth, main = "MAX DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Drainage_area, main = "DRAINAGE AREA", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$WRT, main = "WRT", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Elevation, main = "ELEVATION", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Connectivity, main = "CONNECTIVITY", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Species_richness.T, main = "RICHNESS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Diversity.T, main = "DIVERSITY", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$tot_Cyprinidae, main = "NON HOST ABUNDANCE", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$tot_fish, main = "COMMUNITY ABUNDANCE", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Silt, main = "SILT", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Sand, main = "SAND", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Rock, main = "ROCK", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Metric_block, main = "BLOCK", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Macrophyte, main = "MACROPHYTE", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$MeanDepth.site, main = "DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Trunk, main = "TRUNK", group = as.factor(ParaSpaceMod$Lake))

dev.off()

### Collinearity ----
#### Correlation matrix ----

par(mfrow = c(1, 1), mar = c(3, 3, 3, 1))
trans.corr.all <- ParaSpaceMod %>% 
  select("Silt", "Sand", "Rock", "Metric_block", "Macrophyte", "MeanDepth.site", "Trunk", 
          "Temp.T", "Cond.T", "DO.T", "Turb.T", "pH.T", 
          "TOC.T", "TN.T", "TP.T", "TN_TP.T",
          "Lake_area", "Perimeter", "Area_Perimeter", "MeanDepth.lake", "Max_depth", "WRT", 
          "Drainage_area", "Elevation", "Connectivity",
          "Species_richness.T", "Diversity.T", "tot_fish", "tot_Cyprinidae")

pdf(paste0(to.figs, "Corrplot_Trans.pdf"), width = 20, height = 15)
rquery.cormat(trans.corr.all, type = "full")
dev.off()

pdf(paste0(to.rédaction, "Support_information/FigureS1.pdf"), width = 20, height = 15)
rquery.cormat(trans.corr.all, type = "full")
dev.off()

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

pdf(paste0(to.figs, "KmeansPCA_Trans.pdf"), width = 20, height = 15)

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
trans.TNTP <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(TN_TP.T, prev_fish))
trans.Mdepth <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(MeanDepth.lake, prev_fish))
trans.Xdepth <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Max_depth, prev_fish))
trans.Area <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Lake_area, prev_fish))
trans.Peri <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Perimeter, prev_fish))
trans.AreaPeri <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Area_Perimeter, prev_fish))
trans.WRT <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(WRT, prev_fish))
trans.DA <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Drainage_area, prev_fish))
trans.elevation <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Elevation, prev_fish))
trans.connect <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Connectivity, prev_fish))
trans.SR <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Species_richness.T, prev_fish))
trans.NonHost <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(tot_Cyprinidae, prev_fish))
trans.TotFish <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(tot_fish, prev_fish))
trans.diversity <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Diversity.T, prev_fish))
trans.silt <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Silt, prev_fish))
trans.sand <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Sand, prev_fish))
trans.rock <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Rock, prev_fish))
trans.block <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Metric_block, prev_fish))
trans.macro <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Macrophyte, prev_fish))
trans.trunk <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Trunk, prev_fish))
trans.depth <- ggplot(data = ParaSpaceMod) +
  geom_point(aes(MeanDepth.site, prev_fish))

relationships.trans <- plot_grid(trans.temp, trans.turb, trans.pH, trans.DO, trans.cond, trans.TN, trans.TP, trans.TNTP, trans.TOC, trans.Mdepth, trans.Area, trans.Peri, trans.AreaPeri, trans.WRT, trans.DA, trans.elevation, trans.connect, trans.SR, trans.NonHost, trans.TotFish, trans.diversity, trans.macro, trans.sand, trans.silt, trans.rock, trans.block, trans.trunk, trans.depth,
          ncol = 6, nrow = 5)
#Relationships don't always suggest linear patterns

ggsave(paste0(to.figs, "Relationships_Trans.pdf"), plot = relationships.trans, dpi = 500, width = 20, height = 10) #Saving plot grid

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
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$TOC.L, main = "TOC", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TN.L, main = "TN", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TP.L, main = "TP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TN_TP.L, main = "TN_TP", group = as.factor(ParaSpaceMod$Lake))
#No outliers

#Biotic
par(mfrow = c(2, 1), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Species_richness.L, main = "RICHNESS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Diversity.L, main = "DIVERSITY", group = as.factor(ParaSpaceMod$Lake))
#Triton is an outlier for Diversity

#All
pdf(paste0(to.figs, "Outliers_Lake.pdf"), width = 20, height = 15)

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
dotchart(ParaSpaceMod$TN_TP.L, main = "TN:TP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Species_richness.L, main = "RICHNESS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Diversity.L, main = "DIVERSITY", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Lake_area, main = "LAKE AREA", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Perimeter, main = "PERIMETER", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Area_Perimeter, main = "AREA:PERIMETER", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$MeanDepth.lake, main = "MEAN DEPTH", group = as.factor(ParaSpaceMod$Lake))
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
  select( "Temp.L", "Cond.L", "DO.L", "Turb.L", "pH.L", 
          "TOC.L", "TN.L", "TP.L", "TN_TP.L",
          "Lake_area", "Perimeter", "Area_Perimeter", "MeanDepth.lake", "Max_depth", "WRT", 
          "Drainage_area", "Elevation", "Connectivity", 
          "Species_richness.L", "Diversity.L")

pdf(paste0(to.figs, "Corrplot_Lake.pdf"), width = 20, height = 15)
rquery.cormat(lake.corr.all, type = "full")
dev.off()

#### Ordination ----

lake.rda.data <- data.frame(lake.corr.all, row.names = ParaSpaceMod$Transect_ID)

lake.rda <- rda(lake.rda.data, scale = TRUE) #Data must be scaled because variables have different units
summary(lake.rda)

par(mfrow = c(1, 1), mar = c(3, 3, 3, 1)) #Visualization of rda plots
biplot(lake.rda, scaling = 1)
biplot(lake.rda, scaling = 2)

#Incorporating k-means groups to rda
lake.groups <- cascadeKM(dist(scale(lake.rda.data)), 2, 4, criterion = "ssi")
plot(lake.groups)

lake.grKM <- as.vector(lake.groups$partition[,3]) #Extracting groups to add on rda plot
lake.grKM <- as.factor(lake.grKM)
col.groups <- c("yellowgreen","forestgreen","orange","dodgerblue")

pdf(paste0(to.figs, "KmeansPCA_Lake.pdf"), width = 20, height = 15)

plot(lake.rda, scaling = 1, type = "n", main = "PCA + k-means cluster") #Visualization of k-means groups
with(lake.rda.data, points(lake.rda, display = "sites", col = col.groups[lake.grKM], scaling = 1, pch = 21, bg = col.groups[lake.grKM]))
arrows(0, 0, scores(lake.rda, scaling = 1 )$species[,1], scores(lake.rda, scaling = 1)$species[,2], col="black", code = 2, length = 0.05)
ordiellipse(lake.rda, groups = lake.grKM, display = "sites", conf = 0.95, scaling = 1, col = col.groups, lwd = 2)
text(scores(lake.rda, scaling = 1)$sites, row.names(lake.rda.data), cex=0.5,pos=3, col="black")
text(lake.rda, display = "species", scaling = 1, cex = 0.5, col = "black", pos = 2)

dev.off()

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
lake.TNTP <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(TN_TP.L, prev_fish))
lake.SR <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Species_richness.L, prev_fish))
lake.diversity <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Diversity.L, prev_fish))
lake.Mdepth <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(MeanDepth.lake, prev_fish))
lake.Xdepth <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Max_depth, prev_fish))
lake.Area <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Lake_area, prev_fish))
lake.Peri <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Perimeter, prev_fish))
lake.AreaPeri <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Area_Perimeter, prev_fish))
lake.WRT <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(WRT, prev_fish))
lake.DA <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Drainage_area, prev_fish))
lake.elevation <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Elevation, prev_fish))
lake.connect <- ggplot(data = ParaSpaceMod) + 
  geom_point(aes(Connectivity, prev_fish))

relationships.lake <- plot_grid(lake.temp, lake.turb, lake.pH, lake.DO, lake.cond, lake.TOC, lake.TN, lake.TP, lake.TNTP, lake.SR, lake.diversity, lake.Mdepth, lake.Xdepth, lake.Area, lake.Peri, lake.AreaPeri, lake.WRT, lake.DA, lake.elevation, lake.connect,
                                 ncol = 4, nrow = 5)
#Relationships don't necessarly suggest linear patterns...

ggsave(paste0(to.figs, "Relationships_Lake.pdf"), plot = relationships.lake, dpi = 500, width = 20, height = 10) #Saving plot grid

# ---- Variables corrections ----

mod.data <- ParaSpaceMod[-2,] #Deleting ACHI2 as prevalence & Diversity index cannot be calculated on absence of fish

## Saving new data set ----

mod.data$Lake <- as.factor(mod.data$Lake)
mod.data$Transect_ID <- as.factor(mod.data$Transect_ID)
mod.data$Watershed <- as.factor(mod.data$Watershed)
write.csv(mod.data, paste0(to.output, "ModelAnalysis_Df.csv"), row.names = FALSE) #This data frame is ready and cleaned for model analysis

