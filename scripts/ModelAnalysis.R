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

ParaSpaceMod <- read.csv(paste0(to.output, "Transects_Lake_Data.csv"))

# ---- Data exploration ----

## Transect scale ----
### Outliers ----
#Prevalences
par(mfrow = c(1, 1), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$prev_fish, main = "PREV_FISH", group = as.factor(ParaSpaceMod$Lake))
#No outliers.

#PhysicoChemistry
par(mfrow = c(3, 2), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Temp.T, main = "TEMP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Cond.T, main = "COND", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$DO.T, main = "DO", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Turb.T, main = "TURB", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$pH.T, main = "PH", group = as.factor(ParaSpaceMod$Lake))
#No outliers.

#Nutrients
par(mfrow = c(3, 1), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$TOC.T, main = "TOC", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TN.T, main = "TN", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TP.T, main = "TP", group = as.factor(ParaSpaceMod$Lake))

#Morphometry
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Lake_area, main = "LAKE AREA", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Perimeter, main = "PERIMETER", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Mean_depth, main = "MEAN DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Max_depth, main = "MAX DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$WRT, main = "WRT", group = as.factor(ParaSpaceMod$Lake))
#Achigan is an outlier for lake area & perimeter.

#Space
par(mfrow = c(3, 1), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Drainage_area, main = "DRAINAGE AREA", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Elevation, main = "ELEVATION", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Connectivity, main = "CONNECTIVITY", group = as.factor(ParaSpaceMod$Lake))
#Achigan is an outlier for drainage area.

#Biotic
par(mfrow = c(3, 1), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Centrarchids.T, main = "CENTRARCHIDS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Species_richness.T, main = "RICHNESS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Diversity.T, main = "DIVERSITY", group = as.factor(ParaSpaceMod$Lake))
#No big ouliers.

#Habitat
par(mfrow = c(4, 2), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Silt, main = "SILT", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Sand, main = "SAND", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Rock, main = "ROCK", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Block, main = "BLOCK", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Macrophyte, main = "MACROPHYTE", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Depth, main = "DEPTH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Trunk, main = "TRUNK", group = as.factor(ParaSpaceMod$Lake))
#ACHI1 is an outlier for Sand, CORN4 is an oulier for Depth & Trunk.

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
trans.corr <- ParaSpaceMod[c(7:14, 16, 18, 20, 22, 24, 26, 28, 30:38, 40, 42)]
rquery.cormat(trans.corr, type = "full")

#Where does it cause problem ? 
#TN & TP; We could do TN:TP ratio instead.
#Drainage area & Perimeter + Drainage area & Lake_area; Not in the same model.
#Lake_area & Perimeter; We could do Area:Perimeter ratio instead.
#Mean_depth & Max_depth; We will keep Mean_depth as it is more important for littoral fish communities than max_depth.
#Cond & pH + pH & DO; We will keep only keep pH as it has a potentially strong effect on parasite or snail population.
#Silt & Rock; We will do a PCA and extract axis as all substrate variables are a fraction of 100%.

#Creating new subsrtate variables with PCA axis
substrate.vars <- trans.corr[c(1:4)]

substrate.rda <- rda(substrate.vars, scale = FALSE) 
summary(substrate.rda)
biplot(substrate.rda, scaling = 1)
biplot(substrate.rda, scaling = 2)

site.scores <- scores(substrate.rda, choices = c(1,2), display= "sites", tidy = FALSE) #Extracting axis 
sub1 <- site.scores[,1] #PCA1 - sub1 explains most of the variation in percentage of silt & rock.
sub2 <- site.scores[,2] #PCA2 - sub2 explains most of the variation in percentage of block.

#Adjusting data frame
mod.data <- ParaSpaceMod %>% 
  mutate(TN_TP.T = TN.T / TP.T, .keep = "unused") %>% relocate(TN_TP.T, .after = "TOC.T") %>% 
  mutate(Area_Perimeter = (Lake_area*1000000/Perimeter), .keep = "unused") %>% relocate(Area_Perimeter, .before = "Mean_depth") %>% 
  mutate(Sub1 = sub1) %>% relocate(Sub1, .before = "Macrophyte") %>% 
  mutate(Sub2 = sub2)  %>% relocate(Sub2, .before = "Macrophyte")

mod.data <- within(mod.data, rm("DO.T", "Max_depth", "Cond.T", "Silt", "Sand", "Rock", "Block"))

#Rerunning corr matrix
trans.corr2 <- mod.data[c(7:12, 16, 18, 20, 21, 25:31, 33, 35)]
rquery.cormat(trans.corr2, type = "full") 
#Drainage_area & Area_Perimeter are still correlated but they will not be used in the same model.
#No more collinearity problems detected.

#Rerunning outliers
pdf(paste0(to.figs, "Outliers_trans_2.pdf"), width = 20, height = 15)

par(mfrow = c(4, 5), mar = c(3, 3, 3, 1))
dotchart(mod.data$prev_fish, main = "PREV_FISH", group = as.factor(mod.data$Lake))
dotchart(mod.data$Temp.T, main = "TEMP", group = as.factor(mod.data$Lake))
dotchart(mod.data$Turb.T, main = "TURB", group = as.factor(mod.data$Lake))
dotchart(mod.data$pH.T, main = "PH", group = as.factor(mod.data$Lake))
dotchart(mod.data$TOC.T, main = "TOC", group = as.factor(mod.data$Lake))
dotchart(mod.data$TN_TP.T, main = "TN_TP", group = as.factor(mod.data$Lake))
dotchart(mod.data$Area_Perimeter, main = "AREA:PERIMETER", group = as.factor(mod.data$Lake))
dotchart(mod.data$Mean_depth, main = "MEAN DEPTH", group = as.factor(mod.data$Lake))
dotchart(mod.data$Drainage_area, main = "DRAINAGE AREA", group = as.factor(mod.data$Lake))
dotchart(mod.data$WRT, main = "WRT", group = as.factor(mod.data$Lake))
dotchart(mod.data$Elevation, main = "ELEVATION", group = as.factor(mod.data$Lake))
dotchart(mod.data$Connectivity, main = "CONNECTIVITY", group = as.factor(mod.data$Lake))
dotchart(mod.data$Centrarchids.T, main = "CENTRARCHIDS", group = as.factor(mod.data$Lake))
dotchart(mod.data$Species_richness.T, main = "RICHNESS", group = as.factor(mod.data$Lake))
dotchart(mod.data$Diversity.T, main = "DIVERSITY", group = as.factor(mod.data$Lake))
dotchart(mod.data$Sub1, main = "SUB1", group = as.factor(mod.data$Lake))
dotchart(mod.data$Sub2, main = "SUB2", group = as.factor(mod.data$Lake))
dotchart(mod.data$Macrophyte, main = "MACROPHYTE", group = as.factor(mod.data$Lake))
dotchart(mod.data$Trunk, main = "TRUNK", group = as.factor(mod.data$Lake))
dotchart(mod.data$Depth, main = "DEPTH", group = as.factor(mod.data$Lake))

dev.off()
#Manipulations doesn't correct all of the ouliers

#### Ordination ----
trans.rda.data <- data.frame(trans.corr2, row.names = ParaSpaceMod$Transect_ID)

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

pdf(paste0(to.figs, "KmeansPCA_trans.pdf"), width = 20, height = 15)

plot(trans.rda, scaling = 1, type = "n", main = "PCA + k-means cluster")
with(trans.rda.data, points(trans.rda, display = "sites", col = col.groups[trans.grKM], scaling = 1, pch = 21, bg = col.groups[trans.grKM]))
arrows(0, 0, scores(trans.rda, scaling = 1 )$species[,1], scores(trans.rda, scaling = 1)$species[,2], col="black", code = 2, length = 0.05)
ordiellipse(trans.rda, groups = trans.grKM, display = "sites", conf = 0.95, scaling = 1, col = col.groups, lwd = 2)
text(scores(trans.rda, scaling = 1)$sites, row.names(trans.rda.data), cex=0.5,pos=3, col="black")
text(trans.rda, display = "species", scaling = 1, cex = 0.5, col = "black", pos = 2)

dev.off()

### Relationships ----
trans.temp <- ggplot(data = mod.data) + 
  geom_point(aes(Temp.T, prev_fish))
trans.turb <- ggplot(data = mod.data) + 
  geom_point(aes(Turb.T, prev_fish))
trans.pH <- ggplot(data = mod.data) + 
  geom_point(aes(pH.T, prev_fish))
trans.TOC <- ggplot(data = mod.data) + 
  geom_point(aes(TOC.T, prev_fish))
trans.TNTP <- ggplot(data = mod.data) + 
  geom_point(aes(TN_TP.T, prev_fish))
trans.Mdepth <- ggplot(data = mod.data) + 
  geom_point(aes(Mean_depth, prev_fish))
trans.AP <- ggplot(data = mod.data) + 
  geom_point(aes(Area_Perimeter, prev_fish))
trans.WRT <- ggplot(data = mod.data) + 
  geom_point(aes(WRT, prev_fish))
trans.DA <- ggplot(data = mod.data) + 
  geom_point(aes(Drainage_area, prev_fish))
trans.elevation <- ggplot(data = mod.data) + 
  geom_point(aes(Elevation, prev_fish))
trans.connect <- ggplot(data = mod.data) + 
  geom_point(aes(Connectivity, prev_fish))
trans.centrar <- ggplot(data = mod.data) + 
  geom_point(aes(Centrarchids.T, prev_fish))
trans.SR <- ggplot(data = mod.data) + 
  geom_point(aes(Species_richness.T, prev_fish))
trans.diversity <- ggplot(data = mod.data) + 
  geom_point(aes(Diversity.T, prev_fish))
trans.sub1 <- ggplot(data = mod.data) + 
  geom_point(aes(Sub1, prev_fish))
trans.sub2 <- ggplot(data = mod.data) + 
  geom_point(aes(sub2, prev_fish))
trans.macro <- ggplot(data = mod.data) + 
  geom_point(aes(Macrophyte, prev_fish))
trans.trunk <- ggplot(data = mod.data) + 
  geom_point(aes(Trunk, prev_fish))
trans.depth <- ggplot(data = mod.data) +
  geom_point(aes(Depth, prev_fish))

relationships.trans <- plot_grid(trans.temp, trans.turb, trans.pH, trans.TOC, trans.Mdepth, trans.TNTP, trans.AP, trans.WRT, trans.DA, trans.elevation, trans.connect, trans.centrar, trans.SR, trans.diversity, trans.macro, trans.sub1, trans.sub2, trans.trunk, trans.depth,
          ncol = 4, nrow = 5)
#Relationships don't always suggest linear patterns...

ggsave(paste0(to.figs, "Relationships_trans.png"), plot = relationships.trans, dpi = 500, width = 20, height = 10)

## Lake scale ----
### Outliers ----

#We only present variable we reduced to lake mean as they might differ from the transect scale data exploration

#PhysicoChemistry
par(mfrow = c(3, 2), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Temp.L, main = "TEMP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Cond.L, main = "COND", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$DO.L, main = "DO", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Turb.L, main = "TURB", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$pH.L, main = "PH", group = as.factor(ParaSpaceMod$Lake))
#Cromwell is an outlier for DO.

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
#Triton is an outlier for diversity.

#All
pdf(paste0(to.figs, "Outliers_lake.pdf"), width = 20, height = 15)

par(mfrow = c(3, 4), mar = c(3, 3, 3, 1))
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

dev.off()
#No new outliers

### Collinearity ----
#### Correlation matrix ----
par(mfrow = c(1, 1), mar = c(3, 3, 3, 1))
lake.corr <- ParaSpaceMod[c(15, 17, 19, 21, 23, 25, 27, 29, 30:37, 39, 41, 43)]
rquery.cormat(lake.corr, type = "full")

#Where does it cause problem ? 
#TN & TP; We will use TN:TP ratio instead.
#Perimeter & Drainage area + Lake area & Drainage area; Not in the same model.
#Lake area & Perimeter; We will use Area:Perimeter ratio instead.
#Mean depth & Max depth ; We will keep Mean depth as it has a potentially more meaningful impact on host-parasite ecology.
#DO & pH + Cond & pH; We will keep pH as it has a potentially more meaningful impact on host-parasite ecology.

#Adjusting data frame
mod.data <- mod.data %>% 
  mutate(TN_TP.L = TN.L / TP.L, .keep = "unused") %>% relocate(TN_TP.L, .after = "TOC.L")

mod.data <- within(mod.data, rm("DO.L", "Cond.L"))

#Rerunning outliers with new variables
pdf(paste0(to.figs, "Outliers_lake_2.pdf"), width = 20, height = 15)

par(mfrow = c(5, 2), mar = c(3, 3, 3, 1))
dotchart(ParaSpaceMod$Temp.L, main = "TEMP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Turb.L, main = "TURB", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$pH.L, main = "PH", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TOC.L, main = "TOC", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TN.L, main = "TN", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$TP.L, main = "TP", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Centrarchids.L, main = "CENTRARCHIDS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Species_richness.L, main = "RICHNESS", group = as.factor(ParaSpaceMod$Lake))
dotchart(ParaSpaceMod$Diversity.L, main = "DIVERSITY", group = as.factor(ParaSpaceMod$Lake))

dev.off()
#Manipulations doesn't correct all of the oulier in diversity (Triton)

### Relationships ----

lake.temp <- ggplot(data = mod.data) + 
  geom_point(aes(Temp.L, prev_fish))
lake.turb <- ggplot(data = mod.data) + 
  geom_point(aes(Turb.L, prev_fish))
lake.pH <- ggplot(data = mod.data) + 
  geom_point(aes(pH.L, prev_fish))
lake.TOC <- ggplot(data = mod.data) + 
  geom_point(aes(TOC.L, prev_fish))
lake.TNTP <- ggplot(data = mod.data) + 
  geom_point(aes(TN_TP.L, prev_fish))
lake.centrar <- ggplot(data = mod.data) + 
  geom_point(aes(Centrarchids.L, prev_fish))
lake.SR <- ggplot(data = mod.data) + 
  geom_point(aes(Species_richness.L, prev_fish))
lake.diversity <- ggplot(data = mod.data) + 
  geom_point(aes(Diversity.L, prev_fish))

relationships.lake <- plot_grid(lake.temp, lake.turb, lake.pH, lake.TOC, lake.TNTP, lake.centrar, lake.SR, lake.diversity,
                                 ncol = 2, nrow = 4)
#Relationships don't necessarly suggest linear patterns...

ggsave(paste0(to.figs, "Relationships_lake.png"), plot = relationships.lake, dpi = 500, width = 20, height = 10)

# ---- Data analysis ----

## Testing method on nutrient model ##
library(lme4)
library(performance)
library(glmmTMB)
library(MASS)
library(aod)
library(mgcv)
library(gamlss)
library(gratia)

### linear models ###
test.glm <- glm(cbind(inf_fish, tot_fish-inf_fish) ~ TOC.T * TN_TP.T, family = binomial, data = mod.data)
summary(test.glm)
#All variables significant
#AIC = 2438.5
check_overdispersion(test.glm)
#Overdispersion detected

test1.RI.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1|Lake), family = binomial, data = mod.data)
summary(test1.RI.glmm)
#Suggest rescaling values - large eigenvalue ratio
#All variables significative
#AIC = 1043.7
check_overdispersion(test1.RI.glmm)
overdisp_fun(test1.RI.glmm)
#Overdispersion detected

test2.RI.glmm <- glmer(cbind(inf_fish, tot_fish-inf_fish) ~ TOC.T * scale(TN_TP.T) + (1|Lake), family = binomial, data = mod.data)
summary(test2.RI.glmm)
#All significative
#AIC = 1073.3
check_overdispersion(test2.RI.glmm)
overdisp_fun(test2.RI.glmm)
#Overdispersion detected

test3.RI.glmm <- glmer(cbind(inf_fish, tot_fish-inf_fish) ~ TOC.T * log(TN_TP.T) + (1|Lake), family = binomial, data = mod.data)
summary(test3.RI.glmm)
#All significative
#AIC = 1043.7
check_overdispersion(test3.RI.glmm)
overdisp_fun(test3.RI.glmm)
#Overdispersion detected

mod.data2 <- mod.data[-2,] #This model doesn't take NA values
test4.RI.glmm <- glmmPQL(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T, random = ~1|Lake, data = mod.data2, family = quasibinomial)
summary(test4.RI.glmm)
#No significative at all
#AIC = NA
#Ce genre de modèle prend en compte la sudispersion (pas besoin de regarder phi).

test5.RI.glmm <- glmmPQL(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T, random = ~1|Watershed/Lake, data = mod.data2, family = quasibinomial)
summary(test5.RI.glmm)
#No significative at all
#AIC = NA

test6.RI.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1|Watershed/Lake), family = binomial, data = mod.data)
summary(test6.RI.glmm)
#Suggest rescaling values - large eigenvalue ratio
#All variables significative
#AIC = 1074.0
check_overdispersion(test6.RI.glmm)
overdisp_fun(test6.RI.glmm)
#Overdispersion detected

test7.RI.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1|Lake/Transect_ID), family = binomial, data = mod.data)
summary(test7.RI.glmm)
#Suggest rescaling values - large eigenvalue ratio
#All variables unsignificatives
#AIC = 338.6
check_overdispersion(test7.RI.glmm)
overdisp_fun(test7.RI.glmm)
#Test non concluant
#Donne excatement le même AIC que betabin2 ~Lake....

test1.RIS.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1 + TOC.T|Lake), family = binomial, data = mod.data)
summary(test1.RIS.glmm)
#Significant
#Need to rescale
#AIC = 868.6
overdisp_fun(test1.RIS.glmm)
check_overdispersion(test1.RIS.glmm)
#Overdispersion detected

test2.RIS.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1 + TN_TP.T|Lake), family = binomial, data = mod.data)
summary(test2.RIS.glmm)
#Significant
#Need to rescale
#AIC = 670.5
overdisp_fun(test2.RIS.glmm)
check_overdispersion(test2.RIS.glmm)
#Overdispersion decteted

test3.RIS.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ TOC.T * TN_TP.T + (1 + TN_TP.T|Lake) + (1 + TOC.T|Lake), family = binomial, data = mod.data)
summary(test3.RIS.glmm)#No better
#Not significant 
#AIC = 602.0 
overdisp_fun(test3.RIS.glmm)
check_overdispersion(test3.RIS.glmm)
#Overdispersion decteted

test.RE.glmm <- glmer(cbind(inf_fish, tot_fish - inf_fish) ~ 1 + (1|Lake), family = binomial, data = mod.data)
summary(test.RE.glmm)
#AIC = 1101.5
overdisp_fun(test.RE.glmm)
check_overdispersion(test.RE.glmm)

test1.betabin <- betabin(cbind(inf_fish, tot_fish - inf_fish) ~ TN_TP.T + TOC.T, ~1, data = mod.data, link = "logit")
summary(test1.betabin)
#AIC = 343.1
#Aucun significatif
#Pas de surdispersion

test2.betabin <- betabin(cbind(inf_fish, tot_fish - inf_fish) ~ TN_TP.T + TOC.T, ~Lake, data = mod.data, link = "logit")
summary(test2.betabin)
#AIC = 338.7 (mais AICc supérieur)
#Aucun significatif
#Pas de surdispersion

test3.betabin <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ TN_TP.T * TOC.T + (1|Lake), family = betabinomial, data = mod.data)
summary(test3.betabin)
#AIC = 334
#Aucun significatif
#Que veut dire dispersion parameter?

### GAM ###
test1.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T), family = binomial, data = mod.data, method = "REML")
summary(test1.gam)
#Donne adj. R-sq (1.114) et deviance explained (27.4%)
#Significatif
plot(test1.gam)

test2.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr"), family = binomial, data = mod.data, method = "REML")
summary(test2.gam)
#Adj. R-sq = 0.13
#Deviance explained = 28.4%
plot(test2.gam)

test3.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(TOC.T, bs = "cr"), family = binomial, data = mod.data, method = "REML")
summary(test3.gam)
#Adj. R-sq = 0.625
#Deviance explained = 75.4%
plot(test3.gam)
check_overdispersion(test3.gam)

test4.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(TOC.T, bs = "cr"), family = quasibinomial, data = mod.data, method = "REML")
summary(test4.gam)
#Adj. R-sq = 0.536
#Deviance explained = 57.1%
#Est-ce qu'il faut regarder sur dispersion dans un gam ?
#REML bcp plus petit (mieux) que test3.gam
plot(test4.gam)
gam.check(test4.gam) #Interprétation ?
check_overdispersion(test4.gam) #Améliore un peu la sudispersion...
appraise(test4.gam, method = "simulate")

test5.gam <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), family = BB, data = mod.data2)
summary(test5.gam)
#Pas sur de coprendre la sortie
#AIC = 334.3 (comme betabin3)

test1.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~1, family = binomial, data = mod.data, method = "REML")
summary(test1.gamm)
#All significatives
#Adj. R-sq. = 0.302
#Deviance explained = 29.8

test2.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~Lake, family = binomial, data = mod.data, method = "REML")
summary(test2.gamm)
#All significatives
#Adj. R-sq. = 0.302
#Deviance explained = 29.8
#Exactement la même sortie...
check_overdispersion(test2.gamm)

test3.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~1, family = quasibinomial, data = mod.data, method = "REML")
summary(test3.gamm)
#TOC significatif
#Adj. R-sq. = 0.302
#Deviance explained = 29.8
#MAIIS REML beaucoup plus bas (donc better fit ?)

test4.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~Lake, family = quasibinomial, data = mod.data, method = "REML")
summary(test4.gamm)
#TOC significatif
#Adj. R-sq. = 0.302
#Deviance explained = 29.8
#Comme test3.gamm
check_overdispersion(test4.gamm)
#mmmh dispersion ratio encore plus haut que binomial sans effet aléatoire. Bizzare
gam.check(test4.gamm)
library(gratia)
appraise(test4.gamm, method = "simulate")

test5.gamm <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~Lake, family = BB, data = mod.data2)
summary(test5.gamm)
#No significatif
#AIC = 334.3
#Exactement même sortie que sans l'effet aléatoire (test5.gam)

