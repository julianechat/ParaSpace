## Candidates models analysis ##

# ----- R Setup ----- #

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"

# ----- Loading packages and functions ----- #

library(corrplot)
library(dplyr)
library(tidyr)
library(vegan)
library(lme4)
library(MuMIn)
library(performance)
library(glmmTMB)
library(AICcmodavg)  

source(paste0(to.R, "rquery.cormat.R"))

# ----- Loading data ----- #

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))
TransectData <- read.csv(paste0(to.output, "Transects_WideData.csv"))
Lakes_Caracteristics <- read.csv(paste0(to.data, "Lakes_Caracteristics.csv"), sep=";")
Trans_Biotic <- read.csv(paste0(to.output, "Trans_BioticData.csv"))

# ------------------------ #

#### ----- Lake scale analysis ----- ####
LakesName <- Lakes_Caracteristics$Lake
lake_scale <- CombinedData %>% filter(!Sampling_method == "Transect") #Dropping transect data as we chose to not use this data at lake scale

lake.exp.data <- lake_scale %>% 
  group_by(Lake) %>% 
  select(c(1, 43:50, 51:62)) %>% 
  distinct() 

lake.comm.matrix <- CombinedData %>% 
  group_by(Lake) %>% 
  select(starts_with("tot") | starts_with("inf")) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))

lake_scale <- data.frame(lake.comm.matrix, lake.exp.data)[-36] 

## Exploration of variables ##
# Response variables #
lake.fish.tot <- lake.comm.matrix %>% select(starts_with("tot")) %>% rowSums()
lake.fish.inf <- lake.comm.matrix %>% select(starts_with("inf")) %>% rowSums()

lake.prev.fish = (lake.fish.inf) / (lake.fish.tot)
lake.prev.LeGi = (lake.comm.matrix$inf_LeGi) / (lake.comm.matrix$tot_LeGi)
lake.prev.df = as.data.frame(cbind(Lake = LakesName, Prev_fish = lake.prev.fish, Prev_LeGi = lake.prev.LeGi))

# Correlation between explicative variables #
lake.corr <- lake.exp.data[c(2:9, 11:18)]
rquery.cormat(lake.corr, type = "full")

# Ordination #
lake.physico.vars <- data.frame((lake.corr), row.names = LakesName) #We omitted biotic variables because they contained many 0's and NA (Evenness of Tracy lake) cannot be considered in rda. 
lake.rda <- rda(lake.physico.vars, scale = TRUE) #Data must be scaled because variables have different units
summary(lake.rda)
biplot(lake.rda, scaling = 1)
biplot(lake.rda, scaling = 2)

# k-means groups #
lake.physico.groups <- kmeans(lake.physico.vars, centers = 4, iter.max = 4)
lake.physico.groups

lake.physico.groups2 <- cascadeKM(dist(scale(lake.physico.vars)), 2, 4, criterion = "ssi")
lake.physico.groups2
plot(lake.physico.groups2)

lake.grKM <- as.vector(lake.physico.groups2$partition[,3]) #Extracting groups to add on rda plot
lake.grKM <- as.factor(lake.grKM)
col.groups <- c("yellowgreen","forestgreen","orange","dodgerblue")

png(filename = paste0(to.figs, "PCA_clusters.png"))
PCA_clusters <- plot(lake.rda, scaling = 1, type = "n", main = "PCA + k-means cluster")
with(lake.physico.vars, points(lake.rda, display = "sites", col = col.groups[lake.grKM], scaling = 1, pch = 21, bg = col.groups[lake.grKM]))
text(scores(lake.rda, scaling = 1)$sites, row.names(lake.physico.vars), cex=0.7,pos=3, col="black")
text(lake.rda, display = "species", scaling = 1, cex = 0.5, col = "black", pos = 2)
arrows(0, 0, scores(lake.rda, scaling = 1 )$species[,1], scores(lake.rda, scaling = 1)$species[,2], col="black", code = 2, length = 0.05)
ordiellipse(lake.rda, groups = lake.grKM, display = "sites", conf = 0.95, scaling = 1, col = col.groups, lwd = 2)

## Variables adjustments ##
lake_scale2 <- lake_scale %>% 
  mutate("TN_TP" = (TN/TP), .keep = "unused") %>% #New column for TN:TP ratio to correct for high correlation between variables
  mutate("Area_Perimeter" = (Lake_area*1000000/as.numeric(Perimeter)), .keep = "unused") #New column for lake_area:lake_perimeter ratio to correct for high correlation between variables

lake_scale2 <- within(lake_scale2, rm("Temp", "Max_depth", "WRT")) #Dropping variables that will not be use in any models

lake.corr2 <- lake_scale2[c(36:40, 42:45, 49, 50)]
rquery.cormat(lake.corr2, type = "full")

# Scaling data #
lake.scale.exp <- lake_scale2[-c(2:35)] %>% relocate("Watershed", .after = "Lake") #Scaling explicative data
lake.scale.exp <- as.data.frame(scale(lake.scale.exp[-c(1,2)]))

lake.mod.data <- cbind(Lake = LakesName, Watershed = lake_scale$Watershed, lake.scale.exp, tot_LeGi = lake_scale$tot_LeGi, inf_LeGi = lake_scale$inf_LeGi, tot_fish = lake.fish.tot, inf_fish = lake.fish.inf)
lake.mod.data <- lake.mod.data[-c(2, 10, 13, 14),] #Omitting lakes with no LeGi

## -- LeGi models -- ##
hist(lake.mod.data$inf_LeGi/lake.mod.data$tot_LeGi) #Not normally distributed
shapiro.test(lake.mod.data$inf_LeGi/lake.mod.data$tot_LeGi)

#We use binomial distribution because our response variable is a proportion
#We model no interaction between explicative variables for interpretation simplicity & because we do not want fitted values
#We don't model watershed as random factor as 5 out of 6 watershed contains only one sampled lake
#We model beta-binomial regression to account for overdispersion (see test.bin.R)
#We use cloglog as link function because we assume that prevalence in an infection context is to follow an asymmetric function
lake.LeGi.nul <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ 1, data = lake.mod.data, family = betabinomial(link = "cloglog"))
summary(lake.LeGi.nul)

# -- Nutrients model -- #
hist(lake.mod.data$TN_TP) #Normally distributed
hist(lake.mod.data$TOC) #Normally distributed

cor(lake.mod.data$TN_TP, lake.mod.data$TOC) #Colinearity does not cause problem

lake.LeGi.nutrients <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC, data = lake.mod.data, family = betabinomial(link = "cloglog"))
summary(lake.LeGi.nutrients) #TN_TP & TOC both negatively correlated to Prev_LeGi. TOC > TN_TP

# -- Biotic model -- #
hist(lake.mod.data$Centrarchids) #Not normally distributed
hist(lake.mod.data$Species_richness) #Normally distributed
hist(lake.mod.data$Diversity) #Normally distributed

cor(lake.mod.data$Centrarchids, lake.mod.data$Species_richness) #Colinearity does not cause problem
cor(lake.mod.data$Centrarchids, lake.mod.data$Diversity) #Colinearity does not cause problem
cor(lake.mod.data$Species_richness, lake.mod.data$Diversity) #Colinearity does not cause problem

lake.LeGi.biotic <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Centrarchids + Species_richness + Diversity, data = lake.mod.data, family = betabinomial(link = "cloglog"))
summary(lake.LeGi.biotic) #Centrarchids, Evenness & Species_Richness positively correlated. Species_Richness > Centrarchids > Evenness

# -- Space model -- #
hist(lake.mod.data$Drainage_area) #Not normally distributed
hist(lake.mod.data$Elevation) #Normally distributed
hist(lake.mod.data$Connectivity) #Normally distributed

cor(lake.mod.data$Drainage_area, lake.mod.data$Elevation) #Colinearity does not cause problem
cor(lake.mod.data$Elevation, lake.mod.data$Connectivity) #Colinearity does not cause problem
cor(lake.mod.data$Connectivity, lake.mod.data$Drainage_area) #Colinearity does not cause problem

lake.LeGi.space <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Drainage_area + Elevation + Connectivity, data = lake.mod.data, family = betabinomial(link = "cloglog"))
summary(lake.LeGi.space) #Drainage_area & Elevation negatively correlated to prevalence, Connectivity positively correlated. Connectivity > Drainage_area > Elevation
#Drainage_area is significant in the model !

# -- Morphometric model -- #
hist(lake.mod.data$Area_Perimeter) #Not normally distributed
hist(lake.mod.data$Mean_depth) #Normally distributed

cor(lake.mod.data$Area_Perimeter, lake.mod.data$Mean_depth) #Colinearity does not cause problem

lake.LeGi.morpho <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Area_Perimeter + Mean_depth, data = lake.mod.data, family = betabinomial(link = "cloglog"))
summary(lake.LeGi.morpho) #Area_Perimeter & Mean_depth positively correlated to prevalence. Mean_depth > Area_Perimeter

# -- Physico-chemistry model -- #
hist(lake.mod.data$Cond) #Not normally distributed
hist(lake.mod.data$Turb) #Not normally distributed
hist(lake.mod.data$pH) #Normally distributed
hist(lake.mod.data$DO) #Normally distributed

cor(lake.mod.data$Cond, lake.mod.data$Turb)  #Colinearity does not cause problem
cor(lake.mod.data$Cond, lake.mod.data$pH)  #Colinearity causes problem
cor(lake.mod.data$Cond, lake.mod.data$DO) #Colinearity does not cause problem
cor(lake.mod.data$Turb, lake.mod.data$pH) #Colinearity does not cause problem
cor(lake.mod.data$Turb, lake.mod.data$DO) #Colinearity does not cause problem
cor(lake.mod.data$pH, lake.mod.data$DO)  #Colinearity causes problem

#Omitting Cond & DO because of colinarity problems with pH who is a variable of big interest 
lake.LeGi.physico <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Turb + pH, data = lake.mod.data, family = betabinomial(link = "cloglog"))
summary(lake.LeGi.physico) #pH is positively correlated while turbidity is negatively correlated. pH > Turb

# -- Lake LeGi models comparaison -- #
lake.LeGi.sel <- model.sel(lake.LeGi.nul, lake.LeGi.biotic, lake.LeGi.morpho, lake.LeGi.nutrients, lake.LeGi.physico, lake.LeGi.space)
lake.LeGi.sel <- lake.LeGi.sel[, c("df", "logLik", "AICc", "delta")]  

## -- Fish models -- ##
hist(lake.mod.data$inf_fish/lake.mod.data$tot_fish) #Normally distributed
shapiro.test(lake.mod.data$inf_fish/lake.mod.data$tot_fish) 

lake.fish.nul <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ 1, data = lake.mod.data, family = betabinomial(link = "cloglog"))
summary(lake.fish.nul)

# -- Nutrients model -- #
lake.fish.nutrients <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ TN_TP + TOC, data = lake.mod.data, family = betabinomial(link = "cloglog"))
summary(lake.fish.nutrients) #TN_TP & TOC negatively correlated to prevalence. TOC > TN_TP

# -- Biotic model --#
lake.fish.biotic <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ Centrarchids + Species_richness + Diversity, data = lake.mod.data, family = betabinomial(link = "cloglog"))
summary(lake.fish.biotic) #Centrarchids & Species_richness positively correlated while Evenness negatively correlated. Centrarchids > Species_Richness > Evenness

# -- Space model -- #
lake.fish.space <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ Drainage_area + Connectivity + Elevation, data = lake.mod.data, family = betabinomial(link = "cloglog"))
summary(lake.fish.space) #Connectivity positively correlated while Drainage_area & Elevation negatively correlated. Connectivity > Drainage_area > Elevation

# -- Morphometric model -- #
lake.fish.morpho <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ Area_Perimeter + Mean_depth, data = lake.mod.data, family = betabinomial(link = "cloglog"))
summary(lake.fish.morpho) #Area_Perimeter & Mean_depth both positively correlated to prevalence. Area_Perimeter > Mean_depth

# -- Physico-chemistry model -- #
lake.fish.physico <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ pH + Turb, data = lake.mod.data, family = betabinomial(link = "cloglog"))
summary(lake.fish.physico) #Turb negatively correlated while pH positively correlated. pH > Turb

# -- Lake fish models comparaison -- #
lake.fish.sel <- model.sel(lake.fish.nul, lake.fish.biotic, lake.fish.morpho, lake.fish.nutrients, lake.fish.physico, lake.fish.space)
lake.fish.sel <- lake.fish.sel[, c("df", "logLik", "AICc", "delta")]  

# -- Lake models comparaison -- #
lake.sel <- model.sel(lake.fish.nul, lake.fish.biotic, lake.fish.morpho, lake.fish.nutrients, lake.fish.physico, lake.fish.space, lake.LeGi.nul, lake.LeGi.biotic, lake.LeGi.morpho, lake.LeGi.nutrients, lake.LeGi.physico, lake.LeGi.space)
lake.sel[, c("df", "logLik", "AICc", "delta")]  

#### ----- Transect scale analysis ----- ####
trans_scale <- merge(TransectData, Lakes_Caracteristics, by.x = "Lake")
trans_scale <- merge(trans_scale, Trans_Biotic, by.x = "Transect_ID") 
TransectsName <- levels(TransectData$Transect_ID)

## Exploration of variables ##
# Response variables #
trans.fish.tot <- trans_scale %>% 
  select(starts_with("tot")) %>% 
  rowSums()

trans.fish.inf <- trans_scale %>% 
  select(starts_with("inf")) %>% 
  rowSums()

trans.prev.fish <- (trans.fish.inf) / (trans.fish.tot)
trans.prev.LeGi <- (trans_scale$inf_LeGi) / (trans_scale$tot_LeGi)
trans.prev.df <- as.data.frame(cbind(Transect = TransectsName, Prev_fish = trans.prev.fish, Prev_LeGi = trans.prev.LeGi))

hist(trans.prev.fish)
hist(trans.prev.LeGi)

# Correlation between explicative variables #
trans.corr <- trans_scale[c(19:33,35:42)]
rquery.cormat(trans.corr, type = "full")

# Ordination #
trans.physico.vars <- data.frame(trans.corr, row.names = trans_scale$Transect_ID) #Omitting biotic variables because they contained many 0's.
trans.physico.vars <- drop_na(trans.physico.vars) #Dropping lakes with no transects

trans.rda <- rda(trans.physico.vars, scale = TRUE) #Data must be scaled because variables have different units
summary(trans.rda)
biplot(trans.rda, scaling = 1)
biplot(trans.rda, scaling = 2)

# k-means groups #
trans.physico.groups <- kmeans(trans.physico.vars, centers = 4, iter.max = 4) #Quoi la différence entre les deux méthodes k-means??
trans.physico.groups

trans.physico.groups2 <- cascadeKM(dist(scale(trans.physico.vars)), 2, 4, criterion = "ssi")
plot(trans.physico.groups2)

trans.grKM <- as.vector(trans.physico.groups2$partition[,3]) #Extracting groups to add on rda plot
trans.grKM <- as.factor(trans.grKM)
col.groups <- c("yellowgreen","forestgreen","orange","dodgerblue")

plot(trans.rda, scaling = 1, type = "n", main = "PCA + k-means cluster")
with(trans.physico.vars, points(trans.rda, display = "sites", col = col.groups[trans.grKM], scaling = 1, pch = 21, bg = col.groups[trans.grKM]))
arrows(0, 0, scores(trans.rda, scaling = 1 )$species[,1], scores(trans.rda, scaling = 1)$species[,2], col="black", code = 2, length = 0.05)
ordiellipse(trans.rda, groups = trans.grKM, display = "sites", conf = 0.95, scaling = 1, col = col.groups, lwd = 2)
text(scores(trans.rda, scaling = 1)$sites, row.names(trans.physico.vars), cex=0.5,pos=3, col="black")
text(trans.rda, display = "species", scaling = 1, cex = 0.5, col = "black", pos = 2)

## Variables adjustments ##
trans_scale2 <- trans_scale %>% 
  mutate("TN_TP" = (TN/TP), .keep = "unused") %>% #New column for TN:TP ratio to correct for high correlation between variables
  mutate("Area_Perimeter" = (Lake_area*1000000/Perimeter), .keep = "unused") #New column for lake_area:lake_perimeter ratio to correct for high correlation between variables

trans_scale2 <- within(trans_scale2, rm("Temp", "Max_depth", "WRT")) #Dropping variables that will not be use in any models

trans.corr2 <- trans_scale2[c(19:30,32:35)]
rquery.cormat(trans.corr2, type = "full")

# Reduce substrate to PCA axis #
substrate.vars <- trans.corr2[c(1:4)]
substrate.vars <- na.omit(substrate.vars)

substrate.rda <- rda(substrate.vars, scale = FALSE) #Data must be scaled because variables have different units
summary(substrate.rda)
biplot(substrate.rda, scaling = 1)
biplot(substrate.rda, scaling = 2)

site.scores <- scores(substrate.rda, choices = c(1,2), display= "sites", tidy = FALSE) #Extracting axis 
sub1 <- site.scores[,1] #PCA1
sub2 <- site.scores[,2] #PCA2

trans_scale2 <- trans_scale2 %>% 
  mutate(Sub1 = sub1) %>% 
  mutate(Sub2 = sub2)

# Scaling data #
trans.scale.exp <- trans_scale2[c(19:30, 32:42)] #Scaling explicative data
trans.scale.exp <- as.data.frame(scale(trans.scale.exp))

trans.mod.data <- cbind(Transect_ID = trans_scale$Transect_ID, Lake = trans_scale$Lake, trans.scale.exp, inf_fish = trans.fish.inf, tot_fish = trans.fish.tot, inf_LeGi = trans_scale$inf_LeGi, tot_LeGi = trans_scale$tot_LeGi)

## -- LeGi Models -- ##
hist(trans.mod.data$inf_LeGi/trans.mod.data$tot_LeGi) #Not noramlly distributed
shapiro.test(trans.mod.data$inf_LeGi/trans.mod.data$tot_LeGi)

trans.LeGi.nul <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ 1, data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.LeGi.nul)

#Variation of resids by lake
random <- resid(trans.LeGi.nul)
plot(random ~ as.factor(trans.mod.data$Lake),
     xlab = "Lake", ylab = "Standardized residuals")
abline(0, 0, lty = 2)
#Lake should be include as a random factor in the models

trans.LeGi.nul2 <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ 1 + (1 | Lake), data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.LeGi.nul2)

#Variation of resids by lake
random <- resid(trans.LeGi.nul2)
plot(random ~ as.factor(trans.mod.data$Lake),
     xlab = "Lake", ylab = "Standardized residuals")
abline(0, 0, lty = 2)
#Much better

# -- Nutrients model -- # 
hist(trans.mod.data$TN_TP) #Normally distributed
hist(trans.mod.data$TOC) #Not normally distributed

cor(trans.mod.data$TN_TP, trans.mod.data$TOC) #Colinearity does not cause problem

trans.LeGi.nutrients <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC, data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.LeGi.nutrients) #TN_TP positively while TOC negatively correlated to prevalence. TN_TP > TOC

trans.LeGi.nutrients2 <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC + (1|Lake), data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.LeGi.nutrients2)

trans.LeGi.nutrients3 <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC + (1|Lake/Transect_ID), data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.LeGi.nutrients3)

# -- Biotic model -- #
hist(trans.mod.data$Centrarchids) #Not normally distributed
hist(trans.mod.data$Species_richness) #Not normally distributed
hist(trans.mod.data$Diversity) #Normally distributed (barely)

cor(trans.mod.data$Centrarchids, trans.mod.data$Species_richness) #Colinearity does not cause problem
cor(trans.mod.data$Centrarchids, trans.mod.data$Diversity)
cor(trans.mod.data$Species_richness, trans.mod.data$Diversity)

trans.LeGi.biotic <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Centrarchids + Species_richness + Diversity, data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.LeGi.biotic) #Centrarchids & Species_Richness positively correlated while Evenness negatively correlated to prevalence. Centrarchids > Species_Richness > Evenness

trans.LeGi.biotic2 <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Centrarchids + Species_richness + Diversity + (1|Lake), data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.LeGi.biotic2)

trans.LeGi.biotic3 <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Centrarchids + Species_richness + Diversity + (1|Lake/Transect_ID), data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.LeGi.biotic3)

# -- Morphometric model -- #
hist(trans.mod.data$Area_Perimeter) #Not normally distributed
hist(trans.mod.data$Mean_depth) #Not normally distributed

cor(trans.mod.data$Area_Perimeter, trans.mod.data$Mean_depth) #Colinearity does not cause problem

trans.LeGi.morpho <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Area_Perimeter + Mean_depth, data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.LeGi.morpho) #Area_Perimeter positively correlated while Mean_depth negatively correlated. Area_Perimeter > Mean_depth

trans.LeGi.morpho2 <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Area_Perimeter + Mean_depth + (1|Lake), data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.LeGi.morpho2)

# -- Space model -- #
hist(trans.mod.data$Elevation) #Not normally distributed
hist(trans.mod.data$Connectivity) #Not normally distributed
hist(trans.mod.data$Drainage_area) #Not normally distributed

cor(trans.mod.data$Elevation, trans.mod.data$Connectivity) #Colinearity does not cause problem
cor(trans.mod.data$Elevation, trans.mod.data$Drainage_area) #Colinearity does not cause problem
cor(trans.mod.data$Drainage_area, trans.mod.data$Connectivity) #Colinearity does not cause problem

trans.LeGi.space <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Elevation + Drainage_area + Connectivity, data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.LeGi.space) #Elevation & Drainage are both negatively correlated while Connectivity positively correlated to prevalence. Elevation > Connectivity > Drainage_area

trans.LeGi.space2 <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Elevation + Drainage_area + Connectivity + (1|Lake), data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.LeGi.space2)

# -- Physico-chemistry model -- #
hist(trans.mod.data$pH) #Not normally distributed
hist(trans.mod.data$DO) #Not normally distributed
hist(trans.mod.data$Turb) #Not normally distributed
hist(trans.mod.data$Cond) #Not normally distributed
     
cor(trans.mod.data$pH, trans.mod.data$DO) #Colinearity does cause problem
cor(trans.mod.data$pH, trans.mod.data$Turb) #Colinearity does not cause problem
cor(trans.mod.data$pH, trans.mod.data$Cond) #Colinearity does cause problem
cor(trans.mod.data$DO, trans.mod.data$Turb) #Colinearity does not cause problem
cor(trans.mod.data$DO, trans.mod.data$Cond) #Colinearity does not cause problem
cor(trans.mod.data$Turb, trans.mod.data$Cond) #Colinearity does not cause problem

#Omitting DO & Cond has they are strongly correlated to pH (and we assume that pH is a very important variable)
trans.LeGi.physico <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ pH + Turb, data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.LeGi.physico) #DO & Turb are negatively correlated while pH & Cond are positively correlated. pH > Cond > Turb > DO

trans.LeGi.physico2 <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ pH + Turb + (1|Lake), data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.LeGi.physico2)

# -- Habitat model -- #
hist(trans.mod.data$Macrophyte) #Not normally distributed
hist(trans.mod.data$Trunk) #Not normally distributed
hist(trans.mod.data$Depth) #Normally distributed
hist(trans.mod.data$Sub1)
hist(trans.mod.data$Sub2) 

cor(trans.mod.data$Macrophyte, trans.mod.data$Trunk) 
cor(trans.mod.data$Macrophyte, trans.mod.data$Depth)
cor(trans.mod.data$Trunk, trans.mod.data$Depth)

trans.LeGi.habitat <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ sub1 + sub2 + Macrophyte + Trunk + Depth, data = trans.mod.data, family = betabinomial("cloglog"))
summary(trans.LeGi.habitat)

trans.LeGi.habitat2 <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ sub1 + sub2 + Macrophyte + Trunk + Depth + (1|Lake), data = trans.mod.data, family = betabinomial("cloglog"))
summary(trans.LeGi.habitat2)

# -- LeGi models comparaison -- #
trans.LeGi.sel1 <- model.sel(trans.LeGi.nul, trans.LeGi.biotic, trans.LeGi.morpho, trans.LeGi.nutrients, trans.LeGi.physico, trans.LeGi.space, trans.LeGi.habitat)
trans.LeGi.sel1 <- trans.LeGi.sel1[, c("df", "logLik", "AICc", "delta")]  

trans.LeGi.sel2 <- model.sel(trans.LeGi.nul2, trans.LeGi.biotic2, trans.LeGi.morpho2, trans.LeGi.nutrients2, trans.LeGi.physico2, trans.LeGi.space2, trans.LeGi.habitat2)
trans.LeGi.sel2 <- trans.LeGi.sel2[, c("df", "logLik", "AICc", "delta")]  

## Fish models ##
hist(trans.mod.data$inf_fish / trans.mod.data$tot_fish)
shapiro.test(trans.mod.data$inf_fish/trans.mod.data$tot_fish) #Not normally distributed

trans.fish.nul <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ 1, data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.fish.nul)

trans.fish.nul2 <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ 1 + (1|Lake), data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.fish.nul2)

# -- Nutrient model -- #
trans.fish.nutrients <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ TN_TP + TOC, data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.fish.nutrients)

trans.fish.nutrients2 <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ TN_TP + TOC + (1|Lake), data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.fish.nutrients2)

# -- Biotic model -- #
trans.fish.biotic <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ Centrarchids + Species_richness + Diversity, data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.fish.biotic)

trans.fish.biotic2 <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ Centrarchids + Species_richness + Diversity + (1|Lake), data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.fish.biotic2)

# -- Morphometric model -- #
trans.fish.morpho <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ Area_Perimeter + Mean_depth, data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.fish.morpho)

trans.fish.morpho2 <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ Area_Perimeter + Mean_depth + (1|Lake), data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.fish.morpho2)

# -- Space model -- #
trans.fish.space <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ Elevation + Drainage_area + Connectivity, data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.fish.space)

trans.fish.space2 <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ Elevation + Drainage_area + Connectivity + (1|Lake), data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.fish.space2)

# -- PhysicoChemistry model -- #
trans.fish.physico <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ pH + Turb, data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.fish.physico)

trans.fish.physico2 <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ pH + Turb + (1|Lake), data = trans.mod.data, family = betabinomial(link = "cloglog"))
summary(trans.fish.physico2)

# -- Habitat model -- #
trans.fish.habitat <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ sub1 + sub2 + Macrophyte + Trunk + Depth, data = trans.mod.data, family = betabinomial("cloglog"))
summary(trans.fish.habitat)

trans.fish.habitat2 <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ sub1 + sub2 + Macrophyte + Trunk + Depth + (1|Lake), data = trans.mod.data, family = betabinomial("cloglog"))
summary(trans.LeGi.habitat2)

# -- Trans fish models comparaison -- # 
trans.fish.sel <- model.sel(trans.fish.nul, trans.fish.biotic, trans.fish.morpho, trans.fish.nutrients, trans.fish.physico, trans.fish.space, trans.fish.habitat)
trans.fish.sel <- trans.fish.sel[, c("df", "logLik", "AICc", "delta")]  

trans.fish.sel2 <- model.sel(trans.fish.nul2, trans.fish.biotic2, trans.fish.morpho2, trans.fish.nutrients2, trans.fish.physico2, trans.fish.space2, trans.fish.habitat2)
trans.fish.sel2 <- trans.fish.sel2[, c("df", "logLik", "AICc", "delta")]  

# -- Trans models comparaison -- #
trans.sel <- model.sel(trans.fish.nul, trans.fish.biotic, trans.fish.morpho, trans.fish.nutrients, trans.fish.physico, trans.fish.space, trans.fish.habitat, trans.LeGi.nul, trans.LeGi.biotic, trans.LeGi.morpho, trans.LeGi.nutrients, trans.LeGi.physico, trans.LeGi.space, trans.LeGi.habitat)
trans.sel[, c("df", "logLik", "AICc", "delta")]  

# Comparaison avec BIC

BIC.trans.fish.sel <- list(trans.fish.nul2, trans.fish.biotic2, trans.fish.morpho2, trans.fish.nutrients2, trans.fish.physico2, trans.fish.space2, trans.fish.habitat2)
BIC.trans.fish.sel <- bictab(BIC.trans.fish.sel, 
       modnames = c("trans.fish.nul2", "trans.fish.biotic2", "trans.fish.morpho2", "trans.fish.nutrients2", "trans.fish.physico2", "trans.fish.space2", "trans.fish.habitat2"))

BIC.trans.LeGi.sel <- list(trans.LeGi.nul2, trans.LeGi.biotic2, trans.LeGi.morpho2, trans.LeGi.nutrients2, trans.LeGi.physico2, trans.LeGi.space2, trans.LeGi.habitat2)
BIC.trans.LeGi.sel <- bictab(BIC.trans.LeGi.sel, 
       modnames = c("trans.LeGi.nul2", "trans.LeGi.biotic2", "trans.LeGi.morpho2", "trans.LeGi.nutrients2", "trans.LeGi.physico2", "trans.LeGi.space2", "trans.LeGi.habitat2"))

BIC.lake.fish.sel <- list(lake.fish.nul, lake.fish.biotic, lake.fish.morpho, lake.fish.nutrients, lake.fish.physico, lake.fish.space)
BIC.lake.fish.sel <- bictab(BIC.lake.fish.sel, 
       modnames = c("lake.fish.nul", "lake.fish.biotic", "lake.fish.morpho", "lake.fish.nutrients", "lake.fish.physico", "lake.fish.space"))

BIC.lake.LeGi.sel <- list(lake.LeGi.nul, lake.LeGi.biotic, lake.LeGi.morpho, lake.LeGi.nutrients, lake.LeGi.physico, lake.LeGi.space)
BIC.lake.LeGi.sel <- bictab(BIC.lake.LeGi.sel, 
       modnames = c("lake.LeGi.nul", "lake.LeGi.biotic", "lake.LeGi.morpho", "lake.LeGi.nutrients", "lake.LeGi.physico", "lake.LeGi.space"))

## Test models on mean transect data ##
View(trans_scale2)

trans.exp.data <- trans_scale2 %>% select(Lake, Transect_ID, Turb, pH, TOC, TN_TP, Sub1, Sub2, Macrophyte, Trunk, Depth, Mean_depth, Drainage_area, Elevation, Connectivity, Centrarchids, Species_richness, Diversity, Area_Perimeter)
trans.exp.data <- trans.exp.data %>% group_by(Lake) %>% summarise(across(.cols = everything(), mean))
trans.exp.data <- trans.exp.data[c(3:19)] %>% scale()

trans.legi.prev <- trans_scale2 %>% select(Lake, inf_LeGi, tot_LeGi)
trans.legi.prev <- trans.legi.prev %>% group_by(Lake) %>% summarise(across(.cols = everything(), sum))

trans.fish.prev <- trans_scale2[c(2, 9:18)]
trans.fish.1 <- trans.fish.prev[c(2:6)] %>% rowSums()
trans.fish.2 <- trans.fish.prev[c(7:11)] %>% rowSums()
trans.fish.prev <- cbind(tot_fish = trans.fish.1, inf_fish = trans.fish.2)
blop <- trans_scale2[c(1,2)]

trans.fish.prev <- cbind(blop, trans.fish.prev)
trans.fish.prev <- trans.fish.prev %>% group_by(Lake) %>% summarise(across(.cols = inf_fish | tot_fish, sum))
trans.mean.data <- cbind(trans.exp.data, trans.legi.prev, trans.fish.prev)

## FISH ##
trans.mean.fish.nul <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ 1, data = trans.mean.data, family = betabinomial(link = "cloglog"))
summary(trans.mean.fish.nul)

# -- Nutrient model -- #
trans.mean.fish.nutrients <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ TN_TP + TOC, data = trans.mean.data, family = betabinomial(link = "cloglog"))
summary(trans.mean.fish.nutrients)

# -- Biotic model -- #
trans.mean.fish.biotic <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ Centrarchids + Species_richness + Diversity, data = trans.mean.data, family = betabinomial(link = "cloglog"))
summary(trans.mean.fish.biotic)

# -- Morphometric model -- #
trans.mean.fish.morpho <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ Area_Perimeter + Mean_depth, data = trans.mean.data, family = betabinomial(link = "cloglog"))
summary(trans.mean.fish.morpho)

# -- Space model -- #
trans.mean.fish.space <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ Elevation + Drainage_area + Connectivity, data = trans.mean.data, family = betabinomial(link = "cloglog"))
summary(trans.mean.fish.space)

# -- PhysicoChemistry model -- #
trans.mean.fish.physico <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ pH + Turb, data = trans.mean.data, family = betabinomial(link = "cloglog"))
summary(trans.mean.fish.physico)

# -- Habitat model -- #
trans.mean.fish.habitat <- glmmTMB(cbind(inf_fish, tot_fish - inf_fish) ~ Sub1 + Sub2 + Macrophyte + Trunk + Depth, data = trans.mean.data, family = betabinomial("cloglog"))
summary(trans.mean.fish.habitat)

trans.mean.sel <- model.sel(trans.mean.fish.nul, trans.mean.fish.biotic, trans.mean.fish.morpho, trans.mean.fish.nutrients, trans.mean.fish.physico, trans.mean.fish.space, trans.mean.fish.habitat)
trans.mean.sel[, c("df", "logLik", "AICc", "delta")]  

##LEGI##

trans.mean.LeGi.nul <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ 1, data = trans.mean.data, family = betabinomial(link = "cloglog"))
summary(trans.mean.LeGi.nul)

# -- Nutrient model -- #
trans.mean.LeGi.nutrients <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC, data = trans.mean.data, family = betabinomial(link = "cloglog"))
summary(trans.mean.LeGi.nutrients)

# -- Biotic model -- #
trans.mean.LeGi.biotic <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Centrarchids + Species_richness + Diversity, data = trans.mean.data, family = betabinomial(link = "cloglog"))
summary(trans.mean.LeGi.biotic)

# -- Morphometric model -- #
trans.mean.LeGi.morpho <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Area_Perimeter + Mean_depth, data = trans.mean.data, family = betabinomial(link = "cloglog"))
summary(trans.mean.LeGi.morpho)

# -- Space model -- #
trans.mean.LeGi.space <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Elevation + Drainage_area + Connectivity, data = trans.mean.data, family = betabinomial(link = "cloglog"))
summary(trans.mean.LeGi.space)

# -- PhysicoChemistry model -- #
trans.mean.LeGi.physico <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ pH + Turb, data = trans.mean.data, family = betabinomial(link = "cloglog"))
summary(trans.mean.LeGi.physico)

# -- Habitat model -- #
trans.mean.LeGi.habitat <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ Sub1 + Sub2 + Macrophyte + Trunk + Depth, data = trans.mean.data, family = betabinomial("cloglog"))
summary(trans.mean.LeGi.habitat)

trans.mean.sel2 <- model.sel(trans.mean.LeGi.nul, trans.mean.LeGi.biotic, trans.mean.LeGi.morpho, trans.mean.LeGi.nutrients, trans.mean.LeGi.physico, trans.mean.LeGi.space, trans.mean.LeGi.habitat)
trans.mean.sel2[, c("df", "logLik", "AICc", "delta")]  
