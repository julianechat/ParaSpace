## Script name : Multivariate analyses

## Authors : Juliane Vigneault
## Date created : October 16, 2024

## Copyright (c) Juliane Vigneault, 2024
## Email: Juliane.Vigneault@uqar.ca

# ---- Script setup ----

## R Setup ----

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"
to.carto <- "./carto/"
to.doc <- "./doc/"
to.rédaction <- "./rédaction/"

## Loading packages and functions ----

library(MASS)
library(dplyr)
library(vegan)
library(tidyverse)
library(stringr)

#source("~/Dropbox/ParaSpace/R/hcoplot.R")

## Loading data ----

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))

# ---- Data exploration ----

tot.matrix <- CombinedData %>% 
  select(starts_with("tot")) %>% 
  rename_with(~ str_remove(., "tot_"), everything()) %>% 
  na.omit()

## All species ----

#Distribution of all species confounded
tot.matrix <- as.matrix(tot.matrix)
tot.bar <- table(unlist(tot.matrix))
barplot <- barplot(tot.bar,
                   las = 1,
                   xlab = "Abundance class",
                   ylab = "Frequency",
                   col = gray(5:0/5))

#Number of absence
sum(tot.matrix == 0)

#Proportion of zeros in the community data set
sum(tot.matrix == 0)/(nrow(tot.matrix)*ncol(tot.matrix))

## Species ----

#Map of the sampling sites
spa.site <- CombinedData %>% 
  select(Site_longitude, Site_latitude)
plot(spa.site, asp=1, type = "n", main="Site Locations", xlab= "x coordinate", ylab = "y coordinate")
text(spa.site, row.names(spa.site), cex=0.8, col = "red")

#Map of the sampling lakes
spa.lake <- CombinedData %>% 
  select(Lake, Lake_longitude, Lake_latitude) %>%
  group_by(Lake) %>% 
  slice(1) %>% 
  ungroup()
spa.lake <- spa.lake %>% 
  column_to_rownames(var = "Lake")

plot(spa.lake, asp=1, type = "n", main="Lake Locations", xlab= "x coordinate", ylab = "y coordinate")
text(spa.lake, rownames(spa.lake), cex=0.8, col = "red")

#Species abundance visualization

lake.sp.matrix <- CombinedData %>% 
  select(Lake, starts_with("tot")) %>% 
  na.omit() %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))
lake.sp.matrix <- lake.sp.matrix %>% 
  column_to_rownames(var = "Lake") %>% 
  rename_with(~ str_remove(., "tot_"), everything())

par(mfrow=c(2,4))
plot(spa.lake, asp=1, col="brown", cex=lake.sp.matrix$LeGi, main = "Pumpkinseed sunfish")
plot(spa.lake, asp=1, col="brown", cex=lake.sp.matrix$AmRu, main = "Rock bass")
plot(spa.lake, asp=1, col="brown", cex=lake.sp.matrix$MiDo, main = "Smallmouth bass")
plot(spa.lake, asp=1, col="brown", cex=lake.sp.matrix$PeFl, main = "Yellow perch")
plot(spa.lake, asp=1, col="brown", cex=lake.sp.matrix$ChrosomusSpp., main = "Chrosomus spp.")
plot(spa.lake, asp=1, col="brown", cex=lake.sp.matrix$Cyprinidae, main = "Cyprinidae")
plot(spa.lake, asp=1, col="brown", cex=lake.sp.matrix$SeAt, main = "Creek chub")
plot(spa.lake, asp=1, col="brown", cex=lake.sp.matrix$AmNe, main = "Brown bullhead")

#Number of sites in which each species is present
spe.pres <- apply(lake.sp.matrix > 0, 2, sum)
spe.pres <- as.data.frame(spe.pres)
spe.freq <- as.data.frame(100*spe.pres/nrow(spe.pres))

par(mfrow=c(1,2))
sort(spe.pres$spe.pres)
hist(spe.pres$spe.pres, main = "Species occurence", right = FALSE, las = 1,
     xlab="Number of occurrences", ylab="Number of species",
     breaks = seq(0,30, by=5), col="bisque")
hist(spe.pres$spe.pres, main = "Species Relative Frequencies", right = FALSE, las = 1,
     xlab="Number of occurrences", ylab="Number of species",
     breaks = seq(0,100, by=10), col="bisque")
 
#Species richness
sit.pres <- apply(lake.sp.matrix > 0, 1, sum)
sort(sit.pres)

par(mfrow=c(1,2))
plot(sit.pres, type="s", las=1, col="gray", main="Species Richness")
text(sit.pres, row.names(lake.sp.matrix), cex=.8, col = "red")

plot(spa.lake, asp=1, main="map of species richness", pch=21, col = "white", bg="brown", cex=5*sit.pres/max(sit.pres))

#Diversity
N0 <- rowSums(lake.sp.matrix > 0) #Species richness
H <- diversity(lake.sp.matrix) #Shannon entropy
N1 <- exp(H) #Shannon diversity number
N2 <- diversity(lake.sp.matrix, "inv") #Simpson diversity
J <- H/log(N0) #Pielou evenness
E1 <- N1/N0 #Shannon evenness
E2 <- N2/N0 #Simpson evenness
div <- data.frame(N0, H, N1, N2, E1, E2, J)

#Transform into presence/absence
decostand(lake.sp.matrix, method = "pa")

## Environmental data

enviro <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% 
  select(Lake, Trunk, Silt, Sand, Rock, Boulder, Macrophyte, Site_depth, 
         Temperature, Conductivity, DO, Turbidity, pH,
         TOC, TN, TP, 
         Lake_area, Max_depth, Mean_depth, WRT, Drainage_area, Elevation, Perimeter, Connectivity) 
enviro.lake <- enviro %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), mean))
enviro.phys <- enviro.lake %>% 
  na.omit()

par(mfrow=c(3,3))
plot(spa.lake, asp=1, main="Trunk", pch=21, col="black", bg="darkblue", cex=5*enviro.phys$Trunk/max(enviro.phys$Trunk))
plot(spa.lake, asp=1, main="Silt", pch=21, col="black", bg="darkred", cex=5*enviro.phys$Silt/max(enviro.phys$Silt))
plot(spa.lake, asp=1, main="Sand", pch=21, col="black", bg="darkred", cex=5*enviro.phys$Sand/max(enviro.phys$Sand))
plot(spa.lake, asp=1, main="Rock", pch=21, col="black", bg="darkred", cex=5*enviro.phys$Rock/max(enviro.phys$Rock))
plot(spa.lake, asp=1, main="Boulder", pch=21, col="black", bg="darkred", cex=5*enviro.phys$Boulder/max(enviro.phys$Boulder))
plot(spa.lake, asp=1, main="Macrophyte", pch=21, col="black", bg="darkgreen", cex=5*enviro.phys$Macrophyte/max(enviro.phys$Macrophyte))
plot(spa.lake, asp=1, main="Site depth", pch=21, col="black", bg="violet", cex=5*enviro.phys$Site_depth/max(enviro.phys$Site_depth))

par(mfrow=c(3,2))
plot(spa.lake, asp=1, main="Temperature", pch=21, col="black", bg="darkred", cex=5*enviro.lake$Temperature/max(enviro.lake$Temperature))
plot(spa.lake, asp=1, main="Conductivity", pch=21, col="black", bg="darkgreen", cex=5*enviro.lake$Conductivity/max(enviro.lake$Conductivity))
plot(spa.lake, asp=1, main="DO", pch=21, col="black", bg="darkblue", cex=5*enviro.lake$DO/max(enviro.lake$DO))
plot(spa.lake, asp=1, main="Turbidity", pch=21, col="black", bg="darkorange", cex=5*enviro.lake$Turbidity/max(enviro.lake$Turbidity))
plot(spa.lake, asp=1, main="pH", pch=21, col="black", bg="violet", cex=5*enviro.lake$pH/max(enviro.lake$pH))

par(mfrow=c(1,3))
plot(spa.lake, asp=1, main="TOC", pch=21, col="black", bg="darkred", cex=5*enviro.lake$TOC/max(enviro.lake$TOC))
plot(spa.lake, asp=1, main="TN", pch=21, col="black", bg="darkorange", cex=5*enviro.lake$TN/max(enviro.lake$TN))
plot(spa.lake, asp=1, main="TP", pch=21, col="black", bg="darkblue", cex=5*enviro.lake$TP/max(enviro.lake$TP))

par(mfrow=c(4,2))
plot(spa.lake, asp=1, main="Lake area", pch=21, col="black", bg="darkblue", cex=5*enviro.lake$Lake_area/max(enviro.lake$Lake_area))
plot(spa.lake, asp=1, main="Max depth", pch=21, col="black", bg="darkred", cex=5*enviro.lake$Max_depth/max(enviro.lake$Max_depth))
plot(spa.lake, asp=1, main="Mean depth", pch=21, col="black", bg="darkgreen", cex=5*enviro.lake$Mean_depth/max(enviro.lake$Mean_depth))
plot(spa.lake, asp=1, main="WRT", pch=21, col="black", bg="darkorange", cex=5*enviro.lake$WRT/max(enviro.lake$WRT))
plot(spa.lake, asp=1, main="Darinage area", pch=21, col="black", bg="violet", cex=5*enviro.lake$Drainage_area/max(enviro.lake$Drainage_area))
plot(spa.lake, asp=1, main="Elevation", pch=21, col="black", bg="yellow4", cex=5*enviro.lake$Elevation/max(enviro.lake$Elevation))
plot(spa.lake, asp=1, main="Perimeter", pch=21, col="black", bg="brown1", cex=5*enviro.lake$Perimeter/max(enviro.lake$Perimeter))
plot(spa.lake, asp=1, main="Connectivity", pch=21, col="black", bg="lightblue", cex=5*enviro.lake$Connectivity/max(enviro.lake$Connectivity))

# ---- Agglomerative clustering ----

## Site clustering ----

agg.site <- CombinedData %>% 
  select(Sampling_ID, starts_with("tot")) %>% 
  rename_with(~ str_remove(., "tot_"), everything()) %>% 
  column_to_rownames(var = "Sampling_ID") %>% 
  na.omit()

norm.sp.site <- decostand(agg.site, "normalize")
euc.sp.site <- vegdist(norm.sp.site, "euc")

upgma.site <- hclust(euc.sp.site, method = "average")
par(mfrow=c(1,1))
plot(upgma.site)

## Lake clustering ----

norm.sp.lake <- decostand(lake.sp.matrix, "normalize")
euc.sp.lake <- vegdist(norm.sp.lake, "euc")

upgma.lake <- hclust(euc.sp.lake, method = "average")
plot(upgma.lake)

ward.lake <- hclust(euc.sp.lake, method = "ward.D2")
plot(ward.lake)
ward.lake$height <- sqrt(ward.lake$height) #Correct the distended look of the dendrogram
plot(ward.lake)

#Visualizing groups
k <- 4
spebc.ward <- cutree(ward.lake, k)
spe.chwo <- reorder(ward.lake, euc.sp.lake)
plot(spe.chwo, hang = -1, xlab = "4 groups", ylab = "height")
rect.hclust(spe.chwo, k=k)
hcoplot(ward.lake, euc.sp.lake, k=4)

plot(spa.lake, asp=1, type = "n", main = "Four Ward groups")
grw <- spebc.ward
k <- length(levels(factor(grw)))
for (i in 1:k) {
  points(spa.lake[grw==i,1], spa.lake[grw==i,2], pch=i+20, cex=3, col=i+1, bg=i+1)
}
text(spa.lake, row.names(spa.lake), cex=0.8, col = "black", font = 2)         
legend("bottomright", paste("Group", 1:k), pch = (1:k)+20, col=2:(k+1), pt.bg=2:(k+1), pt.cex=2, bty="n")

#k-means partitioning
spe.kmeans <- kmeans(norm.sp.lake, centers = 4, nstart = 100)
spe.KM.casade <- cascadeKM(norm.sp.lake, inf.gr = 2, sup.gr = 10, iter=100, criterion = "ssi")
plot(spe.KM.casade, sortg = TRUE)

plot(spa.lake, asp=1, type = "n", main = "Four Ward groups") #same groups as ward clustering
grw <- spe.kmeans$cluster
k <- length(levels(factor(grw)))
for (i in 1:k) {
  points(spa.lake[grw==i,1], spa.lake[grw==i,2], pch=i+20, cex=3, col=i+1, bg=i+1)
}
text(spa.lake, row.names(spa.lake), cex=0.8, col = "black", font = 2)         
legend("bottomright", paste("Group", 1:k), pch = (1:k)+20, col=2:(k+1), pt.bg=2:(k+1), pt.cex=2, bty="n")

## Comparing with environmental variables
attach(enviro.lake)
boxplot(sqrt(Macrophyte) ~ spe.kmeans$cluster, main = "Macrophyte cover", las = 1, col=2:5, varwidth=TRUE)
boxplot(sqrt(Temperature) ~ spe.kmeans$cluster, main = "Temperature", las = 1, col=2:5, varwidth=TRUE)
boxplot(sqrt(DO) ~ spe.kmeans$cluster, main = "Dissolved oxygen", las = 1, col=2:5, varwidth=TRUE)
boxplot(sqrt(pH) ~ spe.kmeans$cluster, main = "pH", las = 1, col=2:5, varwidth=TRUE)
boxplot(sqrt(Perimeter) ~ spe.kmeans$cluster, main = "Perimeter", las = 1, col=2:5, varwidth=TRUE)

# ---- Species assemblage ----

#mean abundances on k-means clusters
groups <- as.factor(spe.kmeans$cluster)
spe.means <-  matrix(0, ncol(lake.sp.matrix), length(levels(groups)))
row.names(spe.means) <- colnames(lake.sp.matrix)
for (i in 1:ncol(lake.sp.matrix)) {
  spe.means[i,] <- tapply(lake.sp.matrix[,i], spe.kmeans$cluster, mean)
}
grp1 <- round(sort(spe.means[,1], decreasing = TRUE), 2)
grp2 <- round(sort(spe.means[,2], decreasing = TRUE), 2)
grp3 <- round(sort(spe.means[,3], decreasing = TRUE), 2)
grp4 <- round(sort(spe.means[,4], decreasing = TRUE), 2)

group1.domin <- which(grp1>mean(grp1)) #Dominating species
group2.domin <- which(grp2>mean(grp2))
group3.domin <- which(grp3>mean(grp3))
group4.domin <- which(grp4>mean(grp4))

#Does sampled community change according to the sampling method used?


# ---- Community composition and lakes ----

#Est-ce que la communauté échantillonnée varie selon le lac?

## Hole fish community ----

lake.data <- CombinedData %>% 
  select(Lake, 
         starts_with("tot"),
         starts_with("inf")) %>% 
  na.omit()

lake.data <- lake.data %>% 
  group_by(Lake) %>% 
  summarise(across(.cols= everything(), sum))

lake.data <- lake.data %>% 
  filter(!(Lake == "Tracy" | Lake == "Beaver" | Lake == "Montaubois" | Lake == "St-Onge")) %>% 
  select(!(c(tot_Centrarchidae, inf_Centrarchidae)))

lake.tot <- lake.data %>% 
  column_to_rownames(var = "Lake") %>% 
  select(starts_with("tot")) %>%
  rename_with(~ str_remove(., "tot_"), everything())

lake.tot.h <- decostand(lake.tot, method = "hellinger")
lake <- lake.data$Lake

#On fait un pca sur la matrice de communauté des lacs
lake.comm.pca <- rda(lake.tot.h)
biplot(lake.comm.pca)


#On fait un rda avec la matrice de communauté comme variable réponse et le lac comme variable explicative.

#rda.com.tot <- rda(lake.tot.h ~ lake) #Overfitted
#summary(rda.com.tot)
#RsquareAdj(rda.com.tot)
#anova.cca(rda.com.tot, permutations = 1000)
#Analyse de fonctionne pas si 1 lac = 1 ligne

#Essayons sans grouper les échantillons (not summarized)
samp.data <- CombinedData %>% 
  select(Lake, 
         starts_with("tot"),
         starts_with("inf")) %>% 
  na.omit()

samp.data <- samp.data %>% 
  filter(!(Lake == "Tracy" | Lake == "Beaver" | Lake == "Montaubois" | Lake == "St-Onge")) %>% 
  select(!(c(tot_Centrarchidae, inf_Centrarchidae)))

samp.tot <- samp.data %>% 
  column_to_rownames(var = "Lake") %>% 
  select(starts_with("tot")) %>%
  rename_with(~ str_remove(., "tot_"), everything())

samp.tot.h <- decostand(samp.tot, method = "hellinger")
lake <- samp.data$Lake

rda.samp.tot <- rda(samp.tot.h ~ lake)
summary(rda.samp.tot)
RsquareAdj(rda.samp.tot)
anova.cca(rda.samp.tot, permutations = 1000) #Modèle significatif
#Communauté varie en fonction du lac

#Visualisation 
ordiplot(rda.samp.tot,
         scaling = 1,
         type = "text")

ordiplot(rda.samp.tot,
         scaling = 2,
         type = "text")

## Infected fishes ----
inf.data <- CombinedData %>% 
  select(Lake,
         starts_with("inf")) %>% 
  na.omit()

inf.data <- inf.data %>% 
  group_by(Lake) %>% 
  summarise(across(.cols= everything(), sum))

inf.data <- inf.data %>% 
  filter(!(Lake == "Tracy" | Lake == "Beaver" | Lake == "Montaubois" | Lake == "St-Onge")) %>% 
  select(!(c(inf_Centrarchidae)))

lake.inf <- inf.data %>% 
  filter(!(Lake == "Triton")) %>% #Enlever triton car 0 infection
  column_to_rownames(var = "Lake") %>% 
  rename_with(~ str_remove(., "inf_"), everything())

lake.inf.h <- decostand(lake.inf, method = "hellinger")

lake.inf.pca <- rda(lake.inf.h)
biplot(lake.inf.pca)

#RDA on samplings
samp.inf.data <- CombinedData %>% 
  select(Lake, 
         starts_with("inf")) %>% 
  na.omit()

samp.inf.data <- samp.inf.data %>% 
  filter(!(Lake == "Tracy" | Lake == "Beaver" | Lake == "Montaubois" | Lake == "St-Onge" | Lake == "Triton")) %>% 
  select(!(c(inf_Centrarchidae)))

samp.inf<- samp.inf.data %>% 
  #column_to_rownames(var = "Lake") %>% 
  select(starts_with("inf")) %>%
  rename_with(~ str_remove(., "inf_"), everything())

samp.inf.h <- decostand(samp.inf, method = "hellinger")
lake.inf <- samp.inf.data$Lake

rda.samp.inf <- rda(samp.inf.h ~ lake.inf)
summary(rda.samp.inf)
RsquareAdj(rda.samp.inf)
anova.cca(rda.samp.inf, permutations = 1000) #Modèle significatif
#Communauté infectée varie en fonction du lac

#Fonctionne pas sur matrice prévalence à cause des NA
#lake.prv <- lake.inf/lake.tot
#lake.prv <- lake.prv %>% 
  #mutate(across(everything(), ~ na_if(., NaN)))

## Community comparison ----
#Est-ce que la communauté infectée varie en fonction de la communauté totale?

comp.data <- CombinedData %>% 
  select(Lake, starts_with("inf"), starts_with("tot"))

comp.inf <- comp.data %>% 
  select(starts_with("inf")) %>% 
  rename_with(~ str_remove(., "inf_"), everything())

row_sub <- apply(comp.inf, 1, function(row) any(row !=0 ))
comp.inf <- comp.inf[row_sub, ]
comp.inf <- na.omit(comp.inf)
comp.inf.h <- decostand(comp.inf, method = "hellinger")

comp.tot <- comp.data %>% 
  select(starts_with("tot")) %>% 
  rename_with(~ str_remove(., "tot_"), everything())
comp.tot <- comp.tot[row_sub, ]
comp.tot <- na.omit(comp.tot)
comp.tot.h <- decostand(comp.tot, method = "hellinger")

rda.comp <- rda(comp.inf.h ~ comp.tot.h) #Fonctionne pas



##nMDS
lake.nmds <- metaMDS(lake.tot, distance = 'bray', k = 2)
stressplot(lake.nmds, main = "Shepard plot") #fit is good

plot(lake.nmds, type = "none",
     main = paste("NMDS/Bray - Stress =",
                  round(lake.nmds$stress, 3)),
     xlab = c("NMDS1"), ylab = "NMDS2")
points(scores(lake.nmds, display = "sites",
              choiches = c(1,2),
              pch = 21,
              col = "black",
              g = "steelblue",
              cex = 1.2))
text(scores(lake.nmds, display = "sites"),
     labels = rownames(scores(lake.nmds, display = "sites")))
text(scores(lake.nmds, display = "species", choices = c(1)),
     scores(lake.nmds, display = "species", choices = c(2)),
     labels = rownames(scores(lake.nmds, display = "species")),
     col = "red", cex = 0.8)

lake.inf.nmds <- metaMDS(lake.inf, distance = 'bray', k = 2)
stressplot(lake.inf.nmds, main = "Shepard plot") #fit is good

plot(lake.inf.nmds, type = "none",
     main = paste("NMDS/Bray - Stress =",
                  round(lake.inf.nmds$stress, 3)),
     xlab = c("NMDS1"), ylab = "NMDS2")
points(scores(lake.inf.nmds, display = "sites",
              choiches = c(1,2),
              pch = 21,
              col = "black",
              g = "steelblue",
              cex = 1.2))
text(scores(lake.inf.nmds, display = "sites"),
     labels = rownames(scores(lake.inf.nmds, display = "sites")))
text(scores(lake.inf.nmds, display = "species", choices = c(1)),
     scores(lake.inf.nmds, display = "species", choices = c(2)),
     labels = rownames(scores(lake.inf.nmds, display = "species")),
     col = "red", cex = 0.8)

# ---- Community composition and methods ----

## Hole fish community ----

# Can lake fish community can be explained by the sampling method used ?

meth.tot.lake.data <- CombinedData %>% 
  select(starts_with("tot"), Sampling_method, Lake)

meth.tot.lake.data <- meth.tot.lake.data %>% 
  group_by(Lake, Sampling_method) %>% 
  summarise(across(.cols = everything(), sum)) %>% 
  na.omit()

meth.tot.lake.data <- meth.tot.lake.data %>% 
  rename_with(~ str_remove(., "tot_"), everything()) %>% 
  select(!(Centrarchidae))
  
row_sub <- apply(meth.tot.lake.data, 1, function(row) any(row !=0 )) # Obtenir les lignes avec qui ont des valeurs differents de zero
meth.tot.lake.data <- meth.tot.lake.data[row_sub, ]  
meth.tot.lake.data <- meth.tot.lake.data[-38,]
meth.tot.lake.data$Lake <- as.factor(meth.tot.lake.data$Lake)
meth.tot.lake.data$Sampling_method <- as.factor(meth.tot.lake.data$Sampling_method)

meth.tot.lake.sp <- meth.tot.lake.data[3:18]
meth.tot.lake.sph <- decostand(meth.tot.lake.sp, method = "hellinger")
meth.tot.lake.gp <- meth.tot.lake.data$Sampling_method

# RDA
meth.tot.lake.rda <- rda(meth.tot.lake.sph ~ meth.tot.lake.data$Sampling_method)
summary(meth.tot.lake.rda) #Sampling method explains 6.7% of lake fish communities
RsquareAdj(meth.tot.lake.rda)
anova.cca(meth.tot.lake.rda, permutations = 1000) #Modèle significatif

ordiplot(meth.tot.lake.rda,
         scaling = 2,
         type = "text")

## db-RDA
meth.tot.lake.dbrda <- dbrda(meth.tot.lake.sp ~ meth.tot.lake.gp, distance = "bray") #%>% 
  scores(tidy = TRUE) %>% 
  split(.[["score"]])

summary(meth.tot.lake.dbrda) #Constained = 17.55%

install.packages("geomtextpath")
library(geomtextpath)
library(colorspace)

meth.tot.lake.dbrda.plot <- ggplot(meth.tot.lake.dbrda$sites, aes(dbRDA1, dbRDA2)) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_point(aes(fill = factor(meth.tot.lake.data$Lake), 
                 shape = factor(meth.tot.lake.data$Sampling_method)),
                 size = 4, color = "black", show.legend = TRUE) +
  stat_ellipse(data = within(meth.tot.lake.dbrda$sites, Method <- meth.tot.lake.data$Sampling_method), 
               aes(label = Method, group = Method), color = "black",
               geom = "textpath", hjust=0.65, vjust=1.2, linetype=2) +
  scale_shape_manual(values = c(21, 22, 24)) +
  scale_fill_discrete_diverging(palette= "Vik")+
  theme_void() +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.background = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank()) +
    #panel.border = element_rect(colour = "black", fill = NULL)) +
  guides(fill = guide_legend(title = "Lake"),
         shape = guide_legend(title = "Sampling Method"))
meth.tot.lake.dbrda.plot

# LDA
meth.tot.lake.lda <- lda(meth.tot.lake.sp, meth.tot.lake.data$Sampling_method)  
lda.plotdf <- data.frame(group = meth.tot.lake.data$Sampling_method, lda = predict(meth.tot.lake.lda)$x)  
spe.class <- predict(meth.tot.lake.lda)$class  
spe.post <- predict(meth.tot.lake.lda)$posterior

(spe.table <- table(meth.tot.lake.data$Sampling_method, spe.class))
diag(prop.table(spe.table, 1))
# Fonctionne généralement bien pour prédire la communauté de lac
#Pourquoi si différent du résultat de la rda?

# Can local fish community can be explained by the sampling method used ?

meth.tot.samp.data <- CombinedData %>% 
  dplyr::select(starts_with("tot"), Sampling_method)

meth.tot.samp.data <- meth.tot.samp.data %>% 
  na.omit()

meth.tot.samp.data <- meth.tot.samp.data %>% 
  rename_with(~ str_remove(., "tot_"), everything()) %>% 
  dplyr::select(!(Centrarchidae))

meth.tot.samp.sp <- meth.tot.samp.data[1:16]
row_sub <- apply(meth.tot.samp.sp, 1, function(row) any(row !=0 )) # Obtenir les lignes avec qui ont des valeurs differents de zero
meth.tot.samp.sp <- meth.tot.samp.sp[row_sub, ] 

meth.tot.samp.sph <- decostand(meth.tot.samp.sp, method = "hellinger")

meth.tot.samp.gp <- as.matrix(meth.tot.samp.data$Sampling_method)
meth.tot.samp.gp <- meth.tot.samp.gp[row_sub, ] 

# RDA
meth.tot.samp.rda <- rda(meth.tot.samp.sph ~ meth.tot.samp.gp)
summary(meth.tot.samp.rda) #Sampling method explains 3.4% of lake fish communities
RsquareAdj(meth.tot.samp.rda)
anova.cca(meth.tot.samp.rda, permutations = 1000) #Modèle significatif

ordiplot(meth.tot.samp.rda,
         scaling = 1,
         type = "text")

# LDA 
meth.tot.samp.lda <- lda(meth.tot.samp.sp, meth.tot.samp.gp)  
lda.plotdf <- data.frame(group = meth.tot.samp.gp, lda = predict(meth.tot.samp.lda)$x)  
spe.class <- predict(meth.tot.samp.lda)$class  
spe.post <- predict(meth.tot.samp.lda)$posterior

(spe.table <- table(meth.tot.samp.gp, spe.class))
diag(prop.table(spe.table, 1))
#Pourri pour prédire la communauté échantillonnée avec la seine, mais excellent pour minnow trap.
#Sélection de l'habitat

## Infected fish community ----

meth.inf.lake.data <- CombinedData %>% 
  dplyr::select(starts_with("inf"), Sampling_method, Lake)

meth.inf.lake.data <- meth.inf.lake.data %>% 
  group_by(Lake, Sampling_method) %>% 
  summarise(across(.cols = everything(), sum)) %>% 
  na.omit()

meth.inf.lake.data <- meth.inf.lake.data %>% 
  rename_with(~ str_remove(., "inf_"), everything()) %>% 
  dplyr::select(!(Centrarchidae)) %>% 
  ungroup()

row_sub <- apply(meth.inf.lake.data, 1, function(row) any(row !=0 )) # Obtenir les lignes avec qui ont des valeurs differents de zero
meth.inf.lake.data <- meth.inf.lake.data[row_sub, ]  

meth.inf.lake.sp <- meth.inf.lake.data[c(3, 5:8, 10, 12)] #On prend juste les espèces qui ont de l'infection
meth.inf.lake.sph <- decostand(meth.inf.lake.sp, method = "hellinger")

# Can lake fish community can be explained by the sampling method used ?

# RDA
meth.inf.lake.rda <- rda(meth.inf.lake.sph ~ meth.inf.lake.data$Sampling_method)
summary(meth.inf.lake.rda) #Sampling method explains 1.6% of lake fish communities
RsquareAdj(meth.inf.lake.rda)
anova.cca(meth.inf.lake.rda, permutations = 1000) #Modèle non significatif

# LDA
meth.inf.lake.lda <- lda(meth.inf.lake.sp, meth.inf.lake.data$Sampling_method)  
lda.plotdf <- data.frame(group = meth.inf.lake.data$Sampling_method, lda = predict(meth.inf.lake.lda)$x)  
spe.class <- predict(meth.inf.lake.lda)$class  
spe.post <- predict(meth.inf.lake.lda)$posterior

(spe.table <- table(meth.inf.lake.data$Sampling_method, spe.class))
diag(prop.table(spe.table, 1))
# Fonctionne généralement bien pour minnow trap

# Can local fish community can be explained by the sampling method used ?

meth.inf.samp.data <- CombinedData %>% 
  dplyr::select(starts_with("inf"), Sampling_method)

meth.inf.samp.data <- meth.inf.samp.data %>% 
  na.omit()

meth.inf.samp.data <- meth.inf.samp.data %>% 
  rename_with(~ str_remove(., "inf_"), everything()) %>% 
  dplyr::select(!(Centrarchidae))

meth.inf.samp.sp <- meth.inf.samp.data[c(1, 3:6, 8, 10)]
row_sub <- apply(meth.tot.samp.sp, 1, function(row) any(row !=0 )) # Obtenir les lignes avec qui ont des valeurs differents de zero
meth.inf.samp.sp <- meth.inf.samp.sp[row_sub, ] 

meth.inf.samp.sph <- decostand(meth.inf.samp.sp, method = "hellinger")

meth.inf.samp.gp <- as.matrix(meth.inf.samp.data$Sampling_method)
meth.inf.samp.gp <- meth.inf.samp.gp[row_sub, ] 

# RDA
meth.inf.samp.rda <- rda(meth.inf.samp.sph ~ meth.inf.samp.gp)
summary(meth.inf.samp.rda) #Sampling method explains 3.5% of lake fish communities
RsquareAdj(meth.inf.samp.rda)
anova.cca(meth.inf.samp.rda, permutations = 1000) #Modèle significatif

ordiplot(meth.inf.samp.rda,
         scaling = 1,
         type = "text")

# LDA 
meth.inf.samp.lda <- lda(meth.inf.samp.sp, meth.inf.samp.gp)  
lda.plotdf <- data.frame(group = meth.inf.samp.gp, lda = predict(meth.inf.samp.lda)$x)  
spe.class <- predict(meth.inf.samp.lda)$class  
spe.post <- predict(meth.inf.samp.lda)$posterior

(spe.table <- table(meth.inf.samp.gp, spe.class))
diag(prop.table(spe.table, 1))
#Pourri pour prédire la communauté échantillonnée avec la seine, mais excellent pour minnow trap.
#Sélection de l'habitat


