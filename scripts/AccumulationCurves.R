## Script name : Accumulation curves

## Authors : Juliane Vigneault & Éric Harvey
## Date created : January 11, 2023

## Copyright (c) Juliane Vigneault, 2023 ## je ne suis pas certain du type de copyright qui s'applique sur un script R ?
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

## Loading packages ----

library(vegan)
library(ggplot2)
library(colorspace)
library(patchwork)
library(dplyr)
library(gt)
library(broom)

## Loading data ----

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))

# ---- Species accumulation cruves ----

## Minnow traps ----

MTdata <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") #Selecting method 

MTdata <- MTdata %>% 
  select(starts_with("tot")) #Keeping community matrix (total abundance)

acc.MT <- specaccum(MTdata, method = "random", permutations = 1000, gamma = "jack1") #Accumulation curve function
MT.plot <- plot(acc.MT, col = "blue", xlab = "sampling", ylab = "species")

## Seine ----

Sdata <- CombinedData %>% 
  filter(Sampling_method == "Seine") #Selecting method

Sdata <- Sdata %>% 
  select(starts_with("tot")) #Keeping community matrix (total abundance)

acc.S <- specaccum(Sdata, method = "random", permutations = 1000, gamma = "jack1") #Accumulation curve function
S.plot <- plot(acc.S, col = "red", xlab = "sampling", ylab = "species")

## Transect ----

Tdata <- CombinedData %>% 
  filter(Sampling_method == "Transect") #Selecting transect method

Tdata <- Tdata %>% 
  select(starts_with("tot")) #Keeping community matrix (total abundance)

Tdata <- na.omit(Tdata) #Deleting NA data. specaccum() can not compute NAs

acc.T <- specaccum(Tdata, method = "random", permutations = 1000, gamma = "jack1") #Accumulation curve function
T.plot <- plot(acc.T, col = "darkgoldenrod1", xlab = "sampling", ylab = "species")

## Method comparison ----

pdf(paste0(to.figs, "AccumulationCurves_species.pdf"), width = 15, height = 10)

plot(acc.S, col = "red", xlab = "sampling", ylab = "species", cex = 1)
plot(acc.MT, add = TRUE, col = "blue", xlab = "sampling", ylab = "species")
plot(acc.T, add = TRUE, col = "darkgoldenrod1", xlab = "sampling", ylab = "species")

legend("bottomright", legend = c("Seine", "Minnow Trap", "Transect"),
       fill = c("red", "blue", "darkgoldenrod1"))

dev.off()

# ---- Infected individuals accumulation curves ----

## All methods ----

inf.A.data <- CombinedData %>% 
  select(starts_with("inf")) %>% #Selecting infected individuals 
  na.omit %>% 
  rowSums() #Sum of all species
  
# We want to randomly sample n fishing sample, 999 times for N = c(1,2,3,5,7,10,15,20,35)

# --- TEST --- # 
# Sample within a loop
#for (n in seq_along(2:10)) {
 # print(replicate(999, sum(sample(inf.F.data, n))))
#}
# Unable to put the result in an objet for plotting 
# --- END TEST --- #

#Sampling
inf.A1 <- replicate(999, sample(inf.A.data, 1))
inf.A2 <- replicate(999, sum(sample(inf.A.data, 2)))
inf.A3 <- replicate(999, sum(sample(inf.A.data, 3)))
inf.A5 <- replicate(999, sum(sample(inf.A.data, 5)))
inf.A7 <- replicate(999, sum(sample(inf.A.data, 7)))
inf.A10 <- replicate(999, sum(sample(inf.A.data, 10)))
inf.A15 <- replicate(999, sum(sample(inf.A.data, 15)))
inf.A20 <- replicate(999, sum(sample(inf.A.data, 20)))
inf.A25 <- replicate(999, sum(sample(inf.A.data, 25)))
inf.A35 <- replicate(999, sum(sample(inf.A.data, 35)))

#Placing results in a data frame
inf <- c(inf.A1, inf.A2, inf.A3, inf.A5, inf.A7, inf.A10, inf.A15, inf.A20, inf.A25, inf.A35) 
N <- c(rep(1,999), rep(2,999), rep(3,999), rep(5,999), rep(7,999), rep(10,999), rep(15,999), rep(20,999), rep(25,999), rep(35,999))
Method <- c(rep("All", 9990))
df.All <- data.frame(N, Method, inf, row.names = NULL)

## Minnow trap method ----

MinnowTrapData <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") #Selecting minnow trap methods

inf.MT.data <- MinnowTrapData %>% 
  select(starts_with("inf")) %>% #Selecting infected individuals
  rowSums() #Sum of all species

#Sampling
inf.MT1 <- replicate(999, sample(inf.MT.data, 1))
inf.MT2 <- replicate(999, sum(sample(inf.MT.data, 2)))
inf.MT3 <- replicate(999, sum(sample(inf.MT.data, 3)))
inf.MT5 <- replicate(999, sum(sample(inf.MT.data, 5)))
inf.MT7 <- replicate(999, sum(sample(inf.MT.data, 7)))
inf.MT10 <- replicate(999, sum(sample(inf.MT.data, 10)))
inf.MT15 <- replicate(999, sum(sample(inf.MT.data, 15)))
inf.MT20 <- replicate(999, sum(sample(inf.MT.data, 20)))
inf.MT25 <- replicate(999, sum(sample(inf.MT.data, 25)))
inf.MT35 <- replicate(999, sum(sample(inf.MT.data, 35)))

#Placing results the a data frame
inf <- c(inf.MT1, inf.MT2, inf.MT3, inf.MT5, inf.MT7, inf.MT10, inf.MT15, inf.MT20, inf.MT25, inf.MT35)
Method <- c(rep("Minnow trap", 9990))
df.MinnowTrap <- data.frame(N, Method, inf, row.names = NULL)

## Seine method ----

SeineData <- CombinedData %>% 
  filter(Sampling_method == "Seine") #Selecting minnow trap methods

inf.S.data <- SeineData %>% 
  select(starts_with("inf")) %>% #Selecting infected individuals
  rowSums() #Sum of all species

#Sampling
inf.S1 <- replicate(999, sample(inf.S.data, 1))
inf.S2 <- replicate(999, sum(sample(inf.S.data, 2)))
inf.S3 <- replicate(999, sum(sample(inf.S.data, 3)))
inf.S5 <- replicate(999, sum(sample(inf.S.data, 5)))
inf.S7 <- replicate(999, sum(sample(inf.S.data, 7)))
inf.S10 <- replicate(999, sum(sample(inf.S.data, 10)))
inf.S15 <- replicate(999, sum(sample(inf.S.data, 15)))
inf.S20 <- replicate(999, sum(sample(inf.S.data, 20)))
inf.S25 <- replicate(999, sum(sample(inf.S.data, 25)))
inf.S35 <- replicate(999, sum(sample(inf.S.data, 35)))

#Placing results the data frame
inf <- c(inf.S1, inf.S2, inf.S3, inf.S5, inf.S7, inf.S10, inf.S15, inf.S20, inf.S25, inf.S35)
Method <- c(rep("Seine net", 9990))
df.Seine <- data.frame(N, Method, inf, row.names = NULL)

## Transect method ----

TransectData <- CombinedData %>% 
  filter(Sampling_method == "Transect") #Selecting transect method

inf.T.data <- TransectData %>% 
  select(starts_with("inf")) %>% #Selecting infected individuals
  na.omit() %>% #Deleting NAs
  rowSums()#Sum of all species 

#Sampling
inf.T1 <- replicate(999, sample(inf.T.data, 1))
inf.T2 <- replicate(999, sum(sample(inf.T.data, 2)))
inf.T3 <- replicate(999, sum(sample(inf.T.data, 3)))
inf.T5 <- replicate(999, sum(sample(inf.T.data, 5)))
inf.T7 <- replicate(999, sum(sample(inf.T.data, 7)))
inf.T10 <- replicate(999, sum(sample(inf.T.data, 10)))
inf.T15 <- replicate(999, sum(sample(inf.T.data, 15)))
inf.T20 <- replicate(999, sum(sample(inf.T.data, 20)))
inf.T25<- replicate(999, sum(sample(inf.T.data, 25)))
inf.T35 <- replicate(999, sum(sample(inf.T.data, 35)))

#Placing results the data frame
inf <- c(inf.T1, inf.T2, inf.T3, inf.T5, inf.T7, inf.T10, inf.T15, inf.T20, inf.T25, inf.T35)
Method <- c(rep("Transect", 9990))
df.Transect <- data.frame(N, Method, inf, row.names = NULL)

## Plotting simulation ----

col.pal <- c("#7E7E7E", "#2A5676", "#999600", "#966F1E")
#col.pal4 <- c("#7E7E7E", "#005260", "#A4473D", "#A57E00")

#Binding data
df.inf <- rbind(df.All, df.MinnowTrap, df.Seine, df.Transect)

inf.acc.plot <- ggplot(df.inf) + 
  stat_summary(aes(x = N, y = inf, group = Method, color = Method, shape = Method), fun = mean, size = 1) +
  geom_smooth(aes(x= N, y = inf, group = Method, color = Method, fill = Method), method = "glm", se = TRUE, lineend = "round") +
  scale_x_continuous(breaks = c(1, 2, 3, 5, 7, 10, 15, 25, 20, 25, 35)) +
  scale_y_continuous(breaks = round(seq(0, 2500, by = 400), 1)) +
  labs(x = "Number of samplings", y = "Infected fish abundance", tag = "A") +
  scale_color_manual(values = c("#7E7E7E", "#2A5676", "#999600", "#966F1E"),
                     aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(0,5,2,1)) +
  guides(fill = guide_legend(override.aes = list(fill = NA, linetype = 0))) +
theme(text = element_text(size = 20, family = "Calibri Light", color = "black"),
      panel.background = element_blank(),
      legend.key = element_rect(fill = NA),
      axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
      axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.line.x = element_line(color = "black", lineend = "round"),
      axis.line.y = element_line(color = "black", lineend = "round"),
      plot.tag = element_text(face = "bold"))

ggsave(paste0(to.figs, "AccumulationCurves_infection.png"), plot = inf.acc.plot, dpi = 300, width = 15, height = 10)  

# ---- Individuals accumulation curves ---- 

## All methods ----

tot.A.data <- CombinedData %>% 
  select(starts_with("tot")) %>% #Selecting infected individuals
  na.omit() %>% 
  rowSums() #Sum of all species

#Sampling
tot.A1 <- replicate(999, sample(tot.A.data, 1))
tot.A2 <- replicate(999, sum(sample(tot.A.data, 2)))
tot.A3 <- replicate(999, sum(sample(tot.A.data, 3)))
tot.A5 <- replicate(999, sum(sample(tot.A.data, 5)))
tot.A7 <- replicate(999, sum(sample(tot.A.data, 7)))
tot.A10 <- replicate(999, sum(sample(tot.A.data, 10)))
tot.A15 <- replicate(999, sum(sample(tot.A.data, 15)))
tot.A20 <- replicate(999, sum(sample(tot.A.data, 20)))
tot.A25 <- replicate(999, sum(sample(tot.A.data, 25)))
tot.A35 <- replicate(999, sum(sample(tot.A.data, 35)))

#Placing results the data frame
tot <- c(tot.A1, tot.A2, tot.A3, tot.A5, tot.A7, tot.A10, tot.A15, tot.A20, tot.A25, tot.A35)
df.All <- df.All %>% 
  mutate(tot)

## Minnow trap method ----

tot.MT.data <- MinnowTrapData %>% 
  select(starts_with("tot")) %>% #Selecting infected individuals
  rowSums() #Sum of all species

#Sampling
tot.MT1 <- replicate(999, sample(tot.MT.data, 1))
tot.MT2 <- replicate(999, sum(sample(tot.MT.data, 2)))
tot.MT3 <- replicate(999, sum(sample(tot.MT.data, 3)))
tot.MT5 <- replicate(999, sum(sample(tot.MT.data, 5)))
tot.MT7 <- replicate(999, sum(sample(tot.MT.data, 7)))
tot.MT10 <- replicate(999, sum(sample(tot.MT.data, 10)))
tot.MT15 <- replicate(999, sum(sample(tot.MT.data, 15)))
tot.MT20 <- replicate(999, sum(sample(tot.MT.data, 20)))
tot.MT25 <- replicate(999, sum(sample(tot.MT.data, 25)))
tot.MT35 <- replicate(999, sum(sample(tot.MT.data, 35)))

#Placing results the data frame
tot <- c(tot.MT1, tot.MT2, tot.MT3, tot.MT5, tot.MT7, tot.MT10, tot.MT15, tot.MT20, tot.MT25, tot.MT35)
df.MinnowTrap <- df.MinnowTrap %>% 
  mutate(tot)

## Seine method ----

tot.S.data <- SeineData %>% 
  select(starts_with("tot")) %>% #Selecting infected individuals
  rowSums() #Sum of all species

#Sampling
tot.S1 <- replicate(999, sample(tot.S.data, 1))
tot.S2 <- replicate(999, sum(sample(tot.S.data, 2)))
tot.S3 <- replicate(999, sum(sample(tot.S.data, 3)))
tot.S5 <- replicate(999, sum(sample(tot.S.data, 5)))
tot.S7 <- replicate(999, sum(sample(tot.S.data, 7)))
tot.S10 <- replicate(999, sum(sample(tot.S.data, 10)))
tot.S15 <- replicate(999, sum(sample(tot.S.data, 15)))
tot.S20 <- replicate(999, sum(sample(tot.S.data, 20)))
tot.S25 <- replicate(999, sum(sample(tot.S.data, 25)))
tot.S35 <- replicate(999, sum(sample(tot.S.data, 35)))

#Placing results the data frame
tot <- c(tot.S1, tot.S2, tot.S3, tot.S5, tot.S7, tot.S10, tot.S15, tot.S20, tot.S25, tot.S35)
df.Seine <- df.Seine %>% 
  mutate(tot)

## Transect method ---

tot.T.data <- TransectData %>% 
  select(starts_with("tot")) %>% #Selecting infected individuals
  na.omit() %>%  #Deleting NAs
  rowSums() #Sum of all species 

#Sampling
tot.T1 <- replicate(999, sample(tot.T.data, 1))
tot.T2 <- replicate(999, sum(sample(tot.T.data, 2)))
tot.T3 <- replicate(999, sum(sample(tot.T.data, 3)))
tot.T5 <- replicate(999, sum(sample(tot.T.data, 5)))
tot.T7 <- replicate(999, sum(sample(tot.T.data, 7)))
tot.T10 <- replicate(999, sum(sample(tot.T.data, 10)))
tot.T15 <- replicate(999, sum(sample(tot.T.data, 15)))
tot.T20 <- replicate(999, sum(sample(tot.T.data, 20)))
tot.T25 <- replicate(999, sum(sample(tot.T.data, 25)))
tot.T35 <- replicate(999, sum(sample(tot.T.data, 35)))

#Placing results the data frame
tot <- c(tot.T1, tot.T2, tot.T3, tot.T5, tot.T7, tot.T10, tot.T15, tot.T20, tot.T25, tot.T35)
df.Transect <- df.Transect %>% 
  mutate(tot)

## Plotting simulation ----

df.tot <- rbind(df.All, df.MinnowTrap, df.Seine, df.Transect)

tot.acc.plot <- ggplot(df.tot) + 
  stat_summary(aes(x = N, y = tot, group = Method, color = Method, shape = Method), fun = mean, size = 1) +
  geom_smooth(aes(x= N, y = tot, group = Method, color = Method, fill = Method), method = "glm", se = TRUE, lineend = "round") +
  scale_x_continuous(breaks = c(1, 2, 3, 5, 7, 10, 15, 25, 20, 25, 35)) +
  scale_y_continuous(breaks = round(seq(0, 6500, by = 1000), 1)) +
  labs(x = "Number of samplings", y = "Total fish abundance", tag = "B") +
  scale_color_manual(values = c("#7E7E7E", "#2A5676", "#999600", "#966F1E"),
                     aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(0, 5, 2, 1)) +
  guides(fill = guide_legend(override.aes = list(fill = NA, linetype = 0))) +
  theme(text = element_text(size = 20, family = "Calibri Light", color = "black"),
        panel.background = element_blank(),
        legend.key = element_rect(fill = NA),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line.x = element_line(color = "black", lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"),
        plot.tag = element_text(face = "bold"))

ggsave(paste0(to.figs, "AccumulationCurves_individuals.png"), plot = tot.acc.plot, dpi = 300, width = 15, height = 10)  

# ---- Prevalence accumulation curves ----

## Fishing method ----

prev.A <- inf.A.data / tot.A.data
#prev.A <- na.omit(prev.A) #Deleting NAs as they mean that no fish were caught
#Si on garde les NA, dès qu'il y en a une moyenne a calculer, résulta = NA

#Sampling

#Essai par méthode Eric sur méthode = All
#Autre méthode n'ont pas été changée

prev.A1 <- replicate(999, sample(prev.A, 1))
mean.A1 <- sum(prev.A1,  na.rm = TRUE)/999

prev.A2 <- replicate(999, sum(sample(prev.A, 2))/2) #On garde les NA, on divise par le nombre d'échantillons pigé
mean.A2 <- sum(prev.A2,  na.rm = TRUE)/999 #On calcule la prévalence moyenne pour un N donné en divisant par le nombre de pige total

prev.A3 <- replicate(999, sum(sample(prev.A, 3))/3)
mean.A3 <- sum(prev.A3,  na.rm = TRUE)/999

prev.A5 <- replicate(999, sum(sample(prev.A, 5))/5)
mean.A5 <- sum(prev.A5,  na.rm = TRUE)/999

prev.A7 <- replicate(999, sum(sample(prev.A, 7))/7)
mean.A7 <- sum(prev.A7,  na.rm = TRUE)/999

prev.A10 <- replicate(999, sum(sample(prev.A, 10))/10)
mean.A10 <- sum(prev.A10,  na.rm = TRUE)/999

prev.A15 <- replicate(999, sum(sample(prev.A, 15))/15)
mean.A15 <- sum(prev.A15,  na.rm = TRUE)/999

prev.A20 <- replicate(999, sum(sample(prev.A, 20))/20)
mean.A20 <- sum(prev.A20,  na.rm = TRUE)/999

prev.A25 <- replicate(999, sum(sample(prev.A, 25))/25)
mean.A25 <- sum(prev.A25,  na.rm = TRUE)/999

prev.A35 <- replicate(999, sum(sample(prev.A, 35))/35)
mean.A35 <- sum(prev.A35,  na.rm = TRUE)/999

#Prévalence diminue exponentiellement avec le nombre d'échantillon jusqu'à 0
#Trop de NA intoduit
#Donne une valeur correct à A1 par contre

#Fonction mean
adapt.mean<- function(x) {
  mean = sum(x, na.rm = TRUE)/length(x)
  print(mean)
}

#Placing results the data frame
prev <- c(prev.A1, prev.A2, prev.A3, prev.A5, prev.A7, prev.A10, prev.A15, prev.A20, prev.A25, prev.A35)
df.All <- df.All %>%  
  mutate(prev)

## Minnow trap method ----

prev.MT <- inf.MT.data / tot.MT.data
prev.MT <- na.omit(prev.MT) #Deleting NAs as they mean that no fish were caught

#Sampling
prev.MT1 <- replicate(999, sample(prev.MT, 1))
prev.MT2 <- replicate(999, mean(sample(prev.MT, 2)))
prev.MT3 <- replicate(999, mean(sample(prev.MT, 3)))
prev.MT5 <- replicate(999, mean(sample(prev.MT, 5)))
prev.MT7 <- replicate(999, mean(sample(prev.MT, 7)))
prev.MT10 <- replicate(999, mean(sample(prev.MT, 10)))
prev.MT15 <- replicate(999, mean(sample(prev.MT, 15)))
prev.MT20 <- replicate(999, mean(sample(prev.MT, 20)))
prev.MT25 <- replicate(999, mean(sample(prev.MT, 25)))
prev.MT35 <- replicate(999, mean(sample(prev.MT, 35)))

#Placing results the data frame
prev <- c(prev.MT1, prev.MT2, prev.MT3, prev.MT5, prev.MT7, prev.MT10, prev.MT15, prev.MT20, prev.MT25, prev.MT35)
df.MinnowTrap <- df.MinnowTrap %>%  
  mutate(prev)

## Seine method ----

prev.S <- inf.S.data / tot.S.data
prev.S <- na.omit(prev.S) #Deleting NAs as they mean that no fish were caught

#Sampling
prev.S1 <- replicate(999, sample(prev.S, 1))
prev.S2 <- replicate(999, mean(sample(prev.S, 2)), tot.S.data)
prev.S3 <- replicate(999, mean(sample(prev.S, 3)), tot.S.data)
prev.S5 <- replicate(999, mean(sample(prev.S, 5)), tot.S.data)
prev.S7 <- replicate(999, mean(sample(prev.S, 7)), tot.S.data)
prev.S10 <- replicate(999, mean(sample(prev.S, 10)), tot.S.data)
prev.S15 <- replicate(999, mean(sample(prev.S, 15)), tot.S.data)
prev.S20 <- replicate(999, mean(sample(prev.S, 20)), tot.S.data)
prev.S25 <- replicate(999, mean(sample(prev.S, 25)), tot.S.data)
prev.S35 <- replicate(999, mean(sample(prev.S, 35)), tot.S.data)

#Placing results the data frame
prev <- c(prev.S1, prev.S2, prev.S3, prev.S5, prev.S7, prev.S10, prev.S15, prev.S20, prev.S25, prev.S35)
df.Seine<- df.Seine %>%  
  mutate(prev)

## Transect method ----

prev.T <- inf.T.data / tot.T.data
prev.T <- na.omit(prev.T) #Deleting NAs as they mean that no fish were observed

#Sampling
prev.T1 <- replicate(999, sample(prev.T, 1))
prev.T2 <- replicate(999, mean(sample(prev.T, 2)))
prev.T3 <- replicate(999, mean(sample(prev.T, 3)))
prev.T5 <- replicate(999, mean(sample(prev.T, 5)))
prev.T7 <- replicate(999, mean(sample(prev.T, 7)))
prev.T10 <- replicate(999, mean(sample(prev.T, 10)))
prev.T15 <- replicate(999, mean(sample(prev.T, 15)))
prev.T20 <- replicate(999, mean(sample(prev.T, 20)))
prev.T25 <- replicate(999, mean(sample(prev.T, 25)))
prev.T35 <- replicate(999, mean(sample(prev.T, 35)))

#Placing results the data frame
prev <- c(prev.T1, prev.T2, prev.T3, prev.T5, prev.T7, prev.T10, prev.T15, prev.T20, prev.T25, prev.T35)
df.Transect <- df.Transect %>%  
  mutate(prev)

## Plotting simulation ----

df.prev <- rbind(df.All, df.MinnowTrap, df.Seine, df.Transect)

prev.acc.plot <- ggplot(df.prev) + 
  stat_summary(aes(x = N, y = prev, group = Method, color = Method, shape = Method), fun = mean, size = 1) +
  geom_smooth(aes(x= N, y = prev, group = Method, color = Method, fill = Method), method = "lm", se = TRUE, lineend = "round", alpha = 0.3) +
  scale_x_continuous(breaks = c(1, 2, 3, 5, 7, 10, 15, 25, 20, 25, 35)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Number of samplings", y = "Mean infection prevalence", tag = "C") +
  scale_color_manual(values = c("#7E7E7E", "#2A5676", "#999600", "#966F1E"),
                     aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(0, 5, 2, 1)) +
  guides(fill = guide_legend(override.aes = list(fill = NA, linetype = 0))) +
  theme(text = element_text(size = 20, family = "Calibri Light", color = "black"),
        panel.background = element_blank(),
        legend.key = element_rect(fill = NA),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line.x = element_line(color = "black", lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"),
        plot.tag = element_text(face = "bold"))

ggsave(paste0(to.figs, "AccumulationCurves_prevalence.png"), plot = prev.acc.plot, dpi = 300, width = 15, height = 10)  

## Slope and Intercept extractions ----

### All methods ----

lm.All <- lm(prev ~ N, data = df.All)
summary(lm.All)

Intercept.All <- lm.All$coefficients[1] #Extracting intercept
Slope.All <- lm.All$coefficients[2] #Extracting slope

### Minnow trap ----

lm.MT <- lm(prev ~ N, data = df.MinnowTrap)
summary(lm.MT)

Intercept.MT <- lm.MT$coefficients[1] #Extracting intercept
Slope.MT <- lm.MT$coefficients[2] #Extracting slope

### Seine net ----

lm.S <- lm(prev ~ N, data = df.Seine)
summary(lm.S)

Intercept.S <- lm.S$coefficients[1] #Extracting intercept
Slope.S <- lm.S$coefficients[2] #Extracting slope

## Transect ----

lm.T <- lm(prev ~ N, data = df.Transect)
summary(lm.T)

Intercept.T <- lm.T$coefficients[1] #Extracting intercept
Slope.T <- lm.T$coefficients[2] #Extracting slope

### Models summary table ----

#Extracting model summaries
tab.lm.All <- tidy(lm.All)
tab.lm.MT <- tidy(lm.MT)
tab.lm.S <- tidy(lm.S)
tab.lm.T <- tidy(lm.T)

#Binding tables and formatting 
tab.mod.summary <- rbind(tab.lm.All, tab.lm.MT, tab.lm.S, tab.lm.T) %>% 
  mutate(Method = c("All", "All", "Minnow trap", "Minnow trap", "Seine net", "Seine net", "Transect", "Transect"), .before = term) %>% 
  gt(groupname_col = "Method") %>% 
  tab_header(md("**TABLE S16.** Summary results of the prevalence accumulation curve simulations. The models are linear models")) %>% 
  cols_label(term = md("**Term**"), estimate = md("**Estimate**"), std.error = md("**Standard error**"), statistic = md("**z-value**"), p.value = md("**p-value**")) %>%
  sub_values(values = "(Intercept)", replacement = "Intercept") %>% 
  sub_values(values = "N", replacement = "Slope") %>% 
  fmt_number(decimals = 3) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              row_group.as_column = TRUE,
              table.border.bottom.style = "hidden") %>% 
  tab_style(style = cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle", weight = "bold"),
            locations = cells_row_groups()) %>% 
  tab_style(style = cell_text(color = "black", font = "Calibri light", size = 9, align = "left", v_align = "middle"),
            locations = cells_title()) %>% 
  tab_style(style = cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(style = cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(style = cell_borders(sides = c("top", "bottom", "right"), color = "darkgrey", weight = px(2)),
            locations = cells_row_groups()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(side = "top", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "All")) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "black"),
            locations =  cells_body(rows = 8)) %>% 
  tab_style(style = cell_borders(side = "bottom", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "Transect")) %>% 
  tab_style(style = cell_borders(side = "bottom", weight = px(2), color = "darkgrey"),
            locations = cells_body(rows = c(2, 4, 6)))

tab.mod.summary  %>% #Saving gt tab
  gtsave("Tab_Simulations_Summary.png", paste0(to.figs))
tab.mod.summary  %>% 
  gtsave("Table_S16.png", paste0(to.rédaction, "./Support_information/"))

# ---- Summary figure ----

summary.acc.plot <- inf.acc.plot + tot.acc.plot + prev.acc.plot +
  plot_layout(ncol = 3,
              nrow = 1, 
              guides = "collect") &
  theme(legend.position = "bottom",
        text = element_text(size = 32, family = "Calibri Light", color = "black"),
        axis.title.x = element_text(margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 10, 0, 0), "mm")),
        legend.margin = margin(unit(c(25, 0, 0, 0), "mm")),
        legend.key.size = unit(20, "mm")) &
  guides(color = guide_legend(override.aes = list(size = 2)))

ggsave(paste0(to.figs, "AccumulationCurves_summary.png"), plot = summary.acc.plot, dpi = 300, width = 30, height = 12)
