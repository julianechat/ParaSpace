## Script name : Accumulation curves

## Authors : Juliane Vigneault & Éric Harvey
## Date created : January 11, 2023

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

## Loading packages ----

library(vegan)
library(ggplot2)
library(dplyr)

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

## Fishing method ----

FishingData <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap" | Sampling_method == "Seine") #Selecting fishing methods

inf.F.data <- FishingData %>% 
  select(starts_with("inf")) %>% #Selecting infected individuals
  rowSums() #Sum of all species
  
# We want to randomly sample n fishing sample, 999 times for n = 1:10

# --- TEST --- # 
# Sample within a loop
for (n in seq_along(2:10)) {
  print(replicate(999, sum(sample(inf.F.data, n))))
}
# Unable to put the result in an objet for plotting 
# --- END TEST --- #

#Sampling
inf.F1 <- replicate(999, sample(inf.F.data, 1))
inf.F2 <- replicate(999, sum(sample(inf.F.data, 2)))
inf.F3 <- replicate(999, sum(sample(inf.F.data, 3)))
inf.F4 <- replicate(999, sum(sample(inf.F.data, 4)))
inf.F5 <- replicate(999, sum(sample(inf.F.data, 5)))
inf.F6 <- replicate(999, sum(sample(inf.F.data, 6)))
inf.F7 <- replicate(999, sum(sample(inf.F.data, 7)))
inf.F8 <- replicate(999, sum(sample(inf.F.data, 8)))
inf.F9 <- replicate(999, sum(sample(inf.F.data, 9)))
inf.F10 <- replicate(999, sum(sample(inf.F.data, 10)))

#Placing results in a data frame
inf.F.all <- c(inf.F1, inf.F2, inf.F3, inf.F4, inf.F5, inf.F6, inf.F7, inf.F8, inf.F9, inf.F10)
N <- c(rep(1,999), rep(2,999), rep(3,999), rep(4,999), rep(5,999), rep(6,999), rep(7,999), rep(8,999), rep(9,999), rep(10,999))
df.simulation <- data.frame(N, inf.F.all, row.names = NULL)

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
inf.MT4 <- replicate(999, sum(sample(inf.MT.data, 4)))
inf.MT5 <- replicate(999, sum(sample(inf.MT.data, 5)))
inf.MT6 <- replicate(999, sum(sample(inf.MT.data, 6)))
inf.MT7 <- replicate(999, sum(sample(inf.MT.data, 7)))
inf.MT8 <- replicate(999, sum(sample(inf.MT.data, 8)))
inf.MT9 <- replicate(999, sum(sample(inf.MT.data, 9)))
inf.MT10 <- replicate(999, sum(sample(inf.MT.data, 10)))

#Placing results the a data frame
inf.MT.all <- c(inf.MT1, inf.MT2, inf.MT3, inf.MT4, inf.MT5, inf.MT6, inf.MT7, inf.MT8, inf.MT9, inf.MT10)
df.simulation <- df.simulation %>%  
  mutate(inf.MT.all = inf.MT.all)

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
inf.S4 <- replicate(999, sum(sample(inf.S.data, 4)))
inf.S5 <- replicate(999, sum(sample(inf.S.data, 5)))
inf.S6 <- replicate(999, sum(sample(inf.S.data, 6)))
inf.S7 <- replicate(999, sum(sample(inf.S.data, 7)))
inf.S8 <- replicate(999, sum(sample(inf.S.data, 8)))
inf.S9 <- replicate(999, sum(sample(inf.S.data, 9)))
inf.S10 <- replicate(999, sum(sample(inf.S.data, 10)))

#Placing results the data frame
inf.S.all <- c(inf.S1, inf.S2, inf.S3, inf.S4, inf.S5, inf.S6, inf.S7, inf.S8, inf.S9, inf.S10)
df.simulation <- df.simulation %>%  
  mutate(inf.S.all = inf.S.all)

## Transect method ----

TransectData <- CombinedData %>% 
  filter(Sampling_method == "Transect") #Selecting transect method

inf.T.data <- TransectData %>% 
  select(starts_with("inf")) %>% #Selecting infected individuals
  rowSums() %>% #Sum of all species 
  na.omit() #Deleting NAs

#Sampling
inf.T1 <- replicate(999, sample(inf.T.data, 1))
inf.T2 <- replicate(999, sum(sample(inf.T.data, 2)))
inf.T3 <- replicate(999, sum(sample(inf.T.data, 3)))
inf.T4 <- replicate(999, sum(sample(inf.T.data, 4)))
inf.T5 <- replicate(999, sum(sample(inf.T.data, 5)))
inf.T6 <- replicate(999, sum(sample(inf.T.data, 6)))
inf.T7 <- replicate(999, sum(sample(inf.T.data, 7)))
inf.T8 <- replicate(999, sum(sample(inf.T.data, 8)))
inf.T9<- replicate(999, sum(sample(inf.T.data, 9)))
inf.T10 <- replicate(999, sum(sample(inf.T.data, 10)))

#Placing results the data frame
inf.T.all <- c(inf.T1, inf.T2, inf.T3, inf.T4, inf.T5, inf.T6, inf.T7, inf.T8, inf.T9, inf.T10)
df.simulation <- df.simulation %>%  
  mutate(inf.T.all = inf.T.all)

## Plotting simulation ----

inf.acc.plot <- ggplot(df.simulation) + 
  scale_x_continuous(breaks = round(seq(min(N), max(N), by = 1), 1)) +
  scale_y_continuous(breaks = round(seq(0, 1000, by = 50), 1)) +
  stat_summary(aes(x = N, y = inf.F.all), fun = mean, color = "purple") + 
  stat_summary(aes(x = N, y = inf.T.all), fun = mean, color = "darkgoldenrod1") + 
  stat_summary(aes(x = N, y = inf.S.all), fun = mean, color = "red") + 
  stat_summary(aes(x = N, y = inf.MT.all), fun = mean, color = "blue") + 
  geom_smooth(aes(x = N, y = inf.T.all), method = "lm", se = TRUE, color = "darkgoldenrod1") + 
  geom_smooth(aes(x= N, y = inf.F.all), method = "lm", se = TRUE, color = "purple") +
  geom_smooth(aes(x = N, y = inf.S.all), method = "lm", se = TRUE, color = "red") + 
  geom_smooth(aes(x= N, y = inf.MT.all), method = "lm", se = TRUE, color = "blue") +
  labs(x = "Number of sampling", y = "Number of infected fishes", caption = "Orange = transects\nPuple = fishing\n Red = seine\nBlue = Minnow trap") + 
  theme_light()

ggsave(paste0(to.figs, "AccumulationCurves_infection.pdf"), plot = inf.acc.plot, dpi = 300, width = 15, height = 10)  

# ---- Individuals accumulation curves ----

## Fishing method ----

tot.F.data <- FishingData %>% 
  select(starts_with("tot")) %>% #Selecting infected individuals
  rowSums() #Sum of all species

#Sampling
tot.F1 <- replicate(999, sample(tot.F.data, 1))
tot.F2 <- replicate(999, sum(sample(tot.F.data, 2)))
tot.F3 <- replicate(999, sum(sample(tot.F.data, 3)))
tot.F4 <- replicate(999, sum(sample(tot.F.data, 4)))
tot.F5 <- replicate(999, sum(sample(tot.F.data, 5)))
tot.F6 <- replicate(999, sum(sample(tot.F.data, 6)))
tot.F7 <- replicate(999, sum(sample(tot.F.data, 7)))
tot.F8 <- replicate(999, sum(sample(tot.F.data, 8)))
tot.F9 <- replicate(999, sum(sample(tot.F.data, 9)))
tot.F10 <- replicate(999, sum(sample(tot.F.data, 10)))

#Placing results the data frame
tot.F.all <- c(tot.F1, tot.F2, tot.F3, tot.F4, tot.F5, tot.F6, tot.F7, tot.F8, tot.F9, tot.F10)

df.simulation <- df.simulation %>%  
  mutate(tot.F.all = tot.F.all)

## Minnow trap method ----

tot.MT.data <- MinnowTrapData %>% 
  select(starts_with("tot")) %>% #Selecting infected individuals
  rowSums() #Sum of all species

#Sampling
tot.MT1 <- replicate(999, sample(tot.MT.data, 1))
tot.MT2 <- replicate(999, sum(sample(tot.MT.data, 2)))
tot.MT3 <- replicate(999, sum(sample(tot.MT.data, 3)))
tot.MT4 <- replicate(999, sum(sample(tot.MT.data, 4)))
tot.MT5 <- replicate(999, sum(sample(tot.MT.data, 5)))
tot.MT6 <- replicate(999, sum(sample(tot.MT.data, 6)))
tot.MT7 <- replicate(999, sum(sample(tot.MT.data, 7)))
tot.MT8 <- replicate(999, sum(sample(tot.MT.data, 8)))
tot.MT9 <- replicate(999, sum(sample(tot.MT.data, 9)))
tot.MT10 <- replicate(999, sum(sample(tot.MT.data, 10)))

#Placing results the data frame
tot.MT.all <- c(tot.MT1, tot.MT2, tot.MT3, tot.MT4, tot.MT5, tot.MT6, tot.MT7, tot.MT8, tot.MT9, tot.MT10)

df.simulation <- df.simulation %>%  
  mutate(tot.MT.all = tot.MT.all)

## Seine method ----

tot.S.data <- SeineData %>% 
  select(starts_with("tot")) %>% #Selecting infected individuals
  rowSums() #Sum of all species

#Sampling
tot.S1 <- replicate(999, sample(tot.S.data, 1))
tot.S2 <- replicate(999, sum(sample(tot.S.data, 2)))
tot.S3 <- replicate(999, sum(sample(tot.S.data, 3)))
tot.S4 <- replicate(999, sum(sample(tot.S.data, 4)))
tot.S5 <- replicate(999, sum(sample(tot.S.data, 5)))
tot.S6 <- replicate(999, sum(sample(tot.S.data, 6)))
tot.S7 <- replicate(999, sum(sample(tot.S.data, 7)))
tot.S8 <- replicate(999, sum(sample(tot.S.data, 8)))
tot.S9 <- replicate(999, sum(sample(tot.S.data, 9)))
tot.S10 <- replicate(999, sum(sample(tot.S.data, 10)))

#Placing results the data frame
tot.S.all <- c(tot.S1, tot.S2, tot.S3, tot.S4, tot.S5, tot.S6, tot.S7, tot.S8, tot.S9, tot.S10)

df.simulation <- df.simulation %>%  
  mutate(tot.S.all = tot.S.all)

## Transect method ---

tot.T.data <- TransectData %>% 
  select(starts_with("tot")) %>% #Selecting infected individuals
  rowSums() %>% #Sum of all species 
  na.omit() #Deleting NAs

#Sampling
tot.T1 <- replicate(999, sample(tot.T.data, 1))
tot.T2 <- replicate(999, sum(sample(tot.T.data, 2)))
tot.T3 <- replicate(999, sum(sample(tot.T.data, 3)))
tot.T4 <- replicate(999, sum(sample(tot.T.data, 4)))
tot.T5 <- replicate(999, sum(sample(tot.T.data, 5)))
tot.T6 <- replicate(999, sum(sample(tot.T.data, 6)))
tot.T7 <- replicate(999, sum(sample(tot.T.data, 7)))
tot.T8 <- replicate(999, sum(sample(tot.T.data, 8)))
tot.T9 <- replicate(999, sum(sample(tot.T.data, 9)))
tot.T10 <- replicate(999, sum(sample(tot.T.data, 10)))

#Placing results the data frame
tot.T.all <- c(tot.T1, tot.T2, tot.T3, tot.T4, tot.T5, tot.T6, tot.T7, tot.T8, tot.T9, tot.T10)
df.simulation <- df.simulation %>%  
  mutate(tot.T.all = tot.T.all)

## Plotting simulation ----

tot.acc.plot <- ggplot(df.simulation) + 
  scale_x_continuous(breaks = round(seq(min(N), max(N), by = 1), 1)) +
  scale_y_continuous(breaks = round(seq(0, 2000, by = 100), 1)) +
  stat_summary(aes(x = N, y = tot.F.all), fun = mean, color = "purple") + 
  stat_summary(aes(x = N, y = tot.T.all), fun = mean, color = "darkgoldenrod1") + 
  stat_summary(aes(x = N, y = tot.S.all), fun = mean, color = "red") + 
  stat_summary(aes(x = N, y = tot.MT.all), fun = mean, color = "blue") + 
  geom_smooth(aes(x = N, y = tot.T.all), method = "lm", se = TRUE, color = "darkgoldenrod1") + 
  geom_smooth(aes(x= N, y = tot.F.all), method = "lm", se = TRUE, color = "purple") +
  geom_smooth(aes(x = N, y = tot.S.all), method = "lm", se = TRUE, color = "red") + 
  geom_smooth(aes(x= N, y = tot.MT.all), method = "lm", se = TRUE, color = "blue") +
  labs(x = "Number of sampling", y = "Number of fishes", caption = "Orange = transects\nPuple = fishing\n Red = seine\nBlue = Minnow trap") + 
  theme_light()

ggsave(paste0(to.figs, "AccumulationCurves_individuals.pdf"), plot = tot.acc.plot, dpi = 300, width = 15, height = 10)  

# ---- Prevalence accumulation curves ----

## Fishing method ----

prev.F <- inf.F.data / tot.F.data
prev.F <- na.omit(prev.F) #Deleting NAs as they mean that no fish were caught

#Sampling
prev.F1 <- replicate(999, sample(prev.F, 1))
prev.F2 <- replicate(999, mean(sample(prev.F, 2)))
prev.F3 <- replicate(999, mean(sample(prev.F, 3)))
prev.F4 <- replicate(999, mean(sample(prev.F, 4)))
prev.F5 <- replicate(999, mean(sample(prev.F, 5)))
prev.F6 <- replicate(999, mean(sample(prev.F, 6)))
prev.F7 <- replicate(999, mean(sample(prev.F, 7)))
prev.F8 <- replicate(999, mean(sample(prev.F, 8)))
prev.F9 <- replicate(999, mean(sample(prev.F, 9)))
prev.F10 <- replicate(999, mean(sample(prev.F, 10)))

#Placing results the data frame
prev.F.all <- c(prev.F1, prev.F2, prev.F3, prev.F4, prev.F5, prev.F6, prev.F7, prev.F8, prev.F9, prev.F10)

df.simulation <- df.simulation %>%  
  mutate(prev.F.all = prev.F.all)

## Minnow trap method ----

prev.MT <- inf.MT.data / tot.MT.data
prev.MT <- na.omit(prev.MT) #Deleting NAs as they mean that no fish were caught

#Sampling
prev.MT1 <- replicate(999, sample(prev.MT, 1))
prev.MT2 <- replicate(999, mean(sample(prev.MT, 2)))
prev.MT3 <- replicate(999, mean(sample(prev.MT, 3)))
prev.MT4 <- replicate(999, mean(sample(prev.MT, 4)))
prev.MT5 <- replicate(999, mean(sample(prev.MT, 5)))
prev.MT6 <- replicate(999, mean(sample(prev.MT, 6)))
prev.MT7 <- replicate(999, mean(sample(prev.MT, 7)))
prev.MT8 <- replicate(999, mean(sample(prev.MT, 8)))
prev.MT9 <- replicate(999, mean(sample(prev.MT, 9)))
prev.MT10 <- replicate(999, mean(sample(prev.MT, 10)))

#Placing results the data frame
prev.MT.all <- c(prev.MT1, prev.MT2, prev.MT3, prev.MT4, prev.MT5, prev.MT6, prev.MT7, prev.MT8, prev.MT9, prev.MT10)

df.simulation <- df.simulation %>%  
  mutate(prev.MT.all = prev.MT.all)

## Seine method ----

prev.S <- inf.S.data / tot.S.data
prev.S <- na.omit(prev.S) #Deleting NAs as they mean that no fish were caught

#Sampling
prev.S1 <- replicate(999, sample(prev.S, 1))
prev.S2 <- replicate(999, mean(sample(prev.S, 2)))
prev.S3 <- replicate(999, mean(sample(prev.S, 3)))
prev.S4 <- replicate(999, mean(sample(prev.S, 4)))
prev.S5 <- replicate(999, mean(sample(prev.S, 5)))
prev.S6 <- replicate(999, mean(sample(prev.S, 6)))
prev.S7 <- replicate(999, mean(sample(prev.S, 7)))
prev.S8 <- replicate(999, mean(sample(prev.S, 8)))
prev.S9 <- replicate(999, mean(sample(prev.S, 9)))
prev.S10 <- replicate(999, mean(sample(prev.S, 10)))

#Placing results the data frame
prev.S.all <- c(prev.S1, prev.S2, prev.S3, prev.S4, prev.S5, prev.S6, prev.S7, prev.S8, prev.S9, prev.S10)

df.simulation <- df.simulation %>%  
  mutate(prev.S.all = prev.S.all)

## Transect method ----

prev.T <- inf.T.data / tot.T.data
prev.T <- na.omit(prev.T) #Deleting NAs as they mean that no fish were caught

#Sampling
prev.T1 <- replicate(999, sample(prev.T, 1))
prev.T2 <- replicate(999, mean(sample(prev.T, 2)))
prev.T3 <- replicate(999, mean(sample(prev.T, 3)))
prev.T4 <- replicate(999, mean(sample(prev.T, 4)))
prev.T5 <- replicate(999, mean(sample(prev.T, 5)))
prev.T6 <- replicate(999, mean(sample(prev.T, 6)))
prev.T7 <- replicate(999, mean(sample(prev.T, 7)))
prev.T8 <- replicate(999, mean(sample(prev.T, 8)))
prev.T9 <- replicate(999, mean(sample(prev.T, 9)))
prev.T10 <- replicate(999, mean(sample(prev.T, 10)))

#Placing results the data frame
prev.T.all <- c(prev.T1, prev.T2, prev.T3, prev.T4, prev.T5, prev.T6, prev.T7, prev.T8, prev.T9, prev.T10)

df.simulation <- df.simulation %>%  
  mutate(prev.T.all = prev.T.all)

## Plotting simulation ----

prev.acc.plot <- ggplot(df.simulation) + 
  scale_x_continuous(breaks = round(seq(min(N), max(N), by = 1), 1)) +
  stat_summary(aes(x = N, y = prev.F.all), fun = mean, color = "purple") + 
  stat_summary(aes(x = N, y = prev.T.all), fun = mean, color = "darkgoldenrod1") + 
  stat_summary(aes(x = N, y = prev.S.all), fun = mean, color = "red") + 
  stat_summary(aes(x = N, y = prev.MT.all), fun = mean, color = "blue") + 
  geom_smooth(aes(x = N, y = prev.T.all), method = "lm", se = TRUE, color = "darkgoldenrod1") + 
  geom_smooth(aes(x= N, y = prev.F.all), method = "lm", se = TRUE, color = "purple") +
  geom_smooth(aes(x = N, y = prev.S.all), method = "lm", se = TRUE, color = "red") + 
  geom_smooth(aes(x= N, y = prev.MT.all), method = "lm", se = TRUE, color = "blue") +
  labs(x = "Number of sampling", y = "Prevalence", caption = "Orange = transects\nPuple = fishing\n Red = seine\nBlue = Minnow trap") + 
  theme_light()

ggsave(paste0(to.figs, "AccumulationCurves_prevalence.pdf"), plot = prev.acc.plot, dpi = 300, width = 15, height = 10)  
