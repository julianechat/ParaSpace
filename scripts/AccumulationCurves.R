## Accumulation curves ##

# ----- R Setup ----- #

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"

# ----- Loading packages ----- #

library(vegan)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

# ----- Loading data ----- #

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))

# ----------------------------- #

#### Species accumulation through sampling ####
## Minnow traps ##
MTdata <- CombinedData %>% filter(Sampling_method == "Minnow_trap") #Selecting minnow trap method
MTdata <- MTdata[c(9:25)] #Keeping community matrix (total abundance)

acc.MT <- specaccum(MTdata, method = "random", permutations = 1000, gamma = "jack1") #Accumulation curve function
plot(acc.MT, col = "blue", xlab = "sampling", ylab = "species")

## Seine ## 
Sdata <- CombinedData %>% filter(Sampling_method == "Seine") #Selecting seine method
Sdata <- Sdata[c(9:25)] #Keeping community matrix (total abundance)

acc.S <- specaccum(Sdata, method = "random", permutations = 1000, gamma = "jack1") #Accumulation curve function
plot(acc.S, col = "red", xlab = "sampling", ylab = "species")

## Transect ##
Tdata <- CombinedData %>% filter(Sampling_method == "Transect") #Selecting transect method
Tdata <- na.omit(Tdata) #Deleting NA data
Tdata <- Tdata[c(9:25)] #Keeping community matrix (total abundance)

acc.T <- specaccum(Tdata, method = "random", permutations = 1000, gamma = "jack1") #Accumulation curve function
plot(acc.T, col = "orange", xlab = "sampling", ylab = "species")

## Method comparison ##
plot(acc.S, col = "red", xlab = "sampling", ylab = "species")
plot(acc.MT, add = TRUE, col = "blue", xlab = "sampling", ylab = "species")
plot(acc.T, add = TRUE, col = "gold", xlab = "sampling", ylab = "species")

legend(52, 5, legend = c("Seine", "Minnow Trap", "Transect"),
       fill = c("red", "blue", "gold"))

#### Infected species through sampling ####
## Fishing method ##
FishingData <- CombinedData %>% filter(Sampling_method == "Minnow_trap" | Sampling_method == "Seine") #Selecting for fishing methods
LeGiFishing <- FishingData %>% filter(!(Lake == "Beaver" | Lake == "St-Onge" | Lake == "Montaubois" | Lake == "Tracy")) #Dropping rows of lake without pumpkinseed sunfish  
LeGiFishing <- LeGiFishing[c(2,30)] #Keeping LeGi data

# Sampling n fishing sample, 999 times for n = 1:10 #
# --- TEST --- # Faire le sampling dans une boucle # Pas capable de mettre dans un objet
for (n in seq_along(2:10)) {
  print(replicate(999, sum(sample(LeGiFishing$inf_LeGi, n))))
}

# --- END TEST --- #

FN1 <- replicate(999, sample(LeGiFishing$inf_LeGi, 1))
FN2 <- replicate(999, sum(sample(LeGiFishing$inf_LeGi, 2)))
FN3 <- replicate(999, sum(sample(LeGiFishing$inf_LeGi, 3)))
FN4 <- replicate(999, sum(sample(LeGiFishing$inf_LeGi, 4)))
FN5 <- replicate(999, sum(sample(LeGiFishing$inf_LeGi, 5)))
FN6 <- replicate(999, sum(sample(LeGiFishing$inf_LeGi, 6)))
FN7 <- replicate(999, sum(sample(LeGiFishing$inf_LeGi, 7)))
FN8 <- replicate(999, sum(sample(LeGiFishing$inf_LeGi, 8)))
FN9 <- replicate(999, sum(sample(LeGiFishing$inf_LeGi, 9)))
FN10 <- replicate(999, sum(sample(LeGiFishing$inf_LeGi, 10)))

# Result df #
F.all <- c(FN1, FN2, FN3, FN4, FN5, FN6, FN7, FN8, FN9, FN10)
Nb.Samp <- c(rep(1,999), rep(2,999), rep(3,999), rep(4,999), rep(5,999), rep(6,999), rep(7,999), rep(8,999), rep(9,999), rep(10,999))
df.infAcc_F <- data.frame(Nb.Samp, F.all, row.names = NULL)

## Transect method ##
TransectData <- CombinedData %>% filter(Sampling_method == "Transect") #Selecting for fishing methods
LeGiTransect <- TransectData %>% filter(!(Lake == "Beaver" | Lake == "St-Onge" | Lake == "Montaubois" | Lake == "Tracy")) #Dropping rows of lake without pumpkinseed sunfish  
LeGiTransect <- LeGiTransect[c(2,30)] #Keeping LGi data

# Sampling n transect sample, 999 times for n = 1:10 #
TN1 <- replicate(999, sample(LeGiTransect$inf_LeGi, 1))
TN2 <- replicate(999, sum(sample(LeGiTransect$inf_LeGi, 2)))
TN3 <- replicate(999, sum(sample(LeGiTransect$inf_LeGi, 3)))
TN4 <- replicate(999, sum(sample(LeGiTransect$inf_LeGi, 4)))
TN5 <- replicate(999, sum(sample(LeGiTransect$inf_LeGi, 5)))
TN6 <- replicate(999, sum(sample(LeGiTransect$inf_LeGi, 6)))
TN7 <- replicate(999, sum(sample(LeGiTransect$inf_LeGi, 7)))
TN8 <- replicate(999, sum(sample(LeGiTransect$inf_LeGi, 8)))
TN9 <- replicate(999, sum(sample(LeGiTransect$inf_LeGi, 9)))
TN10 <- replicate(999, sum(sample(LeGiTransect$inf_LeGi, 10)))

# Result df #
T.all <- c(TN1, TN2, TN3, TN4, TN5, TN6, TN7, TN8, TN9, TN10)
Nb.Samp <- c(rep(1,999), rep(2,999), rep(3,999), rep(4,999), rep(5,999), rep(6,999), rep(7,999), rep(8,999), rep(9,999), rep(10,999))
df.infAcc_T <- data.frame(Nb.Samp, T.all, row.names = NULL)

## Data representation ##
df.infAcc <- cbind(df.infAcc_F, df.infAcc_T[2])

ggplot(df.infAcc) + scale_x_continuous(breaks = round(seq(min(Nb.Samp), max(Nb.Samp), by = 1), 1)) +
  scale_y_continuous(breaks = round(seq(0, 600, by = 50), 1)) +
  stat_summary(aes(x=Nb.Samp, y = F.all), fun = mean, color = "purple") + 
  stat_summary(aes(x=Nb.Samp, y = T.all), fun = mean, color = "orange") + 
  geom_smooth(aes(x= Nb.Samp, y = T.all), method = "lm", se = TRUE, color = "orange") + 
  geom_smooth(aes(x= Nb.Samp, y = F.all), method = "lm", se = TRUE, color = "purple") +
  labs(x = "Number of sampling", y = "Number of infected fish", caption = "orange = transects ; puple = fishing") +
  theme_light()

ggsave(paste0(to.figs, "AccCurves_Infected+Sampling.png"), plot = last_plot(), dpi = 300, width = 15, height = 10)  

#### Prevalence through sampling ####
## Prevalence data ##
tot.matrix <- CombinedData[9:25]
inf.matrix <- CombinedData[26:42]
SamplingIDs <- CombinedData[2]

prev.matrix <- inf.matrix/tot.matrix 
prev.Names <- str_replace_all(colnames(prev.matrix), "inf_", '')
prev.Sampling <- `colnames<-`(prev.matrix, prev.Names)
prev.Sampling <- cbind(SamplingIDs, prev.Sampling) #Prevalence matrix for every species

prev.Samp_LeGi <- prev.Sampling[c(1, 6)] #Infection prevalence for LeGi through sampling
prev.Samp_LeGi$Method <- ifelse(grepl("_N_|_S_", prev.Samp_LeGi$Sampling_ID), "Fishing", "Transects") # Creating method column

## Transect method ##
T.Prev <- prev.Samp_LeGi %>% filter(Method == "Transects") #Selecting for transect method
T.Prev <- na.omit(T.Prev)

# Sampling n transect sample, 999 times for n = 1:10 #
TN1 <- replicate(999, sample(T.Prev$LeGi, 1))
TN2 <- replicate(999, mean(sample(T.Prev$LeGi, 2)))
TN3 <- replicate(999, mean(sample(T.Prev$LeGi, 3)))
TN4 <- replicate(999, mean(sample(T.Prev$LeGi, 4)))
TN5 <- replicate(999, mean(sample(T.Prev$LeGi, 5)))
TN6 <- replicate(999, mean(sample(T.Prev$LeGi, 6)))
TN7 <- replicate(999, mean(sample(T.Prev$LeGi, 7)))
TN8 <- replicate(999, mean(sample(T.Prev$LeGi, 8)))
TN9 <- replicate(999, mean(sample(T.Prev$LeGi, 9)))
TN10 <- replicate(999, mean(sample(T.Prev$LeGi, 10)))

# Result df #
T.all <- c(TN1, TN2, TN3, TN4, TN5, TN6, TN7, TN8, TN9, TN10)
Nb.Samp <- c(rep(1,999), rep(2,999), rep(3,999), rep(4,999), rep(5,999), rep(6,999), rep(7,999), rep(8,999), rep(9,999), rep(10,999))
df.PrevAcc_T <- data.frame(Nb.Samp, T.all, row.names = NULL)

## Fishing methods ##
F.Prev <- prev.Samp_LeGi %>% filter(Method == "Fishing") #Selecting for fishing methods
F.Prev <- na.omit(F.Prev)

# Sampling n fishing sample, 999 times for n = 1:10 #
FN1 <- replicate(999, sample(F.Prev$LeGi, 1))
FN2 <- replicate(999, mean(sample(F.Prev$LeGi, 2)))
FN3 <- replicate(999, mean(sample(F.Prev$LeGi, 3)))
FN4 <- replicate(999, mean(sample(F.Prev$LeGi, 4)))
FN5 <- replicate(999, mean(sample(F.Prev$LeGi, 5)))
FN6 <- replicate(999, mean(sample(F.Prev$LeGi, 6)))
FN7 <- replicate(999, mean(sample(F.Prev$LeGi, 7)))
FN8 <- replicate(999, mean(sample(F.Prev$LeGi, 8)))
FN9 <- replicate(999, mean(sample(F.Prev$LeGi, 9)))
FN10 <- replicate(999, mean(sample(F.Prev$LeGi, 10)))

# Results df #
F.all <- c(FN1, FN2, FN3, FN4, FN5, FN6, FN7, FN8, FN9, FN10)
Nb.Samp <- c(rep(1,999), rep(2,999), rep(3,999), rep(4,999), rep(5,999), rep(6,999), rep(7,999), rep(8,999), rep(9,999), rep(10,999))
df.PrevAcc_F <- data.frame(Nb.Samp, F.all, row.names = NULL)

## Data representation ##
df.PrevAcc <- cbind(df.PrevAcc_F, df.PrevAcc_T[2])

ggplot(df.PrevAcc) + 
  stat_summary(aes(x=Nb.Samp, y = F.all), fun = mean, color = "purple") +
  stat_summary(aes(x=Nb.Samp, y = T.all), fun = mean, color = "orange") +
  geom_smooth(aes(x= Nb.Samp, y = T.all), method = "lm", se = TRUE, color = "orange") + 
  geom_smooth(aes(x= Nb.Samp, y = F.all), method = "lm", se = TRUE, color = "purple") + 
  scale_x_continuous(breaks = round(seq(min(Nb.Samp), max(Nb.Samp), by = 1), 1)) +
  labs(x = "Number of sampling", y = "Prevalence of infection", caption = "orange = transects ; puple = fishing") +
  theme_light()

ggsave(paste0(to.figs, "AccCurves_Prevalence+Sampling.png"), plot = last_plot(), dpi = 300, width = 15, height = 10) 
