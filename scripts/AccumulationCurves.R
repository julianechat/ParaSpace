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
inf.A4 <- replicate(999, sum(sample(inf.A.data, 5)))
inf.A5 <- replicate(999, sum(sample(inf.A.data, 7)))
inf.A6 <- replicate(999, sum(sample(inf.A.data, 10)))
inf.A7 <- replicate(999, sum(sample(inf.A.data, 15)))
inf.A8 <- replicate(999, sum(sample(inf.A.data, 20)))
inf.A9 <- replicate(999, sum(sample(inf.A.data, 25)))
inf.A10 <- replicate(999, sum(sample(inf.A.data, 35)))

#Placing results in a data frame
inf <- c(inf.A1, inf.A2, inf.A3, inf.A4, inf.A5, inf.A6, inf.A7, inf.A8, inf.A9, inf.A10) 
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
inf.MT4 <- replicate(999, sum(sample(inf.MT.data, 5)))
inf.MT5 <- replicate(999, sum(sample(inf.MT.data, 7)))
inf.MT6 <- replicate(999, sum(sample(inf.MT.data, 10)))
inf.MT7 <- replicate(999, sum(sample(inf.MT.data, 15)))
inf.MT8 <- replicate(999, sum(sample(inf.MT.data, 20)))
inf.MT9 <- replicate(999, sum(sample(inf.MT.data, 25)))
inf.MT10 <- replicate(999, sum(sample(inf.MT.data, 35)))

#Placing results the a data frame
inf <- c(inf.MT1, inf.MT2, inf.MT3, inf.MT4, inf.MT5, inf.MT6, inf.MT7, inf.MT8, inf.MT9, inf.MT10)
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
inf.S4 <- replicate(999, sum(sample(inf.S.data, 5)))
inf.S5 <- replicate(999, sum(sample(inf.S.data, 7)))
inf.S6 <- replicate(999, sum(sample(inf.S.data, 10)))
inf.S7 <- replicate(999, sum(sample(inf.S.data, 15)))
inf.S8 <- replicate(999, sum(sample(inf.S.data, 20)))
inf.S9 <- replicate(999, sum(sample(inf.S.data, 25)))
inf.S10 <- replicate(999, sum(sample(inf.S.data, 35)))

#Placing results the data frame
inf <- c(inf.S1, inf.S2, inf.S3, inf.S4, inf.S5, inf.S6, inf.S7, inf.S8, inf.S9, inf.S10)
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
inf.T4 <- replicate(999, sum(sample(inf.T.data, 5)))
inf.T5 <- replicate(999, sum(sample(inf.T.data, 7)))
inf.T6 <- replicate(999, sum(sample(inf.T.data, 10)))
inf.T7 <- replicate(999, sum(sample(inf.T.data, 15)))
inf.T8 <- replicate(999, sum(sample(inf.T.data, 20)))
inf.T9<- replicate(999, sum(sample(inf.T.data, 25)))
inf.T10 <- replicate(999, sum(sample(inf.T.data, 35)))

#Placing results the data frame
inf <- c(inf.T1, inf.T2, inf.T3, inf.T4, inf.T5, inf.T6, inf.T7, inf.T8, inf.T9, inf.T10)
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
tot.A4 <- replicate(999, sum(sample(tot.A.data, 5)))
tot.A5 <- replicate(999, sum(sample(tot.A.data, 7)))
tot.A6 <- replicate(999, sum(sample(tot.A.data, 10)))
tot.A7 <- replicate(999, sum(sample(tot.A.data, 15)))
tot.A8 <- replicate(999, sum(sample(tot.A.data, 20)))
tot.A9 <- replicate(999, sum(sample(tot.A.data, 25)))
tot.A10 <- replicate(999, sum(sample(tot.A.data, 35)))

#Placing results the data frame
tot <- c(tot.A1, tot.A2, tot.A3, tot.A4, tot.A5, tot.A6, tot.A7, tot.A8, tot.A9, tot.A10)
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
tot.MT4 <- replicate(999, sum(sample(tot.MT.data, 5)))
tot.MT5 <- replicate(999, sum(sample(tot.MT.data, 7)))
tot.MT6 <- replicate(999, sum(sample(tot.MT.data, 10)))
tot.MT7 <- replicate(999, sum(sample(tot.MT.data, 15)))
tot.MT8 <- replicate(999, sum(sample(tot.MT.data, 20)))
tot.MT9 <- replicate(999, sum(sample(tot.MT.data, 25)))
tot.MT10 <- replicate(999, sum(sample(tot.MT.data, 35)))

#Placing results the data frame
tot <- c(tot.MT1, tot.MT2, tot.MT3, tot.MT4, tot.MT5, tot.MT6, tot.MT7, tot.MT8, tot.MT9, tot.MT10)

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
tot.S4 <- replicate(999, sum(sample(tot.S.data, 5)))
tot.S5 <- replicate(999, sum(sample(tot.S.data, 7)))
tot.S6 <- replicate(999, sum(sample(tot.S.data, 10)))
tot.S7 <- replicate(999, sum(sample(tot.S.data, 15)))
tot.S8 <- replicate(999, sum(sample(tot.S.data, 20)))
tot.S9 <- replicate(999, sum(sample(tot.S.data, 25)))
tot.S10 <- replicate(999, sum(sample(tot.S.data, 35)))

#Placing results the data frame
tot <- c(tot.S1, tot.S2, tot.S3, tot.S4, tot.S5, tot.S6, tot.S7, tot.S8, tot.S9, tot.S10)

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
tot.T4 <- replicate(999, sum(sample(tot.T.data, 5)))
tot.T5 <- replicate(999, sum(sample(tot.T.data, 7)))
tot.T6 <- replicate(999, sum(sample(tot.T.data, 10)))
tot.T7 <- replicate(999, sum(sample(tot.T.data, 15)))
tot.T8 <- replicate(999, sum(sample(tot.T.data, 20)))
tot.T9 <- replicate(999, sum(sample(tot.T.data, 25)))
tot.T10 <- replicate(999, sum(sample(tot.T.data, 35)))

#Placing results the data frame
tot <- c(tot.T1, tot.T2, tot.T3, tot.T4, tot.T5, tot.T6, tot.T7, tot.T8, tot.T9, tot.T10)
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
prev.A <- na.omit(prev.A) #Deleting NAs as they mean that no fish were caught

#Sampling
prev.A1 <- replicate(999, sample(prev.A, 1))
prev.A2 <- replicate(999, mean(sample(prev.A, 2)))
prev.A3 <- replicate(999, mean(sample(prev.A, 3)))
prev.A4 <- replicate(999, mean(sample(prev.A, 5)))
prev.A5 <- replicate(999, mean(sample(prev.A, 7)))
prev.A6 <- replicate(999, mean(sample(prev.A, 10)))
prev.A7 <- replicate(999, mean(sample(prev.A, 15)))
prev.A8 <- replicate(999, mean(sample(prev.A, 20)))
prev.A9 <- replicate(999, mean(sample(prev.A, 25)))
prev.A10 <- replicate(999, mean(sample(prev.A, 35)))

#Placing results the data frame
prev <- c(prev.A1, prev.A2, prev.A3, prev.A4, prev.A5, prev.A6, prev.A7, prev.A8, prev.A9, prev.A10)
df.All <- df.All %>%  
  mutate(prev)

## Minnow trap method ----

prev.MT <- inf.MT.data / tot.MT.data
prev.MT <- na.omit(prev.MT) #Deleting NAs as they mean that no fish were caught

#Sampling
prev.MT1 <- replicate(999, sample(prev.MT, 1))
prev.MT2 <- replicate(999, mean(sample(prev.MT, 2)))
prev.MT3 <- replicate(999, mean(sample(prev.MT, 3)))
prev.MT4 <- replicate(999, mean(sample(prev.MT, 5)))
prev.MT5 <- replicate(999, mean(sample(prev.MT, 7)))
prev.MT6 <- replicate(999, mean(sample(prev.MT, 10)))
prev.MT7 <- replicate(999, mean(sample(prev.MT, 15)))
prev.MT8 <- replicate(999, mean(sample(prev.MT, 20)))
prev.MT9 <- replicate(999, mean(sample(prev.MT, 25)))
prev.MT10 <- replicate(999, mean(sample(prev.MT, 35)))

#Placing results the data frame
prev <- c(prev.MT1, prev.MT2, prev.MT3, prev.MT4, prev.MT5, prev.MT6, prev.MT7, prev.MT8, prev.MT9, prev.MT10)
df.MinnowTrap <- df.MinnowTrap %>%  
  mutate(prev)

## Seine method ----

prev.S <- inf.S.data / tot.S.data
prev.S <- na.omit(prev.S) #Deleting NAs as they mean that no fish were caught

#Sampling
prev.S1 <- replicate(999, sample(prev.S, 1))
prev.S2 <- replicate(999, mean(sample(prev.S, 2)))
prev.S3 <- replicate(999, mean(sample(prev.S, 3)))
prev.S4 <- replicate(999, mean(sample(prev.S, 5)))
prev.S5 <- replicate(999, mean(sample(prev.S, 7)))
prev.S6 <- replicate(999, mean(sample(prev.S, 10)))
prev.S7 <- replicate(999, mean(sample(prev.S, 15)))
prev.S8 <- replicate(999, mean(sample(prev.S, 20)))
prev.S9 <- replicate(999, mean(sample(prev.S, 25)))
prev.S10 <- replicate(999, mean(sample(prev.S, 35)))

#Placing results the data frame
prev <- c(prev.S1, prev.S2, prev.S3, prev.S4, prev.S5, prev.S6, prev.S7, prev.S8, prev.S9, prev.S10)
df.Seine<- df.Seine %>%  
  mutate(prev)

## Transect method ----

prev.T <- inf.T.data / tot.T.data
prev.T <- na.omit(prev.T) #Deleting NAs as they mean that no fish were observed

#Sampling
prev.T1 <- replicate(999, sample(prev.T, 1))
prev.T2 <- replicate(999, mean(sample(prev.T, 2)))
prev.T3 <- replicate(999, mean(sample(prev.T, 3)))
prev.T4 <- replicate(999, mean(sample(prev.T, 5)))
prev.T5 <- replicate(999, mean(sample(prev.T, 7)))
prev.T6 <- replicate(999, mean(sample(prev.T, 10)))
prev.T7 <- replicate(999, mean(sample(prev.T, 15)))
prev.T8 <- replicate(999, mean(sample(prev.T, 20)))
prev.T9 <- replicate(999, mean(sample(prev.T, 25)))
prev.T10 <- replicate(999, mean(sample(prev.T, 35)))

#Placing results the data frame
prev <- c(prev.T1, prev.T2, prev.T3, prev.T4, prev.T5, prev.T6, prev.T7, prev.T8, prev.T9, prev.T10)
df.Transect <- df.Transect %>%  
  mutate(prev)

## Plotting simulation ----

df.prev <- rbind(df.All, df.MinnowTrap, df.Seine, df.Transect)

prev.acc.plot <- ggplot(df.prev) + 
  stat_summary(aes(x = N, y = prev, group = Method, color = Method, shape = Method), fun = mean, size = 1) +
  geom_smooth(aes(x= N, y = prev, group = Method, color = Method, fill = Method), method = "glm", method.args = list(family = "binomial"), se = TRUE, lineend = "round", alpha = 0.3) +
  scale_x_continuous(breaks = c(1, 2, 3, 5, 7, 10, 15, 25, 20, 25, 35)) +
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

hist(df.Transect$prev)
glm.T <- glm(prev ~ N, data = df.Transect, family = "quasibinomial", weights = tot)
#glm.T2 <- glm(cbind(inf, tot-inf) ~ N, data = df.Transect, family = "quasibinomial")
summary(glm.T)
plogis(glm.T$coefficients[1]) 
#plogis(glm.T$coefficients[2])
#Intercept: 0.4206012*** 
#slope do not differ from 0 (not significative)
#N unsignificative
#slightly negative


hist(df.Seine$prev)
glm.S <- glm(prev ~ N, data = df.Seine, family = "quasibinomial", weights = tot) #Fonctionne pas parce que prev et tot ne sont pas les mêmes simulations
#glm.S2 <- glm(cbind(inf, tot-inf) ~ N, data = df.Seine, family = "quasibinomial") #Inf et tot ne sont pas liés par proportion
summary(glm.S)
plogis(glm.S$coefficients[1])
#plogis(glm.S$coefficients[2])
#Intercept: 0.449682*** 
#slope do not differ from 0 (not significative)
#N unsignificative
#slightly negative


hist(df.MinnowTrap$prev)
glm.MT <- glm(prev ~ N, data = df.MinnowTrap, family = "quasibinomial", weights = tot)
summary(glm.MT)
plogis(glm.MT$coefficients[1])
#Intercept: 0.4485509*** 
#slope do not differ from 0 (not significative)
#N unsignificative
#slightly positive

hist(df.All$prev)
glm.A <- glm(prev ~ N, data = df.All, family = "quasibinomial", weights = tot)
summary(glm.A)
plogis(glm.A$coefficients[1])
#Intercept: 0.4388589***
#slope do not differ from 0 (not significative)
#N significative
#slightly positive


### Models summary table ----

#Changing intercept values for plogis transformed intercept values
tab.glm.A <- tidy(glm.A)
tab.glm.A[1,2] <- plogis(glm.A$coefficients[1])
  
tab.glm.MT <- tidy(glm.MT)
tab.glm.MT[1,2] <- plogis(glm.MT$coefficients[1])

tab.glm.S <- tidy(glm.S)
tab.glm.S[1,2] <- plogis(glm.S$coefficients[1])

tab.glm.T <- tidy(glm.T)
tab.glm.T[1,2] <- plogis(glm.T$coefficients[1])

#Binding tables and formatting 
tab.mod.summary <- rbind(tab.glm.A, tab.glm.MT, tab.glm.S, tab.glm.T) %>% 
  mutate(Method = c("All", "All", "Minnow trap", "Minnow trap", "Seine net", "Seine net", "Transect", "Transect"), .before = term) %>% 
  gt(groupname_col = "Method") %>% 
  tab_header(md("**TABLE S16.** Summary results of the prevalence accumulation curve simulations. The models are glm with a quasibinomial family distribution.")) %>% 
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
            locations = cells_body(rows = c(2, 4, 6))) %>% 
  tab_footnote(footnote = "Value was plogis transformed to make interpretation easier.",
               locations = cells_body(columns = 3, rows = c(1,3,5,7)))

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

############## old ##############

write.csv(df.simulation,paste0(to.output,"Accum.simulation.csv"))
AccumData <- read.csv(paste0(to.output, "Accum.simulation.csv"))

lm.1 <- lm(prev.T.all ~ N, data=AccumData)
summary(lm.1)
#Intercept: 0.421*** 
#slope do not differ from 0 (not significative)
#positive
plot(lm.1)
hist(resid(lm.1))



lm.2 <- lm(prev.S.all ~ N, data=AccumData)
summary(lm.2)
#Intercept: 0.443*** 
#slope do not differ from 0 (not significative)
#slightly negative
plot(lm.2)


lm.3 <- lm(prev.MT.all ~ N, data=AccumData)
summary(lm.3)
#Intercept: 0.449*** 
#slope do not differ from 0 (not significative)
#slightly negative
plot(lm.1)


lm.4 <- lm(prev.A.all ~ N, data=AccumData)
summary(lm.4)
#Intercept: 0.448***
#slope do not differ from 0 (not significative)
#slightly significative
plot(lm.4)

