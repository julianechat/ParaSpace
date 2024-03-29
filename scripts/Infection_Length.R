## Script name : Infection & fish length relationships

## Authors : Juliane Vigneault
## Date created : September 20, 2022

## Copyright (c) Juliane Vigneault, 2022
## Email: juliane.vigneault@umontreal.ca

# ---- Script setup ----

## R Setup ----- 

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"
to.carto <- "./carto/"
to.rédaction <- "./rédaction/"

## Loading packages and functions ----

library(dplyr)
library(ggplot2)
library(cowplot)
library(splitstackshape)
library(dunn.test)
library(tidyr)

source(paste0(to.R, "anova.1way.R"))

## Loading data ----

Fishing_RawData <- read.csv(paste0(to.data, "Fishing_RawData.csv"), sep=";")

# ---- Mean length calculation ----

FishData <- Fishing_RawData[-c(596,613),] #Deleting lost data

LengthData <- FishData %>% 
  select(Lake, Gear_type, Gear_ID, Species_ID, Intensity_class, Length, Abundance) %>% 
  arrange(Lake, Species_ID) %>% 
  na.omit() #Delete sampling with no fish caught

LengthData <- expandRows(LengthData, "Abundance") #Reshaping data frame for 1 row = 1 individual format

## Landscape-scale ----

### Combined methods (Fishing methods) ----

Landscape.mean <- mean(LengthData$Length)
Landscape.sd <- sd(LengthData$Length)
  
### Seine net ----

LengthData.S <- LengthData %>% 
  filter(Gear_type == "Seine")

Landscape.mean.S <- mean(LengthData.S$Length)
Landscape.sd.S <- sd(LengthData.S$Length)

### Minnow traps ----

LengthData.MT <- LengthData %>% 
  filter(Gear_type == "Minnow_trap")

Landscape.mean.MT <- mean(LengthData.MT$Length)
Landscape.sd.MT <- sd(LengthData.MT$Length)

### Method comparison ----

hist(LengthData$Length)
hist(LengthData.MT$Length)
hist(LengthData.S$Length)

#Data distribution is right-skewed. Use a non parametric test is needed. 

#Test if length distribution of fish caught with minnow traps is different than of fish caught with seine net
wilcox.test(LengthData.MT$Length, LengthData.S$Length, alternative = "two.sided", paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# Methods caught different fish distribution

#Test if length distribution of fish caught with minnow traps is greater than of fish caught with seine net
wilcox.test(LengthData.MT$Length, LengthData.S$Length, alternative = "greater", paired = FALSE, conf.int = TRUE, conf.level = 0.95)
#Minnow traps caught longer fish than seine net

### Trap comparison ----

LengthData.MT.small <- LengthData.MT %>% 
  filter(Gear_ID %in% c("N1", "N2", "N3", "N4", "N5", "N12", "N13", "N14", "N15")) #Select small minnow traps

LengthData.MT.large <- LengthData.MT %>% 
  filter(Gear_ID %in% c("N6", "N7", "N8", "N9", "N10", "N11", "N16", "N17")) #Select large minnow traps

#Test if length distribution of fish caught with large minnow traps is greater than of fish caught with small minnow traps.
wilcox.test(LengthData.MT.large$Length, LengthData.MT.small$Length, alternative = "greater", paired = FALSE, conf.int = TRUE, conf.level = 0.95)
#Large minnow trap caught longer fish than small traps 

#Test if length distribution of fish caught with small minnow traps is less than of fish caught with seine net
wilcox.test(LengthData.MT.small$Length, LengthData.S$Length, alternative = "less", paired = FALSE, conf.int = TRUE, conf.level = 0.95)
#Small trap caught smaller fish than seine net

### By species ----

Length.SpeciesMean <- LengthData %>% #Summary statistic by species (mean, sd and N)
  group_by(Species_ID) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

## Lake-scale ----

### Combined methods ----

Length.LakeMean <- LengthData %>% #Summary statistic by lake (mean, sd and N)
  group_by(Lake) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

### Seine net ----

Length.LakeMean.S <- LengthData.S %>% 
  group_by(Lake) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

### Minnow trap ----

Length.LakeMean.MT <- LengthData.MT %>% 
  group_by(Lake) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

### By species ----

Length.SpeciesLakeMean <- LengthData %>% #Summarizing number of individuals, mean length and sd for each species within each lake
  group_by(Lake, Species_ID, .add = TRUE) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

# ---- Intensity & Length relationships ----

PeFlData <- LengthData %>% 
  filter(Species_ID == "PeFl") #Selecting PeFl data

LeGiData <- LengthData %>% 
  filter(Species_ID == "LeGi") #Selecting LeGi data

LengthData$Intensity_class <- as.factor(LengthData$Intensity_class) #Set intensity class as a factor column
PeFlData$Intensity_class <- as.factor(PeFlData$Intensity_class) #Set intensity class as a factor column
LeGiData$Intensity_class <- as.factor(LeGiData$Intensity_class) #Set intensity class as a factor column

## Boxplots ----

### Fish community ----

Comm.boxplot <- ggplot(LengthData, aes(y = Length, x = Intensity_class)) +
  geom_boxplot(color = "grey", fill = "grey", alpha = 0.5) +
  scale_y_continuous(breaks = c(5,10,15,20,25,30,35,40)) +
  labs(x = "Infection intensity class", y = "Length (cm)") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "Calibri Light", size = 20),
        axis.title.y = element_text(family = "Calibri Light", size = 20, vjust = 5),
        axis.title.x = element_text(family = "Calibri Light", size = 20, vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm")) 

Comm.boxplot  

ggsave(paste0(to.figs, "Intensity_Length_Comm.png"), plot = Comm.boxplot, dpi = 500, width = 15, height = 10)

### Perca flavescens ----

PeFl.boxplot <- ggplot(PeFlData, aes(y = Length, x = Intensity_class)) +
  geom_boxplot(color = "#6C464F", fill = "#6C464F", alpha = 0.5) +
  scale_y_continuous(breaks = c(5,10,15,20,25)) +
  labs(x = "Infection intensity class", y = "Length (cm)") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "Calibri Light", size = 20),
        axis.title.y = element_text(family = "Calibri Light", size = 20, vjust = 5),
        axis.title.x = element_text(family = "Calibri Light", size = 20, vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm")) 

PeFl.boxplot  

ggsave(paste0(to.figs, "Intensity_Length_PeFl.png"), plot = PeFl.boxplot, dpi = 500, width = 15, height = 10)

### Lepomis gibbosus ----

LeGi.boxplot <- ggplot(LeGiData, aes(y = Length, x = Intensity_class)) +
  geom_boxplot(color = "#587289", fill = "#587289", alpha = 0.5) +
  scale_y_continuous(breaks = c(5,10,15,20,25)) +
  labs(x = "Infection intensity class", y = "Length (cm)") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "Calibri Light", size = 20),
        axis.title.y = element_text(family = "Calibri Light", size = 20, vjust = 5),
        axis.title.x = element_text(family = "Calibri Light", size = 20, vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm")) 

LeGi.boxplot

ggsave(paste0(to.figs, "Intensity_Length_LeGi.png"), plot = LeGi.boxplot, dpi = 500, width = 15, height = 10)

## Length comparison between intensity class ----

#Use a non parametric test as data is not normally distributed

### Fish community ----
anova.1way(Length~Intensity_class, data = LengthData, nperm=999)
#Means differ between groups

dunn.test(LengthData$Length, LengthData$Intensity_class)
#Intensity 0 differs from Intensity 1
#Intensity 1 differs from Intensity 2
#Intensity 2 differs from Intensity 3
#Intensity 3 does not differ from Intensity 4
#Intensity 4 does not differ from Intensity 5

### Perca flavescens ----

anova.1way(Length~Intensity_class, data = PeFlData, nperm=999)
#Means differ between groups

dunn.test(PeFlData$Length, PeFlData$Intensity_class)
#Intensity 0 differs from Intensity 5
#Intensity 1 differs from Intensity 5

### Lepomis gibbosus ----

anova.1way(Length~Intensity_class, data = LeGiData, nperm=999)
#Means differ between groups

dunn.test(LeGiData$Length, LeGiData$Intensity_class)
#Intensity 0 does not differ from 1
#Intensity 1 differs from 2
#Intensity 2 differs from 3
#Intensity 3 does not differ from 4
#Intensity 4 does not differ from 5

# ---- Prevalence & Length relationships ----

## Prevalence values by length ----

LengthData.prev <- FishData %>% #Selecting data
  select(Lake, Gear_type, Gear_ID, Species_ID, Intensity_class, Length, Abundance) %>% 
  arrange(Lake, Species_ID) %>% 
  na.omit() #Delete sampling with no fish caught

FishData.inf  <- ifelse(LengthData.prev$Intensity_class > 0, LengthData.prev$Abundance, 0) #New abundance column for infected fish

LengthData.prev <- LengthData.prev %>% #Binding column
  mutate(Infected = FishData.inf)

### Fish community ----

CommPrevData <- LengthData.prev %>% #Sum abundance columns by length
  group_by(Length) %>% 
  summarise(across(.cols = Abundance | Infected, sum)) 

CommPrevData <- CommPrevData %>% #Create prevalence column
  mutate(Prevalence = Infected/Abundance)

#By gear type
CommPrevData.gear <- LengthData.prev %>% #Sum abundance columns by length and gear type
  group_by(Gear_type, Length) %>% 
  summarise(across(.cols = Abundance | Infected, sum))

CommPrevData.gear <- CommPrevData.gear %>% #Create prevalence column
  mutate(Prevalence = Infected/Abundance)

#By size of minnow traps
TrapData <- LengthData.prev %>% 
  filter(Gear_type == "Minnow_trap")

TrapSize <- ifelse(TrapData$Gear_ID %in% c("N1", "N2", "N3", "N4", "N5", "N12", "N13", "N14", "N15"), "Small", "Large") 

TrapData <- TrapData %>% 
  mutate(Size = TrapSize)

CommPrevData.size <- TrapData %>% #Sum abundance columns by length and trap size
  group_by(Size, Length) %>% 
  summarise(across(.cols = Abundance | Infected, sum))

CommPrevData.size <- CommPrevData.size %>% #Create prevalence column
  mutate(Prevalence = Infected/Abundance)

#By lake 
LakePrevData <- LengthData.prev %>% 
  group_by(Lake, Length) %>% 
  summarise(across(.cols = Abundance | Infected, sum))

LakePrevData <- LakePrevData %>% #Create prevalence column
  mutate(Prevalence = Infected/Abundance)

#By lake and gear type
LakePrevData.gear <- LengthData.prev %>% 
  group_by(Lake, Gear_type, Length) %>% 
  summarise(across(.cols = Abundance | Infected, sum))

LakePrevData.gear <- LakePrevData.gear %>% #Create prevalence column
  mutate(Prevalence = Infected/Abundance)

### Lepomis gibbosus ----

LeGiPrevLength <- LengthData.prev %>% #Select LeGi data
  filter(Species_ID == "LeGi")

LeGiPrevData <- LeGiPrevLength %>% #Sum abundance columns by length
  group_by(Length) %>% 
  summarise(across(.cols = Abundance | Infected, sum))

LeGiPrevData <- LeGiPrevData %>% #Create prevalence column
  mutate(Prevalence = Infected/Abundance)

#By gear  
LeGiPrevData.gear <- LeGiPrevLength %>% #Sum abundance columns by length and gear type
  group_by(Gear_type, Length) %>% 
  summarise(across(.cols = Abundance | Infected, sum))

LeGiPrevData.gear <- LeGiPrevData.gear %>% #Create prevalence column
  mutate(Prevalence = Infected/Abundance)

### Perca flavescens ----

PeFlPrevLength <- LengthData.prev %>% #Select PeFl data
  filter(Species_ID == "PeFl")

PeFlPrevData <- PeFlPrevLength %>% #Sum abundance columns by length
  group_by(Length) %>% 
  summarise(across(.cols = Abundance | Infected, sum))

PeFlPrevData <- PeFlPrevData %>% #Create prevalence column
  mutate(Prevalence = Infected/Abundance)

#By gear type
PeFlPrevData.gear <- PeFlPrevLength %>% #Sum abundance columns by length and gear type
  group_by(Gear_type, Length) %>% 
  summarise(across(.cols = Abundance | Infected, sum))

PeFlPrevData.gear <- PeFlPrevData.gear %>% #Create prevalence column
  mutate(Prevalence = Infected/Abundance)

## Trend plot ----

### Community scale ----

#All data
Comm.plot <- ggplot(CommPrevData , aes(x = Length, y = Prevalence)) + 
  geom_point(color = "grey", fill = "grey") +
  geom_smooth(color = "grey", fill = "grey", method="glm", method.args=list(family="binomial"), alpha = 0.2) +
  labs(x = "Length (cm)", y = "Prevalence") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0,5,10,15,20)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "Calibri Light", size = 20),
        axis.title.y = element_text(family = "Calibri Light", size = 20, vjust = 5),
        axis.title.x = element_text(family = "Calibri Light", size = 20, vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm"))

Comm.plot

ggsave(paste0(to.figs, "Prevalence_Length_Comm.png"), plot = Comm.plot, dpi = 500, width = 25, height = 10)

cor.test(CommPrevData$Prevalence,CommPrevData$Length) 
#Correlation between length and prevalence = -0.169

#By gear type
Comm.plot.gear <- ggplot(CommPrevData.gear , aes(x = Length, y = Prevalence)) + 
  geom_point(color = "grey", fill = "grey") +
  geom_smooth(color = "grey", fill = "grey", method="glm", method.args=list(family="binomial"), alpha = 0.2) +
  labs(x = "Length (cm)", y = "Prevalence") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0,5,10,15,20)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "Calibri Light", size = 20),
        axis.title.y = element_text(family = "Calibri Light", size = 20, vjust = 5),
        axis.title.x = element_text(family = "Calibri Light", size = 20, vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  facet_wrap(~Gear_type)

Comm.plot.gear

#By trap size
Comm.plot.size <- ggplot(CommPrevData.size , aes(x = Length, y = Prevalence)) + 
  geom_point(color = "grey", fill = "grey") +
  geom_smooth(color = "grey", fill = "grey", method="glm", method.args=list(family="binomial"), alpha = 0.2) +
  labs(x = "Length (cm)", y = "Prevalence") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0,5,10,15,20)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "Calibri Light", size = 20),
        axis.title.y = element_text(family = "Calibri Light", size = 20, vjust = 5),
        axis.title.x = element_text(family = "Calibri Light", size = 20, vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  facet_wrap(~Size)

Comm.plot.size

#By lake
Lake.plot <- ggplot(LakePrevData , aes(x = Length, y = Prevalence)) + 
  geom_point(color = "grey", fill = "grey") +
  geom_smooth(color = "grey", fill = "grey", method="glm", method.args=list(family="binomial"), alpha = 0.2) +
  labs(x = "Length (cm)", y = "Prevalence") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0,5,10,15,20)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "Calibri Light", size = 20),
        axis.title.y = element_text(family = "Calibri Light", size = 20, vjust = 5),
        axis.title.x = element_text(family = "Calibri Light", size = 20, vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  facet_wrap(~Lake)

Lake.plot

ggsave(paste0(to.figs, "Prevalence_Length_Lakes.png"), plot = Lake.plot, dpi = 500, width = 20, height = 10)

#By lake and gear_type
Lake.plot.gear <- ggplot(LakePrevData.gear , aes(x = Length, y = Prevalence, group = Gear_type, shape = Gear_type)) + 
  geom_point(color = "grey", fill = "grey") +
  geom_smooth(color = "grey", fill = "grey", method="glm", method.args=list(family="binomial"), alpha = 0.2) +
  labs(x = "Length (cm)", y = "Prevalence") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0,5,10,15,20)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "Calibri Light", size = 20),
        axis.title.y = element_text(family = "Calibri Light", size = 20, vjust = 5),
        axis.title.x = element_text(family = "Calibri Light", size = 20, vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  facet_wrap(~Lake)

Lake.plot.gear

### Lepomis gibbosus ----

LeGi.plot <- ggplot(LeGiPrevData , aes(x = Length, y = Prevalence)) + 
  geom_point(color = "#587289", fill = "#587289") +
  geom_smooth(color = "#587289", fill = "#587289", method="glm", method.args=list(family="binomial"), alpha = 0.2) +
  labs(x = "Length (cm)", y = "Prevalence") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0,5,10,15,20)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "Calibri Light", size = 20),
        axis.title.y = element_text(family = "Calibri Light", size = 20, vjust = 5),
        axis.title.x = element_text(family = "Calibri Light", size = 20, vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm"))

LeGi.plot

ggsave(paste0(to.figs, "Prevalence_Length_LeGi.png"), plot = LeGi.plot, dpi = 500, width = 25, height = 10)

cor.test(LeGiPrevData$Prevalence,LeGiPrevData$Length) 
#Correlation between length and prevalence = 0.193

LeGi.plot.gear <- ggplot(LeGiPrevData.gear , aes(x = Length, y = Prevalence)) + 
  geom_point(color = "#587289", fill = "#587289") +
  geom_smooth(color = "#587289", fill = "#587289", method="glm", method.args=list(family="binomial"), alpha = 0.2) +
  labs(x = "Length (cm)", y = "Prevalence") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0,5,10,15,20)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "Calibri Light", size = 20),
        axis.title.y = element_text(family = "Calibri Light", size = 20, vjust = 5),
        axis.title.x = element_text(family = "Calibri Light", size = 20, vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  facet_wrap(~Gear_type)

LeGi.plot.gear
      
## Perca flavescens ----

PeFl.plot <- ggplot(PeFlPrevData, aes(x = Length, y = Prevalence)) + 
  geom_point(color = "#6C464F", fill ="#6C464F") +
  geom_smooth(method="glm", method.args=list(family="binomial"), color = "#6C464F", fill = "#6C464F", alpha = 0.2) +
  labs(x = "Length (cm)") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0,5,10,15,20)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "Calibri Light", size = 20),
        axis.title.y = element_text(family = "Calibri Light", size = 20, vjust = 5),
        axis.title.x = element_text(family = "Calibri Light", size = 20, vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm"))
PeFl.plot

ggsave(paste0(to.figs, "Prevalence_Length_PeFl.png"), plot = PeFl.plot, dpi = 500, width = 15, height = 10)

cor.test(PeFlPrevData$Prevalence, PeFlPrevData$Length) 
#Correlation between length and prevalence = 0.402

#By gear type
PeFl.plot.gear <- ggplot(PeFlPrevData.gear, aes(x = Length, y = Prevalence)) + 
  geom_point(color = "#6C464F", fill ="#6C464F") +
  geom_smooth(method="glm", method.args=list(family="binomial"), color = "#6C464F", fill = "#6C464F", alpha = 0.2) +
  labs(x = "Length (cm)") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0,5,10,15,20)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "Calibri Light", size = 20),
        axis.title.y = element_text(family = "Calibri Light", size = 20, vjust = 5),
        axis.title.x = element_text(family = "Calibri Light", size = 20, vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  facet_wrap(~Gear_type)

PeFl.plot.gear

# Summary plot ----
  
length.sum.plot <- plot_grid(Comm.boxplot, Comm.plot, LeGi.boxplot, LeGi.plot, PeFl.boxplot, PeFl.plot,
                             nrow = 3, ncol = 2,
                             labels = "AUTO", 
                             label_fontface = "plain",
                             label_fontfamily = "Calibri Light", 
                             label_size = 20)

length.sum.plot

ggsave(paste0(to.figs, "Infection_Length_Summary.png"), plot = length.sum.plot, dpi = 500, width = 15, height = 15)
  