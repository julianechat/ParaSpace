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

source(paste0(to.R, "bartlettperm.r"))
source(paste0(to.R, "anova.1way.R"))

## Loading data ----

Fishing_RawData <- read.csv(paste0(to.data, "Fishing_RawData.csv"), sep=";")

# ---- Intensity & Length relationships ----

Fish_Data <- Fishing_RawData[-c(596,613),] #Deleting lost data

PeFl_Data <- Fish_Data %>% filter(Species_ID == "PeFl") #Selecting only PeFl data
PeFl_Data2 <- PeFl_Data[rep(row.names(PeFl_Data), PeFl_Data$Abundance), 1:12] #Repeating data by abundance column
PeFl_Data2$Intensity_class <- as.factor(PeFl_Data2$Intensity_class)

LeGi_Data <- Fish_Data %>% filter(Species_ID == "LeGi") #Selecting only LeGi data
LeGi_Data2 <- LeGi_Data[rep(row.names(LeGi_Data), LeGi_Data$Abundance), 1:12] #Repeating data by abundance column
LeGi_Data2$Intensity_class <- as.factor(LeGi_Data2$Intensity_class)

## Boxplots ----

PeFl.boxplot <- ggplot(PeFl_Data2, aes(y = Length, x = Intensity_class)) +
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



LeGi.boxplot <- ggplot(LeGi_Data2, aes(y = Length, x = Intensity_class)) +
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

## ANOVA test ##
# Terms of application #
shapiro.test(resid(aov(PeFl_Data2$Length ~ PeFl_Data2$Intensity_class))) #Resids are not normal
bartlett.perm(PeFl_Data2$Length, PeFl_Data2$Intensity_class, centr = "MEAN", nperm = 999, alpha = 0.05) #Variances are homogeneous

shapiro.test(resid(aov(LeGi_Data3$Length ~ LeGi_Data3$Intensity_class))) #Resids are not normal
bartlett.perm(LeGi_Data3$Length, LeGi_Data3$Intensity_class, centr = "MEAN", nperm = 999, alpha = 0.05) #Variances are homogeneous

# ANOVA by permutations #
anova.1way(Length~Intensity_class, data = PeFl_Data3, nperm=999) #Means are different between PeFl groups
##TukeyHSD(anova.1way(Length~Intensity_class, data = PeFl_Data2, nperm=999))##FONCTIONNE PAS##

anova.1way(Length~Intensity_class, data = LeGi_Data3, nperm=999) #Means are different between LeGi groups
##TukeyHSD(anova.1way(Length~Intensity_class, data = LeGi_Data2, nperm=999))##FONCTIONNE PAS##

# ---- Prevalence & Length relationships ----

LeGi_AllData <- LeGi_Data[-c(1:11)] #Deleting unnecessary columns

LeGi_InfData <- Fish_Data %>% filter(Species_ID == "LeGi" & Intensity_class >0) #Infected LeGi
LeGi_InfData2 <- LeGi_InfData[-c(1:11)] #Deleting unnecessary columns

PeFl_AllData <- PeFl_Data[-c(1:11)] #Deleting unnecessary columns

PeFl_InfData <- Fish_Data %>% filter(Species_ID == "PeFl" & Intensity_class > 0) #Infected PeFl data
PeFl_InfData2 <- PeFl_InfData[-c(1:11)]

## Prevalence values by length ##
# LeGi #
LeGi_LengthSum <- LeGi_AllData %>% #Abundance of LeGi per length
  group_by(Length) %>%
  summarise(across(.cols = Abundance, sum)) 

LeGiInf_LengthSum <- LeGi_InfData2 %>% #Abundance of infected LeGi per length
  group_by(Length) %>%
  summarise(across(.cols = Abundance, sum))

LeGiInf_LengthSum2 <- rbind(LeGiInf_LengthSum, c(24,1)) #Adding missing row

Df_Length <- cbind.data.frame(LeGiInf_LengthSum2, LeGi_LengthSum)
CNames <- c("LengthInf", "AbundanceInf", "LengthAll", "AbundanceAll")
Df_Length2 <- `colnames<-`(Df_Length,CNames)
Df_Length3 <- Df_Length2[-3]

attach(Df_Length3)
Prev_Length <- AbundanceInf/AbundanceAll #Prevalence values
Df_Prev_Length <- cbind(Df_Length3, Prev_Length) 
Df_Prev_Length2 <- Df_Prev_Length[-c(2,3)]
Df_Prev_Length2$Prev_Length <- Df_Prev_Length2$Prev_Length*100

# PeFl #
PeFl_LengthSum <- PeFl_AllData %>% #Abundance of LeGi per length
  group_by(Length) %>%
  summarise(across(.cols = Abundance, sum))

PeFlInf_LengthSum <- PeFl_InfData2 %>% #Abundance of infected LeGi per length
  group_by(Length) %>%
  summarise(across(.cols = Abundance, sum))

Missing1 <- c(2,3,4,5,17,20,26)
Missing2 <- c(0,0,0,0,0,0,0)
Missing3 <- data.frame(Missing1, Missing2)
df.Missing <- `colnames<-`(Missing3, c("Length", "Abundance"))
df.inf.length <- rbind(PeFlInf_LengthSum, df.Missing)

attach(df.inf.length)
df.inf.length2 <-df.inf.length[order(df.inf.length$Length),]

df.PeFl1 <- cbind(PeFl_LengthSum, df.inf.length2)
df.PeFl2 <- df.PeFl1[-3]
df.PeFl3 <- `colnames<-`(df.PeFl2, c("Length","All_Abund", "Inf_Abund"))
attach(df.PeFl3)
df.PeFl4 <- cbind(df.PeFl3, c(Inf_Abund/All_Abund))
df.PeFl4 <- df.PeFl4[c(1,4)]
df.PeFl4 <- `colnames<-`(df.PeFl4, c("Length", "Prevalence"))

## Tendency plot ----

## Lepomis gibbosus ----

LeGi.plot <- ggplot(Df_Prev_Length2, aes(x = LengthInf, y = Prev_Length)) + 
  geom_point(color = "#587289", fill = "#587289") +
  geom_smooth(method="glm", method.args=list(family="binomial"), color = "#587289", fill = "#587289", alpha = 0.2) +
  labs(x = "Length (cm)", y = "Prevalence") +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "Calibri Light", size = 20),
        axis.title.y = element_text(family = "Calibri Light", size = 20, vjust = 5),
        axis.title.x = element_text(family = "Calibri Light", size = 20, vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm"))
LeGi.plot

ggsave(paste0(to.figs, "Prevalence_Length_LeGi.png"), plot = LeGi.plot, dpi = 500, width = 25, height = 10)
                    
cor.test(LengthInf, Prev_Length) 
      
## Perca flavescens ----

PeFl.plot <- ggplot(df.PeFl4, aes(x = Length, y = Prevalence)) + 
  geom_point(color = "#6C464F", fill ="#6C464F") +
  geom_smooth(method="glm", method.args=list(family="binomial"), color = "#6C464F", fill = "#6C464F", alpha = 0.2) +
  labs(x = "Length (cm)") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(5,10,15,20,25)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5), 
        panel.grid.major = element_blank(),
        axis.text = element_text(family = "Calibri Light", size = 20),
        axis.title.y = element_text(family = "Calibri Light", size = 20, vjust = 5),
        axis.title.x = element_text(family = "Calibri Light", size = 20, vjust = -2),
        plot.margin = unit(c(1,1,1,1), "cm"))
PeFl.plot

ggsave(paste0(to.figs, "Prevalence_Length_PeFl.png"), plot = PeFl.plot, dpi = 500, width = 15, height = 10)

cor.test(df.PeFl4$Length,df.PeFl4$Prevalence)

# Summary plot ----
  
length.sum.plot <- plot_grid(LeGi.boxplot, LeGi.plot, PeFl.boxplot, PeFl.plot,
                             nrow = 2, ncol = 2,
                             labels = "AUTO", 
                             label_fontface = "plain",
                             label_fontfamily = "Calibri Light", 
                             label_size = 20)

length.sum.plot
ggsave(paste0(to.figs, "Infection_Length_Summary.png"), plot = length.sum.plot, dpi = 500, width = 15, height = 10)

# Prevalence values by length for each lake #
all.length.lake <- LeGi_Data %>% 
  mutate(Infected = ifelse(Intensity_class > 0, Abundance, 0))

prev.length.lake <- all.length.lake %>% 
  select(Lake, Infected, Abundance, Length) %>% 
  group_by(Lake, Length) %>% 
  summarise(across(.cols = everything(), sum))

prev.length.lake2 <- prev.length.lake %>% 
  group_by(Lake, Length) %>% 
  transmute(Prevalence = Infected / Abundance)
  
ggplot(data = prev.length.lake2) + 
  geom_point(aes(x = Length, y = Prevalence, color = Lake)) +
  scale_y_continuous(c(0,1)) +
  geom_smooth(aes( x = Length, y = Prevalence, color = Lake), se = FALSE) + 
  facet_wrap(~Lake) + 
  labs(title = "LeGi Prevalence/Length relation in each lake")

ggsave(paste0(to.figs, "Prevalence_Length_Lakes.png"), plot = last_plot(), dpi = 500, width = 20, height = 10)
  
#### LeGi Length means ####
LeGi.length.mean <- LeGi_Data2 %>% 
  select(Lake, Length) %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = Length, mean))

LeGi.length.sd <- LeGi_Data2 %>% 
  select(Lake, Length) %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = Length, sd))

LeGi.lengths <- cbind(LeGi.length.mean, LeGi.length.sd[2]) 
colnames(LeGi.lengths)[c(2,3)] <- c("mean_length", "sd_length")
