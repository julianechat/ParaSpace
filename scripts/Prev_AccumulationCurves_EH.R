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

# ---- Extract relevant data for prevalence accumulation curves by methods ----

## For each sampling method generate a dataset with only three relevant columns

### Minnow trap 
MTdata.2 <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") %>% #Selecting method
  select("Lake",starts_with(c("inf", "tot"))) %>% 
  na.omit()

MTdata.inf <- MTdata.2 %>% 
  select(starts_with("inf")) 

MTdata.tot <- MTdata.2 %>% 
  select(starts_with("tot"))

infected <- rowSums(MTdata.inf)
total <- rowSums(MTdata.tot)
lake <- MTdata.2$Lake

MTdata <- data.frame(lake,infected,total)

MTdata %>%  #Check if each lake is represented equally 
  group_by(lake) %>%
  summarise(no_rows = length(lake))


### Seine

Sdata.2 <- CombinedData %>% 
  filter(Sampling_method == "Seine") %>% #Selecting method
  select("Lake",starts_with(c("inf", "tot"))) %>% 
  na.omit()

Sdata.inf <- Sdata.2 %>% 
  select(starts_with("inf")) 

Sdata.tot <- Sdata.2 %>% 
  select(starts_with("tot"))

infected <- rowSums(Sdata.inf)
total <- rowSums(Sdata.tot)
lake <- Sdata.2$Lake

Sdata <- data.frame(lake,infected,total)

Sdata %>%  #Check if each lake is represented equally (4 is mininum)
  group_by(lake) %>%
  summarise(no_rows = length(lake))


### Transect 

Tdata.2 <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% #Selecting method
  select("Lake",starts_with(c("inf", "tot"))) %>% 
  na.omit()

Tdata.inf <- Tdata.2 %>% 
  select(starts_with("inf")) 

Tdata.tot <- Tdata.2 %>% 
  select(starts_with("tot"))

infected <- rowSums(Tdata.inf)
total <- rowSums(Tdata.tot)
lake <- Tdata.2$Lake

Tdata <- data.frame(lake,infected,total)

Tdata %>%  #Check if each lake is represented equally (2 is miminum)
  group_by(lake) %>%
  summarise(no_rows = length(lake))


# ---- Prevalence accumulation curves ----

# For Minnow trap 

Mtprev <- data.frame()
n.samp <- nrow(MTdata)-1 #nb of lines sampled (i)
resampling <- 10 #nb of times each i is repeated 

for(i in 1:n.samp) {

for(j in 1:resampling) {
  
  #Samp <- MTdata %>% group_by(lake) %>% sample_n(size=10)
  line <- sample(1:nrow(MTdata),i) #sample i lines randomly
  prev.site.1 <- MTdata[line,"infected"]/MTdata[line,"total"]
  prev.site <- na.omit(prev.site.1)
  prev <- sum(prev.site)/length(prev.site.1)
  
  output <- data.frame(n.samp=i,resampling=j,prev) #save output in temporary data.frame (changed at each iterations)
  
  Mtprev <- rbind(Mtprev,output)
  
}
  
 }
  
str(Mtprev)
head(Mtprev)

plot(prev ~ n.samp,data=Mtprev)

# For Seine 

Sprev <- data.frame()
n.samp <- nrow(Sdata)-1 #nb of lines sampled (i)
resampling <- 10 #nb of times each i is repeated 

for(i in 1:n.samp) {
  
  # Samp <- Sdata %>% group_by(lake) %>% sample_n(size=4) #resample 4 lines per lake 
  
  for(j in 1:resampling) {
    
    line <- sample(1:nrow(Sdata),i) #sample i lines randomly
    prev.site.1 <- Sdata[line,"infected"]/Sdata[line,"total"]
    prev.site <- na.omit(prev.site.1)
    prev <- sum(prev.site)/length(prev.site.1)
    
    output <- data.frame(n.samp=i,resampling=j,prev) #save output in temporary data.frame (changed at each iterations)
    
    Sprev <- rbind(Sprev,output)
    
  }
  
}


str(Sprev)
head(Sprev)
plot(prev ~ n.samp,data=Sprev)
points(prev ~ n.samp,data=Mtprev,col="blue")

# For Transect 

Tprev <- data.frame()
n.samp <- nrow(Tdata)-1 #nb of lines sampled (i)
resampling <- 10 #nb of times each i is repeated 

for(i in 1:n.samp) {
  
  for(j in 1:resampling) {
    
    #Samp <- Tdata %>% group_by(lake) %>% sample_n(size=2)
    line <- sample(1:nrow(Tdata),i) #sample i lines randomly
    prev.site.1 <- Tdata[line,"infected"]/Tdata[line,"total"]
    prev.site <- na.omit(prev.site.1)
    prev <- sum(prev.site)/length(prev.site.1)
    
    output <- data.frame(n.samp=i,resampling=j,prev) #save output in temporary data.frame (changed at each iterations)
    
    Tprev <- rbind(Tprev,output)
    
  }
  
}

str(Tprev)
head(Tprev)
plot(prev ~ n.samp,data=Mtprev,col="blue")
points(prev ~ n.samp,data=Sprev,col="darkred")
points(prev ~ n.samp,data=Tprev,col="darkgreen")










