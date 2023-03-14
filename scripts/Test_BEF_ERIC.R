#---- r_setup ----

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"

#---- load package ----
library(tidyverse)
library(vegan)

#---- raw load data ----
raw.d <- read.csv(paste0(to.output,"Fishing_WideData.csv"), sep=",")

# if want to select on gear
#raw.d <- raw.d %>% 
 # filter(Gear_type == "Minnow_trap")

#---- Working data ----

working.d <- raw.d %>% #Create working data with minimum info needed
  select(Lake,starts_with("tot"),starts_with("inf")) 

#working.d <- working.d[rowSums(working.d[2:18]) !=0,] #if want to remove all samples with no fish 


#---- Among-lake BEF ----

### Summarize information at lake level

by.lake.d <- working.d %>% #summarise info by lakes
  group_by(Lake) %>%
  summarise(across(everything(),sum)) %>% 
  ungroup()


### Measure total parasite prevalence in lakes

by.lake.d$sum.ab <- by.lake.d %>% #Measure sum abundances for each lake
  select(starts_with("tot")) %>% 
  rowSums()

by.lake.d$sum.inf <- by.lake.d %>% #Measure sum infections for each lake
  select(starts_with("inf")) %>% 
  rowSums()

by.lake.d$prev.total <- by.lake.d$sum.inf/by.lake.d$sum.ab #Measure total prevalence for each lake

### Remove lakes where prevalence is 0 (can't test hypothesis on those!)

by.lake.d <- by.lake.d %>% 
  filter(prev.total != 0)

### Measure prevalence for LeGI (focal species)

by.lake.d$prev.LeGi <- by.lake.d$inf_LeGi / by.lake.d$tot_LeGi 
#prev.LeGi <- replace_na(prev.LeGi,0) 

### Measure total lake richness and diversity

by.lake.d$lake.rich <- by.lake.d %>% #Measure total lake richness
  select(starts_with("tot")) %>% 
  specnumber()

by.lake.d$lake.div <- by.lake.d %>% #Measure total lake diversity 
  select(starts_with("tot")) %>% 
  diversity()

# Exploratory plots

plot(prev.LeGi ~ lake.rich,data=by.lake.d)
lm.mod1 <- lm(prev.LeGi ~ lake.rich,data=by.lake.d)
summary(lm.mod1)

plot(prev.LeGi ~ lake.div,data=by.lake.d)
lm.mod2 <- lm(prev.LeGi ~ lake.div,data=by.lake.d)
summary(lm.mod2)

plot(prev.total ~ lake.rich,data=by.lake.d)
lm.mod3 <- lm(prev.total ~ lake.rich,data=by.lake.d)
summary(lm.mod3)
plot(prev.total ~ lake.div,data=by.lake.d)
lm.mod4 <- lm(prev.total ~ lake.div,data=by.lake.d)
summary(lm.mod4)

#---- Within-lake BEF: Transform and extract relevant data ----

### Measure total prevalence in lakes

sample.sum.ab <- working.d %>% #Measure sum abundances for each lake
  select(starts_with("tot")) %>% 
  rowSums()

sample.sum.inf <- working.d %>% #Measure sum infetions for each lake
  select(starts_with("inf")) %>% 
  rowSums()

working.d$sample.prev.total <- sample.sum.inf/sample.sum.ab #Measure total prevalence for each lake

### Remove sampling with 0 prevalence

sample.d <- working.d %>% 
  filter(sample.prev.total != 0)

### Measure LeGi sample prevalence 

sample.d$sample.prev.LeGi <-sample.d$inf_LeGi / sample.d$tot_LeGi #measure peveralence for LeGi
#sample.prev.LeGi <- replace_na(sample.prev.LeGi,0) 

### Measure sample richness and diversity

sample.d$sample.rich <- sample.d %>% #Measure total lake richness
  select(starts_with("tot")) %>% 
  specnumber()

sample.d$sample.div <- sample.d %>% #Measure total lake diversity 
  select(starts_with("tot")) %>% 
  diversity()

# Exploratory plots

boxplot(sample.prev.LeGi ~ sample.rich, data=sample.d)
lm.mod5 <- aov(sample.prev.LeGi ~ sample.rich,data=sample.d)
summary(lm.mod5)

plot(sample.prev.LeGi ~ sample.div,data=sample.d)
lm.mod7 <- lm(sample.prev.LeGi ~ sample.div,data=sample.d)
summary(lm.mod7)


boxplot(sample.d$sample.prev.total ~ sample.rich,data=sample.d)
lm.mod6 <- lm(sample.prev.total ~ sample.rich,data=sample.d)
summary(lm.mod6)

plot(sample.prev.total ~ sample.div,data=sample.d)
lm.mod8 <- lm(sample.prev.total ~ sample.div,data=sample.d)
summary(lm.mod8)

#Interpretation: 
# No difference on patterns on using Senne or Minnow_Trap
# Lake vs. sample: patterns are stronger within lake than among lakes (could be only because N is much higher within lake)
# LeGi vs. whole community - interestingly (especially within lakes) diversity/richness seems to increase prevalence for LeGi but decrease prev. at the whole community level
