### Script name : Prevalence estimates

## Authors : Juliane Vigneault & Éric Harvey
## Date created : October 2, 2022

## Copyright (c) Juliane Vigneault, 2022
## Email: juliane.vigneault@umontreal.ca

# ---- Script setup ----

## R Setup ----

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"

## Loading packages ----

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(writexl)
library(splitstackshape)
library(janitor)

## Loading data ----

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv")) 

# ---- Regional prevalence ----

## Community prevalence by methods ----
#Lake Tracy is included at regional scale

### All methods ----

Reg.pool.All <- CombinedData %>% 
  select(starts_with(c("inf", "tot"))) %>% 
  na.omit()

Reg.pool.inf.All <- Reg.pool.All %>% 
  select(starts_with("inf"))
Reg.pool.inf.All <- sum(Reg.pool.inf.All)

Reg.pool.tot.All <- Reg.pool.All %>% 
  select(starts_with("tot"))
Reg.pool.tot.All <- sum(Reg.pool.tot.All)

Reg.pool.prev.All <- (Reg.pool.inf.All/Reg.pool.tot.All)*100

### Minnow traps ----

Reg.pool.MT <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") %>% 
  select(starts_with(c("inf", "tot"))) %>% 
  na.omit()

Reg.pool.inf.MT <- Reg.pool.MT %>% 
  select(starts_with("inf"))
Reg.pool.inf.MT <- sum(Reg.pool.inf.MT)

Reg.pool.tot.MT <- Reg.pool.MT %>% 
  select(starts_with("tot"))
Reg.pool.tot.MT <- sum(Reg.pool.tot.MT)

Reg.pool.prev.MT <- (Reg.pool.inf.MT/Reg.pool.tot.MT)*100

### Seine net ----

Reg.pool.S <- CombinedData %>% 
  filter(Sampling_method == "Seine") %>% 
  select(starts_with(c("inf", "tot"))) %>% 
  na.omit()

Reg.pool.inf.S <- Reg.pool.S %>% 
  select(starts_with("inf"))
Reg.pool.inf.S <- sum(Reg.pool.inf.S)

Reg.pool.tot.S <- Reg.pool.S %>% 
  select(starts_with("tot"))
Reg.pool.tot.S <- sum(Reg.pool.tot.S)

Reg.pool.prev.S <- (Reg.pool.inf.S/Reg.pool.tot.S)*100

### Transect ----

Reg.pool.T <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% 
  select(starts_with(c("inf", "tot"))) %>% 
  na.omit()

Reg.pool.inf.T <- Reg.pool.T %>% 
  select(starts_with("inf"))
Reg.pool.inf.T <- sum(Reg.pool.inf.T)

Reg.pool.tot.T <- Reg.pool.T %>% 
  select(starts_with("tot"))
Reg.pool.tot.T <- sum(Reg.pool.tot.T)

Reg.pool.prev.T <- (Reg.pool.inf.T/Reg.pool.tot.T)*100

## Species prevalence by methods ----

### All methods ----

Reg.sp.All <- CombinedData %>%  
  select(Lake, starts_with(c("inf", "tot"))) %>% 
  na.omit() %>% 
  adorn_totals(where = "row")

Reg.sp.prev.All <- Reg.sp.All %>% 
  filter(Lake == "Total") %>% 
  select(!(Lake))

Reg.sp.prev.All <- Reg.sp.prev.All %>% 
  mutate(prev_AmRu = (inf_AmRu/tot_AmRu)*100, .keep = "unused") %>% 
  mutate(prev_FuDi = (inf_FuDi/tot_FuDi)*100, .keep = "unused") %>%
  mutate(prev_MiDo = (inf_MiDo/tot_MiDo)*100, .keep = "unused") %>%
  mutate(prev_Centrarchidae = (inf_Centrarchidae/tot_Centrarchidae)*100, .keep = "unused") %>% 
  mutate(prev_LeGi = (inf_LeGi/tot_LeGi)*100, .keep = "unused") %>%
  mutate(prev_PeFl = (inf_PeFl/tot_PeFl)*100, .keep = "unused") %>%
  mutate(prev_PiPr = (inf_PiPr/tot_PiPr)*100, .keep = "unused") %>%
  mutate(prev_ChrosomusSp. = (inf_ChrosomusSp./tot_ChrosomusSp.)*100, .keep = "unused") %>%
  mutate(prev_PiNo = (inf_PiNo/tot_PiNo)*100, .keep = "unused") %>%
  mutate(prev_Cyprinidae = (inf_Cyprinidae/tot_Cyprinidae)*100, .keep = "unused") %>% 
  mutate(prev_SeAt = (inf_SeAt/tot_SeAt)*100, .keep = "unused") %>%
  mutate(prev_LuCo = (inf_LuCo/tot_LuCo)*100, .keep = "unused") %>%
  mutate(prev_AmNe = (inf_AmNe/tot_AmNe)*100, .keep = "unused") %>%
  mutate(prev_CaCo = (inf_CaCo/tot_CaCo)*100, .keep = "unused") %>%
  mutate(prev_EsMa = (inf_EsMa/tot_EsMa)*100, .keep = "unused") %>%
  mutate(prev_UmLi = (inf_UmLi/tot_UmLi)*100, .keep = "unused") %>%
  mutate(prev_RhAt = (inf_RhAt/tot_RhAt)*100, .keep = "unused")

### Minnow traps ----

Reg.sp.MT <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") %>% 
  select(Lake, starts_with(c("inf", "tot"))) %>% 
  na.omit() %>% 
  adorn_totals(where = "row")

Reg.sp.prev.MT <- Reg.sp.MT %>% 
  filter(Lake == "Total") %>% 
  select(!(Lake))

Reg.sp.prev.MT <- Reg.sp.prev.MT %>% 
  mutate(prev_AmRu = (inf_AmRu/tot_AmRu)*100, .keep = "unused") %>% 
  mutate(prev_FuDi = (inf_FuDi/tot_FuDi)*100, .keep = "unused") %>%
  mutate(prev_MiDo = (inf_MiDo/tot_MiDo)*100, .keep = "unused") %>%
  mutate(prev_Centrarchidae = (inf_Centrarchidae/tot_Centrarchidae)*100, .keep = "unused") %>% 
  mutate(prev_LeGi = (inf_LeGi/tot_LeGi)*100, .keep = "unused") %>%
  mutate(prev_PeFl = (inf_PeFl/tot_PeFl)*100, .keep = "unused") %>%
  mutate(prev_PiPr = (inf_PiPr/tot_PiPr)*100, .keep = "unused") %>%
  mutate(prev_ChrosomusSp. = (inf_ChrosomusSp./tot_ChrosomusSp.)*100, .keep = "unused") %>%
  mutate(prev_PiNo = (inf_PiNo/tot_PiNo)*100, .keep = "unused") %>%
  mutate(prev_Cyprinidae = (inf_Cyprinidae/tot_Cyprinidae)*100, .keep = "unused") %>% 
  mutate(prev_SeAt = (inf_SeAt/tot_SeAt)*100, .keep = "unused") %>%
  mutate(prev_LuCo = (inf_LuCo/tot_LuCo)*100, .keep = "unused") %>%
  mutate(prev_AmNe = (inf_AmNe/tot_AmNe)*100, .keep = "unused") %>%
  mutate(prev_CaCo = (inf_CaCo/tot_CaCo)*100, .keep = "unused") %>%
  mutate(prev_EsMa = (inf_EsMa/tot_EsMa)*100, .keep = "unused") %>%
  mutate(prev_UmLi = (inf_UmLi/tot_UmLi)*100, .keep = "unused") %>%
  mutate(prev_RhAt = (inf_RhAt/tot_RhAt)*100, .keep = "unused")

### Seine net ----

Reg.sp.S <- CombinedData %>%  
  filter(Sampling_method == "Seine") %>% 
  select(Lake, starts_with(c("inf", "tot"))) %>% 
  na.omit() %>% 
  adorn_totals(where = "row")

Reg.sp.prev.S <- Reg.sp.S %>% 
  filter(Lake == "Total") %>% 
  select(!(Lake))

Reg.sp.prev.S <- Reg.sp.prev.S %>% 
  mutate(prev_AmRu = (inf_AmRu/tot_AmRu)*100, .keep = "unused") %>% 
  mutate(prev_FuDi = (inf_FuDi/tot_FuDi)*100, .keep = "unused") %>%
  mutate(prev_MiDo = (inf_MiDo/tot_MiDo)*100, .keep = "unused") %>%
  mutate(prev_Centrarchidae = (inf_Centrarchidae/tot_Centrarchidae)*100, .keep = "unused") %>% 
  mutate(prev_LeGi = (inf_LeGi/tot_LeGi)*100, .keep = "unused") %>%
  mutate(prev_PeFl = (inf_PeFl/tot_PeFl)*100, .keep = "unused") %>%
  mutate(prev_PiPr = (inf_PiPr/tot_PiPr)*100, .keep = "unused") %>%
  mutate(prev_ChrosomusSp. = (inf_ChrosomusSp./tot_ChrosomusSp.)*100, .keep = "unused") %>%
  mutate(prev_PiNo = (inf_PiNo/tot_PiNo)*100, .keep = "unused") %>%
  mutate(prev_Cyprinidae = (inf_Cyprinidae/tot_Cyprinidae)*100, .keep = "unused") %>% 
  mutate(prev_SeAt = (inf_SeAt/tot_SeAt)*100, .keep = "unused") %>%
  mutate(prev_LuCo = (inf_LuCo/tot_LuCo)*100, .keep = "unused") %>%
  mutate(prev_AmNe = (inf_AmNe/tot_AmNe)*100, .keep = "unused") %>%
  mutate(prev_CaCo = (inf_CaCo/tot_CaCo)*100, .keep = "unused") %>%
  mutate(prev_EsMa = (inf_EsMa/tot_EsMa)*100, .keep = "unused") %>%
  mutate(prev_UmLi = (inf_UmLi/tot_UmLi)*100, .keep = "unused") %>%
  mutate(prev_RhAt = (inf_RhAt/tot_RhAt)*100, .keep = "unused")

### Transect ----

Reg.sp.T <- CombinedData %>%
  filter(Sampling_method == "Transect") %>% 
  select(Lake, starts_with(c("inf", "tot"))) %>% 
  na.omit() %>% 
  adorn_totals(where = "row")

Reg.sp.prev.T <- Reg.sp.T %>% 
  filter(Lake == "Total") %>% 
  select(!(Lake))

Reg.sp.prev.T <- Reg.sp.prev.T %>% 
  mutate(prev_AmRu = (inf_AmRu/tot_AmRu)*100, .keep = "unused") %>% 
  mutate(prev_FuDi = (inf_FuDi/tot_FuDi)*100, .keep = "unused") %>%
  mutate(prev_MiDo = (inf_MiDo/tot_MiDo)*100, .keep = "unused") %>%
  mutate(prev_Centrarchidae = (inf_Centrarchidae/tot_Centrarchidae)*100, .keep = "unused") %>% 
  mutate(prev_LeGi = (inf_LeGi/tot_LeGi)*100, .keep = "unused") %>%
  mutate(prev_PeFl = (inf_PeFl/tot_PeFl)*100, .keep = "unused") %>%
  mutate(prev_PiPr = (inf_PiPr/tot_PiPr)*100, .keep = "unused") %>%
  mutate(prev_ChrosomusSp. = (inf_ChrosomusSp./tot_ChrosomusSp.)*100, .keep = "unused") %>%
  mutate(prev_PiNo = (inf_PiNo/tot_PiNo)*100, .keep = "unused") %>%
  mutate(prev_Cyprinidae = (inf_Cyprinidae/tot_Cyprinidae)*100, .keep = "unused") %>% 
  mutate(prev_SeAt = (inf_SeAt/tot_SeAt)*100, .keep = "unused") %>%
  mutate(prev_LuCo = (inf_LuCo/tot_LuCo)*100, .keep = "unused") %>%
  mutate(prev_AmNe = (inf_AmNe/tot_AmNe)*100, .keep = "unused") %>%
  mutate(prev_CaCo = (inf_CaCo/tot_CaCo)*100, .keep = "unused") %>%
  mutate(prev_EsMa = (inf_EsMa/tot_EsMa)*100, .keep = "unused") %>%
  mutate(prev_UmLi = (inf_UmLi/tot_UmLi)*100, .keep = "unused") %>%
  mutate(prev_RhAt = (inf_RhAt/tot_RhAt)*100, .keep = "unused")

# ---- Local prevalence ----

CombinedData <- CombinedData %>% 
  filter(!(Lake == "Tracy"))

## Community prevalence by methods ----

Loc.pool.All <- CombinedData %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Loc.pool.All <- Loc.pool.All %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.pool.inf.All <- Loc.pool.All %>% 
  select(Lake, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Loc.pool.inf.All <- Loc.pool.inf.All %>% 
  select(Lake, inf_fish)

Loc.pool.tot.All <- Loc.pool.All %>% 
  select(Lake, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Loc.pool.tot.All <- Loc.pool.tot.All %>% 
  select(Lake, tot_fish)

Loc.pool.prev.All <- merge(Loc.pool.inf.All, Loc.pool.tot.All, by = "Lake") %>% 
  mutate(prev_fish = (inf_fish/tot_fish)*100)

### Minnow trap ----

Loc.pool.MT <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Loc.pool.MT <- Loc.pool.MT %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.pool.inf.MT <- Loc.pool.MT %>% 
  select(Lake, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Loc.pool.inf.MT <- Loc.pool.inf.MT %>% 
  select(Lake, inf_fish)

Loc.pool.tot.MT <- Loc.pool.MT %>% 
  select(Lake, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Loc.pool.tot.MT <- Loc.pool.tot.MT %>% 
  select(Lake, tot_fish)

Loc.pool.prev.MT <- merge(Loc.pool.inf.MT, Loc.pool.tot.MT, by = "Lake") %>% 
  mutate(prev_fish = (inf_fish/tot_fish)*100)

### Seine net ----

Loc.pool.S <- CombinedData %>% 
  filter(Sampling_method == "Seine") %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Loc.pool.S <- Loc.pool.S %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.pool.inf.S <- Loc.pool.S %>% 
  select(Lake, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Loc.pool.inf.S <- Loc.pool.inf.S %>% 
  select(Lake, inf_fish)

Loc.pool.tot.S <- Loc.pool.S %>% 
  select(Lake, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Loc.pool.tot.S <- Loc.pool.tot.S %>% 
  select(Lake, tot_fish)

Loc.pool.prev.S <- merge(Loc.pool.inf.S, Loc.pool.tot.S, by = "Lake") %>% 
  mutate(prev_fish = (inf_fish/tot_fish)*100)

### Transect ----
Loc.pool.T <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Loc.pool.T <- Loc.pool.T %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.pool.inf.T <- Loc.pool.T %>% 
  select(Lake, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Loc.pool.inf.T <- Loc.pool.inf.T %>% 
  select(Lake, inf_fish)

Loc.pool.tot.T <- Loc.pool.T %>% 
  select(Lake, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Loc.pool.tot.T <- Loc.pool.tot.T %>% 
  select(Lake, tot_fish)

Loc.pool.prev.T <- merge(Loc.pool.inf.T, Loc.pool.tot.T, by = "Lake") %>% 
  mutate(prev_fish = (inf_fish/tot_fish)*100)

## Species prevalence by methods ----

### All methods ----

Loc.sp.All <- CombinedData %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Loc.sp.All <- Loc.sp.All %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.sp.prev.All <- Loc.sp.All %>% 
  mutate(prev_AmRu = (inf_AmRu/tot_AmRu)*100, .keep = "unused") %>% 
  mutate(prev_FuDi = (inf_FuDi/tot_FuDi)*100, .keep = "unused") %>%
  mutate(prev_MiDo = (inf_MiDo/tot_MiDo)*100, .keep = "unused") %>%
  mutate(prev_Centrarchidae = (inf_Centrarchidae/tot_Centrarchidae)*100, .keep = "unused") %>% 
  mutate(prev_LeGi = (inf_LeGi/tot_LeGi)*100, .keep = "unused") %>%
  mutate(prev_PeFl = (inf_PeFl/tot_PeFl)*100, .keep = "unused") %>%
  mutate(prev_PiPr = (inf_PiPr/tot_PiPr)*100, .keep = "unused") %>%
  mutate(prev_ChrosomusSp. = (inf_ChrosomusSp./tot_ChrosomusSp.)*100, .keep = "unused") %>%
  mutate(prev_PiNo = (inf_PiNo/tot_PiNo)*100, .keep = "unused") %>%
  mutate(prev_Cyprinidae = (inf_Cyprinidae/tot_Cyprinidae)*100, .keep = "unused") %>% 
  mutate(prev_SeAt = (inf_SeAt/tot_SeAt)*100, .keep = "unused") %>%
  mutate(prev_LuCo = (inf_LuCo/tot_LuCo)*100, .keep = "unused") %>%
  mutate(prev_AmNe = (inf_AmNe/tot_AmNe)*100, .keep = "unused") %>%
  mutate(prev_CaCo = (inf_CaCo/tot_CaCo)*100, .keep = "unused") %>%
  mutate(prev_EsMa = (inf_EsMa/tot_EsMa)*100, .keep = "unused") %>%
  mutate(prev_UmLi = (inf_UmLi/tot_UmLi)*100, .keep = "unused") %>%
  mutate(prev_RhAt = (inf_RhAt/tot_RhAt)*100, .keep = "unused")

### Minnow trap ----

Loc.sp.MT <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Loc.sp.MT <- Loc.sp.MT %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.sp.prev.MT <- Loc.sp.MT %>% 
  mutate(prev_AmRu = (inf_AmRu/tot_AmRu)*100, .keep = "unused") %>% 
  mutate(prev_FuDi = (inf_FuDi/tot_FuDi)*100, .keep = "unused") %>%
  mutate(prev_MiDo = (inf_MiDo/tot_MiDo)*100, .keep = "unused") %>%
  mutate(prev_Centrarchidae = (inf_Centrarchidae/tot_Centrarchidae)*100, .keep = "unused") %>% 
  mutate(prev_LeGi = (inf_LeGi/tot_LeGi)*100, .keep = "unused") %>%
  mutate(prev_PeFl = (inf_PeFl/tot_PeFl)*100, .keep = "unused") %>%
  mutate(prev_PiPr = (inf_PiPr/tot_PiPr)*100, .keep = "unused") %>%
  mutate(prev_ChrosomusSp. = (inf_ChrosomusSp./tot_ChrosomusSp.)*100, .keep = "unused") %>%
  mutate(prev_PiNo = (inf_PiNo/tot_PiNo)*100, .keep = "unused") %>%
  mutate(prev_Cyprinidae = (inf_Cyprinidae/tot_Cyprinidae)*100, .keep = "unused") %>% 
  mutate(prev_SeAt = (inf_SeAt/tot_SeAt)*100, .keep = "unused") %>%
  mutate(prev_LuCo = (inf_LuCo/tot_LuCo)*100, .keep = "unused") %>%
  mutate(prev_AmNe = (inf_AmNe/tot_AmNe)*100, .keep = "unused") %>%
  mutate(prev_CaCo = (inf_CaCo/tot_CaCo)*100, .keep = "unused") %>%
  mutate(prev_EsMa = (inf_EsMa/tot_EsMa)*100, .keep = "unused") %>%
  mutate(prev_UmLi = (inf_UmLi/tot_UmLi)*100, .keep = "unused") %>%
  mutate(prev_RhAt = (inf_RhAt/tot_RhAt)*100, .keep = "unused")

### Seine net ----

Loc.sp.S <- CombinedData %>% 
  filter(Sampling_method == "Seine") %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Loc.sp.S <- Loc.sp.S %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.sp.prev.S <- Loc.sp.S %>% 
  mutate(prev_AmRu = (inf_AmRu/tot_AmRu)*100, .keep = "unused") %>% 
  mutate(prev_FuDi = (inf_FuDi/tot_FuDi)*100, .keep = "unused") %>%
  mutate(prev_MiDo = (inf_MiDo/tot_MiDo)*100, .keep = "unused") %>%
  mutate(prev_Centrarchidae = (inf_Centrarchidae/tot_Centrarchidae)*100, .keep = "unused") %>% 
  mutate(prev_LeGi = (inf_LeGi/tot_LeGi)*100, .keep = "unused") %>%
  mutate(prev_PeFl = (inf_PeFl/tot_PeFl)*100, .keep = "unused") %>%
  mutate(prev_PiPr = (inf_PiPr/tot_PiPr)*100, .keep = "unused") %>%
  mutate(prev_ChrosomusSp. = (inf_ChrosomusSp./tot_ChrosomusSp.)*100, .keep = "unused") %>%
  mutate(prev_PiNo = (inf_PiNo/tot_PiNo)*100, .keep = "unused") %>%
  mutate(prev_Cyprinidae = (inf_Cyprinidae/tot_Cyprinidae)*100, .keep = "unused") %>% 
  mutate(prev_SeAt = (inf_SeAt/tot_SeAt)*100, .keep = "unused") %>%
  mutate(prev_LuCo = (inf_LuCo/tot_LuCo)*100, .keep = "unused") %>%
  mutate(prev_AmNe = (inf_AmNe/tot_AmNe)*100, .keep = "unused") %>%
  mutate(prev_CaCo = (inf_CaCo/tot_CaCo)*100, .keep = "unused") %>%
  mutate(prev_EsMa = (inf_EsMa/tot_EsMa)*100, .keep = "unused") %>%
  mutate(prev_UmLi = (inf_UmLi/tot_UmLi)*100, .keep = "unused") %>%
  mutate(prev_RhAt = (inf_RhAt/tot_RhAt)*100, .keep = "unused")

### Transect ----

Loc.sp.T <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Loc.sp.T <- Loc.sp.T %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.sp.prev.T <- Loc.sp.T %>% 
  mutate(prev_AmRu = (inf_AmRu/tot_AmRu)*100, .keep = "unused") %>% 
  mutate(prev_FuDi = (inf_FuDi/tot_FuDi)*100, .keep = "unused") %>%
  mutate(prev_MiDo = (inf_MiDo/tot_MiDo)*100, .keep = "unused") %>%
  mutate(prev_Centrarchidae = (inf_Centrarchidae/tot_Centrarchidae)*100, .keep = "unused") %>% 
  mutate(prev_LeGi = (inf_LeGi/tot_LeGi)*100, .keep = "unused") %>%
  mutate(prev_PeFl = (inf_PeFl/tot_PeFl)*100, .keep = "unused") %>%
  mutate(prev_PiPr = (inf_PiPr/tot_PiPr)*100, .keep = "unused") %>%
  mutate(prev_ChrosomusSp. = (inf_ChrosomusSp./tot_ChrosomusSp.)*100, .keep = "unused") %>%
  mutate(prev_PiNo = (inf_PiNo/tot_PiNo)*100, .keep = "unused") %>%
  mutate(prev_Cyprinidae = (inf_Cyprinidae/tot_Cyprinidae)*100, .keep = "unused") %>% 
  mutate(prev_SeAt = (inf_SeAt/tot_SeAt)*100, .keep = "unused") %>%
  mutate(prev_LuCo = (inf_LuCo/tot_LuCo)*100, .keep = "unused") %>%
  mutate(prev_AmNe = (inf_AmNe/tot_AmNe)*100, .keep = "unused") %>%
  mutate(prev_CaCo = (inf_CaCo/tot_CaCo)*100, .keep = "unused") %>%
  mutate(prev_EsMa = (inf_EsMa/tot_EsMa)*100, .keep = "unused") %>%
  mutate(prev_UmLi = (inf_UmLi/tot_UmLi)*100, .keep = "unused") %>%
  mutate(prev_RhAt = (inf_RhAt/tot_RhAt)*100, .keep = "unused")

# ---- Fine scale ----

## Community prevalence by transect ----

Fine.pool.T <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% 
  select(Sampling_ID, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Fine.pool.inf.T <- Fine.pool.T %>% 
  select(Sampling_ID, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Fine.pool.inf.T <- Fine.pool.inf.T %>% 
  select(Sampling_ID, inf_fish)

Fine.pool.tot.T <- Fine.pool.T %>% 
  select(Sampling_ID, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Fine.pool.tot.T <- Fine.pool.tot.T %>% 
  select(Sampling_ID, tot_fish)

Fine.pool.prev.T <- merge(Fine.pool.inf.T, Fine.pool.tot.T, by = "Sampling_ID") %>% 
  mutate(prev_fish = (inf_fish/tot_fish)*100)

## Species prevalence by transect ----

Fine.sp.T <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% 
  select(Sampling_ID, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Fine.sp.prev.T <- Fine.sp.T %>% 
  mutate(prev_AmRu = (inf_AmRu/tot_AmRu)*100, .keep = "unused") %>% 
  mutate(prev_FuDi = (inf_FuDi/tot_FuDi)*100, .keep = "unused") %>%
  mutate(prev_MiDo = (inf_MiDo/tot_MiDo)*100, .keep = "unused") %>%
  mutate(prev_Centrarchidae = (inf_Centrarchidae/tot_Centrarchidae)*100, .keep = "unused") %>% 
  mutate(prev_LeGi = (inf_LeGi/tot_LeGi)*100, .keep = "unused") %>%
  mutate(prev_PeFl = (inf_PeFl/tot_PeFl)*100, .keep = "unused") %>%
  mutate(prev_PiPr = (inf_PiPr/tot_PiPr)*100, .keep = "unused") %>%
  mutate(prev_ChrosomusSp. = (inf_ChrosomusSp./tot_ChrosomusSp.)*100, .keep = "unused") %>%
  mutate(prev_PiNo = (inf_PiNo/tot_PiNo)*100, .keep = "unused") %>%
  mutate(prev_Cyprinidae = (inf_Cyprinidae/tot_Cyprinidae)*100, .keep = "unused") %>% 
  mutate(prev_SeAt = (inf_SeAt/tot_SeAt)*100, .keep = "unused") %>%
  mutate(prev_LuCo = (inf_LuCo/tot_LuCo)*100, .keep = "unused") %>%
  mutate(prev_AmNe = (inf_AmNe/tot_AmNe)*100, .keep = "unused") %>%
  mutate(prev_CaCo = (inf_CaCo/tot_CaCo)*100, .keep = "unused") %>%
  mutate(prev_EsMa = (inf_EsMa/tot_EsMa)*100, .keep = "unused") %>%
  mutate(prev_UmLi = (inf_UmLi/tot_UmLi)*100, .keep = "unused") %>%
  mutate(prev_RhAt = (inf_RhAt/tot_RhAt)*100, .keep = "unused")
