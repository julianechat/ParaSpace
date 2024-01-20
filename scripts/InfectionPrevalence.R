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
to.rédaction <- "./rédaction/"

## Loading packages ----

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(writexl)
library(splitstackshape)
library(janitor)
library(gt)

## Loading data ----

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv")) 

# ---- Regional prevalence ----

## Community prevalence by methods ----

#Lake Tracy is included at regional scale
#All infected fishes (from all lakes) diveded by the total of fishes

### All methods ----

Reg.pool.All <- CombinedData %>% #Selecting abundance data
  select(starts_with(c("inf", "tot"))) %>% 
  na.omit()

Reg.pool.inf.All <- Reg.pool.All %>% #Regional infected fish abundance
  select(starts_with("inf"))
Reg.pool.inf.All <- sum(Reg.pool.inf.All)

Reg.pool.tot.All <- Reg.pool.All %>% #Regional total fish abundance
  select(starts_with("tot"))
Reg.pool.tot.All <- sum(Reg.pool.tot.All)

Reg.pool.prev.All <- (Reg.pool.inf.All/Reg.pool.tot.All)*100 #Regional prevalence

### Minnow traps ----

Reg.pool.MT <- CombinedData %>%  #Selecting abundance data & method
  filter(Sampling_method == "Minnow_trap") %>% 
  select(starts_with(c("inf", "tot"))) %>% 
  na.omit()

Reg.pool.inf.MT <- Reg.pool.MT %>% #Regional infected fish abundance
  select(starts_with("inf"))
Reg.pool.inf.MT <- sum(Reg.pool.inf.MT)

Reg.pool.tot.MT <- Reg.pool.MT %>% #Regional total fish abundance
  select(starts_with("tot"))
Reg.pool.tot.MT <- sum(Reg.pool.tot.MT)

Reg.pool.prev.MT <- (Reg.pool.inf.MT/Reg.pool.tot.MT)*100 #Regional prevalence

### Seine net ----

Reg.pool.S <- CombinedData %>%  #Selecting abundance data & method
  filter(Sampling_method == "Seine") %>%
  select(starts_with(c("inf", "tot"))) %>% 
  na.omit()

Reg.pool.inf.S <- Reg.pool.S %>% #Regional infected fish abundance
  select(starts_with("inf"))
Reg.pool.inf.S <- sum(Reg.pool.inf.S)

Reg.pool.tot.S <- Reg.pool.S %>% #Regional total fish abundance
  select(starts_with("tot"))
Reg.pool.tot.S <- sum(Reg.pool.tot.S)

Reg.pool.prev.S <- (Reg.pool.inf.S/Reg.pool.tot.S)*100 #Regional prevalence

### Transect ----

Reg.pool.T <- CombinedData %>%  #Selecting abundance data & method
  filter(Sampling_method == "Transect") %>%
  select(starts_with(c("inf", "tot"))) %>% 
  na.omit()

Reg.pool.inf.T <- Reg.pool.T %>% #Regional infected fish abundance
  select(starts_with("inf"))
Reg.pool.inf.T <- sum(Reg.pool.inf.T)

Reg.pool.tot.T <- Reg.pool.T %>% #Regional total fish abundance
  select(starts_with("tot"))
Reg.pool.tot.T <- sum(Reg.pool.tot.T)

Reg.pool.prev.T <- (Reg.pool.inf.T/Reg.pool.tot.T)*100 #Regional prevalence

### Summary table ----

Reg.A.data <- c(Method = "Combined", Prevalence = Reg.pool.prev.All)
Reg.MT.data <- c(Method = "Minnow trap", Prevalence = Reg.pool.prev.MT)
Reg.S.data <- c(Method = "Seine net", Prevalence = Reg.pool.prev.S)
Reg.T.data <- c(Method = "Transect", Prevalence = Reg.pool.prev.T)

Reg.summary.data <- data.frame(rbind(Reg.A.data, Reg.MT.data, Reg.S.data, Reg.T.data), row.names = NULL)
Reg.summary.data$Prevalence <- as.numeric(Reg.summary.data$Prevalence)

Table.Syy <- gt(Reg.summary.data) %>% 
  tab_header(md("**TABLE Syy.** Observed landscape prevalence estimated by each sampling method. All values are given in percentage.")) %>% 
  cols_label(Prevalence = md("Prevalence (%)")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              table.width = pct(100)) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style= cell_borders(sides = c("bottom"), weight = px(2)), 
            location = list(cells_body(rows = 4))) %>% 
  fmt_number(decimals = 2)
Table.Syy

Table.Syy %>% #Saving gt tab
  gtsave("Tab_LandscapePrev_Methods.png", paste0(to.figs))
Table.Syy %>% 
  gtsave("Table_Syy.png", paste0(to.rédaction, "./Support_information/"))

## Species prevalence by methods ----

### All methods ----

Reg.sp.All <- CombinedData %>%  #Selecting abundance data 
  select(Lake, starts_with(c("inf", "tot"))) %>% 
  na.omit() %>% 
  adorn_totals(where = "row") #Summarizing regional abundance by species

Reg.sp.prev.All <- Reg.sp.All %>% #Extracting abundance sums
  filter(Lake == "Total") %>% 
  select(!(Lake))

Reg.sp.prev.All <- Reg.sp.prev.All %>% #Regional prevalence by species
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
  filter(Sampling_method == "Minnow_trap") %>% #Selecting method
  select(Lake, starts_with(c("inf", "tot"))) %>% #Selecting abundance data 
  na.omit() %>% 
  adorn_totals(where = "row") #Summarizing regional abundance by species

Reg.sp.prev.MT <- Reg.sp.MT %>% #Extracting abundance sums
  filter(Lake == "Total") %>% 
  select(!(Lake))

Reg.sp.prev.MT <- Reg.sp.prev.MT %>% #Regional prevalence by species
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
  filter(Sampling_method == "Seine") %>% #Selecting method
  select(Lake, starts_with(c("inf", "tot"))) %>%  #Selecting abundance data 
  na.omit() %>% 
  adorn_totals(where = "row") #Summarizing regional abundance by species

Reg.sp.prev.S <- Reg.sp.S %>% #Extracting abundance sums
  filter(Lake == "Total") %>% 
  select(!(Lake))

Reg.sp.prev.S <- Reg.sp.prev.S %>% #Regional prevalence by species
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
  filter(Sampling_method == "Transect") %>% #Selecting method
  select(Lake, starts_with(c("inf", "tot"))) %>% #Selecting abundance data
  na.omit() %>% 
  adorn_totals(where = "row")#Summarizing regional abundance by species

Reg.sp.prev.T <- Reg.sp.T %>% #Extracting abundance sums
  filter(Lake == "Total") %>% 
  select(!(Lake))

Reg.sp.prev.T <- Reg.sp.prev.T %>%  #Regional prevalence by species
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

CombinedData <- CombinedData %>% #Deleting lake Tracy because we cannot calculate a prevalence on a unique data point (1 fish)
  filter(!(Lake == "Tracy"))

## Community prevalence by methods ----

### All methods ----

Loc.pool.All <- CombinedData %>% #Selecting abundance data
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Loc.pool.All <- Loc.pool.All %>% #Summarizing abundance columns by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.pool.inf.All <- Loc.pool.All %>% #Summarizing local infected community abundance 
  select(Lake, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Loc.pool.inf.All <- Loc.pool.inf.All %>% #Extracting local infected community abundances
  select(Lake, inf_fish)

Loc.pool.tot.All <- Loc.pool.All %>% #Summarizing local total community abundance 
  select(Lake, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Loc.pool.tot.All <- Loc.pool.tot.All %>% #Extracting local infected community abundances
  select(Lake, tot_fish)

Loc.pool.prev.All <- merge(Loc.pool.inf.All, Loc.pool.tot.All, by = "Lake") %>% #Local communities prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Loc.mean.All <- weighted.mean(Loc.pool.prev.All$prev_fish, Loc.pool.prev.All$tot_fish) #Regional prevalence by mean of local communities prevalence

Loc.All.tab <- Loc.pool.prev.All %>% 
  mutate(Method = "All", .after = Lake)
  
### Minnow trap ----

Loc.pool.MT <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") %>% #Selecting method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Loc.pool.MT <- Loc.pool.MT %>% #Summarizing abundance columns by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.pool.inf.MT <- Loc.pool.MT %>% #Summarizing local infected community abundance 
  select(Lake, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Loc.pool.inf.MT <- Loc.pool.inf.MT %>% #Extracting local infected community abundances
  select(Lake, inf_fish)

Loc.pool.tot.MT <- Loc.pool.MT %>% #Summarizing local total community abundance 
  select(Lake, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Loc.pool.tot.MT <- Loc.pool.tot.MT %>% #Extracting local infected community abundances
  select(Lake, tot_fish)

Loc.pool.prev.MT <- merge(Loc.pool.inf.MT, Loc.pool.tot.MT, by = "Lake") %>% #Local communities prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Loc.mean.MT <- weighted.mean(Loc.pool.prev.MT$prev_fish, Loc.pool.prev.MT$tot_fish) #Regional prevalence by mean of local communities prevalence

Loc.MT.tab <- Loc.pool.prev.MT %>% 
  mutate(Method = "Minnow trap", .after = Lake)

### Seine net ----

Loc.pool.S <- CombinedData %>% 
  filter(Sampling_method == "Seine") %>% #Selecting method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Loc.pool.S <- Loc.pool.S %>% #Summarizing abundance columns by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.pool.inf.S <- Loc.pool.S %>% #Summarizing local infected community abundance 
  select(Lake, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Loc.pool.inf.S <- Loc.pool.inf.S %>% #Extracting local infected community abundances
  select(Lake, inf_fish)

Loc.pool.tot.S <- Loc.pool.S %>%  #Summarizing local total community abundance 
  select(Lake, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Loc.pool.tot.S <- Loc.pool.tot.S %>% #Extracting local infected community abundances
  select(Lake, tot_fish)

Loc.pool.prev.S <- merge(Loc.pool.inf.S, Loc.pool.tot.S, by = "Lake") %>% #Local communities prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Loc.mean.S <- weighted.mean(Loc.pool.prev.S$prev_fish, Loc.pool.prev.S$tot_fish) #Regional prevalence by mean of local communities prevalence

Loc.S.tab <- Loc.pool.prev.S %>% 
  mutate(Method = "Seine net", .after = Lake)

### Transect ----

Loc.pool.T <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% #Selecting method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Loc.pool.T <- Loc.pool.T %>% #Summarizing abundance columns by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.pool.inf.T <- Loc.pool.T %>% #Summarizing local infected community abundance 
  select(Lake, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Loc.pool.inf.T <- Loc.pool.inf.T %>% #Extracting local infected community abundances
  select(Lake, inf_fish)

Loc.pool.tot.T <- Loc.pool.T %>% #Summarizing local total community abundance 
  select(Lake, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Loc.pool.tot.T <- Loc.pool.tot.T %>% #Extracting local infected community abundances
  select(Lake, tot_fish)

Loc.pool.prev.T <- merge(Loc.pool.inf.T, Loc.pool.tot.T, by = "Lake") %>% #Local communities prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Loc.mean.T <- weighted.mean(Loc.pool.prev.T$prev_fish, Loc.pool.prev.T$tot_fish) #Regional prevalence by mean of local communities prevalence

Loc.T.tab <- Loc.pool.prev.T %>% 
  mutate(Method = "Transect", .after = Lake)

### Summary table ----

Loc.T.data <- Loc.T.tab %>% 
  select(Lake, Method, prev_fish)
Loc.T.data <- Loc.T.data %>% 
  pivot_wider(names_from = Lake, values_from = prev_fish)
Loc.T.data <- Loc.T.data %>% 
  mutate(Beaver = NA, .after = "Achigan") %>% 
  mutate(Montaubois = NA, .after = "Fournelle") %>% 
  mutate("St-Onge" = NA, .after = "Pin_rouge") %>% 
  mutate(Tracy = NA, .before = "Triton")
  
Loc.A.data <- Loc.All.tab %>% 
  select(Lake, Method, prev_fish)
Loc.A.data <- Loc.A.data %>% 
  pivot_wider(names_from = Lake, values_from = prev_fish)
Loc.A.data$Method[1] <- "Combined"

Loc.MT.data <- Loc.MT.tab %>% 
  select(Lake, Method, prev_fish)
Loc.MT.data <- Loc.MT.data %>% 
  pivot_wider(names_from = Lake, values_from = prev_fish)

Loc.S.data <- Loc.S.tab %>% 
  select(Lake, Method, prev_fish)
Loc.S.data <- Loc.S.data %>% 
  pivot_wider(names_from = Lake, values_from = prev_fish)

Loc.summary.data <- rbind(Loc.A.data, Loc.MT.data, Loc.S.data, Loc.T.data)

Table.Suu <- gt(Loc.summary.data) %>% 
  tab_header(md("**TABLE Suu.** Observed lake prevalence estimated by each sampling method. All values are given in percentage. Lake Tracy was not included because only one fish was caughted through all methods. NAs mean that the lake was not sampled with the corresponding method.")) %>% 
  cols_label(Pin_rouge = md("Pin rouge")) %>% 
  tab_spanner(md("Prevalence (%)"), columns = c(2:16)) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "center", weight = "bold"),
            locations = cells_column_spanners()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              table.width = pct(100)) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style= cell_borders(sides = c("bottom"), weight = px(2)), 
            location = list(cells_body(rows = 4))) %>% 
  fmt_number(decimals = 2) %>% 
  cols_hide(Tracy)
Table.Suu

Table.Suu %>% #Saving gt tab
  gtsave("Tab_LocalPrev_Methods.png", paste0(to.figs))
Table.Suu %>% 
  gtsave("Table_Suu.png", paste0(to.rédaction, "./Support_information/"))

## Species prevalence by methods ----

### All methods ----

Loc.sp.All <- CombinedData %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Loc.sp.All <- Loc.sp.All %>% #Summarizing local species abundance
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.sp.prev.All <- Loc.sp.All %>% #Local prevalence by species
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
  filter(Sampling_method == "Minnow_trap") %>% #Selecting method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Loc.sp.MT <- Loc.sp.MT %>% #Summarizing local species abundance
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.sp.prev.MT <- Loc.sp.MT %>% #Local prevalence by species
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
  filter(Sampling_method == "Seine") %>% #Selecting method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Loc.sp.S <- Loc.sp.S %>% #Summarizing local species abundance
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.sp.prev.S <- Loc.sp.S %>% #Local prevalence by species
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
  filter(Sampling_method == "Transect") %>% #Selecting method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Loc.sp.T <- Loc.sp.T %>% #Summarizing local species abundance
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Loc.sp.prev.T <- Loc.sp.T %>% #Local prevalence by species
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

## Community prevalence by method ----

### All methods ----

Fine.pool.All <- CombinedData %>% #Selecting abundance data
  select(Lake, Sampling_ID, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Fine.pool.inf.All <- Fine.pool.All %>% #Fine-scale infected fish abundance
  select(Lake, Sampling_ID, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Fine.pool.inf.All <- Fine.pool.inf.All %>% #Extracting fine-scale infected fish abundance
  select(Lake, Sampling_ID, inf_fish)

Fine.pool.tot.All <- Fine.pool.All %>% #Fine-scale total fish abundance
  select(Lake, Sampling_ID, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Fine.pool.tot.All <- Fine.pool.tot.All %>% #Extracting fine-scale total fish abundance
  select(Lake, Sampling_ID, tot_fish)

Fine.pool.prev.All <- merge(Fine.pool.inf.All, Fine.pool.tot.All, by = "Sampling_ID") %>% #Fine-scale prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100) 

Fine.pool.prev.All <- Fine.pool.prev.All[-4]

Fine.mean.All <- weighted.mean(Fine.pool.prev.All$prev_fish, Fine.pool.prev.All$tot_fish) #Regional prevalence by mean of fine-scale communities prevalence

### Minnow trap ----

Fine.pool.MT <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") %>% #Selecting method
  select(Sampling_ID, Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Fine.pool.inf.MT <- Fine.pool.MT %>% #Fine-scale infected fish abundance
  select(Sampling_ID, Lake, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Fine.pool.inf.MT <- Fine.pool.inf.MT %>% #Extracting fine-scale infected fish abundance
  select(Lake, Sampling_ID, inf_fish)

Fine.pool.tot.MT <- Fine.pool.MT %>% #Fine-scale total fish abundance
  select(Lake, Sampling_ID, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Fine.pool.tot.MT <- Fine.pool.tot.MT %>% #Extracting fine-scale total fish abundance
  select(Lake, Sampling_ID, tot_fish)

Fine.pool.prev.MT <- merge(Fine.pool.inf.MT, Fine.pool.tot.MT, by = "Sampling_ID") %>% #Fine-scale prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Fine.pool.prev.MT <- Fine.pool.prev.MT[-4]

Fine.mean.MT <- weighted.mean(Fine.pool.prev.MT$prev_fish, Fine.pool.prev.MT$tot_fish) #Regional prevalence by mean of fine-scale communities prevalence

### Seine net ----

Fine.pool.S <- CombinedData %>% 
  filter(Sampling_method == "Seine") %>% #Selecting method
  select(Lake, Sampling_ID, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Fine.pool.inf.S <- Fine.pool.S %>% #Fine-scale infected fish abundance
  select(Lake, Sampling_ID, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Fine.pool.inf.S <- Fine.pool.inf.S %>% #Extracting fine-scale infected fish abundance
  select(Lake, Sampling_ID, inf_fish)

Fine.pool.tot.S <- Fine.pool.S %>%  #Fine-scale total fish abundance
  select(Lake, Sampling_ID, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Fine.pool.tot.S <- Fine.pool.tot.S %>% #Extracting fine-scale total fish abundance
  select(Lake, Sampling_ID, tot_fish)

Fine.pool.prev.S <- merge(Fine.pool.inf.S, Fine.pool.tot.S, by = "Sampling_ID") %>% #Fine-scale prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Fine.pool.prev.S <- Fine.pool.prev.S[-4]

Fine.mean.S <- weighted.mean(Fine.pool.prev.S$prev_fish, Fine.pool.prev.S$tot_fish) #Regional prevalence by mean of fine-scale communities prevalence

### Transect ----

Fine.pool.T <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% #Selecting method
  select(Lake, Sampling_ID, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Fine.pool.inf.T <- Fine.pool.T %>% #Fine-scale infected fish abundance
  select(Lake, Sampling_ID, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Fine.pool.inf.T <- Fine.pool.inf.T %>% #Extracting fine-scale infected fish abundance
  select(Lake, Sampling_ID, inf_fish)

Fine.pool.tot.T <- Fine.pool.T %>% #Fine-scale total fish abundance
  select(Lake, Sampling_ID, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Fine.pool.tot.T <- Fine.pool.tot.T %>% #Extracting fine-scale total fish abundance
  select(Lake, Sampling_ID, tot_fish)

Fine.pool.prev.T <- merge(Fine.pool.inf.T, Fine.pool.tot.T, by = "Sampling_ID") %>% #Fine-scale prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Fine.pool.prev.T <- Fine.pool.prev.T[-4]
  
Fine.mean.T <- weighted.mean(Fine.pool.prev.T$prev_fish, Fine.pool.prev.T$tot_fish) #Regional prevalence by mean of fine-scale communities prevalence

### Summary table ----

Fine.All.data <- Fine.pool.prev.All %>% 
  select(Lake.x, Sampling_ID, prev_fish) %>% 
  mutate(Method = "Combined", .before = "Sampling_ID") %>% 
  na.omit()
Fine.All.data <- Fine.All.data %>% 
  pivot_wider(names_from = Lake.x, values_from = prev_fish)

Fine.MT.data <- Fine.pool.prev.MT %>% 
  select(Lake.x, Sampling_ID, prev_fish) %>% 
  mutate(Method = "Minnow trap", .before = "Sampling_ID") %>% 
  na.omit()
Fine.MT.data <- Fine.MT.data %>% 
  pivot_wider(names_from = Lake.x, values_from = prev_fish)

Fine.S.data <- Fine.pool.prev.S %>% 
  select(Lake.x, Sampling_ID, prev_fish) %>% 
  mutate(Method = "Seine net", .before = "Sampling_ID") %>% 
  na.omit()
Fine.S.data <- Fine.S.data %>% 
  pivot_wider(names_from = Lake.x, values_from = prev_fish)
Fine.S.data <- Fine.S.data %>% 
  mutate(Tracy = NA, .before = "Triton")

Fine.T.data <- Fine.pool.prev.T %>% 
  select(Lake.x, Sampling_ID, prev_fish) %>% 
  mutate(Method = "Transect", .before = "Sampling_ID") %>% 
  na.omit()
Fine.T.data <- Fine.T.data %>% 
  pivot_wider(names_from = Lake.x, values_from = prev_fish) 
Fine.T.data <- Fine.T.data %>% 
  mutate(Beaver = NA, .after = "Achigan") %>% 
  mutate(Montaubois = NA, .after = "Fournelle") %>% 
  mutate("St-Onge" = NA, .after = "Pin_rouge") %>% 
  mutate(Tracy = NA, .before = "Triton")

Fine.summary.data <- rbind(Fine.All.data, Fine.MT.data, Fine.S.data, Fine.T.data)

Table.Soo <- gt(Fine.summary.data, groupname_col = "Method") %>% 
  tab_header(md("**TABLE Soo.** Observed site prevalence estimated by each sampling method. Affiliated lake are given as columns. All values are given in percentage. Samples with no captures were omitted to alleviate the table.")) %>% 
  cols_label(Sampling_ID = md("Sampling ID"), Pin_rouge = md("Pin rouge")) %>% 
  tab_spanner(md("Prevalence (%)"), columns = c(3:17)) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "center", weight = "bold"),
            locations = cells_column_spanners()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "left", v_align = "middle", weight = "bold"),
            locations = cells_row_groups()) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              table.width = pct(100)) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style= cell_borders(sides = c("bottom"), weight = px(2)), 
            location = list(cells_body(rows = 460))) %>% 
  tab_style(style= cell_borders(sides = c("top", "bottom"), weight = px(2)), 
            location = list(cells_row_groups())) %>% 
  fmt_number(decimals = 2)
Table.Soo

Table.Soo %>% #Saving gt tab
  gtsave("Tab_FinePrev_Methods.png", paste0(to.figs))
Table.Soo %>% 
  gtsave("Table_Soo.png", paste0(to.rédaction, "./Support_information/"))

## Species prevalence by transect ----

Fine.sp.T <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% #Selecting method
  select(Sampling_ID, starts_with(c("tot", "inf"))) %>% #Selecting abundance
  na.omit()

Fine.sp.prev.T <- Fine.sp.T %>% #Fine-scale prevalence by species
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
