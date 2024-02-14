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

# ---- Landscape prevalence ----

## Community prevalence by methods ----

#Lake Tracy is included at regional scale
#All infected fishes (from all lakes) diveded by the total of fishes

### Combined methods ----

Land.pool.C <- CombinedData %>% #Selecting abundance data
  select(starts_with(c("inf", "tot"))) %>% 
  na.omit()

Land.pool.inf.C <- Land.pool.C %>% #Regional infected fish abundance
  select(starts_with("inf"))
Land.pool.inf.C <- sum(Land.pool.inf.C)

Land.pool.tot.C <- Land.pool.C %>% #Regional total fish abundance
  select(starts_with("tot"))
Land.pool.tot.C <- sum(Land.pool.tot.C)

Land.pool.prev.C <- (Land.pool.inf.C/Land.pool.tot.C)*100 #Regional prevalence

### Minnow traps ----

Land.pool.MT <- CombinedData %>%  #Selecting abundance data & method
  filter(Sampling_method == "Minnow_trap") %>% 
  select(starts_with(c("inf", "tot"))) %>% 
  na.omit()

Land.pool.inf.MT <- Land.pool.MT %>% #Regional infected fish abundance
  select(starts_with("inf"))
Land.pool.inf.MT <- sum(Land.pool.inf.MT)

Land.pool.tot.MT <- Land.pool.MT %>% #Regional total fish abundance
  select(starts_with("tot"))
Land.pool.tot.MT <- sum(Land.pool.tot.MT)

Land.pool.prev.MT <- (Land.pool.inf.MT/Land.pool.tot.MT)*100 #Regional prevalence

### Seine net ----

Land.pool.S <- CombinedData %>%  #Selecting abundance data & method
  filter(Sampling_method == "Seine") %>%
  select(starts_with(c("inf", "tot"))) %>% 
  na.omit()

Land.pool.inf.S <- Land.pool.S %>% #Landional infected fish abundance
  select(starts_with("inf"))
Land.pool.inf.S <- sum(Land.pool.inf.S)

Land.pool.tot.S <- Land.pool.S %>% #Landional total fish abundance
  select(starts_with("tot"))
Land.pool.tot.S <- sum(Land.pool.tot.S)

Land.pool.prev.S <- (Land.pool.inf.S/Land.pool.tot.S)*100 #Landional prevalence

### Transect ----

Land.pool.T <- CombinedData %>%  #Selecting abundance data & method
  filter(Sampling_method == "Transect") %>%
  select(starts_with(c("inf", "tot"))) %>% 
  na.omit()

Land.pool.inf.T <- Land.pool.T %>% #Landional infected fish abundance
  select(starts_with("inf"))
Land.pool.inf.T <- sum(Land.pool.inf.T)

Land.pool.tot.T <- Land.pool.T %>% #Landional total fish abundance
  select(starts_with("tot"))
Land.pool.tot.T <- sum(Land.pool.tot.T)

Land.pool.prev.T <- (Land.pool.inf.T/Land.pool.tot.T)*100 #Landional prevalence

### Summary table ----

Land.C.data <- c(Method = "Combined", Prevalence = Land.pool.prev.C)
Land.MT.data <- c(Method = "Minnow trap", Prevalence = Land.pool.prev.MT)
Land.S.data <- c(Method = "Seine net", Prevalence = Land.pool.prev.S)
Land.T.data <- c(Method = "Transect", Prevalence = Land.pool.prev.T)

Land.summary.data <- data.frame(rbind(Land.C.data, Land.MT.data, Land.S.data, Land.T.data), row.names = NULL)
Land.summary.data$Prevalence <- as.numeric(Land.summary.data$Prevalence)

Table.S12 <- gt(Land.summary.data) %>% 
  tab_header(md("**TABLE S12.** Observed landscape prevalence estimated by each sampling method. All values are given in percentage.")) %>% 
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
Table.S12

Table.S12 %>% #Saving gt tab
  gtsave("Tab_LandscapePrev_Methods.png", paste0(to.figs))
Table.S12 %>% 
  gtsave("Table_S12.png", paste0(to.rédaction, "./Support_information/"))

## Species prevalence by methods ----
### Combined methods ----

Land.sp.C <- CombinedData %>%  #Selecting abundance data 
  select(Lake, starts_with(c("inf", "tot"))) %>% 
  na.omit() %>% 
  adorn_totals(where = "row") #Summarizing Landional abundance by species

Land.sp.prev.C <- Land.sp.C %>% #Extracting abundance sums
  filter(Lake == "Total") %>% 
  select(!(Lake))

Land.sp.prev.C <- Land.sp.prev.C %>% #Landional prevalence by species
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

Land.sp.MT <- CombinedData %>%
  filter(Sampling_method == "Minnow_trap") %>% #Selecting method
  select(Lake, starts_with(c("inf", "tot"))) %>% #Selecting abundance data 
  na.omit() %>% 
  adorn_totals(where = "row") #Summarizing CLandional abundance by species

Land.sp.prev.MT <- Land.sp.MT %>% #Extracting abundance sums
  filter(Lake == "Total") %>% 
  select(!(Lake))

Land.sp.prev.MT <- Land.sp.prev.MT %>% #CLandional prevalence by species
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

Land.sp.S <- CombinedData %>%  
  filter(Sampling_method == "Seine") %>% #Selecting method
  select(Lake, starts_with(c("inf", "tot"))) %>%  #Selecting abundance data 
  na.omit() %>% 
  adorn_totals(where = "row") #Summarizing Landional abundance by species

Land.sp.prev.S <- Land.sp.S %>% #Extracting abundance sums
  filter(Lake == "Total") %>% 
  select(!(Lake))

Land.sp.prev.S <- Land.sp.prev.S %>% #Landional prevalence by species
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

Land.sp.T <- CombinedData %>%
  filter(Sampling_method == "Transect") %>% #Selecting method
  select(Lake, starts_with(c("inf", "tot"))) %>% #Selecting abundance data
  na.omit() %>% 
  adorn_totals(where = "row")#Summarizing Landional abundance by species

Land.sp.prev.T <- Land.sp.T %>% #Extracting abundance sums
  filter(Lake == "Total") %>% 
  select(!(Lake))

Land.sp.prev.T <- Land.sp.prev.T %>%  #Landional prevalence by species
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

### Summary table ----

Land.sp.sum.C <- cbind(Method = "Combined", Land.sp.prev.C)
Land.sp.sum.MT <- cbind(Method = "Minnow trap", Land.sp.prev.MT)
Land.sp.sum.S <- cbind(Method = "Seine net", Land.sp.prev.S)
Land.sp.sum.T <- cbind(Method = "Transect", Land.sp.prev.T)

Land.sp.sum.data <- data.frame(rbind(Land.sp.sum.C, Land.sp.sum.MT, Land.sp.sum.S, Land.sp.sum.T), row.names = NULL)

Land.sp.sum.data <- pivot_longer(Land.sp.sum.data, cols = c(2:18), names_to = "Species", values_to = "Prevalence")
Land.sp.sum.data <- pivot_wider(Land.sp.sum.data, names_from = "Method", values_from = "Prevalence")

Species <- c("Ambloplites rupestris", "Fundulus diaphanus", "Micropterus dolomieu","Unknown centrarchids", "Lepomis gibbosus", "Perca flavescens", "Pimephales promelas", "Chrosomus spp.", "Pimephales notatus", "Unknown cyprinids", "Semotilus atromaculatus", "Luxilus cornutus", "Ameiurus nebulosus", "Catostomus commersonii", "Esox masquinongy", "Umbra limi", "Rhinichthys atratulus")

Land.sp.sum.data$Species <- Species

Table.S19 <- gt(Land.sp.sum.data) %>% 
  tab_header(md("**TABLE S19.** Host specificity of the black spot disease at landscape-scale accrording to the different sampling methods. NaN means that no fish were caught in the corresponding category.")) %>% 
  tab_spanner("Method", columns = c(2:5)) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "center", weight = "bold"),
            locations = cells_column_spanners()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle", style = "italic"),
            locations = cells_body(columns = 1, rows = c(1:3, 5:9, 11:17))) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              table.width = pct(100)) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style= cell_borders(sides = c("bottom"), weight = px(2)), 
            location = list(cells_body(rows = 17))) %>% 
  fmt_number(decimals = 2)
Table.S19  

Table.S19 %>% #Saving gt tab
  gtsave("Tab_LandscapeSpec_Methods.png", paste0(to.figs))
Table.S19 %>% 
  gtsave("Table_S19.png", paste0(to.rédaction, "./Support_information/"))

# ---- Lake prevalence ----

CombinedData <- CombinedData %>% #Deleting lake Tracy because we cannot calculate a prevalence on a unique data point (1 fish)
  filter(!(Lake == "Tracy"))

## Community prevalence by methods ----

### Combined methods ----

Lake.pool.C <- CombinedData %>% #Selecting abundance data
  select(Lake, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Lake.pool.C <- Lake.pool.C %>% #Summarizing abundance columns by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Lake.pool.inf.C <- Lake.pool.C %>% #Summarizing Lakeal infected community abundance 
  select(Lake, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Lake.pool.inf.C <- Lake.pool.inf.C %>% #Extracting Lakeal infected community abundances
  select(Lake, inf_fish)

Lake.pool.tot.C <- Lake.pool.C %>% #Summarizing Lakeal total community abundance 
  select(Lake, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Lake.pool.tot.C <- Lake.pool.tot.C %>% #Extracting Lakeal infected community abundances
  select(Lake, tot_fish)

Lake.pool.prev.C <- merge(Lake.pool.inf.C, Lake.pool.tot.C, by = "Lake") %>% #Lakeal communities prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Lake.mean.C <- weighted.mean(Lake.pool.prev.C$prev_fish, Lake.pool.prev.C$tot_fish) #Regional prevalence by mean of Lakeal communities prevalence

Lake.C.tab <- Lake.pool.prev.C %>% 
  mutate(Method = "Combined", .after = Lake)
  
### Minnow trap ----

Lake.pool.MT <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") %>% #Selecting method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Lake.pool.MT <- Lake.pool.MT %>% #Summarizing abundance columns by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Lake.pool.inf.MT <- Lake.pool.MT %>% #Summarizing Lakeal infected community abundance 
  select(Lake, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Lake.pool.inf.MT <- Lake.pool.inf.MT %>% #Extracting Lakeal infected community abundances
  select(Lake, inf_fish)

Lake.pool.tot.MT <- Lake.pool.MT %>% #Summarizing Lakeal total community abundance 
  select(Lake, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Lake.pool.tot.MT <- Lake.pool.tot.MT %>% #Extracting Lakeal infected community abundances
  select(Lake, tot_fish)

Lake.pool.prev.MT <- merge(Lake.pool.inf.MT, Lake.pool.tot.MT, by = "Lake") %>% #Lakeal communities prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Lake.mean.MT <- weighted.mean(Lake.pool.prev.MT$prev_fish, Lake.pool.prev.MT$tot_fish) #Regional prevalence by mean of Lakeal communities prevalence

Lake.MT.tab <- Lake.pool.prev.MT %>% 
  mutate(Method = "Minnow trap", .after = Lake)

### Seine net ----

Lake.pool.S <- CombinedData %>% 
  filter(Sampling_method == "Seine") %>% #Selecting method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Lake.pool.S <- Lake.pool.S %>% #Summarizing abundance columns by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Lake.pool.inf.S <- Lake.pool.S %>% #Summarizing Lakeal infected community abundance 
  select(Lake, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Lake.pool.inf.S <- Lake.pool.inf.S %>% #Extracting Lakeal infected community abundances
  select(Lake, inf_fish)

Lake.pool.tot.S <- Lake.pool.S %>%  #Summarizing Lakeal total community abundance 
  select(Lake, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Lake.pool.tot.S <- Lake.pool.tot.S %>% #Extracting Lakeal infected community abundances
  select(Lake, tot_fish)

Lake.pool.prev.S <- merge(Lake.pool.inf.S, Lake.pool.tot.S, by = "Lake") %>% #Lakeal communities prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Lake.mean.S <- weighted.mean(Lake.pool.prev.S$prev_fish, Lake.pool.prev.S$tot_fish) #Regional prevalence by mean of Lakeal communities prevalence

Lake.S.tab <- Lake.pool.prev.S %>% 
  mutate(Method = "Seine net", .after = Lake)

### Transect ----

Lake.pool.T <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% #Selecting method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Lake.pool.T <- Lake.pool.T %>% #Summarizing abundance columns by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Lake.pool.inf.T <- Lake.pool.T %>% #Summarizing Lakeal infected community abundance 
  select(Lake, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Lake.pool.inf.T <- Lake.pool.inf.T %>% #Extracting Lakeal infected community abundances
  select(Lake, inf_fish)

Lake.pool.tot.T <- Lake.pool.T %>% #Summarizing Lakeal total community abundance 
  select(Lake, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Lake.pool.tot.T <- Lake.pool.tot.T %>% #Extracting Lakeal infected community abundances
  select(Lake, tot_fish)

Lake.pool.prev.T <- merge(Lake.pool.inf.T, Lake.pool.tot.T, by = "Lake") %>% #Lakeal communities prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Lake.mean.T <- weighted.mean(Lake.pool.prev.T$prev_fish, Lake.pool.prev.T$tot_fish) #Regional prevalence by mean of Lakeal communities prevalence

Lake.T.tab <- Lake.pool.prev.T %>% 
  mutate(Method = "Transect", .after = Lake)

### Summary table ----

Lake.T.data <- Lake.T.tab %>% 
  select(Lake, Method, prev_fish)
Lake.T.data <- Lake.T.data %>% 
  pivot_wider(names_from = Lake, values_from = prev_fish)
Lake.T.data <- Lake.T.data %>% 
  mutate(Beaver = NA, .after = "Achigan") %>% 
  mutate(Montaubois = NA, .after = "Fournelle") %>% 
  mutate("St-Onge" = NA, .after = "Pin_rouge")
  
Lake.C.data <- Lake.C.tab %>% 
  select(Lake, Method, prev_fish)
Lake.C.data <- Lake.C.data %>% 
  pivot_wider(names_from = Lake, values_from = prev_fish)

Lake.MT.data <- Lake.MT.tab %>% 
  select(Lake, Method, prev_fish)
Lake.MT.data <- Lake.MT.data %>% 
  pivot_wider(names_from = Lake, values_from = prev_fish)

Lake.S.data <- Lake.S.tab %>% 
  select(Lake, Method, prev_fish)
Lake.S.data <- Lake.S.data %>% 
  pivot_wider(names_from = Lake, values_from = prev_fish)

Lake.summary.data <- rbind(Lake.C.data, Lake.MT.data, Lake.S.data, Lake.T.data)

Table.S13 <- gt(Lake.summary.data) %>% 
  tab_header(md("**TABLE S13.** Observed lake prevalence estimated by each sampling method. All values are given in percentage. Lake Tracy was not included because only one fish was caughted through all methods. NAs mean that the lake was not sampled with the corresponding method.")) %>% 
  cols_label(Pin_rouge = md("Pin rouge")) %>% 
  tab_spanner(md("Prevalence (%)"), columns = c(2:15)) %>% 
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
  fmt_number(decimals = 2)
Table.S13

Table.S13 %>% #Saving gt tab
  gtsave("Tab_LakePrev_Methods.png", paste0(to.figs))
Table.S13 %>% 
  gtsave("Table_S13.png", paste0(to.rédaction, "./Support_information/"))

## Species prevalence by methods ----

### Combined methods ----

Lake.sp.C <- CombinedData %>% 
  select(Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Lake.sp.C <- Lake.sp.C %>% #Summarizing Lakeal species abundance
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Lake.sp.prev.C <- Lake.sp.C %>% #Lakeal prevalence by species
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

Table.A1 <- gt(Lake.sp.prev.C) %>% 
  tab_header(md("**TABLE A1.** Prévalence d'infection pour chaque espèce de poisson dans les lacs échantillonnés. NaN indique qu'aucun individu n'a été observé pour la catégorie correspondante. Les valeurs de prévalence sont présentées en pourcentage.")) %>% 
  cols_label(prev_AmRu = md("*Ambloplites rupestris*"), prev_FuDi = md("*Fundulus diaphanus*"), prev_MiDo = md("*Micropterus dolomieu*"), prev_Centrarchidae = md("Unknown centrarchids"), prev_LeGi = md("*Lepomis gibbosus*"), prev_PeFl = md("*Perca flavescens*"), prev_PiPr = md("*Pimephales promelas*"), prev_ChrosomusSp. = md("*Chrosomus* spp."), prev_PiNo = md("*Pimephales notatus*"), prev_Cyprinidae = md("Unknown cyprinids"), prev_SeAt = md("*Semotilus atromaculatus*"), prev_LuCo = md("*Luxilus cornutus*"), prev_AmNe = md("*Ameiurus nebulosus*"), prev_CaCo = md("*Catostomus commersonii*"), prev_EsMa = md("*Esox masquinongy*"), prev_UmLi = md("*Umbra limi*"), prev_RhAt = md("*Rhinichthys atratulus*")) %>% 
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
            location = list(cells_body(rows = 14))) %>% 
  sub_values(values = "Pin_rouge", replacement = "Pin rouge") %>% 
  fmt_number(decimals = 2)
Table.A1 

Table.A1 %>% #Saving gt tab
  gtsave("Tab_LakeSpec.png", paste0(to.figs))

### Minnow trap ----

Lake.sp.MT <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") %>% #Selecting method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Lake.sp.MT <- Lake.sp.MT %>% #Summarizing Lakeal species abundance
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Lake.sp.prev.MT <- Lake.sp.MT %>% #Lakeal prevalence by species
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

Lake.sp.S <- CombinedData %>% 
  filter(Sampling_method == "Seine") %>% #Selecting method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Lake.sp.S <- Lake.sp.S %>% #Summarizing Lakeal species abundance
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Lake.sp.prev.S <- Lake.sp.S %>% #Lakeal prevalence by species
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

Lake.sp.T <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% #Selecting method
  select(Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Lake.sp.T <- Lake.sp.T %>% #Summarizing Lakeal species abundance
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum))

Lake.sp.prev.T <- Lake.sp.T %>% #Lakeal prevalence by species
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

# ---- Site prevalence ----

## Community prevalence by method ----

### Combined methods ----

Site.pool.C <- CombinedData %>% #Selecting abundance data
  select(Lake, Sampling_ID, starts_with(c("tot", "inf"))) %>% 
  na.omit()

Site.pool.inf.C <- Site.pool.C %>% #Site-scale infected fish abundance
  select(Lake, Sampling_ID, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Site.pool.inf.C <- Site.pool.inf.C %>% #Extracting Site-scale infected fish abundance
  select(Lake, Sampling_ID, inf_fish)

Site.pool.tot.C <- Site.pool.C %>% #Site-scale total fish abundance
  select(Lake, Sampling_ID, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Site.pool.tot.C <- Site.pool.tot.C %>% #Extracting Site-scale total fish abundance
  select(Lake, Sampling_ID, tot_fish)

Site.pool.prev.C <- merge(Site.pool.inf.C, Site.pool.tot.C, by = "Sampling_ID") %>% #Site-scale prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100) 

Site.pool.prev.C <- Site.pool.prev.C[-4]

Site.mean.C <- weighted.mean(Site.pool.prev.C$prev_fish, Site.pool.prev.C$tot_fish) #Regional prevalence by mean of Site-scale communities prevalence

### Minnow trap ----

Site.pool.MT <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") %>% #Selecting method
  select(Sampling_ID, Lake, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Site.pool.inf.MT <- Site.pool.MT %>% #Site-scale infected fish abundance
  select(Sampling_ID, Lake, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Site.pool.inf.MT <- Site.pool.inf.MT %>% #Extracting Site-scale infected fish abundance
  select(Lake, Sampling_ID, inf_fish)

Site.pool.tot.MT <- Site.pool.MT %>% #Site-scale total fish abundance
  select(Lake, Sampling_ID, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Site.pool.tot.MT <- Site.pool.tot.MT %>% #Extracting Site-scale total fish abundance
  select(Lake, Sampling_ID, tot_fish)

Site.pool.prev.MT <- merge(Site.pool.inf.MT, Site.pool.tot.MT, by = "Sampling_ID") %>% #Site-scale prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Site.pool.prev.MT <- Site.pool.prev.MT[-4]

Site.mean.MT <- weighted.mean(Site.pool.prev.MT$prev_fish, Site.pool.prev.MT$tot_fish) #Regional prevalence by mean of Site-scale communities prevalence

### Seine net ----

Site.pool.S <- CombinedData %>% 
  filter(Sampling_method == "Seine") %>% #Selecting method
  select(Lake, Sampling_ID, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Site.pool.inf.S <- Site.pool.S %>% #Site-scale infected fish abundance
  select(Lake, Sampling_ID, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Site.pool.inf.S <- Site.pool.inf.S %>% #Extracting Site-scale infected fish abundance
  select(Lake, Sampling_ID, inf_fish)

Site.pool.tot.S <- Site.pool.S %>%  #Site-scale total fish abundance
  select(Lake, Sampling_ID, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Site.pool.tot.S <- Site.pool.tot.S %>% #Extracting Site-scale total fish abundance
  select(Lake, Sampling_ID, tot_fish)

Site.pool.prev.S <- merge(Site.pool.inf.S, Site.pool.tot.S, by = "Sampling_ID") %>% #Site-scale prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Site.pool.prev.S <- Site.pool.prev.S[-4]

Site.mean.S <- weighted.mean(Site.pool.prev.S$prev_fish, Site.pool.prev.S$tot_fish) #Regional prevalence by mean of Site-scale communities prevalence

### Transect ----

Site.pool.T <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% #Selecting method
  select(Lake, Sampling_ID, starts_with(c("tot", "inf"))) %>% #Selecting abundance data
  na.omit()

Site.pool.inf.T <- Site.pool.T %>% #Site-scale infected fish abundance
  select(Lake, Sampling_ID, starts_with("inf")) %>% 
  adorn_totals(where = "col", name = "inf_fish")

Site.pool.inf.T <- Site.pool.inf.T %>% #Extracting Site-scale infected fish abundance
  select(Lake, Sampling_ID, inf_fish)

Site.pool.tot.T <- Site.pool.T %>% #Site-scale total fish abundance
  select(Lake, Sampling_ID, starts_with("tot")) %>% 
  adorn_totals(where = "col", name = "tot_fish")

Site.pool.tot.T <- Site.pool.tot.T %>% #Extracting Site-scale total fish abundance
  select(Lake, Sampling_ID, tot_fish)

Site.pool.prev.T <- merge(Site.pool.inf.T, Site.pool.tot.T, by = "Sampling_ID") %>% #Site-scale prevalence
  mutate(prev_fish = (inf_fish/tot_fish)*100)

Site.pool.prev.T <- Site.pool.prev.T[-4]
  
Site.mean.T <- weighted.mean(Site.pool.prev.T$prev_fish, Site.pool.prev.T$tot_fish) #Regional prevalence by mean of Site-scale communities prevalence

### Summary table ----

Site.C.data <- Site.pool.prev.C %>% 
  select(Lake.x, Sampling_ID, prev_fish) %>% 
  mutate(Method = "Combined", .before = "Sampling_ID") %>% 
  na.omit()
Site.C.data <- Site.C.data %>% 
  pivot_wider(names_from = Lake.x, values_from = prev_fish)

Site.MT.data <- Site.pool.prev.MT %>% 
  select(Lake.x, Sampling_ID, prev_fish) %>% 
  mutate(Method = "Minnow trap", .before = "Sampling_ID") %>% 
  na.omit()
Site.MT.data <- Site.MT.data %>% 
  pivot_wider(names_from = Lake.x, values_from = prev_fish)

Site.S.data <- Site.pool.prev.S %>% 
  select(Lake.x, Sampling_ID, prev_fish) %>% 
  mutate(Method = "Seine net", .before = "Sampling_ID") %>% 
  na.omit()
Site.S.data <- Site.S.data %>% 
  pivot_wider(names_from = Lake.x, values_from = prev_fish)

Site.T.data <- Site.pool.prev.T %>% 
  select(Lake.x, Sampling_ID, prev_fish) %>% 
  mutate(Method = "Transect", .before = "Sampling_ID") %>% 
  na.omit()
Site.T.data <- Site.T.data %>% 
  pivot_wider(names_from = Lake.x, values_from = prev_fish) 
Site.T.data <- Site.T.data %>% 
  mutate(Beaver = NA, .after = "Achigan") %>% 
  mutate(Montaubois = NA, .after = "Fournelle") %>% 
  mutate("St-Onge" = NA, .after = "Pin_rouge")

Site.summary.data <- rbind(Site.C.data, Site.MT.data, Site.S.data, Site.T.data)

Table.S14 <- gt(Site.summary.data, groupname_col = "Method") %>% 
  tab_header(md("**TABLE S14.** Observed site prevalence estimated by each sampling method. Affiliated lake are given as columns. All values are given in percentage. Samples with no captures were omitted to alleviate the table. Lake Tracy was not included because only one fish was caughted through all methods.")) %>% 
  cols_label(Sampling_ID = md("Sampling ID"), Pin_rouge = md("Pin rouge")) %>% 
  tab_spanner(md("Prevalence (%)"), columns = c(3:16)) %>% 
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
            location = list(cells_body(rows = 458))) %>% 
  tab_style(style= cell_borders(sides = c("top", "bottom"), weight = px(2)), 
            location = list(cells_row_groups())) %>% 
  fmt_number(decimals = 2)
Table.S14

Table.S14 %>% #Saving gt tab
  gtsave("Tab_SitePrev_Methods.png", paste0(to.figs))
Table.S14 %>% 
  gtsave("Table_S14.png", paste0(to.rédaction, "./Support_information/"))

## Species prevalence by transect ----

Site.sp.T <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% #Selecting method
  select(Sampling_ID, starts_with(c("tot", "inf"))) %>% #Selecting abundance
  na.omit()

Site.sp.prev.T <- Site.sp.T %>% #Site-scale prevalence by species
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

