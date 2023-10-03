## Script name : Appendix tables

## Authors : Juliane Vigneault & Éric Harvey
## Date created : September 20, 2023

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
to.doc <- "./doc/"

## Loading packages ----

library(dplyr)
library(gt)
library(janitor)
library(splitstackshape)

## Loading data ----

LakesCharacteristics <- read.csv(paste0(to.data, "Lakes_Characteristics.csv"), sep = ";")
SamplingEffort <- read.csv(paste0(to.data, "SamplingEffort_Det.csv"), sep = ";")
FishingGear <- read.csv(paste0(to.data, "Gear_dimensions.csv"), sep = ";")
CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))
FishingRaw <- read.csv(paste0(to.data, "Fishing_RawData.csv"), sep = ";")
TransectData <- read.csv(paste0(to.output, "Transects_WideData.csv"))
References <- read.csv(paste0(to.doc, "Appendix_S4_BSxHost.csv"), sep = ";")

# ---- Appendix S1 : Study area and sampling ----

## Table S1 : Geographical and morphometrix characteristics ----

S1.S1 <- gt(LakesCharacteristics) %>% 
  cols_label(Lake = md("**Lake**"), Watershed = md("**Watershed**"), Latitude = md("**Latitude**"), Longitude = md("**Longitude**"), Lake_area = md("**Area (km<sup>2</sup>)**"), Max_depth = md("**Maximum depth (m)**"), Mean_depth = md("**Mean depth (m)**"), WRT = md("**Residence time (year)**"), Drainage_area = md("**Drainage area (km<sup>2</sup>)**"), Elevation = md("**Elevation (m)**"), Perimeter = md("**Perimeter (m)**"), Connectivity = md("**Distance to nearest lake (m)**")) %>%
  tab_header(md("**Table S1.** Geographical and morphometric lake characteristics.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              table.width = pct(100)) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(cell_borders(color = "black", sides = c("top", "bottom"), weight = px(2)),
            locations = cells_column_labels()) %>% 
  tab_style(cell_borders(color = "black", sides = "bottom", weight = px(2)),
           locations = cells_body(rows = 15)) %>% 
  sub_values(columns = 1, rows = 12, values = "Pin_rouge", replacement = "Pin rouge")
  
S1.S1 %>% #Saving gt tab
  gtsave("Tab_LakesCharacteristics.png", paste0(to.figs))

## Table S2 : Sampling effort ----

S1.S2 <- gt(SamplingEffort) %>% 
  cols_label(Area_classes = md("**Area class (km<sup>2</sup>)**"), Sample_size = md("**Nb. lakes**"), Nb_transects = md("**Nb. transects**"), Nb_Seines = md("**Nb. seine nets**"), Nb_MinnowTraps = md("**Nb. minnow traps**"), Nb_Samplings = md("**Nb. samplings**")) %>% 
  tab_header(md("**Table S2.** Determination of sampling effort according to lake area.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              table.width = pct(100)) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(cell_borders(color = "black", sides = c("top", "bottom"), weight = px(2)),
            locations = cells_column_labels()) %>% 
  tab_style(cell_borders(color = "black", sides = "bottom", weight = px(2)),
            locations = cells_body(rows = 5))

S1.S2 %>% #Saving gt tab
  gtsave("Tab_SamplingEffort.png", paste0(to.figs))

## Table S3 : Gear dimensions ----

S1.S3 <- gt(FishingGear) %>% 
  cols_label(Gear_ID = md("**Gear ID**"), Gear_type = md("**Gear type**"), Length..cm. = md("**Length (cm)**"), Width..cm. = md("**Width (cm)**"), Mesh..cm. = md("**Mesh (cm)**"), Diameter..cm. = md("**Diameter (cm)**"), Opening..cm. = md("**Opening (cm)**")) %>% 
  tab_header(md("**Table S3.** Fishing gear dimensions.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              table.width = pct(100)) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(cell_borders(color = "black", sides = c("top", "bottom"), weight = px(2)),
            locations = cells_column_labels()) %>% 
  tab_style(cell_borders(color = "black", sides = "bottom", weight = px(2)),
            locations = cells_body(rows = 19)) %>% 
  sub_values(columns = 2, rows = c(1,2), values = "Senne", replacement = "Seine net") %>% 
  sub_values(columns = 2, rows = c(3:19), values = "Minnow_trap", replacement = "Minnow trap")

S1.S3 %>% #Saving gt tab
  gtsave("Tab_GearDimensions.png", paste0(to.figs))

# ---- Appendix S2 : Abundance data ----

## Table S1 : All methods ----

SpAbund.All <- CombinedData %>% #Selecting capture data
  na.omit() %>% 
  select(Lake, starts_with("tot"))

SpAbund.All <- SpAbund.All %>% #Summarizing by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum)) 

SpAbund.All <- SpAbund.All %>% 
  adorn_totals(c("row", "col")) #Total by species and total by lake

S2.S1 <- gt(SpAbund.All) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), tot_AmRu = md("**AmRu**"), tot_FuDi = md("**FuDi**"), tot_MiDo = md("**MiDo**"), tot_LeGi = md("**LeGi**"), tot_PeFl = md("**PeFl**"), tot_PiPr = md("**PiPr**"), tot_ChrosomusSp. = md("**Chrosomus spp.**"), tot_PiNo = md("**PiNo**"), tot_SeAt = md("**SeAt**"), tot_LuCo = md("**LuCo**"), tot_AmNe = md("**AmNe**"), tot_CaCo = md("**CaCo**"), tot_EsMa = md("**EsMa**"), tot_UmLi = md("**UmLi**"), tot_RhAt = md("**RhAt**"), tot_Cyprinidae = md("**Unknown Cyprinids**"), tot_Centrarchidae = md("**Unknown Centrarchids**"), Total = md("**Total**")) %>% 
  tab_header(md("**Table S1.** Abundance of fish species in the 15 sampled lakes according to all sampling methods.")) %>% 
  cols_move(columns = c("tot_ChrosomusSp.", "tot_Cyprinidae", "tot_Centrarchidae"), after = "tot_RhAt") %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              table.width = pct(100)) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "darkgrey"),
            locations =  cells_body(rows = 15)) %>%
  tab_style(style = cell_borders(sides = "bottom", weight = px(2)),
            locations =  cells_body(rows = 16)) %>%
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(column = everything())) %>% 
  tab_style(style = cell_text(align = "center", v_align = "middle"), 
            locations = cells_column_labels()) %>% 
  tab_style(style = cell_borders(sides = "left", weight = px(2), color = "darkgrey"),
            locations =  cells_body(column = 19)) %>% 
  tab_style(style= cell_borders(sides = "left", weight = px(2), color = "darkgrey"), 
            location = list(cells_column_labels(column = 19))) %>% 
  sub_values(columns = 1, rows = 12, values = "Pin_rouge", replacement = "Pin rouge")

S2.S1 %>% #Saving gt tab
  gtsave("Tab_SpAbund_All.png", paste0(to.figs))

## Table S2 : Minnow traps ----

SpAbund.MT <- CombinedData %>% #Selecting capture data
  na.omit() %>% 
  filter(Sampling_method == "Minnow_trap") %>% 
  select(Lake, starts_with("tot"))

SpAbund.MT <- SpAbund.MT %>% #Summarizing by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum)) 

SpAbund.MT <-SpAbund.MT %>% 
  adorn_totals(c("row", "col")) #Total by species and total by lake

S2.S2 <- gt(SpAbund.MT) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), tot_AmRu = md("**AmRu**"), tot_FuDi = md("**FuDi**"), tot_MiDo = md("**MiDo**"), tot_LeGi = md("**LeGi**"), tot_PeFl = md("**PeFl**"), tot_PiPr = md("**PiPr**"), tot_ChrosomusSp. = md("**Chrosomus spp.**"), tot_PiNo = md("**PiNo**"), tot_SeAt = md("**SeAt**"), tot_LuCo = md("**LuCo**"), tot_AmNe = md("**AmNe**"), tot_CaCo = md("**CaCo**"), tot_EsMa = md("**EsMa**"), tot_UmLi = md("**UmLi**"), tot_RhAt = md("**RhAt**"), tot_Cyprinidae = md("**Unknown Cyprinids**"), tot_Centrarchidae = md("**Unknown Centrarchids**"), Total = md("**Total**")) %>% 
  tab_header(md("**Table S2.** Abundance of fish species in the 15 sampled lakes according to minnow traps method.")) %>% 
  cols_move(columns = c("tot_ChrosomusSp.", "tot_Cyprinidae", "tot_Centrarchidae"), after = "tot_RhAt") %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              table.width = pct(100)) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "darkgrey"),
            locations =  cells_body(rows = 15)) %>%
  tab_style(style = cell_borders(sides = "bottom", weight = px(2)),
            locations =  cells_body(rows = 16)) %>%
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(column = everything())) %>% 
  tab_style(style = cell_text(align = "center", v_align = "middle"), 
            locations = cells_column_labels()) %>% 
  tab_style(style = cell_borders(sides = "left", weight = px(2), color = "darkgrey"),
            locations =  cells_body(column = 19)) %>% 
  tab_style(style= cell_borders(sides = "left", weight = px(2), color = "darkgrey"), 
            location = list(cells_column_labels(column = 19))) %>% 
  sub_values(columns = 1, rows = 12, values = "Pin_rouge", replacement = "Pin rouge")

S2.S2 %>% #Saving gt tab
  gtsave("Tab_SpAbund_MT.png", paste0(to.figs))

## Table S3 : Seine nets ----

SpAbund.S <- CombinedData %>% #Selecting capture data
  filter(Sampling_method == "Seine") %>% 
  select(Lake, starts_with("tot"))

SpAbund.S <- SpAbund.S %>% #Summarizing by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum)) 

SpAbund.S <-SpAbund.S %>% 
  adorn_totals(c("row", "col")) #Total by species and total by lake

S2.S3 <- gt(SpAbund.S) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), tot_AmRu = md("**AmRu**"), tot_FuDi = md("**FuDi**"), tot_MiDo = md("**MiDo**"), tot_LeGi = md("**LeGi**"), tot_PeFl = md("**PeFl**"), tot_PiPr = md("**PiPr**"), tot_ChrosomusSp. = md("**Chrosomus spp.**"), tot_PiNo = md("**PiNo**"), tot_SeAt = md("**SeAt**"), tot_LuCo = md("**LuCo**"), tot_AmNe = md("**AmNe**"), tot_CaCo = md("**CaCo**"), tot_EsMa = md("**EsMa**"), tot_UmLi = md("**UmLi**"), tot_RhAt = md("**RhAt**"), tot_Cyprinidae = md("**Unknown Cyprinids**"), tot_Centrarchidae = md("**Unknown Centrarchids**"), Total = md("**Total**")) %>% 
  tab_header(md("**Table S3.** Abundance of fish species in the 15 sampled lakes according to seine nets method.")) %>% 
  cols_move(columns = c("tot_ChrosomusSp.", "tot_Cyprinidae", "tot_Centrarchidae"), after = "tot_RhAt") %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              table.width = pct(100)) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "darkgrey"),
            locations =  cells_body(rows = 15)) %>%
  tab_style(style = cell_borders(sides = "bottom", weight = px(2)),
            locations =  cells_body(rows = 16)) %>%
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(column = everything())) %>% 
  tab_style(style = cell_text(align = "center", v_align = "middle"), 
            locations = cells_column_labels()) %>% 
  tab_style(style = cell_borders(sides = "left", weight = px(2), color = "darkgrey"),
            locations =  cells_body(column = 19)) %>% 
  tab_style(style= cell_borders(sides = "left", weight = px(2), color = "darkgrey"), 
            location = list(cells_column_labels(column = 19))) %>% 
  sub_values(columns = 1, rows = 12, values = "Pin_rouge", replacement = "Pin rouge")

S2.S3 %>% #Saving gt tab
  gtsave("Tab_SpAbund_S.png", paste0(to.figs))

## Table S4 : Transects ----

SpAbund.T <- CombinedData %>% #Selecting capture data
  filter(Sampling_method == "Transect") %>% 
  select(Lake, starts_with("tot"))

SpAbund.T <- SpAbund.T %>% #Summarizing by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum)) 

SpAbund.T <-SpAbund.T %>% 
  adorn_totals(c("row", "col")) #Total by species and total by lake

S2.S4 <- gt(SpAbund.T) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), tot_AmRu = md("**AmRu**"), tot_FuDi = md("**FuDi**"), tot_MiDo = md("**MiDo**"), tot_LeGi = md("**LeGi**"), tot_PeFl = md("**PeFl**"), tot_PiPr = md("**PiPr**"), tot_ChrosomusSp. = md("**Chrosomus spp.**"), tot_PiNo = md("**PiNo**"), tot_SeAt = md("**SeAt**"), tot_LuCo = md("**LuCo**"), tot_AmNe = md("**AmNe**"), tot_CaCo = md("**CaCo**"), tot_EsMa = md("**EsMa**"), tot_UmLi = md("**UmLi**"), tot_RhAt = md("**RhAt**"), tot_Cyprinidae = md("**Unknown Cyprinids**"), tot_Centrarchidae = md("**Unknown Centrarchids**"), Total = md("**Total**")) %>% 
  tab_header(md("**Table S4.** Abundance of fish species in the 15 sampled lakes according to transects method.")) %>% 
  cols_move(columns = c("tot_ChrosomusSp.", "tot_Cyprinidae", "tot_Centrarchidae"), after = "tot_RhAt") %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              table.width = pct(100)) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "darkgrey"),
            locations =  cells_body(rows = 15)) %>%
  tab_style(style = cell_borders(sides = "bottom", weight = px(2)),
            locations =  cells_body(rows = 16)) %>%
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(column = everything())) %>% 
  tab_style(style = cell_text(align = "center", v_align = "middle"), 
            locations = cells_column_labels()) %>% 
  tab_style(style = cell_borders(sides = "left", weight = px(2), color = "darkgrey"),
            locations =  cells_body(column = 19)) %>% 
  tab_style(style= cell_borders(sides = "left", weight = px(2), color = "darkgrey"), 
            location = list(cells_column_labels(column = 19))) %>% 
  sub_values(columns = 1, rows = 12, values = "Pin_rouge", replacement = "Pin rouge")

S2.S4 %>% #Saving gt tab
  gtsave("Tab_SpAbund_T.png", paste0(to.figs))

# ---- Appendix S3 : Length data ----

FishLength <- FishingRaw %>% #Selecting data of interest
  select(Lake, Species_ID, Length, Abundance) %>% 
  na.omit() %>% 
  arrange(Lake, Species_ID)

FishLength <- expandRows(FishLength, "Abundance") #Reshaping data frame for 1 row = 1 individual format

Length.TotMean <- FishLength %>% #All data summary statistics
  select(Length) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

## Table S1 : Overall mean length by lakes ----

Length.LakeMean <- FishLength %>% #Summary statistic by lake
  group_by(Lake) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

S3.S1 <- gt(Length.LakeMean) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), md("**Mean**"), md("**sd**"), md("**N**")) %>% 
  tab_header(md("**Table S1.** Overall mean fish length for all lakes")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2)),
            locations =  cells_body(rows = 15)) %>%
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(column = everything())) %>% 
  tab_style(style = cell_text(align = "center", v_align = "middle"), 
            locations = cells_column_labels()) %>% 
  sub_values(columns = 1, rows = 12, values = "Pin_rouge", replacement = "Pin rouge")

S3.S1 %>% #Saving gt tab
  gtsave("Tab_Length_Lake.png", paste0(to.figs))

## Table S2 : Overall mean length by species ----

Length.SpeciesMean <- FishLength %>% #Summary statistic by species
  group_by(Species_ID) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

S3.S2 <- gt(Length.SpeciesMean) %>% #Creating gt tab and editing style
  cols_label(Species_ID = md("**Species**"), md("**Mean**"), md("**sd**"), md("**N**")) %>% 
  tab_header(md("**Table S2.** Overall mean fish length for all species.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2)),
            locations =  cells_body(rows = 17)) %>%
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(column = everything())) %>% 
  tab_style(style = cell_text(align = "center", v_align = "middle"), 
            locations = cells_column_labels()) %>% 
  sub_values(columns = 1, rows = 5, values = "Chrosomus sp.", replacement = "Chrosomus spp.") %>% 
  sub_values(columns = 1, rows = 4, values = "Centrarchidae.", replacement = "Unkown Centrarchids") %>% 
  sub_values(columns = 1, rows = 6, values = "Cyprinidae", replacement = "Unkown Cyprinids")

S3.S2 %>% #Saving gt tab
  gtsave("Tab_Length_Species.png", paste0(to.figs))

## Table S3 : Overall mean length by species and lakes ----

Length.SpeciesLakeMean <- FishLength %>% #Summarizing number of individuals, mean length and sd for each species within each lake
  group_by(Lake, Species_ID, .add = TRUE) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

S3.S3 <- gt(Length.SpeciesLakeMean) %>% 
  cols_label(Species_ID = md("**Species**"), md("**Mean**"), md("**sd**"), md("**N**")) %>% 
  tab_header(md("**Table S3.** Mean species length in the 15 sampled lakes.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              row.striping.include_table_body = TRUE,
              row_group.as_column = TRUE,
              heading.border.bottom.color = "black") %>% 
  sub_values(columns = 2, rows = c(7, 19, 25, 30, 59), values = "Chrosomus sp.", replacement = "Chrosomus spp.") %>% 
  sub_values(columns = 2, rows = 2, values = "Centrarchidae", replacement = "Unknown Centrarchids") %>% 
  sub_values(columns = 2, rows = c(8, 20), values = "Cyprinidae", replacement = "Unknown Cyprinids") %>% 
  #sub_values(columns = everything(), rows = everything(), values = "Pin_rouge", replacement = "Pin rouge") %>% 
  tab_style(style = cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle", weight = "bold"),
            locations = cells_row_groups()) %>% 
  tab_style(style = cell_borders(sides = c("top", "bottom", "right"), color = "darkgrey", weight = px(2)),
            locations = cells_row_groups()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(side = "top", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "Achigan")) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "black"),
            locations =  cells_body(rows = 60)) %>% 
  tab_style(style = cell_borders(side = "bottom", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "Triton")) %>% 
  tab_style(style = cell_borders(side = "bottom", weight = px(2), color = "darkgrey"),
            locations = cells_body(rows = c(6, 10, 13, 16 ,23, 28, 33, 41, 45, 48, 51, 55, 57, 58)))

S3.S3 %>% #Saving gt tab
  gtsave("Tab_Length_LakesSpecies.png", paste0(to.figs))

# ---- Appendix S4 : Species' black spot infection references ----

## Table 1 : Species' black spot infection reference ----

S4.S1 <- gt(References, groupname_col = "Species_name") %>% 
  cols_hide(columns = "Species_ID") %>% 
  cols_label(BlacksSpot_Sp = md("**Black spot trematode species**"), Mention = md("**References**")) %>% 
  tab_header(md("**Table S1.** Species black spot infection references. *Ameiurus nebulosus* and *Esox masquinongy* have no mention of black spot disease.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              row.striping.include_table_body = TRUE,
              row_group.as_column = TRUE,
              heading.border.bottom.color = "black") %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_style(style = cell_text(color = "black", font = "Calibri light", style = "italic", size = 9, align = "center", v_align = "middle", weight = "bold"),
            locations = cells_row_groups()) %>% 
  tab_style(style = cell_borders(sides = c("top", "bottom", "right"), color = "darkgrey", weight = px(2)),
            locations = cells_row_groups()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(side = "top", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "Lepomis gibbosus")) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "black"),
            locations =  cells_body(rows = 26)) %>% 
  tab_style(style = cell_borders(side = "bottom", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "Perca flavescens")) %>% 
  tab_style(style = cell_borders(side = "bottom", weight = px(2), color = "darkgrey"),
            locations = cells_body(rows = c(3, 5, 6, 7, 8, 9, 11, 14, 16, 19, 21, 22 ,23, 24)))

S4.S1 %>% #Saving gt tab
  gtsave("Tab_InfectionRefs.png", paste0(to.figs))

## Table 2 : Local prevalence

tot.loc <- CombinedData %>% 
  select(Lake, starts_with("tot")) %>% 
  na.omit() 

tot.loc <- tot.loc %>% 
  group_by(Lake) %>% 
  summarise(across(everything(), sum))

tot.loc <- tot.loc %>% 
  adorn_totals(where = "col")

tot.loc <- tot.loc %>% 
  select(Lake, Total)

inf.loc <- CombinedData %>% 
  select(Lake, starts_with("inf")) %>% 
  na.omit() 

inf.loc <- inf.loc %>% 
  group_by(Lake) %>% 
  summarise(across(everything(), sum))

inf.loc <- inf.loc %>% 
  adorn_totals(where = "col", name = "Infected")

inf.loc <- inf.loc %>% 
  select(Lake, Infected)

prev.loc <- merge(inf.loc, tot.loc, by = "Lake") %>% 
  mutate(Prevalence = Infected/Total)

S4.S2 <- gt(prev.loc) %>% 
  cols_label(Lake = md("**Lake**"), Infected = md("**Nb. infected**"), Total = md("**Nb. total**"), Prevalence = md("**Prevalence**")) %>% 
  tab_header(md("**Table S2.** Local prevalence of the sampled lakes. Theses estimations consider data from all sampling methods.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              row.striping.include_table_body = TRUE,
              heading.border.bottom.color = "black") %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "black"),
            locations =  cells_body(rows = 15)) %>% 
  sub_values(columns = 1, rows = 12, values = "Pin_rouge", replacement = "Pin rouge")
  
S4.S2 %>% #Saving gt tab
  gtsave("Tab_Prevalence_Lake.png", paste0(to.figs))

# Table 3 : Fine-scale prevalence ----

tot.fine <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% 
  select(Lake, Sampling_ID, starts_with("tot")) %>% 
  na.omit() 

tot.fine <- tot.fine %>% 
  adorn_totals(where = "col")

tot.fine <- tot.fine %>% 
  select(Lake, Sampling_ID, Total)

inf.fine <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% 
  select(Lake, Sampling_ID, starts_with("inf")) %>% 
  na.omit() 

inf.fine <- inf.fine %>% 
  adorn_totals(where = "col", name = "Infected")

inf.fine <- inf.fine %>% 
  select(Lake, Sampling_ID, Infected)

prev.fine <- merge(tot.fine, inf.fine, by = c("Lake", "Sampling_ID")) %>% 
  mutate(Prevalence = Infected / Total)

S4.S3 <- prev.fine %>% group_by(Lake) %>% 
  gt() %>% 
  cols_label(Sampling_ID = md("**Sampling ID**"), Infected = md("**Nb. Infected**"), Total = md("**Nb. total**"), Prevalence = md("**Prevalence**")) %>% 
  tab_header(md("**Table S3.** Fine-scale prevalence of the sampled lakes. Theses estimations consider data from transects sampling only.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              row.striping.include_table_body = TRUE,
              heading.border.bottom.color = "black",
              row_group.as_column = TRUE) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "black"),
            locations =  cells_body(rows = 39)) %>% 
  tab_style(style = cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle", weight = "bold"),
            locations = cells_row_groups()) %>% 
  tab_style(style = cell_borders(sides = c("top", "bottom", "right"), color = "darkgrey", weight = px(2)),
            locations = cells_row_groups()) %>%
  tab_style(style = cell_borders(side = "top", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "Achigan")) %>% 
  tab_style(style = cell_borders(side = "bottom", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "Triton")) %>% 
  tab_style(style = cell_borders(side = "bottom", weight = px(2), color = "darkgrey"),
            locations = cells_body(rows = c(6, 10, 14, 16, 19, 22, 27, 30, 34, 37)))

S4.S2 %>% #Saving gt tab
  gtsave("Tab_Prevalence_Local.png", paste0(to.figs))


# ---- Appendix S5 : Habitat description ----

## Table 1 : Water habitat characteristics ----

WaterQuality <- TransectData %>% 
  select(Lake, Transect_ID, Temperature, Conductivity, DO, Turbidity, pH, TOC, TN, TP)

S5.S1 <- gt(WaterQuality, groupname_col = "Lake") %>% 
  cols_label(Lake = md("**Lake**"), Transect_ID = md("**Transect ID**"), Temperature = md("**Temperature (°C)**"), Conductivity = md("**Conductivity (μS/cm)**"), DO = md("**Dissolved oxygen (%)**"), Turbidity = md("**Turbidity (NTU)**"), pH = md("**pH**"), TOC = md("**TOC (mg/L)**"), TN = md("**TN (mg/L)**"), TP = md("**TP (mg/L)**")) %>% 
  tab_header(md("**Table S1.** Physico-chemistry and nutrients results. Water quality parameters were measured on 48 transects. Results are grouped by lake.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              row.striping.include_table_body = TRUE,
              heading.border.bottom.color = "black", 
              row_group.as_column = TRUE) %>% 
  tab_style(style = cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle", weight = "bold"),
            locations = cells_row_groups()) %>% 
  tab_style(style = cell_borders(sides = c("top", "bottom", "right"), color = "darkgrey", weight = px(2)),
            locations = cells_row_groups()) %>% 
  tab_style(style = cell_borders(sides = "right", color = "darkgrey", weight = px(2)),
            locations = cells_body(columns = 2)) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(side = "top", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "Achigan")) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "black"),
            locations =  cells_body(rows = 48)) %>% 
  tab_style(style = cell_borders(side = "bottom", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "Triton")) %>% 
  tab_style(style = cell_borders(side = "bottom", weight = px(2), color = "darkgrey"),
            locations = cells_body(rows = c(6, 8, 12, 16 ,18, 21, 24, 29, 32, 35, 39, 42, 44, 46)))
 
S4.S3 %>% #Saving gt tab
  gtsave("Tab_Prevalence_FineScale.png", paste0(to.figs))

## Table 2 : Physical habitat characteristics ----

Habitat <- TransectData %>% 
  select(Lake, Transect_ID, Trunk, Silt, Sand, Rock, Metric_block, Macrophyte, Mean_depth)

S5.S2 <- gt(Habitat, groupname_col = "Lake") %>% 
  cols_label(Lake = md("**Lake**"), Transect_ID = md("**Transect ID**"), Trunk = md("**Trunk**"), Silt = md("**Silt (%)**"), Sand = md("**Sand (%)**"), Rock = md("**Rock (%)**"), Metric_block = md("**Metric block (%)**"), Macrophyte = md("**Macrophyte cover (%)**"), Mean_depth = md("**Mean depth (cm)**")) %>% 
  tab_header(md("**Table S2.** Physical habitat description. The results presented for the 48 transects are means of every 10 m estimations exept trunks are total by transect. Results are grouped by lake. Lakes Beaver, Tracy, Montaubois and St-Onge were not sampled for transect method.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              row.striping.include_table_body = TRUE,
              heading.border.bottom.color = "black", 
              row_group.as_column = TRUE) %>% 
  tab_style(style = cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle", weight = "bold"),
            locations = cells_row_groups()) %>% 
  tab_style(style = cell_borders(sides = c("top", "bottom", "right"), color = "darkgrey", weight = px(2)),
            locations = cells_row_groups()) %>% 
  tab_style(style = cell_borders(sides = "right", color = "darkgrey", weight = px(2)),
            locations = cells_body(columns = 2)) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style = cell_borders(side = "top", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "Achigan")) %>% 
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "black"),
            locations =  cells_body(rows = 48)) %>% 
  tab_style(style = cell_borders(side = "bottom", weight = px(2), color = "black"),
            locations = cells_row_groups(groups = "Triton")) %>% 
  tab_style(style = cell_borders(side = "bottom", weight = px(2), color = "darkgrey"),
            locations = cells_body(rows = c(6, 8, 12, 16 ,18, 21, 24, 29, 32, 35, 39, 42, 44, 46)))

S5.S2 %>% #Saving gt tab
  gtsave("Tab_Habitat.png", paste0(to.figs))




