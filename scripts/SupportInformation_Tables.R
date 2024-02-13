## Script name : Support information

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
to.rédaction <- "./rédaction/"

## Loading packages ----

library(dplyr)
library(gt)
library(janitor)
library(splitstackshape)
library(stringr)

## Loading data ----

LakesCharacteristics <- read.csv(paste0(to.data, "Lakes_Characteristics.csv"), sep = ";")
SamplingEffort <- read.csv(paste0(to.data, "SamplingEffort_Det.csv"), sep = ";")
FishingGear <- read.csv(paste0(to.data, "Gear_dimensions.csv"), sep = ";")
CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))
FishingRaw <- read.csv(paste0(to.data, "Fishing_RawData.csv"), sep = ";")
TransectData <- read.csv(paste0(to.output, "Transects_WideData.csv"))
References <- read.csv(paste0(to.doc, "Appendix_S4_BSxHost.csv"), sep = ";")

# ---- Table S1 : Geographical and morphometric characteristics ----

Table.S1 <- gt(LakesCharacteristics) %>% 
  cols_label(Lake = md("**Lake**"), Watershed = md("**Watershed**"), Latitude = md("**Latitude**"), Longitude = md("**Longitude**"), Lake_area = md("**Area (km<sup>2</sup>)**"), Max_depth = md("**Maximum depth (m)**"), Mean_depth = md("**Mean depth (m)**"), WRT = md("**Residence time (year)**"), Drainage_area = md("**Drainage area (km<sup>2</sup>)**"), Elevation = md("**Elevation (m)**"), Perimeter = md("**Perimeter (m)**"), Connectivity = md("**Distance to nearest lake (m)**")) %>%
  tab_header(md("**TABLE S1.** Geographical and morphometric lake characteristics on the 15 lakes sampled.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              table.width = pct(100), 
              table.border.bottom.style = "hidden") %>% 
  cols_width(Latitude ~ px(100),
             Longitude ~ px(100)) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(cell_borders(color = "black", sides = c("top", "bottom"), weight = px(2)),
            locations = cells_column_labels()) %>% 
  tab_style(cell_borders(color = "black", sides = "bottom", weight = px(2)),
           locations = cells_body(rows = 15)) %>% 
  sub_values(columns = 1, rows = 12, values = "Pin_rouge", replacement = "Pin rouge") %>% 
  fmt_number(columns = c(5, 9), decimals = 3, use_seps = FALSE) %>% 
  fmt_number(columns = c(6, 7, 10, 11), decimals = 1, use_seps = FALSE) %>% 
  fmt_number(columns = c(8), decimals = 2, use_seps = FALSE) %>% 
  tab_footnote(footnote = "The data was extracted from the government of Québec documentation (Atlas de l'eau).",
               locations = cells_column_labels(columns = 2)) %>% 
  tab_footnote(footnote = "The data was extracted from the bathymetric maps available on https://crelaurentides.org/atlas-des-lacs/.",
               locations = cells_column_labels(columns = c(3:10))) %>% 
  tab_footnote(footnote = "The estimations were computed on QGIS.", 
               locations = cells_column_labels(c(11,12))) %>% 
  tab_footnote(footnote = "The measurement was made from centroid to centroid.", 
               locations = cells_column_labels(12))

Table.S1 %>% #Saving gt tab
  gtsave("Tab_GeoMorpho.png", paste0(to.figs))
Table.S1 %>% 
  gtsave("Table_S1.png", paste0(to.rédaction, "./Support_information/"))

# ---- Table S2 : Sampling effort ----

Table.S2 <- gt(SamplingEffort) %>% 
  cols_label(Area_classes = md("**Area class (km<sup>2</sup>)**"), Sample_size = md("**Nb. lakes**"), Nb_transects = md("**Nb. transects**"), Nb_Seines = md("**Nb. seine nets**"), Nb_MinnowTraps = md("**Nb. minnow traps**"), Nb_Samplings = md("**Nb. samplings**")) %>% 
  tab_header(md("**TABLE S2.** Determination of the sampling effort within lakes according to the lake area.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage") %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(cell_borders(color = "black", sides = c("top", "bottom"), weight = px(2)),
            locations = cells_column_labels()) %>% 
  tab_style(cell_borders(color = "black", sides = "bottom", weight = px(2)),
            locations = cells_body(rows = 5))

Table.S2 %>% #Saving gt tab
  gtsave("Tab_SamplingEffort.png", paste0(to.figs))
Table.S2 %>% 
  gtsave("Table_S2.png", paste0(to.rédaction, "./Support_information/"))

# ---- Table S3 : Gear dimensions ----

Table.S3 <- gt(FishingGear) %>% 
  cols_label(Gear_ID = md("**Gear ID**"), Gear_type = md("**Gear type**"), Length..cm. = md("**Length (cm)**"), Width..cm. = md("**Width (cm)**"), Mesh..cm. = md("**Mesh (cm)**"), Diameter..cm. = md("**Diameter (cm)**"), Opening..cm. = md("**Opening (cm)**")) %>% 
  tab_header(md("**Table S3.** Fishing gear dimensions.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              page.width = px(100),
              table.border.bottom.style = "hidden") %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(cell_borders(color = "black", sides = c("top", "bottom"), weight = px(2)),
            locations = cells_column_labels()) %>% 
  tab_style(cell_borders(color = "black", sides = "bottom", weight = px(2)),
            locations = cells_body(rows = 19)) %>% 
  sub_values(columns = 2, rows = c(1,2), values = "Senne", replacement = "Seine net") %>% 
  sub_values(columns = 2, rows = c(3:19), values = "Minnow_trap", replacement = "Minnow trap") %>% 
  tab_footnote(footnote = "Squared minnow trap", 
               locations = cells_body(columns = 1, rows = c(3:7, 14:17))) %>% 
  tab_footnote(footnote = "Rounded minnow trap",
               locations = cells_body(columns = 1, rows = c(8:13, 18:19)))

Table.S3 %>% #Saving gt tab
  gtsave("Tab_GearDimensions.png", paste0(to.figs))
Table.S3 %>% 
  gtsave("Table_S3.png", paste0(to.rédaction, "./Support_information/"))

# ---- Table S4 : Abundance in combined methods ----

SpAbund.C <- CombinedData %>% #Selecting capture data
  select(Lake, starts_with("tot")) 

SpAbund.C <- SpAbund.C %>% #Summarizing by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum,  na.rm = TRUE)) 

SpAbund.C <- SpAbund.C %>% 
  adorn_totals(c("row", "col")) #Total by species and total by lake

Table.S4 <- gt(SpAbund.C) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), tot_AmRu = md("***Ambloplites rupestris***"), tot_FuDi = md("***Fundulus diaphanus***"), tot_MiDo = md("***Micropterus dolomieu***"), tot_LeGi = md("***Lepomis gibbosus***"), tot_PeFl = md("***Perca flavescens***"), tot_PiPr = md("***Pimephales promelas***"), tot_ChrosomusSp. = md("**Chrosomus spp.**"), tot_PiNo = md("***Pimephales notatus***"), tot_SeAt = md("***Semotilus atromaculatus***"), tot_LuCo = md("***Luxilus cornutus***"), tot_AmNe = md("***Ameiurus nebulosus***"), tot_CaCo = md("***Catostomus commersonii***"), tot_EsMa = md("***Esox masquinongy***"), tot_UmLi = md("***Umbra limi***"), tot_RhAt = md("***Rhinichthys atratulus***"), tot_Cyprinidae = md("**Unknown cyprinids**"), tot_Centrarchidae = md("**Unknown centrarchids**"), Total = md("**Total**")) %>% 
  tab_header(md("**TABLE S4.** The abundance of fishes species in the 15 sampled lakes according to all the sampling methods.")) %>% 
  cols_move(columns = c("tot_ChrosomusSp.", "tot_Cyprinidae", "tot_Centrarchidae"), after = "tot_RhAt") %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage", 
              page.width = px(200)) %>%
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

Table.S4 %>% #Saving gt tab
  gtsave("Tab_SpAbund_Combined.png", paste0(to.figs), vwidth = 2000, vheight = 1000)
Table.S4 %>% 
  gtsave("Table_S4.png", paste0(to.rédaction, "./Support_information/"), vwidth = 2000, vheight = 1000)

# ---- Table S5 : Abundance in minnow traps -----

SpAbund.MT <- CombinedData %>% #Selecting capture data
  na.omit() %>% 
  filter(Sampling_method == "Minnow_trap") %>% 
  select(Lake, starts_with("tot"))

SpAbund.MT <- SpAbund.MT %>% #Summarizing by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum)) 

SpAbund.MT <-SpAbund.MT %>% 
  adorn_totals(c("row", "col")) #Total by species and total by lake

Table.S5 <- gt(SpAbund.MT) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), tot_AmRu = md("***Ambloplites rupestris***"), tot_FuDi = md("***Fundulus diaphanus***"), tot_MiDo = md("***Micropterus dolomieu***"), tot_LeGi = md("***Lepomis gibbosus***"), tot_PeFl = md("***Perca flavescens***"), tot_PiPr = md("***Pimephales promelas***"), tot_ChrosomusSp. = md("**Chrosomus spp.**"), tot_PiNo = md("***Pimephales notatus***"), tot_SeAt = md("***Semotilus atromaculatus***"), tot_LuCo = md("***Luxilus cornutus***"), tot_AmNe = md("***Ameiurus nebulosus***"), tot_CaCo = md("***Catostomus commersonii***"), tot_EsMa = md("***Esox masquinongy***"), tot_UmLi = md("***Umbra limi***"), tot_RhAt = md("***Rhinichthys atratulus***"), tot_Cyprinidae = md("**Unknown cyprinids**"), tot_Centrarchidae = md("**Unknown centrarchids**"), Total = md("**Total**")) %>% 
  tab_header(md("**TABLE S5.** Abundance of fish species in the 15 sampled lakes according to the minnow trap method.")) %>% 
  cols_move(columns = c("tot_ChrosomusSp.", "tot_Cyprinidae", "tot_Centrarchidae"), after = "tot_RhAt") %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage") %>% 
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

Table.S5 %>% #Saving gt tab
  gtsave("Tab_SpAbund_MT.png", paste0(to.figs), vwidth = 2000, vheight = 1000)
Table.S5 %>% 
  gtsave("Table_S5.png", paste0(to.rédaction, "./Support_information/"), vwidth = 2000, vheight = 1000)

# ---- Table S6 : Abundance in seine nets ----

SpAbund.S <- CombinedData %>% #Selecting capture data
  filter(Sampling_method == "Seine") %>% 
  select(Lake, starts_with("tot"))

SpAbund.S <- SpAbund.S %>% #Summarizing by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum)) 

SpAbund.S <-SpAbund.S %>% 
  adorn_totals(c("row", "col")) #Total by species and total by lake

Table.S6 <- gt(SpAbund.S) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), tot_AmRu = md("***Ambloplites rupestris***"), tot_FuDi = md("***Fundulus diaphanus***"), tot_MiDo = md("***Micropterus dolomieu***"), tot_LeGi = md("***Lepomis gibbosus***"), tot_PeFl = md("***Perca flavescens***"), tot_PiPr = md("***Pimephales promelas***"), tot_ChrosomusSp. = md("**Chrosomus spp.**"), tot_PiNo = md("***Pimephales notatus***"), tot_SeAt = md("***Semotilus atromaculatus***"), tot_LuCo = md("***Luxilus cornutus***"), tot_AmNe = md("***Ameiurus nebulosus***"), tot_CaCo = md("***Catostomus commersonii***"), tot_EsMa = md("***Esox masquinongy***"), tot_UmLi = md("***Umbra limi***"), tot_RhAt = md("***Rhinichthys atratulus***"), tot_Cyprinidae = md("**Unknown cyprinids**"), tot_Centrarchidae = md("**Unknown centrarchids**"), Total = md("**Total**")) %>% 
  tab_header(md("**TABLE S6.** Abundance of fish species in the 15 sampled lakes according to the seine net method.")) %>% 
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

Table.S6 %>% #Saving gt tab
  gtsave("Tab_SpAbund_S.png", paste0(to.figs), vwidth = 2000, vheight = 1000)
Table.S6 %>% 
  gtsave("Table_S6.png", paste0(to.rédaction, "./Support_information/"), vwidth = 2000, vheight = 1000)

# ---- Table S7 : Abundance in transects ----

SpAbund.T <- CombinedData %>% #Selecting capture data
  filter(Sampling_method == "Transect") %>% 
  select(Lake, starts_with("tot"))

SpAbund.T <- SpAbund.T %>% #Summarizing by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum)) 

SpAbund.T <-SpAbund.T %>% 
  adorn_totals(c("row", "col")) #Total by species and total by lake

Table.S7 <- gt(SpAbund.T) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), tot_AmRu = md("***Ambloplites rupestris***"), tot_FuDi = md("***Fundulus diaphanus***"), tot_MiDo = md("***Micropterus dolomieu***"), tot_LeGi = md("***Lepomis gibbosus***"), tot_PeFl = md("***Perca flavescens***"), tot_PiPr = md("***Pimephales promelas***"), tot_ChrosomusSp. = md("**Chrosomus spp.**"), tot_PiNo = md("***Pimephales notatus***"), tot_SeAt = md("***Semotilus atromaculatus***"), tot_LuCo = md("***Luxilus cornutus***"), tot_AmNe = md("***Ameiurus nebulosus***"), tot_CaCo = md("***Catostomus commersonii***"), tot_EsMa = md("***Esox masquinongy***"), tot_UmLi = md("***Umbra limi***"), tot_RhAt = md("***Rhinichthys atratulus***"), tot_Cyprinidae = md("**Unknown cyprinids**"), tot_Centrarchidae = md("**Unknown centrarchids**"), Total = md("**Total**")) %>% 
  tab_header(md("**TABLE S7.** Abundance of fish species in the 15 sampled lakes according to the transect method.")) %>% 
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

Table.S7 %>% #Saving gt tab
  gtsave("Tab_SpAbund_T.png", paste0(to.figs), vwidth = 2000, vheight = 1000)
Table.S7 %>% 
  gtsave("Table_S7.png", paste0(to.rédaction, "./Support_information/"), vwidth = 2000, vheight = 1000)

# ---- Table S8 : Overall mean length by lakes ----

FishLength <- FishingRaw %>% #Selecting data of interest
  select(Lake, Species_ID, Length, Abundance) %>% 
  na.omit() %>% 
  arrange(Lake, Species_ID)

FishLength <- expandRows(FishLength, "Abundance") #Reshaping data frame for 1 row = 1 individual format

FishLength$Species_ID <- FishLength$Species_ID %>% 
  str_replace_all(c("PeFl" = "Perca flavescens", 
                    "LeGi" = "Lepomis gibbosus",
                    "AmRu" = "Ambloplites rupestris",
                    "FuDi" = "Fundulus diaphanus",
                    "MiDo" = "Microperus dolomieui",
                    "PiNo" = "Pimephales notatus",
                    "PiPr" = "Pimephales promelas",
                    "SeAt" = "Semotilus atromaculatus",
                    "AmNe" = "Ameiurus nebulosus",
                    "CaCo" = "Catostomus commersonii",
                    "LuCo" = "Luxilus cornutus",
                    "EsMa" = "Esox maquinongy",
                    "RhAt" = "Rhinichthys atratulus",
                    "UmLi" = "Umbra limi",
                    "Chrosomus sp." = "Chrosomus spp.",
                    "Centrarchidae" = "Unknown centrarchids",
                    "Cyprinidae" = "Unknown cyprinids"))

FishLength$Lake <- FishLength$Lake %>% 
  str_replace_all("Pin_rouge", "Pin rouge")

Length.TotMean <- FishLength %>% #All data summary statistics
  select(Length) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

Length.LakeMean <- FishLength %>% #Summary statistic by lake
  group_by(Lake) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

Table.S8 <- gt(Length.LakeMean) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), md("**Mean**"), md("**sd**"), md("**N**")) %>% 
  tab_header(md("**TABLE S8.** Overall mean fish length for the 15 sampled lakes. The fishes were caught with minnow traps and seine nets.")) %>% 
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
  fmt_number(columns = c(2, 3), decimals = 2)

Table.S8 %>% #Saving gt tab
  gtsave("Tab_Length_Lake.png", paste0(to.figs))
Table.S8 %>% 
  gtsave("Table_S8.png", paste0(to.rédaction, "./Support_information/"))

# ---- Table S9 : Overall mean length by species ----

Length.SpeciesMean <- FishLength %>% #Summary statistic by species
  group_by(Species_ID) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

Table.S9 <- gt(Length.SpeciesMean) %>% #Creating gt tab and editing style
  cols_label(Species_ID = md("**Species**"), md("**Mean**"), md("**sd**"), md("**N**")) %>% 
  tab_header(md("**TABLE S9.** Overall mean fish length for each species. The fishes were caught with minnow traps and seine nets.")) %>% 
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
  tab_style(style = cell_text(style = "italic"),
            locations = cells_body(columns = 1)) %>% 
  fmt_number(columns = c(2, 3), decimals = 2)

Table.S9 %>% #Saving gt tab
  gtsave("Tab_Length_Species.png", paste0(to.figs))
Table.S9 %>% 
  gtsave("Table_S9.png", paste0(to.rédaction, "./Support_information/"))

# ---- Table S10 : Overall mean length by species and lakes ----

Length.SpeciesLakeMean <- FishLength %>% #Summarizing number of individuals, mean length and sd for each species within each lake
  group_by(Lake, Species_ID, .add = TRUE) %>% 
  summarise(Mean = mean(Length), sd = sd(Length), N = n())

Table.S10 <- gt(Length.SpeciesLakeMean) %>% 
  cols_label(Species_ID = md("**Species**"), md("**Mean**"), md("**sd**"), md("**N**")) %>% 
  tab_header(md("**TABLE S10.** Mean length for each species within each lake sampled. The fishes were caught with minnow traps and seine nets.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              row.striping.include_table_body = TRUE,
              row_group.as_column = TRUE,
              heading.border.bottom.color = "black") %>% 
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
            locations = cells_body(rows = c(6, 10, 13, 16 ,23, 28, 33, 41, 45, 48, 51, 55, 57, 58))) %>% 
  fmt_number(columns = c(3, 4), decimals = 2) %>% 
  tab_style(style = cell_text(style = "italic"),
            locations = cells_body(columns = 2))

Table.S10 %>% #Saving gt tab
  gtsave("Tab_Length_LakesSpecies.png", paste0(to.figs))
Table.S10 %>% 
  gtsave("Table_S10.png", paste0(to.rédaction, "./Support_information/"))

# ---- Table S11 : Species' black spot infection references ----

Table.S11 <- gt(References, groupname_col = "Species_name") %>% 
  cols_hide(columns = "Species_ID") %>% 
  cols_label(BlacksSpot_Sp = md("**Black spot trematode species**"), Mention = md("**References**")) %>% 
  tab_header(md("**TABLE S11.** References of the black spot disease in the fishes species sampled our study system. *Ameiurus nebulosus* and *Esox masquinongy* have no mention of black spot disease. This table is not an exhaustive review.")) %>% 
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
            locations = cells_body(rows = c(3, 5, 6, 7, 8, 9, 11, 14, 16, 19, 21, 22 ,23, 24))) %>% 
  tab_style(style = cell_text(style = "italic"),
            locations = cells_body(columns = 3, rows = c(1, 2, 4, 10, 12, 13, 15, 17, 19, 20, 22:25)))

Table.S11 %>% #Saving gt tab
  gtsave("Tab_InfectionRefs.png", paste0(to.figs))
Table.S11 %>% 
  gtsave("Table_S11.png", paste0(to.rédaction, "./Support_information/"))

# ---- Table S15 : Site characteristics ----

Site.desc <- TransectData %>% 
  select(Lake, Transect_ID, 
         Temperature, Conductivity, DO, Turbidity, pH, 
         TOC, TN, TP,
         Trunk,
         Silt, 
         Sand,
         Rock,
         Metric_block,
         Macrophyte,
         Mean_depth)

Site.desc$Lake <- Site.desc$Lake %>% 
  str_replace_all("Pin_rouge", "Pin rouge")

Table.X <- gt(Site.desc, groupname_col = "Lake") %>% 
  cols_label(Transect_ID = md("Transect ID"), 
             Temperature = md("Temperature (°C)"), Conductivity = md("Conductivity (μS/cm)"), DO = md("Dissolved oxygen (%)"), Turbidity = md("Turbidity (NTU)"), pH = md("pH"), 
             TOC = md("TOC (mg/L)"), TN = md("TN (mg/L)"), TP = md("TP (mg/L)"),
             Silt = md("Silt (%)"), Sand = md("Sand (%)"), Rock = md("Rock (%)"), Metric_block = md("Boulder (%)"),
             Macrophyte = md("Macrophyte (%)"), Mean_depth = md("Mean depth (cm)")) %>% 
  tab_header(md("**TABLE X.** Transect sites water and habitat characteristics. The results are grouped by lake.")) %>% 
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
 
Table.S15 %>% #Saving gt tab
  gtsave("Tab_PhysicoChem.png", paste0(to.figs))
Table.S15 %>% 
  gtsave("Table_S15.png", paste0(to.rédaction, "./Support_information/"))

# Table S16 : Physical habitat characteristics ----

Habitat <- TransectData %>% 
  select(Lake, Transect_ID, Trunk, Silt, Sand, Rock, Metric_block, Macrophyte, Mean_depth)

Habitat$Lake <- Habitat$Lake %>% 
  str_replace_all("Pin_rouge", "Pin rouge")

Table.S16 <- gt(Habitat, groupname_col = "Lake") %>% 
  cols_label(Lake = md("**Lake**"), Transect_ID = md("**Transect ID**"), Trunk = md("**Trunk**"), Silt = md("**Silt (%)**"), Sand = md("**Sand (%)**"), Rock = md("**Rock (%)**"), Metric_block = md("**Metric block (%)**"), Macrophyte = md("**Macrophyte cover (%)**"), Mean_depth = md("**Mean depth (cm)**")) %>% 
  tab_header(md("**TABLE S16.** Description of the physical habitat. The results presented for the 48 transects are means of every 10 m estimations, exept the trunks are the total by transect. Results are grouped by lake. Lakes Beaver, Tracy, Montaubois and St-Onge were not sampled for the prevalence estimation by the transect method.")) %>% 
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

Table.S16 %>% #Saving gt tab
  gtsave("Tab_Habitat.png", paste0(to.figs))
Table.S16 %>% 
  gtsave("Table_S16.png", paste0(to.rédaction, "./Support_information/"))
