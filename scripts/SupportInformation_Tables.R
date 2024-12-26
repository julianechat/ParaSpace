## Script name : Support information tables

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
library(tidyr)

## Loading data ----

LakesCharacteristics <- read.csv(paste0(to.data, "Lakes_Characteristics.csv"), sep = ";")
SamplingEffort <- read.csv(paste0(to.data, "SamplingEffort_Det.csv"), sep = ";")
FishingGear <- read.csv(paste0(to.data, "Gear_dimensions.csv"), sep = ";")
CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))
FishingRaw <- read.csv(paste0(to.data, "Fishing_RawData.csv"), sep = ";")
References <- read.csv(paste0(to.doc, "Appendix_S4_BSxHost.csv"), sep = ";")

# ---- Table S1 : Geographical and morphometric characteristics ----

Table.S1 <- gt(LakesCharacteristics) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), Watershed = md("**Watershed**"), Latitude = md("**Latitude**"), Longitude = md("**Longitude**"), Lake_area = md("**Area (km<sup>2</sup>)**"), Max_depth = md("**Maximum depth (m)**"), Mean_depth = md("**Mean depth (m)**"), WRT = md("**Residence time (year)**"), Drainage_area = md("**Drainage area (km<sup>2</sup>)**"), Elevation = md("**Elevation (m)**"), Perimeter = md("**Perimeter (m)**"), Connectivity = md("**Distance to nearest lake (m)**")) %>%
  tab_header(md("**TABLE S1.** Geographical and morphometric lake characteristics of the 15 lakes sampled.")) %>% 
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
  gtsave("Table_S1.pdf", paste0(to.rédaction, "./Support_information/"), expand = 10)

# ---- Table S2 : Sampling effort ----

Table.S2 <- gt(SamplingEffort) %>% #Creating gt tab and editing style
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

Table.S3 <- gt(FishingGear) %>% #Creating gt tab and editing style
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

# ---- Table S4 : Abundance data by sampling methods ----

#Combined methods
SpAbund.C <- CombinedData %>% #Selecting capture data
  select(Lake, starts_with("tot")) 

SpAbund.C <- SpAbund.C %>% #Summarizing abundance data by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum,  na.rm = TRUE)) 

SpAbund.C <- SpAbund.C %>% 
  mutate(Method = "Combined", .after = Lake)

SpAbund.C <- SpAbund.C %>% 
  adorn_totals(where = "col")

#Minnow trap
SpAbund.MT <- CombinedData %>% #Selecting minnow trap abundance data
  filter(Sampling_method == "Minnow_trap") %>% 
  select(Lake, starts_with("tot")) %>% 
  na.omit()

SpAbund.MT <- SpAbund.MT %>% #Summarizing by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum)) 

SpAbund.MT <- SpAbund.MT %>% 
  mutate(Method = "Minnow_trap", .after = Lake)

SpAbund.MT <- SpAbund.MT %>% 
  adorn_totals(where = "col")

#Seine net
SpAbund.S <- CombinedData %>% #Selecting seine abundance data
  filter(Sampling_method == "Seine") %>% 
  select(Lake, starts_with("tot"))

SpAbund.S <- SpAbund.S %>% #Summarizing by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum)) 

SpAbund.S <- SpAbund.S %>% 
  mutate(Method = "Seine_net", .after = Lake)

SpAbund.S <-SpAbund.S %>% 
 adorn_totals("col") 

#Transect
SpAbund.T <- CombinedData %>% #Selecting transect abundance data
  filter(Sampling_method == "Transect") %>% 
  select(Lake, starts_with("tot"))

SpAbund.T <- SpAbund.T %>% #Summarizing by lake
  group_by(Lake) %>% 
  summarise(across(.cols = everything(), sum)) 

SpAbund.T <- SpAbund.T %>% 
  mutate(Method = "Transect", .after = Lake)

SpAbund.T <- SpAbund.T %>% 
  adorn_totals(where = "col")

#Binding data set
SpAbund.df <- rbind(SpAbund.C, SpAbund.MT, SpAbund.S, SpAbund.T) 

#Histograms
SpAbund.df2 <- SpAbund.df %>% 
  filter(!(Method == "Combined"))

Lake.Abund.plot <- ggplot(data = SpAbund.df2) +
  geom_col(aes(x = Method, y = Total, fill = Method), color = "black") + 
  facet_wrap(vars(Lake), nrow = 3, ncol = 5) +
  scale_fill_manual(values = c("Minnow_trap" = "#2A5676",
                               "Seine_net" = "#999600",
                               "Transect" = "#966F1E")) +
  theme(text = element_text(size = 20, family = "Calibri Light", color = "black"),
        panel.background = element_blank(),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.line.x = element_line(color = "black", lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))
Lake.Abund.plot

SpAbund.stack <- SpAbund.df2 %>% 
  pivot_longer(cols = c(3:19), names_to = "Species", values_to = "Abundance")
  
SpAbund.stack2 <- ggplot(data = SpAbund.stack) +
  geom_col(aes(x = Method, y = Abundance, fill = Species), color = "black") + 
  facet_wrap(vars(Lake), nrow = 3, ncol = 5) +
  theme(text = element_text(size = 20, family = "Calibri Light", color = "black"),
        panel.background = element_blank(),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black", angle = 90, margin = unit(c(5, 0, 0, 0), "mm")),
        axis.text.y = element_text(color = "black"),
        axis.line.x = element_line(color = "black", lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))
SpAbund.stack2

SpAbund.df <- SpAbund.df %>% 
  pivot_wider(names_from = Method, values_from = c(3:20))

SpAbund.df <- SpAbund.df %>% 
  adorn_totals("row")

#Table
S2.S1 <- gt(SpAbund.df) %>% #Creating gt tab and editing style
  cols_label(Lake = md("**Lake**"), 
             tot_AmRu_Combined = md("C"), tot_FuDi_Combined = md("C"), tot_MiDo_Combined = md("C"), tot_LeGi_Combined = md("C"), tot_PeFl_Combined = md("C"), tot_PiPr_Combined = md("C"), tot_ChrosomusSpp._Combined = md("C"), tot_PiNo_Combined = md("C"), tot_SeAt_Combined = md("C"), tot_LuCo_Combined = md("C"), tot_AmNe_Combined = md("C"), tot_CaCo_Combined = md("C"), tot_EsMa_Combined = md("C"), tot_UmLi_Combined = md("C"), tot_RhAt_Combined = md("C"), tot_Cyprinidae_Combined = md("C"), tot_Centrarchidae_Combined = md("C"), Total_Combined = md("C"),
             tot_AmRu_Minnow_trap = md("MT"), tot_FuDi_Minnow_trap = md("MT"), tot_MiDo_Minnow_trap = md("MT"), tot_LeGi_Minnow_trap = md("MT"), tot_PeFl_Minnow_trap = md("MT"), tot_PiPr_Minnow_trap = md("MT"), tot_ChrosomusSpp._Minnow_trap = md("MT"), tot_PiNo_Minnow_trap = md("MT"), tot_SeAt_Minnow_trap = md("MT"), tot_LuCo_Minnow_trap = md("MT"), tot_AmNe_Minnow_trap = md("MT"), tot_CaCo_Minnow_trap = md("MT"), tot_EsMa_Minnow_trap = md("MT"), tot_UmLi_Minnow_trap = md("MT"), tot_RhAt_Minnow_trap = md("MT"), tot_Cyprinidae_Minnow_trap = md("MT"), tot_Centrarchidae_Minnow_trap = md("MT"), Total_Minnow_trap = md("MT"),
             tot_AmRu_Seine_net = md("S"), tot_FuDi_Seine_net = md("S"), tot_MiDo_Seine_net = md("S"), tot_LeGi_Seine_net = md("S"), tot_PeFl_Seine_net = md("S"), tot_PiPr_Seine_net = md("S"), tot_ChrosomusSpp._Seine_net = md("S"), tot_PiNo_Seine_net = md("S"), tot_SeAt_Seine_net = md("S"), tot_LuCo_Seine_net = md("S"), tot_AmNe_Seine_net = md("S"), tot_CaCo_Seine_net = md("S"), tot_EsMa_Seine_net = md("S"), tot_UmLi_Seine_net = md("S"), tot_RhAt_Seine_net = md("S"), tot_Cyprinidae_Seine_net = md("S"), tot_Centrarchidae_Seine_net = md("S"), Total_Seine_net = md("S"),
             tot_AmRu_Transect = md("T"), tot_FuDi_Transect = md("T"), tot_MiDo_Transect = md("T"), tot_LeGi_Transect = md("T"), tot_PeFl_Transect = md("T"), tot_PiPr_Transect = md("T"), tot_ChrosomusSpp._Transect = md("T"), tot_PiNo_Transect = md("T"), tot_SeAt_Transect = md("T"), tot_LuCo_Transect = md("T"), tot_AmNe_Transect = md("T"), tot_CaCo_Transect = md("T"), tot_EsMa_Transect = md("T"), tot_UmLi_Transect = md("T"), tot_RhAt_Transect = md("T"), tot_Cyprinidae_Transect = md("T"), tot_Centrarchidae_Transect = md("T"), Total_Transect = md("T")) %>% 
  tab_spanner(label = md("*Ambloplites rupestris*"), columns = c(2:5)) %>%
  tab_spanner(label = md("*Fundulus diaphanus*"), columns = c(6:9)) %>%
  tab_spanner(label = md("*Micropterus dolomieu*"), columns = c(10:13)) %>%
  tab_spanner(label = md("Unknown centrarchids"), columns = c(14:17)) %>%
  tab_spanner(label = md("*Lepomis gibbosus*"), columns = c(18:21)) %>%
  tab_spanner(label = md("*Perca flavescens*"), columns = c(22:25)) %>%
  tab_spanner(label = md("*Pimephales promelas*"), columns = c(26:29)) %>%
  tab_spanner(label = md("*Chrosomus* spp."), columns = c(30:33)) %>%
  tab_spanner(label = md("*Pimephales notatus*"), columns = c(34:37)) %>%
  tab_spanner(label = md("Unknown cyprinids"), columns = c(38:41)) %>%
  tab_spanner(label = md("*Semotilus atromaculatus*"), columns = c(42:45)) %>%
  tab_spanner(label = md("*Luxilus cornutus*"), columns = c(46:49)) %>%
  tab_spanner(label = md("*Ameiurus nebulosus*"), columns = c(50:53)) %>%
  tab_spanner(label = md("*Catostomus commersonii*"), columns = c(54:57)) %>%
  tab_spanner(label = md("*Esox masquinongy*"), columns = c(58:61)) %>%
  tab_spanner(label = md("*Umbra limi*"), columns = c(62:65)) %>%
  tab_spanner(label = md("*Rhinichthys atratulus*"), columns = c(66:69)) %>%
  tab_spanner(label = md("Total"), columns = c(70:73), ) %>%
  tab_header(md("**TABLE S1.** Abundance of fish species in the 15 sampled lakes across the different sampling methods. **C** stands for combined methods, **MT** for minnow trap, **S** for seine net and **T** for transect. NAs mean that the lake was not sampled with the according method.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage", 
              table.width = px(3000)) %>%
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "top"),
            locations = cells_column_spanners()) %>% 
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
            locations =  cells_body(column = 70)) %>% 
  sub_values(columns = 1, rows = 12, values = "Pin_rouge", replacement = "Pin rouge") %>% 
  cols_width(Lake ~ px(100),
             everything() ~ px(50))

S2.S1

S2.S1 %>% #Saving gt tab
  gtsave("Tab_SpAbund_Methods.html", paste0(to.figs))
Table.S4 %>% 
  gtsave("Table_S4.png", paste0(to.rédaction, "./Support_information/"), vwidth = 2000, vheight = 1000)

# ---- Table S7 : Species' black spot infection references ----

S2.S4 <- gt(References, groupname_col = "Species_name") %>% #Creating gt tab and editing style
  cols_hide(columns = "Species_ID") %>% 
  cols_label(BlacksSpot_Sp = md("**Black spot trematode species**"), Mention = md("**References**")) %>% 
  tab_header(md("**TABLE S4.** References of the black spot disease occurrence in the fish species sampled our study system. *Ameiurus nebulosus* and *Esox masquinongy* have no mention of black spot disease. This table is not an exhaustive review.")) %>% 
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

S2.S4

S2.S4 %>% #Saving gt tab
  gtsave("Tab_InfectionRefs.png", paste0(to.figs))
S2.S4 %>% 
  gtsave("AppendixS2_TableS4.png", paste0(to.rédaction, "./Support_information/"))

# ---- Table S14 : Site characteristics ----

Site.desc <- CombinedData %>% #Select habitat and water parameters data
  filter(Sampling_method == "Transect") %>% 
  select(Lake, Sampling_ID, 
         Temperature, Conductivity, DO, Turbidity, pH, 
         TOC, TN, TP,
         Trunk, Silt, Sand, Rock, Boulder, Macrophyte,
         Site_depth)

Site.desc$Lake <- Site.desc$Lake %>% #Change lake pin rouge name
  str_replace_all("Pin_rouge", "Pin rouge")

S2.S11 <- gt(Site.desc, groupname_col = "Lake") %>% #Creating gt tab and editing style
  cols_label(Sampling_ID = md("Sampling_ID"), 
             Temperature = md("Temperature (°C)"), Conductivity = md("Conductivity (μS/cm)"), DO = md("Dissolved oxygen (%)"), Turbidity = md("Turbidity (NTU)"), pH = md("pH"), 
             TOC = md("TOC (mg/L)"), TN = md("TN (mg/L)"), TP = md("TP (mg/L)"),
             Silt = md("Silt (%)"), Sand = md("Sand (%)"), Rock = md("Rock (%)"), Boulder = md("Boulder (%)"),
             Macrophyte = md("Macrophyte (%)"), Site_depth = md("Mean depth (cm)")) %>% 
  tab_header(md("**TABLE S11.** Transect sites water and habitat characteristics. The results are grouped by lake.")) %>% 
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
 
S2.S11

S2.S11 %>% #Saving gt tab
  gtsave("Tab_SiteDescription.png", paste0(to.figs))
Table.S14 %>% 
  gtsave("Table_S14.png", paste0(to.rédaction, "./Support_information/"))
