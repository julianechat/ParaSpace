## Script name : Prevalence maps

## Authors : Juliane Vigneault & Éric Harvey
## Date created : January 31, 2023

## Copyright (c) Juliane Vigneault, 2023

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

library(sf)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(colorspace)
library(patchwork)

## Loading data ----

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))

CROM <- st_read(paste0(to.carto, "Lake_shapes/Cromwell.shp"))
CROC <- st_read(paste0(to.carto, "Lake_shapes/Croche.shp"))
CORR <- st_read(paste0(to.carto, "Lake_shapes/Corriveau.shp"))
ECHO <- st_read(paste0(to.carto, "Lake_shapes/Echo.shp"))
ACHI <- st_read(paste0(to.carto, "Lake_shapes/Achigan.shp"))
FOUR <- st_read(paste0(to.carto, "Lake_shapes/Fournelle.shp"))
MORE <- st_read(paste0(to.carto, "Lake_shapes/Morency.shp"))
CORN <- st_read(paste0(to.carto, "Lake_shapes/Cornu.shp"))
BEAV <- st_read(paste0(to.carto, "Lake_shapes/Beaver.shp"))
MONT <- st_read(paste0(to.carto, "Lake_shapes/Montaubois.shp"))
TRAC <- st_read(paste0(to.carto, "Lake_shapes/Tracy.shp"))
COEU <- st_read(paste0(to.carto, "Lake_shapes/Coeur.shp"))
PINR <- st_read(paste0(to.carto, "Lake_shapes/Pin_rouge.shp"))
STON <- st_read(paste0(to.carto, "Lake_shapes/St-Onge.shp"))
TRIT <- st_read(paste0(to.carto, "Lake_shapes/Triton.shp"))

creeks <- st_read(paste0(to.carto, "Attribute_templates/Template_creeks.shp"))
lakes <- st_read(paste0(to.carto, "Attribute_templates/Template_lacs.shp"))
watersheds <- st_read(paste0(to.carto, "Attribute_templates/Watershed_template.shp"))
roads <- st_read(paste0(to.carto, "Attribute_templates/Template_roads.shp"))
building <- read_sf(paste0(to.carto, "Attribute_templates/Template_batiments_points.shp"))

# ---- Lake bubble map ----

#Pumpkinseed sunfish prevalence at site-scale
#Bubbles are colored according to the method and sized according to the prevalence value

## Attribute table ----

attributes <- CombinedData %>% 
  select(Sampling_ID, Lake, Sampling_method, tot_LeGi, inf_LeGi, Site_latitude, Site_longitude) %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi, .keep = "unused") #Prevalence column

col.pal <- c("olivedrab", "chocolate", "darkslategrey")

## Cromwell ----

CROM.att <- attributes %>% 
  filter(Lake == "Cromwell") #Select lake

CROM.plot <- ggplot() + #Bubble map
  geom_sf(data = CROM, fill = "lightblue") +
  geom_point(data = CROM.att, 
             aes(x = Site_longitude, y = Site_latitude, size = prev_LeGi, color = Sampling_method)) +
  scale_color_manual(values = col.pal) + 
  geom_text(data = CROM.att, aes(x = Site_longitude, y = Site_latitude, label = Sampling_ID), size = 2) +
  theme_void()
CROM.plot

## Croche ----

CROC.att <- attributes %>% 
  filter(Lake == "Croche")  #Select lake
Croc.simple <- ggplot()+
  geom_sf(data = CROC, fill = "#7F7F7F") +
  theme_void()
Croc.simple

CROC.plot <- ggplot() + #Bubble map
  geom_sf(data = CROC, fill = "lightblue") +
  geom_point(data = CROC.att, aes(x = Site_longitude, y = Site_latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = CROC.att, aes(x = Site_longitude, y = Site_latitude, label = Sampling_ID), size = 2) +
  theme_void()
CROC.plot

## Corriveau ---

CORR.att <- attributes %>% 
  filter(Lake == "Corriveau") #Select lake

CORR.plot <- ggplot() + #Bubble map
  geom_sf(data = CORR, fill = "lightblue") +
  geom_point(data = CORR.att, aes(x = Site_longitude, y = Site_latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = CORR.att, aes(x = Site_longitude, y = Site_latitude, label = Sampling_ID), size = 2) +
  theme_void()
CORR.plot

## Echo ----

ECHO.att <- attributes %>% 
  filter(Lake == "Echo") #Select lake

ECHO.plot <- ggplot() + #Bubble map
  geom_sf(data = ECHO, fill = "lightblue") +
  geom_point(data = ECHO.att, aes(x = Site_longitude, y = Site_latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = ECHO.att, aes(x = Site_longitude, y = Site_latitude, label = Sampling_ID), size = 2) +
  theme_void()
ECHO.plot

## Achigan ----

ACHI.att <- attributes %>% 
  filter(Lake == "Achigan") #Select lake

ACHI.plot <- ggplot() + #Bubble map
  geom_sf(data = ACHI, fill = "lightblue") +
  geom_point(data = ACHI.att, aes(x = Site_longitude, y = Site_latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = ACHI.att, aes(x = Site_longitude, y = Site_latitude, label = Sampling_ID), size = 2) +
  theme_void()
ACHI.plot

## Fournelle ----

FOUR.att <- attributes %>% 
  filter(Lake == "Fournelle") #Select lake

FOUR.plot <- ggplot() + #Bubble map
  geom_sf(data = FOUR, fill = "lightblue") +
  geom_point(data = FOUR.att, aes(x = Site_longitude, y = Site_latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = FOUR.att, aes(x = Site_longitude, y = Site_latitude, label = Sampling_ID), size = 2) +
  theme_void()
FOUR.plot

## Morency ---

MORE.att <- attributes %>% 
  filter(Lake == "Morency") #Select lake

MORE.plot <- ggplot() + #Bubble map
  geom_sf(data = MORE, fill = "lightblue") +
  geom_point(data = MORE.att, aes(x = Site_longitude, y = Site_latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = MORE.att, aes(x = Site_longitude, y = Site_latitude, label = Sampling_ID), size = 2) +
  theme_void()
MORE.plot 

## Cornu ----

CORN.att <- attributes %>% 
  filter(Lake == "Cornu") #Select lake

CORN.plot <- ggplot() + #Bubble map
  geom_sf(data = CORN, fill = "lightblue") +
  geom_point(data = CORN.att, aes(x = Site_longitude, y = Site_latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = CORN.att, aes(x = Site_longitude, y = Site_latitude, label = Sampling_ID), size = 2) +
  theme_void()
CORN.plot

## Beaver ----

BEAV.att <- attributes %>% 
  filter(Lake == "Beaver") #Select lake

BEAV.plot <- ggplot() + #Bubble map
  geom_sf(data = BEAV, fill = "lightblue") +
  geom_point(data = BEAV.att, aes(x = Site_longitude, y = Site_latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = BEAV.att, aes(x = Site_longitude, y = Site_latitude, label = Sampling_ID), size = 2) +
  theme_void()
BEAV.plot #Some shifted coordinates

## Montaubois ----

MONT.att <- attributes %>% 
  filter(Lake == "Montaubois") #Select lake

MONT.plot <- ggplot() + #Bubble map
  geom_sf(data = MONT, fill = "lightblue") +
  geom_point(data = MONT.att, aes(x = Site_longitude, y = Site_latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = MONT.att, aes(x = Site_longitude, y = Site_latitude, label = Sampling_ID), size = 2) +
  theme_void()
MONT.plot

## Tracy ----

TRAC.att <- attributes %>% 
  filter(Lake == "Tracy") #Select lake

TRAC.plot <- ggplot() + #Bubble map
  geom_sf(data = TRAC, fill = "lightblue") +
  geom_point(data = TRAC.att, aes(x = Site_longitude, y = Site_latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = TRAC.att, aes(x = Site_longitude, y = Site_latitude, label = Sampling_ID), size = 2) +
  theme_void()
TRAC.plot

## Coeur ----

COEU.att <- attributes %>% 
  filter(Lake == "Coeur") #Select lake

COEU.plot <- ggplot() + #Bubble map
  geom_sf(data = COEU, fill = "lightblue") +
  geom_point(data = COEU.att, aes(x = Site_longitude, y = Site_latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = COEU.att, aes(x = Site_longitude, y = Site_latitude, label = Sampling_ID), size = 2) +
  theme_void()
COEU.plot

## Pin rouge ----

PINR.att <- attributes %>% 
  filter(Lake == "Pin_rouge") #Select lake

PINR.plot <- ggplot() + #Bubble map
  geom_sf(data = PINR, fill = "lightblue") +
  geom_point(data = PINR.att, aes(x = Site_longitude, y = Site_latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = PINR.att, aes(x = Site_longitude, y = Site_latitude, label = Sampling_ID), size = 2) +
  theme_void()
PINR.plot

## St-Onge ----

STON.att <- attributes %>% 
  filter(Lake == "St-Onge") #Select lake

STON.plot <- ggplot() + #Bubble map
  geom_sf(data = STON, fill = "lightblue") +
  geom_point(data = STON.att, aes(x = Site_longitude, y = Site_latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = STON.att, aes(x = Site_longitude, y = Site_latitude, label = Sampling_ID), size = 2) +
  theme_void()
STON.plot #Shifted coordinates

# ---- Study area cartography ----

#Prevalence maps at community level

## Map frame ----

Study.map <- rbind(ACHI, BEAV, COEU, CORN, CORR, CROC, CROM, ECHO, FOUR, MONT, MORE, PINR, STON, TRAC, TRIT) #Study map attribute table

cropped.frame <- c(xmin = -74.08, #Frame for map
                   ymin = 45.87,
                   xmax = -73.94,
                   ymax = 46.00)

cropped.frame2 <- c(xmin = -433485, #Frame for watershed
                    ymin = 223316,
                    xmax = -421000,
                    ymax =  239250)

cropped.creeks <- st_crop(creeks, cropped.frame) #Crop creeks attributes to determined frame
cropped.lakes <- st_crop(lakes, cropped.frame) #Crop lakes attributes to determined frame
cropped.watersheds <- st_crop(watersheds, cropped.frame2) #Crop watershed attributes to determined frame
cropped.roads <- st_crop(roads, cropped.frame) #Crop roads attributes to determined frame
cropped.building <- st_crop(building, cropped.frame) #Crop building attributes to determined frame

## Prevalence data ----

lake.attributes <- CombinedData %>% #Selecting abundance data
  select(Lake, Sampling_method, Site_latitude, Site_longitude, starts_with(c("tot", "inf")))

lake.attributes <- lake.attributes %>% #Muskellunge and brown bullhead individuals are excluded from the prevalence calculus since they are not host of the black spot disease
  select(!(c(tot_EsMa, inf_EsMa, tot_AmNe, inf_AmNe)))

### Combined methods ----

lake.attributes.C <- lake.attributes %>% #Lake abundance sums
  select(!(Sampling_method)) %>% 
  group_by(Lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

lake.attributes.C <- lake.attributes.C %>% #Creating fish abundance columns
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSpp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_CaCo + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae) %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSpp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_CaCo + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae) 

lake.attributes.C <- lake.attributes.C %>% #Creating prevalence columns
  mutate(prev_fish = inf_fish/tot_fish) %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi)

lake.attributes.C <- Study.map %>% 
  mutate(prev_LeGi = lake.attributes.C$prev_LeGi) %>% 
  mutate(prev_fish = lake.attributes.C$prev_fish)

lake.attributes.C[14,21] <- NA #Changing Tracy value into NA to avoid coloring in maps

### Minnow trap ----

lake.attributes.MT <- lake.attributes %>% #Lake abundance sums
  filter(Sampling_method == "Minnow_trap") %>% 
  select(!(Sampling_method)) %>% 
  group_by(Lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

lake.attributes.MT <- lake.attributes.MT %>% #Creating fish abundance columns
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSpp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_CaCo + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae) %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSpp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_CaCo + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae) 

lake.attributes.MT <- lake.attributes.MT %>% #Creating prevalence columns
  mutate(prev_fish = inf_fish/tot_fish) %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi)

lake.attributes.MT <- Study.map %>% 
  mutate(prev_LeGi = lake.attributes.MT$prev_LeGi) %>% 
  mutate(prev_fish = lake.attributes.MT$prev_fish)

lake.attributes.MT[14,21] <- NA #Changing Tracy value into NA to avoid coloring in maps

### Seine net ----

lake.attributes.S <- lake.attributes %>% #Lake abundance sums
  filter(Sampling_method == "Seine") %>% 
  select(!(Sampling_method)) %>% 
  group_by(Lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

lake.attributes.S <- lake.attributes.S %>% #Creating fish abundance columns
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSpp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_CaCo + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae) %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSpp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_CaCo + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae) 

lake.attributes.S <- lake.attributes.S %>% #Creating prevalence columns
  mutate(prev_fish = inf_fish/tot_fish) %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi)

lake.attributes.S <- Study.map %>% 
  mutate(prev_LeGi = lake.attributes.S$prev_LeGi) %>% 
  mutate(prev_fish = lake.attributes.S$prev_fish)

### Transect ----

lake.attributes.T <- lake.attributes %>% #Lake abundance sums
  filter(Sampling_method == "Transect") %>% 
  select(!(Sampling_method)) %>% 
  group_by(Lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

lake.attributes.T <- lake.attributes.T %>% #Creating fish abundance columns
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSpp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_CaCo + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae) %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSpp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_CaCo + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae) 

lake.attributes.T <- lake.attributes.T %>% #Creating prevalence columns
  mutate(prev_fish = inf_fish/tot_fish) %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi)

lake.attributes.T <- Study.map %>% 
  mutate(prev_LeGi = lake.attributes.T$prev_LeGi) %>% 
  mutate(prev_fish = lake.attributes.T$prev_fish)

## Maps ----

LakesMap <- ggplot() + 
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  #geom_sf(data = cropped.watersheds, fill = NA, linewidth = 0.5, color = "#467092") +
  geom_sf(data = cropped.building, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.roads, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = lake.attributes.C, fill = "#996633", alpha = 0.8, color = "black", size = 0.5) +
  theme(legend.position = c(0.88, 0.15),
        legend.text = element_text(color = "black", size = 9, family = "Calibri Light"),
        legend.title = element_text(color = "black", size = 9, family = "Calibri Bold"),
        legend.key.size = unit(0.5, "cm"),
        legend.key = element_rect(linewidth = 2),
        legend.background = element_blank(), 
        panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_rect(fill = NA),
        panel.grid = element_line(NA), 
        axis.ticks = element_blank(),
        text = element_blank()) +
  annotation_scale(location = "tl", 
                   bar_cols = c("grey60", "white"),
                   height = unit(0.25, "cm"),
                   text_cex = 0.75,
                   text_family = "Calibri Light") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true", 
                         pad_x = unit(0.55, "cm"), 
                         pad_y = unit(0.65, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "black", text_size = 10, text_family = "Calibri Light"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"))
LakesMap

ggsave(paste0(to.figs, "LakesMaps+BV.png"), plot = LakesMap, dpi = 300, width = 10, height = 15, units = "cm")
#ggsave(paste0(to.figs, "LakesMaps.png"), plot = LakesMap, dpi = 300, width = 10, height = 15, units = "cm") #change pad_x at 0.55 instead of 0.85 & omit watersheds polygon line

### Combined methods ----

FishMap.C <- ggplot() + 
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = cropped.building, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.roads, color = "darkgrey", alpha = 0.5, size = 0.5) +
  #geom_sf(data = cropped.watersheds, fill = NA, linewidth = 0.5, color = "#467092") +
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = lake.attributes.C, aes(fill = (prev_fish*100)), color = "black", size = 0.5) +
  scale_fill_continuous_sequential(palette = "YlOrBr", limits = c(0, 100), aesthetics = "fill") +
  theme(legend.position = c(0.88, 0.15),
        legend.text = element_text(color = "black", size = 9, family = "Helvetica"),
        legend.title = element_text(color = "black", size = 9, family = "Helvetica"),
        legend.key.size = unit(0.5, "cm"),
        legend.key = element_rect(linewidth = 2),
        legend.background = element_blank(), 
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_line(NA), 
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  annotation_scale(location = "tl", 
                   bar_cols = c("grey60", "white"),
                   height = unit(0.25, "cm"),
                   text_cex = 0.75,
                   text_family = "Helvetica") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true", 
                         pad_x = unit(0.55, "cm"), 
                         pad_y = unit(0.65, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "black", text_size = 10, text_family = "Calibri Light"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) +
  guides(fill = guide_colorbar(title = "Prevalence (%)",
                               label.position = "right",
                               title.position = "left", title.theme = element_text(angle = 90, size = 10, hjust = 0.5),
                               frame.colour = "black",
                               frame.linewidth = 0.3,
                               ticks.colour = "black"))
FishMap.C

ggsave(paste0(to.figs, "PrevalenceMap_Fish_Combined.png"), plot = FishMap.C, dpi = 300, width = 10, height = 15, units = "cm")
#ggsave(paste0(to.figs, "PrevalenceMap_Fish_Combined+BV.png"), plot = FishMap.C, dpi = 300, width = 10, height = 15, units = "cm")

### Minnow traps ----

FishMap.MT <- ggplot() + 
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = cropped.building, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.roads, color = "darkgrey", alpha = 0.5, size = 0.5) +
  #geom_sf(data = cropped.watersheds, fill = NA, linewidth = 0.5, color = "#467092") +
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = lake.attributes.MT, aes(fill = (prev_fish*100)), color = "black", size = 0.5) +
  scale_fill_continuous_sequential(palette = "YlOrBr", limits = c(0, 100), aesthetics = "fill") +
  theme(legend.position = c(0.88, 0.15),
        legend.text = element_text(color = "black", size = 9, family = "Calibri Light"),
        legend.title = element_text(color = "black", size = 9, family = "Calibri Light"),
        legend.key.size = unit(0.5, "cm"),
        legend.key = element_rect(linewidth = 2),
        legend.background = element_blank(), 
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_line(NA), 
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  annotation_scale(location = "tl", 
                   bar_cols = c("grey60", "white"),
                   height = unit(0.25, "cm"),
                   text_cex = 0.75,
                   text_family = "Calibri Light") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true", 
                         pad_x = unit(0.55, "cm"), 
                         pad_y = unit(0.65, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "black", text_size = 10, text_family = "Calibri Light"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) +
  guides(fill = guide_colorbar(title = "Prevalence (%)",
                               label.position = "right",
                               title.position = "left", title.theme = element_text(angle = 90, size = 10, hjust = 0.5),
                               frame.colour = "black",
                               frame.linewidth = 0.3,
                               ticks.colour = "black"))
FishMap.MT

ggsave(paste0(to.figs, "PrevalenceMap_Fish_MinnowTrap.png"), plot = FishMap.MT, dpi = 300, width = 10, height = 15, units = "cm")
#ggsave(paste0(to.figs, "PrevalenceMap_Fish_MinnowTrap+BV.png"), plot = FishMap.MT, dpi = 300, width = 10, height = 15, units = "cm")

### Seine net ----

FishMap.S <- ggplot() + 
  geom_sf(data = cropped.building, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.roads, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes.S, aes(fill = (prev_fish*100)), color = "black", size = 0.5) +
  #geom_sf(data = cropped.watersheds, fill = NA, linewidth = 0.5, color = "#467092") +
  scale_fill_continuous_sequential(palette = "YlOrBr", limits = c(0, 100), aesthetics = "fill") +
  theme(legend.position = c(0.88, 0.15),
        legend.text = element_text(color = "black", size = 9, family = "Calibri Light"),
        legend.title = element_text(color = "black", size = 9, family = "Calibri Light"),
        legend.key.size = unit(0.5, "cm"),
        legend.key = element_rect(linewidth = 2),
        legend.background = element_blank(), 
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_line(NA), 
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  annotation_scale(location = "tl", 
                   bar_cols = c("grey60", "white"),
                   height = unit(0.25, "cm"),
                   text_cex = 0.75,
                   text_family = "Calibri Light") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true", 
                         pad_x = unit(0.55, "cm"), 
                         pad_y = unit(0.65, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "black", text_size = 10, text_family = "Calibri Light"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) +
  guides(fill = guide_colorbar(title = "Prevalence (%)",
                               label.position = "right",
                               title.position = "left", title.theme = element_text(angle = 90, size = 10, hjust = 0.5),
                               frame.colour = "black",
                               frame.linewidth = 0.3,
                               ticks.colour = "black"))
FishMap.S

ggsave(paste0(to.figs, "PrevalenceMap_Fish_Seine.png"), plot = FishMap.S, dpi = 300, width = 10, height = 15, units = "cm")
#ggsave(paste0(to.figs, "PrevalenceMap_Fish_Seine+BV.png"), plot = FishMap.S, dpi = 300, width = 10, height = 15, units = "cm")

### Transect ----

FishMap.T <- ggplot() + 
  geom_sf(data = cropped.building, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.roads, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes.T, aes(fill = (prev_fish*100)), color = "black", size = 0.5) +
  #geom_sf(data = cropped.watersheds, fill = NA, linewidth = 0.5, color = "#467092") +
  scale_fill_continuous_sequential(palette = "YlOrBr", limits = c(0, 100), aesthetics = "fill") +
  scale_continuous_identity(aesthetics = c(0, 1)) +
  theme(legend.position = c(0.88, 0.15),
        legend.text = element_text(color = "black", size = 9, family = "Calibri Light"),
        legend.title = element_text(color = "black", size = 9, family = "Calibri Light"),
        legend.key.size = unit(0.5, "cm"),
        legend.key = element_rect(linewidth = 2),
        legend.background = element_blank(), 
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_line(NA), 
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  annotation_scale(location = "tl", 
                   bar_cols = c("grey60", "white"),
                   height = unit(0.25, "cm"),
                   text_cex = 0.75,
                   text_family = "Calibri Light") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true", 
                         pad_x = unit(0.55, "cm"), 
                         pad_y = unit(0.65, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "black", text_size = 10, text_family = "Calibri Light"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) +
  guides(fill = guide_colorbar(title = "Prevalence (%)",
                               label.position = "right",
                               title.position = "left", title.theme = element_text(angle = 90, size = 10, hjust = 0.5),
                               frame.colour = "black",
                               frame.linewidth = 0.3,
                               ticks.colour = "black"))
FishMap.T

ggsave(paste0(to.figs, "PrevalenceMap_Fish_Transect.png"), plot = FishMap.T, dpi = 300, width = 10, height = 15, units = "cm")
#ggsave(paste0(to.figs, "PrevalenceMap_Fish_Transect+BV.png"), plot = FishMap.T, dpi = 300, width = 10, height = 15, units = "cm")


### Summary figure ----

#With frequency distributions

#A) Combined methods

#Frequency distribution
C.hist <- ggplot(lake.attributes.C, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#7E7E7E", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency") +
  ylim(0,4) +
  scale_x_continuous(breaks = c(0.0, 0.4, 0.8), labels = c("0", "0.4", "0.8")) +
  theme(text = element_text(size = 10, family = "Helvetica", color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))

C.Grob <- ggplotGrob(C.hist) #Set frequency distribution as a grob

C.Map.Sum <- ggplot() + 
  geom_sf(data = cropped.building, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.roads, color = "darkgrey", alpha = 0.5, size = 0.5) +  
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes.C, aes(fill = (prev_fish*100)), color = "black", size = 0.5) +
  scale_fill_continuous_sequential(palette = "YlOrBr", limits = c(0, 100), aesthetics = "fill") +
  labs(tag = "(a) Combined methods") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_rect(fill = "white"),
        plot.margin = margin(0, 0, 0, 0, unit = "mm"),
        panel.spacing = margin(0, 0, 0, 0, unit = "mm"),
        panel.grid = element_line(NA), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.tag.position = c(0.32, 0.05),
        plot.tag = element_text(family = "Helvetica", size = 12)) +
  annotation_scale(location = "tl", 
                   bar_cols = c("grey60", "white"),
                   height = unit(0.25, "cm"),
                   text_cex = 0.75,
                   text_family = "Helvetica") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true", 
                         pad_x = unit(0.35, "cm"), 
                         pad_y = unit(0.65, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "black", text_size = 10, text_family = "Helvetica"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) +
    annotation_custom(C.Grob, 
                      xmax = -73.935,
                      xmin = -73.992,
                      ymax = 46.005,
                      ymin = 45.965)
C.Map.Sum

#D) Minnow trap

#Frequency distribution
MT.hist <- ggplot(lake.attributes.MT, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#2A5676", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency") + 
  ylim(0,4) +
  scale_x_continuous(breaks = c(0.0, 0.4, 0.8), labels = c("0", "0.4", "0.8")) +
  theme(text = element_text(size = 10, family = "Helvetica", color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))

MT.Grob <- ggplotGrob(MT.hist)

MT.Map.Sum <- ggplot() + 
  geom_sf(data = cropped.building, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.roads, color = "darkgrey", alpha = 0.5, size = 0.5) +  
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes.MT, aes(fill = (prev_fish*100)), color = "black", size = 0.5) +
  scale_fill_continuous_sequential(palette = "YlOrBr", limits = c(0, 100), aesthetics = "fill") +
  labs(tag = "(d) Minnow trap") +
  theme(legend.position = c(1.2, 0.2),
        legend.text = element_text(color = "black", size = 8, family = "Helvetica"),
        legend.title = element_text(color = "black", size = 8, family = "Helvetica"),
        legend.key.size = unit(0.7, "cm"),
        legend.key = element_rect(linewidth = 2),
        legend.background = element_blank(), 
        panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_rect(fill = "white"),
        plot.margin = margin(0, 0, 0, 0, unit = "mm"), 
        panel.spacing = margin(0, 0, 0, 0, unit = "mm"),
        panel.grid = element_line(NA), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.tag.position = c(0.23, 0.05),
        plot.tag = element_text(family = "Helvetica", size = 12)) +
  guides(fill = guide_colorbar(title = "Prevalence (%)",
                               label.position = "right",
                               title.position = "left", title.theme = element_text(angle = 90, size = 12, hjust = 0.5),
                               frame.colour = "black",
                               frame.linewidth = 0.3,
                               ticks.colour = "black")) +
  annotation_custom(MT.Grob, 
                    xmax = -73.935,
                    xmin = -73.992,
                    ymax = 46.005,
                    ymin = 45.965)

MT.Map.Sum

#C) Seine net

#Frequncy distribution
S.hist <- ggplot(lake.attributes.S, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#999600", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency") + 
  scale_x_continuous(breaks = c(0.0, 0.4, 0.8), labels = c("0", "0.4", "0.8")) +
  ylim(0,4) +
  theme(text = element_text(size = 10, family = "Helvetica", color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))

S.Grob <- ggplotGrob(S.hist) #Set frequency distribution as a grob

S.Map.Sum <- ggplot() + 
  geom_sf(data = cropped.building, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.roads, color = "darkgrey", alpha = 0.5, size = 0.5) +  
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes.S, aes(fill = (prev_fish*100)), color = "black", size = 0.5) +
  scale_fill_continuous_sequential(palette = "YlOrBr", limits = c(0, 100), aesthetics = "fill") +
  labs(tag = "(c) Seine net") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_rect(fill = "white"),
        plot.margin = margin(0, 0, 0, 0, unit = "mm"), 
        panel.grid = element_line(NA), 
        panel.spacing = margin(0, 0, 0, 0, unit = "mm"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.tag.position = c(0.195, 0.05),
        plot.tag = element_text(family = "Helvetica", size = 12)) +
  annotation_custom(S.Grob, 
                    xmax = -73.935,
                    xmin = -73.992,
                    ymax = 46.005,
                    ymin = 45.965)
S.Map.Sum

#B) Transect
T.hist <- ggplot(lake.attributes.T, aes(prev_fish)) + 
  geom_histogram(bins = 6, fill = "#966F1E", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency") + 
  scale_x_continuous(breaks = c(0.0, 0.4, 0.8), labels = c("0", "0.4", "0.8")) +
  ylim(0,4) +
  theme(text = element_text(size = 10, family = "Helvetica", color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))

T.Grob <- ggplotGrob(T.hist)

T.Map.Sum <- ggplot() + 
  geom_sf(data = cropped.building, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.roads, color = "darkgrey", alpha = 0.5, size = 0.5) +  
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes.T, aes(fill = (prev_fish*100)), color = "black", size = 0.5) +
  scale_fill_continuous_sequential(palette = "YlOrBr", limits = c(0, 100), aesthetics = "fill") +
  labs(tag = "(b) Transect") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_rect(fill = "white"),
        plot.margin = margin(0, 0, 0, 0, unit = "mm"), 
        panel.grid = element_line(NA), 
        panel.spacing = margin(0, 0, 0, 0, unit = "mm"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.tag.position = c(0.185, 0.05),
        plot.tag = element_text(family = "Helvetica", size = 12)) +
  annotation_custom(T.Grob, 
                    xmax = -73.935,
                    xmin = -73.992,
                    ymax = 46.005,
                    ymin = 45.965)
T.Map.Sum

Summary.map <- C.Map.Sum + T.Map.Sum + S.Map.Sum + MT.Map.Sum +
  plot_layout(ncol = 2,
              nrow = 2,
              tag_level = "keep",
              guides = "keep")

Summary.map

ggsave(paste0(to.figs, "PrevalenceMap_Fish_Summary.pdf"), plot = Summary.map, dpi = 500, width = 20, height = 20, units = "cm")
ggsave(paste0(to.rédaction, "/Figures/Figure5_MapFreqDistribution.png"), plot = Summary.map, dpi = 300, width = 20, height = 20, units = "cm")

## LeGi maps ----

### Combined methods ----

#Frequency distribution
C.hist.legi <- ggplot(lake.attributes.C, aes(prev_LeGi)) + 
  geom_histogram(bins = 6, fill = "#7E7E7E", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency") +
  ylim(0,6) +
  scale_x_continuous(breaks = c(0.0, 0.4, 0.8), labels = c("0", "0.4", "0.8")) +
  theme(text = element_text(size = 9, family = "Calibri Light", color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))

C.Grob.legi <- ggplotGrob(C.hist.legi) #Set frequency distribution as a grob

C.Map.Sum.legi <- ggplot() + 
  geom_sf(data = cropped.building, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.roads, color = "darkgrey", alpha = 0.5, size = 0.5) +  
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes.C, aes(fill = (prev_LeGi*100)), color = "black", size = 0.5) +
  scale_fill_continuous_sequential(palette = "YlOrBr", limits = c(0, 100), aesthetics = "fill") +
  labs(tag = "(a) Combined methods") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_rect(fill = "white"),
        plot.margin = margin(0, 0, 0, 0, unit = "mm"),
        panel.spacing = margin(0, 0, 0, 0, unit = "mm"),
        panel.grid = element_line(NA), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.tag.position = c(0.32, 0.05),
        plot.tag = element_text(family = "Calibri Light")) +
  annotation_scale(location = "tl", 
                   bar_cols = c("grey60", "white"),
                   height = unit(0.25, "cm"),
                   text_cex = 0.75,
                   text_family = "Calibri Light") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true", 
                         pad_x = unit(0.35, "cm"), 
                         pad_y = unit(0.65, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "black", text_size = 10, text_family = "Calibri Light"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) +
  annotation_custom(C.Grob.legi, 
                    xmax = -73.935,
                    xmin = -73.992,
                    ymax = 46.005,
                    ymin = 45.965)
C.Map.Sum.legi

### Minnow traps ----

MT.hist.legi <- ggplot(lake.attributes.MT, aes(prev_LeGi)) + 
  geom_histogram(bins = 6, fill = "#2A5676", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency") + 
  ylim(0,6) +
  scale_x_continuous(breaks = c(0.0, 0.4, 0.8), labels = c("0", "0.4", "0.8")) +
  theme(text = element_text(size = 9, family = "Calibri Light", color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))

MT.Grob.legi <- ggplotGrob(MT.hist.legi)

MT.Map.Sum.legi <- ggplot() + 
  geom_sf(data = cropped.building, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.roads, color = "darkgrey", alpha = 0.5, size = 0.5) +  
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes.MT, aes(fill = (prev_LeGi*100)), color = "black", size = 0.5) +
  scale_fill_continuous_sequential(palette = "YlOrBr", limits = c(0, 100), aesthetics = "fill") +
  labs(tag = "(d) Minnow trap") +
  theme(legend.position = c(1.2, 0.2),
        legend.text = element_text(color = "black", size = 9, family = "Calibri Light"),
        legend.title = element_text(color = "black", size = 9, family = "Calibri Light"),
        legend.key.size = unit(0.7, "cm"),
        legend.key = element_rect(linewidth = 2),
        legend.background = element_blank(), 
        panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_rect(fill = "white"),
        plot.margin = margin(0, 0, 0, 0, unit = "mm"), 
        panel.spacing = margin(0, 0, 0, 0, unit = "mm"),
        panel.grid = element_line(NA), 
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.tag.position = c(0.23, 0.05),
        plot.tag = element_text(family = "Calibri Light")) +
  guides(fill = guide_colorbar(title = "Prevalence (%)",
                               label.position = "right",
                               title.position = "left", title.theme = element_text(angle = 90, size = 12, hjust = 0.5),
                               frame.colour = "black",
                               frame.linewidth = 0.3,
                               ticks.colour = "black")) +
  annotation_custom(MT.Grob.legi, 
                    xmax = -73.935,
                    xmin = -73.992,
                    ymax = 46.005,
                    ymin = 45.965)

MT.Map.Sum.legi

### Seine net ----

S.hist.legi <- ggplot(lake.attributes.S, aes(prev_LeGi)) + 
  geom_histogram(bins = 6, fill = "#999600", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency") + 
  scale_x_continuous(breaks = c(0.0, 0.4, 0.8), labels = c("0", "0.4", "0.8")) +
  ylim(0,6) +
  theme(text = element_text(size = 9, family = "Calibri Light", color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))

S.Grob.legi <- ggplotGrob(S.hist.legi) #Set frequency distribution as a grob

S.Map.Sum.legi <- ggplot() + 
  geom_sf(data = cropped.building, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.roads, color = "darkgrey", alpha = 0.5, size = 0.5) +  
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes.S, aes(fill = (prev_LeGi*100)), color = "black", size = 0.5) +
  scale_fill_continuous_sequential(palette = "YlOrBr", limits = c(0, 100), aesthetics = "fill") +
  labs(tag = "(c) Seine net") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_rect(fill = "white"),
        plot.margin = margin(0, 0, 0, 0, unit = "mm"), 
        panel.grid = element_line(NA), 
        panel.spacing = margin(0, 0, 0, 0, unit = "mm"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.tag.position = c(0.185, 0.05),
        plot.tag = element_text(family = "Calibri Light")) +
  annotation_custom(S.Grob.legi, 
                    xmax = -73.935,
                    xmin = -73.992,
                    ymax = 46.005,
                    ymin = 45.965)
S.Map.Sum.legi

### Transect ----

T.hist.legi <- ggplot(lake.attributes.T, aes(prev_LeGi)) + 
  geom_histogram(bins = 6, fill = "#966F1E", color = "black", alpha = 0.8) +
  labs(x = "Prevalence", y = "Frequency") + 
  scale_x_continuous(breaks = c(0.0, 0.4, 0.8), labels = c("0", "0.4", "0.8")) +
  ylim(0,6) +
  theme(text = element_text(size = 9, family = "Calibri Light", color = "black"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black",lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))

T.Grob.legi <- ggplotGrob(T.hist.legi)

T.Map.Sum.legi <- ggplot() + 
  geom_sf(data = cropped.building, color = "darkgrey", alpha = 0.5, size = 0.5) +
  geom_sf(data = cropped.roads, color = "darkgrey", alpha = 0.5, size = 0.5) +  
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes.T, aes(fill = (prev_LeGi*100)), color = "black", size = 0.5) +
  scale_fill_continuous_sequential(palette = "YlOrBr", limits = c(0, 100), aesthetics = "fill") +
  labs(tag = "(b) Transect") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_rect(fill = "white"),
        plot.margin = margin(0, 0, 0, 0, unit = "mm"), 
        panel.grid = element_line(NA), 
        panel.spacing = margin(0, 0, 0, 0, unit = "mm"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.tag.position = c(0.175, 0.05),
        plot.tag = element_text(family = "Calibri Light")) +
  annotation_custom(T.Grob.legi, 
                    xmax = -73.935,
                    xmin = -73.992,
                    ymax = 46.005,
                    ymin = 45.965)
T.Map.Sum.legi

Summary.map.legi <- C.Map.Sum.legi + T.Map.Sum.legi + S.Map.Sum.legi + MT.Map.Sum.legi +
  plot_layout(ncol = 2,
              nrow = 2,
              tag_level = "keep",
              guides = "keep")

Summary.map.legi

ggsave(paste0(to.figs, "PrevalenceMap_LeGi_Summary.png"), plot = Summary.map.legi, dpi = 500, width = 20, height = 20, units = "cm")

