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

# ---- Intra lake LeGi prevalence bubble map ----

col.pal <- c("chocolate", "goldenrod", "olivedrab") #Setting color palette

## Attribute table ----

attributes <- CombinedData %>% 
  select("Sampling_ID", "Lake", "Sampling_method", "Lat.trans", "Long.trans", "tot_LeGi", "inf_LeGi") %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi, .keep = "unused") 

## Cromwell ----

CROM.att <- attributes %>% 
  filter(Lake == "Cromwell")

CROM.plot <- ggplot() + 
  geom_sf(data = CROM, fill = "lightblue") +
  geom_point(data = CROM.att, 
             aes(x = Long.trans, y = Lat.trans, size = prev_LeGi, color = Sampling_method)) +
  scale_color_manual(values = col.pal) + 
  geom_text(data = CROM.att, aes(x = Long.trans, y = Lat.trans, label = Sampling_ID), size = 2) +
  theme_void()
CROM.plot

## Croche ----

CROC.att <- attributes %>% 
  filter(Lake == "Croche")

CROC.plot <- ggplot() + 
  geom_sf(data = CROC, fill = "lightblue") +
  geom_point(data = CROC.att, aes(x = Long.trans, y = Lat.trans, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = CROC.att, aes(x = Long.trans, y = Lat.trans, label = Sampling_ID), size = 2) +
  theme_void()
CROC.plot

## Corriveau ---

CORR.att <- attributes %>% 
  filter(Lake == "Corriveau")

CORR.plot <- ggplot() + 
  geom_sf(data = CORR, fill = "lightblue") +
  geom_point(data = CORR.att, aes(x = Long.trans, y = Lat.trans, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = CORR.att, aes(x = Long.trans, y = Lat.trans, label = Sampling_ID), size = 2) +
  theme_void()
CORR.plot ##Bubbles shifted

## Echo ----

ECHO.att <- attributes %>% 
  filter(Lake == "Echo")

ECHO.plot <- ggplot() + 
  geom_sf(data = ECHO, fill = "lightblue") +
  geom_point(data = ECHO.att, aes(x = Long.trans, y = Lat.trans, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = ECHO.att, aes(x = Long.trans, y = Lat.trans, label = Sampling_ID), size = 2) +
  theme_void()
ECHO.plot

## Achigan ----

ACHI.att <- attributes %>% 
  filter(Lake == "Achigan")

ACHI.plot <- ggplot() + 
  geom_sf(data = ACHI, fill = "lightblue") +
  geom_point(data = ACHI.att, aes(x = Long.trans, y = Lat.trans, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = ACHI.att, aes(x = Long.trans, y = Lat.trans, label = Sampling_ID), size = 2) +
  theme_void()
ACHI.plot

## Fournelle ----

FOUR.att <- attributes %>% 
  filter(Lake == "Fournelle")

FOUR.plot <- ggplot() + 
  geom_sf(data = FOUR, fill = "lightblue") +
  geom_point(data = FOUR.att, aes(x = Long.trans, y = Lat.trans, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = FOUR.att, aes(x = Long.trans, y = Lat.trans, label = Sampling_ID), size = 2) +
  theme_void()
FOUR.plot

## Morency ---

MORE.att <- attributes %>% 
  filter(Lake == "Morency")

MORE.plot <- ggplot() + 
  geom_sf(data = MORE, fill = "lightblue") +
  geom_point(data = MORE.att, aes(x = Long.trans, y = Lat.trans, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = MORE.att, aes(x = Long.trans, y = Lat.trans, label = Sampling_ID), size = 2) +
  theme_void()
MORE.plot 

## Cornu ----

CORN.att <- attributes %>% 
  filter(Lake == "Cornu")

CORN.plot <- ggplot() + 
  geom_sf(data = CORN, fill = "lightblue") +
  geom_point(data = CORN.att, aes(x = Long.trans, y = Lat.trans, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = CORN.att, aes(x = Long.trans, y = Lat.trans, label = Sampling_ID), size = 2) +
  theme_void()
CORN.plot

## Beaver ----

BEAV.att <- attributes %>% 
  filter(Lake == "Beaver")

BEAV.plot <- ggplot() + 
  geom_sf(data = BEAV, fill = "lightblue") +
  geom_point(data = BEAV.att, aes(x = Long.trans, y = Lat.trans, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = BEAV.att, aes(x = Long.trans, y = Lat.trans, label = Sampling_ID), size = 2) +
  theme_void()
BEAV.plot #Some shifted coordinates

## Montaubois ----

MONT.att <- attributes %>% 
  filter(Lake == "Montaubois")

MONT.plot <- ggplot() + 
  geom_sf(data = MONT, fill = "lightblue") +
  geom_point(data = MONT.att, aes(x = Long.trans, y = Lat.trans, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = MONT.att, aes(x = Long.trans, y = Lat.trans, label = Sampling_ID), size = 2) +
  theme_void()
MONT.plot

## Tracy ----

TRAC.att <- attributes %>% 
  filter(Lake == "Tracy")

TRAC.plot <- ggplot() + 
  geom_sf(data = TRAC, fill = "lightblue") +
  geom_point(data = TRAC.att, aes(x = Long.trans, y = Lat.trans, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = TRAC.att, aes(x = Long.trans, y = Lat.trans, label = Sampling_ID), size = 2) +
  theme_void()
TRAC.plot

## Coeur ----

COEU.att <- attributes %>% 
  filter(Lake == "Coeur")

COEU.plot <- ggplot() + 
  geom_sf(data = COEU, fill = "lightblue") +
  geom_point(data = COEU.att, aes(x = Long.trans, y = Lat.trans, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = COEU.att, aes(x = Long.trans, y = Lat.trans, label = Sampling_ID), size = 2) +
  theme_void()
COEU.plot

## Pin rouge ----

PINR.att <- attributes %>% 
  filter(Lake == "Pin_rouge")

PINR.plot <- ggplot() + 
  geom_sf(data = PINR, fill = "lightblue") +
  geom_point(data = PINR.att, aes(x = Long.trans, y = Lat.trans, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = PINR.att, aes(x = Long.trans, y = Lat.trans, label = Sampling_ID), size = 2) +
  theme_void()
PINR.plot

## St-Onge ----

STON.att <- attributes %>% 
  filter(Lake == "St-Onge")

STON.plot <- ggplot() + 
  geom_sf(data = STON, fill = "lightblue") +
  geom_point(data = STON.att, aes(x = Long.trans, y = Lat.trans, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = STON.att, aes(x = Long.trans, y = Lat.trans, label = Sampling_ID), size = 2) +
  theme_void()
STON.plot #Shifted coordinates

# ---- Study area cartography ----
#Prevalence maps at community level

## Map frame ----

Study.map <- rbind(ACHI, BEAV, COEU, CORN, CORR, CROC, CROM, ECHO, FOUR, MONT, MORE, PINR, STON, TRAC, TRIT)

cropped.frame <- c(xmin = -74.08, 
                   ymin = 45.87,
                   xmax = -73.94,
                   ymax = 46.00)

cropped.frame2 <- c(xmin = -433485,
                    ymin = 223316,
                    xmax = -421000,
                    ymax =  239250)

cropped.creeks <- st_crop(creeks, cropped.frame)
cropped.lakes <- st_crop(lakes, cropped.frame)

cropped.watersheds <- st_crop(watersheds, cropped.frame2)

## Prevalence data ----

lake.attributes <- CombinedData %>% #Selecting abundance data
  select(Lake, Sampling_method, Lat.lake, Long.lake, starts_with(c("tot", "inf")))

### All ----

lake.attributes.All <- lake.attributes %>% #Lake abundance sums
  select(!(Sampling_method)) %>% 
  group_by(Lake, Lat.lake, Long.lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

lake.attributes.All <- lake.attributes.All %>% #Creating fish abundance columns
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_AmNe + tot_CaCo + tot_EsMa + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae) %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_AmNe + inf_CaCo + inf_EsMa + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae) 

lake.attributes.All <- lake.attributes.All %>% #Creating prevalence columns
  mutate(prev_fish = inf_fish/tot_fish) %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi)

lake.attributes.All <- Study.map %>% 
  mutate(prev_LeGi = lake.attributes.All$prev_LeGi) %>% 
  mutate(prev_fish = lake.attributes.All$prev_fish)

### Minnow trap ----

lake.attributes.MT <- lake.attributes %>% #Lake abundance sums
  filter(Sampling_method == "Minnow_trap") %>% 
  select(!(Sampling_method)) %>% 
  group_by(Lake, Lat.lake, Long.lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

lake.attributes.MT <- lake.attributes.MT %>% #Creating fish abundance columns
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_AmNe + tot_CaCo + tot_EsMa + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae) %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_AmNe + inf_CaCo + inf_EsMa + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae) 

lake.attributes.MT <- lake.attributes.MT %>% #Creating prevalence columns
  mutate(prev_fish = inf_fish/tot_fish) %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi)

lake.attributes.MT <- Study.map %>% 
  mutate(prev_LeGi = lake.attributes.MT$prev_LeGi) %>% 
  mutate(prev_fish = lake.attributes.MT$prev_fish)

### Seine net ----

lake.attributes.S <- lake.attributes %>% #Lake abundance sums
  filter(Sampling_method == "Seine") %>% 
  select(!(Sampling_method)) %>% 
  group_by(Lake, Lat.lake, Long.lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

lake.attributes.S <- lake.attributes.S %>% #Creating fish abundance columns
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_AmNe + tot_CaCo + tot_EsMa + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae) %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_AmNe + inf_CaCo + inf_EsMa + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae) 

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
  group_by(Lake, Lat.lake, Long.lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

lake.attributes.T <- lake.attributes.T %>% #Creating fish abundance columns
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_AmNe + tot_CaCo + tot_EsMa + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae) %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_AmNe + inf_CaCo + inf_EsMa + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae) 

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
  geom_sf(data = cropped.watersheds, fill = NA, linewidth = 0.5, color = "#467092") +
  geom_sf(data = lake.attributes.All, fill = "#B65000", alpha = 0.8, color = "black", size = 0.5) +
  theme(legend.position = c(0.88, 0.15),
        legend.text = element_text(color = "black", size = 9, family = "Calibri Light"),
        legend.title = element_text(color = "black", size = 9, family = "Calibri Bold"),
        legend.key.size = unit(0.5, "cm"),
        legend.key = element_rect(linewidth = 2),
        legend.background = element_blank(), 
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white"),
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
                         pad_x = unit(0.85, "cm"), 
                         pad_y = unit(0.65, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "black", text_size = 10, text_family = "Calibri Light"),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"))
LakesMap

ggsave(paste0(to.figs, "LakesMaps+BV.png"), plot = LakesMap, dpi = 300, width = 10, height = 15, units = "cm")
#ggsave(paste0(to.figs, "LakesMaps.png"), plot = LakesMap, dpi = 300, width = 10, height = 15, units = "cm") #change pad_y at 0.55 instead of 0.85 & omit watersheds polygon line

### All ----

FishMap.All <- ggplot() + 
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = cropped.watersheds, fill = NA, linewidth = 0.5, color = "#467092") +
  geom_sf(data = lake.attributes.All, aes(fill = (prev_fish*100)), color = "black", size = 0.5) +
  scale_fill_continuous_sequential(palette = "YlOrBr") +
  scale_continuous_identity(aesthetics = c(0, 0.75)) +
  theme(legend.position = c(0.88, 0.15),
        legend.text = element_text(color = "black", size = 9, family = "Calibri Light"),
        legend.title = element_text(color = "black", size = 9, family = "Calibri Bold"),
        legend.key.size = unit(0.5, "cm"),
        legend.key = element_rect(linewidth = 2),
        legend.background = element_blank(), 
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white"),
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
                         pad_x = unit(0.85, "cm"), 
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
FishMap.All

#ggsave(paste0(to.figs, "PrevalenceMap_Fish_All.png"), plot = FishMap.All, dpi = 300, width = 10, height = 15, units = "cm")
ggsave(paste0(to.figs, "PrevalenceMap_Fish_All+BV.png"), plot = FishMap.All, dpi = 300, width = 10, height = 15, units = "cm")
#ggsave(paste0(to.rédaction,"./Figures/", "Figure2_Map.png"), plot = FishMap.All, dpi = 300, width = 10, height = 15, units = "cm")

### Minnow traps ----

FishMap.MT <- ggplot() + 
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes.MT, aes(fill = (prev_fish*100)), color = "black", size = 0.5) +
  geom_sf(data = cropped.watersheds, fill = NA, linewidth = 0.5, color = "#467092") +
  scale_fill_continuous_sequential(palette = "YlOrBr") +
  scale_continuous_identity(aesthetics = c(0, 0.75)) +
  theme(legend.position = c(0.88, 0.15),
        legend.text = element_text(color = "black", size = 9, family = "Calibri Light"),
        legend.title = element_text(color = "black", size = 9, family = "Calibri Bold"),
        legend.key.size = unit(0.5, "cm"),
        legend.key = element_rect(linewidth = 2),
        legend.background = element_blank(), 
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white"),
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
                         pad_x = unit(0.85, "cm"), 
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

#ggsave(paste0(to.figs, "PrevalenceMap_Fish_MinnowTrap.png"), plot = FishMap.MT, dpi = 300, width = 10, height = 15, units = "cm")
ggsave(paste0(to.figs, "PrevalenceMap_Fish_MinnowTrap+BV.png"), plot = FishMap.MT, dpi = 300, width = 10, height = 15, units = "cm")

### Seine net ----

FishMap.S <- ggplot() + 
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes.S, aes(fill = (prev_fish*100)), color = "black", size = 0.5) +
  geom_sf(data = cropped.watersheds, fill = NA, linewidth = 0.5, color = "#467092") +
  scale_fill_continuous_sequential(palette = "YlOrBr") +
  scale_continuous_identity(aesthetics = c(0, 0.75)) +
  theme(legend.position = c(0.88, 0.15),
        legend.text = element_text(color = "black", size = 9, family = "Calibri Light"),
        legend.title = element_text(color = "black", size = 9, family = "Calibri Bold"),
        legend.key.size = unit(0.5, "cm"),
        legend.key = element_rect(linewidth = 2),
        legend.background = element_blank(), 
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white"),
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
                         pad_x = unit(0.85, "cm"), 
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

#ggsave(paste0(to.figs, "PrevalenceMap_Fish_Seine.png"), plot = FishMap.S, dpi = 300, width = 10, height = 15, units = "cm")
ggsave(paste0(to.figs, "PrevalenceMap_Fish_Seine+BV.png"), plot = FishMap.S, dpi = 300, width = 10, height = 15, units = "cm")

### Transect ----

FishMap.T <- ggplot() + 
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes.T, aes(fill = (prev_fish*100)), color = "black", size = 0.5) +
  geom_sf(data = cropped.watersheds, fill = NA, linewidth = 0.5, color = "#467092") +
  scale_fill_continuous_sequential(palette = "YlOrBr") +
  scale_continuous_identity(aesthetics = c(0, 0.75)) +
  theme(legend.position = c(0.88, 0.15),
        legend.text = element_text(color = "black", size = 9, family = "Calibri Light"),
        legend.title = element_text(color = "black", size = 9, family = "Calibri Bold"),
        legend.key.size = unit(0.5, "cm"),
        legend.key = element_rect(linewidth = 2),
        legend.background = element_blank(), 
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white"),
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
                         pad_x = unit(0.85, "cm"), 
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

#ggsave(paste0(to.figs, "PrevalenceMap_Fish_Transect.png"), plot = FishMap.T, dpi = 300, width = 10, height = 15, units = "cm")
ggsave(paste0(to.figs, "PrevalenceMap_Fish_Transect+BV.png"), plot = FishMap.T, dpi = 300, width = 10, height = 15, units = "cm")


### Summary ----

Summary.map <- FishMap.All + FishMap.T + FishMap.S + FishMap.MT +
  plot_layout(ncol = 2,
              nrow = 2, 
              tag_level = "new") +
  plot_annotation(tag_levels = "A", 
                  title = "Community prevalence maps (A) All methods. (B) Transect. (C) Seine net. (D) Minnow trap",
                  theme = list(title = element_text(size = 20, 
                                                    family = "Calibri Light", 
                                                    color = "black")))
Summary.map

ggsave(paste0(to.figs, "PrevalenceMap_Fish_Summary.png"), plot = Summary.map, dpi = 300, width = 10, height = 15, units = "cm")
