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
#buildings <- st_point(paste0(to.carto, "Attribute_templates/Template_batiments_points.shp"))
#batiments <- st_point(st_read(dsn = "carto/Attribute_templates", layer = "Template_batiments_points.shp"))

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

## Attributes table ----

### Prevalence data ----

lake.attributes <- CombinedData %>% #Selecting abundance data
  select(Lake, Lat.lake, Long.lake, starts_with(c("tot", "inf")))

lake.attributes <- lake.attributes %>% #Lake abundance sums
  group_by(Lake, Lat.lake, Long.lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

lake.attributes <- lake.attributes %>% #Creating fish abundance columns
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_AmNe + tot_CaCo + tot_EsMa + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae) %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_AmNe + inf_CaCo + inf_EsMa + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae) 

lake.attributes <- lake.attributes %>% #Creating prevalence columns
  mutate(prev_fish = inf_fish/tot_fish) %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi)

### Map frame ----

Study.map <- rbind(ACHI, BEAV, COEU, CORN, CORR, CROC, CROM, ECHO, FOUR, MONT, MORE, PINR, STON, TRAC, TRIT)

cropped.frame <- c(xmin = -74.08, 
                   ymin = 45.87,
                   xmax = -73.94,
                   ymax = 46.00)

cropped.creeks <- st_crop(creeks, cropped.frame)
cropped.lakes <- st_crop(lakes, cropped.frame)
#cropped.buildings <- st_crop(batiments, cropped.frame)

lake.attributes <- Study.map %>% 
  mutate(prev_LeGi = lake.attributes$prev_LeGi) %>% 
  mutate(prev_fish = lake.attributes$prev_fish)

## LeGi prevalence map ----
Study.LeGi.plot <- ggplot() + 
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes, aes(fill = (prev_LeGi*100))) +
  theme(legend.position = c(0.845, 0.17),
        legend.text = element_text(color = "#4e4d47", size = 8),
        legend.title = element_text(color = "#4e4d47", size = 9, face = "bold"), 
        legend.background = element_rect(fill = NA, color = "#4e4d47", size = 0.4), 
        panel.background = element_rect(fill = "#f5f5f2", color = "black"), 
        plot.background = element_rect(fill = "#FAFAFA"), 
        panel.grid = element_line(NA), 
        axis.ticks = element_blank(),
        text = element_text(size = 20, family = "Calibri Light", color = "black")) +
  scale_fill_continuous_sequential(palette = "YlOrBr") +
  annotation_scale(location = "tl", 
                   bar_cols = c("grey60", "white")) + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true", 
                         pad_x = unit(0.5, "cm"), 
                         pad_y = unit(0.7, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "#4e4d47")) +
  labs(fill = "Infection \nprevalence (%)")
Study.LeGi.plot

ggsave(paste0(to.figs, "PrevalenceMap_LeGi.png"), plot = Study.LeGi.plot, dpi = 300, width = 15, height = 20)

# Fish Final map #
Study.fish.plot <- ggplot() + 
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, color = "lightblue", alpha = 0.5) +
  geom_sf(data = lake.attributes, aes(fill = (prev_fish*100)), color = "black", size = 0.5) +
  #scale_x_continuous(labels = c("74.08","74.02", "73.94")) +
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
Study.fish.plot

ggsave(paste0(to.figs, "PrevalenceMap_Fish.png"), plot = Study.fish.plot, dpi = 300, width = 10, height = 15, units = "cm")
ggsave(paste0(to.rédaction,"./Figures/", "Figure2_Map.png"), plot = Study.fish.plot, dpi = 300, width = 10, height = 15, units = "cm")
