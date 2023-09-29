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

## Loading packages ----

library(sf)
library(stringr)
library(dplyr)
library(measurements)
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
  select("Sampling_ID", "Lake", "Sampling_method", "Latitude", "Longitude", "tot_LeGi", "inf_LeGi") %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi, .keep = "unused") 

### Coordinates conversion ----

#Latitude
attributes$Latitude <- str_replace(attributes$Latitude, "°", " ")
attributes$Latitude <- str_remove(attributes$Latitude, "'")
attributes$Latitude <- conv_unit(attributes$Latitude, from = "deg_dec_min", to = "dec_deg")
attributes$Latitude <- as.numeric(attributes$Latitude)

#Longitude
attributes$Longitude <- str_replace(attributes$Longitude, "°", " ")
attributes$Longitude <- str_remove(attributes$Longitude, "'")
attributes$Longitude <- conv_unit(attributes$Longitude, from = "deg_dec_min", to = "dec_deg")
attributes$Longitude <- as.numeric(attributes$Longitude)*(-1) #Add negative sign as coordinates are from western hemisphere

## Cromwell ----

CROM.att <- attributes %>% 
  filter(Lake == "Cromwell")

CROM.plot <- ggplot() + 
  geom_sf(data = CROM, fill = "lightblue") +
  geom_point(data = CROM.att, 
             aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) +
  scale_color_manual(values = col.pal) + 
  geom_text(data = CROM.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
CROM.plot

## Croche ----

CROC.att <- attributes %>% 
  filter(Lake == "Croche")

CROC.plot <- ggplot() + 
  geom_sf(data = CROC, fill = "lightblue") +
  geom_point(data = CROC.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = CROC.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
CROC.plot

## Corriveau ---

CORR.att <- attributes %>% 
  filter(Lake == "Corriveau")

CORR.plot <- ggplot() + 
  geom_sf(data = CORR, fill = "lightblue") +
  geom_point(data = CORR.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = CORR.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
CORR.plot ##Bubbles shifted

## Echo ----

ECHO.att <- attributes %>% 
  filter(Lake == "Echo")

ECHO.plot <- ggplot() + 
  geom_sf(data = ECHO, fill = "lightblue") +
  geom_point(data = ECHO.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = ECHO.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
ECHO.plot

## Achigan ----

ACHI.att <- attributes %>% 
  filter(Lake == "Achigan")

ACHI.plot <- ggplot() + 
  geom_sf(data = ACHI, fill = "lightblue") +
  geom_point(data = ACHI.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = ACHI.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
ACHI.plot

## Fournelle ----

FOUR.att <- attributes %>% 
  filter(Lake == "Fournelle")

FOUR.plot <- ggplot() + 
  geom_sf(data = FOUR, fill = "lightblue") +
  geom_point(data = FOUR.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = FOUR.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
FOUR.plot

## Morency ---

MORE.att <- attributes %>% 
  filter(Lake == "Morency")

MORE.plot <- ggplot() + 
  geom_sf(data = MORE, fill = "lightblue") +
  geom_point(data = MORE.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = MORE.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
MORE.plot 

## Cornu ----

CORN.att <- attributes %>% 
  filter(Lake == "Cornu")

CORN.plot <- ggplot() + 
  geom_sf(data = CORN, fill = "lightblue") +
  geom_point(data = CORN.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = CORN.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
CORN.plot

## Beaver ----

BEAV.att <- attributes %>% 
  filter(Lake == "Beaver")

BEAV.plot <- ggplot() + 
  geom_sf(data = BEAV, fill = "lightblue") +
  geom_point(data = BEAV.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = BEAV.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
BEAV.plot #Some shifted coordinates

## Montaubois ----

MONT.att <- attributes %>% 
  filter(Lake == "Montaubois")

MONT.plot <- ggplot() + 
  geom_sf(data = MONT, fill = "lightblue") +
  geom_point(data = MONT.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = MONT.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
MONT.plot

## Tracy ----

TRAC.att <- attributes %>% 
  filter(Lake == "Tracy")

TRAC.plot <- ggplot() + 
  geom_sf(data = TRAC, fill = "lightblue") +
  geom_point(data = TRAC.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = TRAC.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
TRAC.plot

## Coeur ----

COEU.att <- attributes %>% 
  filter(Lake == "Coeur")

COEU.plot <- ggplot() + 
  geom_sf(data = COEU, fill = "lightblue") +
  geom_point(data = COEU.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = COEU.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
COEU.plot

## Pin rouge ----

PINR.att <- attributes %>% 
  filter(Lake == "Pin_rouge")

PINR.plot <- ggplot() + 
  geom_sf(data = PINR, fill = "lightblue") +
  geom_point(data = PINR.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = PINR.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
PINR.plot

## St-Onge ----

STON.att <- attributes %>% 
  filter(Lake == "St-Onge")

STON.plot <- ggplot() + 
  geom_sf(data = STON, fill = "lightblue") +
  geom_point(data = STON.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = STON.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
STON.plot #Shifted coordinates

# ---- Study area cartography ----

## Attributes table ----

### Prevalence data ----

lake.attributes <- CombinedData %>% #Selecting abundance data
  select(Lake, starts_with(c("tot", "inf")))

lake.attributes <- lake.attributes %>% #Lake abundance sums
  group_by(Lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

lake.attributes <- lake.attributes %>% #Creating fish abundance columns
  mutate(tot_fish = tot_AmRu + tot_FuDi + tot_MiDo + tot_LeGi + tot_PeFl + tot_PiPr + tot_ChrosomusSp. + tot_PiNo + tot_SeAt + tot_LuCo + tot_AmNe + tot_CaCo + tot_EsMa + tot_UmLi + tot_RhAt + tot_Centrarchidae + tot_Cyprinidae) %>% 
  mutate(inf_fish = inf_AmRu + inf_FuDi + inf_MiDo + inf_LeGi + inf_PeFl + inf_PiPr + inf_ChrosomusSp. + inf_PiNo + inf_SeAt + inf_LuCo + inf_AmNe + inf_CaCo + inf_EsMa + inf_UmLi + inf_RhAt + inf_Centrarchidae + inf_Cyprinidae) 

lake.attributes <- lake.attributes %>% #Creating prevalence columns
  mutate(prev_fish = inf_fish/tot_fish) %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi)

### Lake coordinates ----
Lake.lat <- c("45 56 34", "45 55 30", "45 58 06", "45 52 53", "45 58 38", "45 59 34", "45 59 21", "45 53 14", "45 54 53", "45 55 20", "45 55 40", "45 57 39", "45 54 52", "45 55 38", "45 59 16") #GPS data from bathymetric maps
Lake.long <- c("-73 58 41", "-74 03 50", "-74 00 36", "-74 00 02", "-74 00 03", "-74 00 34", "-73 59 55", "-74 01 24", "-74 02 28", "-74 04 23", "-74 02 09", "-74 02 28", "-73 57 44", "-74 03 57", "-74 00 29") #GPS data from bathymetric maps

#Latitude
Lake.lat <- conv_unit(Lake.lat, from = "deg_min_sec", to = "dec_deg") #Coordinates conversion
Lake.lat <- as.numeric(Lake.lat)

#Longitude
Lake.long <- conv_unit(Lake.long, from = "deg_min_sec", to = "dec_deg")  #Coordinates conversion
Lake.long <- as.numeric(Lake.long)

### Data frame ----
lake.attributes <- cbind(lake.attributes, Lake.lat, Lake.long)

## Map frame ----

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
  scale_fill_continuous_sequential(palette = "YlOrBr") +
  scale_continuous_identity(aesthetics = c(0, 0.75)) + 
  theme(legend.position = c(0.845, 0.17),
        legend.text = element_text(color = "black", size = 20, family = "Calibri Light"),
        legend.title = element_text(color = "black", size = 20, family = "Calibri Bold"),
        legend.key.size = unit(1.5, "cm"),
        legend.box.margin = margin(10,10,10,10),
        legend.key = element_rect(linewidth = 2),
        legend.background = element_blank(), 
        legend.box.background = element_rect(fill = NA, color = "black", size = 0.5),
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_line(NA), 
        axis.ticks = element_blank(),
        text = element_text(size = 20, family = "Calibri Light", color = "black"),
        plot.caption = element_text(hjust = 0, vjust = 1, size = 20, family = "Calibri Light", color = "black", margin = margin(15,0,0,0, unit = "pt"))) +
  annotation_scale(location = "tl", 
                   bar_cols = c("grey60", "white"),
                   height = unit(0.5, "cm"),
                   text_cex = 1.2,
                   text_family = "Calibri Light") + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true", 
                         pad_x = unit(1.75, "cm"), 
                         pad_y = unit(1, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "black", text_size = 20, text_family = "Calibri Light"),
                         height = unit(3, "cm"),
                         width = unit(3, "cm")) +
  labs(fill = "Infection \nprevalence (%)",
       caption = "Figure 1. Map of the study area. Sampled lakes are colored according to the prevalence the black spot disease infection in the\nfish communities. The data used to estimate infection prevalence comes from all methods combined.") +
  guides(fill = guide_colorbar(title = "Infection \nprevalence (%)",
                               label.position = "right",
                               title.position = "top", title.vjust = 1,
                               frame.colour = "black",
                               frame.linewidth = 0.5))
Study.fish.plot

ggsave(paste0(to.figs, "PrevalenceMap_Fish.png"), plot = Study.fish.plot, dpi = 300, width = 15, height = 20)

