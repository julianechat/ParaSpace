## Cartography ##

# ----- R Setup ----- #

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"
to.carto <- "./carto/"

# ----- Loading packages ----- #

library(sf)
library(stringr)
library(dplyr)
library(measurements)
library(ggplot2)
library(ggspatial)
library(colorspace)

# ----- Loading data ----- #

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))

# ----------------------------- #

#### Intra lake prevalences bubble map #### 
col.pal <- c("chocolate", "goldenrod", "olivedrab")

## Attribute table ##

attributes <- CombinedData %>% 
  select_("Sampling_ID", "Lake", "Sampling_method", "Latitude", "Longitude", "tot_LeGi", "inf_LeGi") %>% 
  mutate(prev_LeGi = inf_LeGi/tot_LeGi, .keep = "unused") 

# Coordinates conversion #
attributes$Latitude <- str_replace(attributes$Latitude, "°", " ")
attributes$Latitude <- str_remove(attributes$Latitude, "'")
attributes$Latitude <- conv_unit(attributes$Latitude, from = "deg_dec_min", to = "dec_deg")
attributes$Latitude <- as.numeric(attributes$Latitude)

attributes$Longitude <- str_replace(attributes$Longitude, "°", " ")
attributes$Longitude <- str_remove(attributes$Longitude, "'")
attributes$Longitude <- conv_unit(attributes$Longitude, from = "deg_dec_min", to = "dec_deg")
attributes$Longitude <- as.numeric(attributes$Longitude)*(-1) #Add negative sign as coordinates are from western hemisphere

# Cromwell #
CROM <- st_read(paste0(to.carto, "Lake_shapes/Cromwell.shp"))

CROM.att <- attributes %>% filter(Lake == "Cromwell")

CROM.plot <- ggplot() + 
  geom_sf(data = CROM, fill = "lightblue") +
  geom_point(data = CROM.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) +
  scale_color_manual(values = col.pal) + 
  geom_text(data = CROM.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
CROM.plot

# Croche #
CROC <- st_read(paste0(to.carto, "Lake_shapes/Croche.shp"))

CROC.att <- attributes %>% filter(Lake == "Croche")

CROC.plot <- ggplot() + 
  geom_sf(data = CROC, fill = "lightblue") +
  geom_point(data = CROC.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = CROC.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
CROC.plot

# Corriveau # 
CORR <- st_read(paste0(to.carto, "Lake_shapes/Corriveau.shp"))

CORR.att <- attributes %>% filter(Lake == "Corriveau")

CORR.plot <- ggplot() + 
  geom_sf(data = CORR, fill = "lightblue") +
  geom_point(data = CORR.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = CORR.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
CORR.plot

# Echo # 
ECHO <- st_read(paste0(to.carto, "Lake_shapes/Echo.shp"))

ECHO.att <- attributes %>% filter(Lake == "Echo")

ECHO.plot <- ggplot() + 
  geom_sf(data = ECHO, fill = "lightblue") +
  geom_point(data = ECHO.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = ECHO.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
ECHO.plot

# Achigan # 
ACHI <- st_read(paste0(to.carto, "Lake_shapes/Achigan.shp"))

ACHI.att <- attributes %>% filter(Lake == "Achigan")

ACHI.plot <- ggplot() + 
  geom_sf(data = ACHI, fill = "lightblue") +
  geom_point(data = ACHI.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = ACHI.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
ACHI.plot

# Fournelle # 
FOUR <- st_read(paste0(to.carto, "Lake_shapes/Fournelle.shp"))

FOUR.att <- attributes %>% filter(Lake == "Fournelle")

FOUR.plot <- ggplot() + 
  geom_sf(data = FOUR, fill = "lightblue") +
  geom_point(data = FOUR.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = FOUR.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
FOUR.plot

# Morency #
MORE <- st_read(paste0(to.carto, "Lake_shapes/Morency.shp"))

MORE.att <- attributes %>% filter(Lake == "Morency")

MORE.plot <- ggplot() + 
  geom_sf(data = MORE, fill = "lightblue") +
  geom_point(data = MORE.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = MORE.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
MORE.plot 

# Cornu #
CORN <- st_read(paste0(to.carto, "Lake_shapes/Cornu.shp"))

CORN.att <- attributes %>% filter(Lake == "Cornu")

CORN.plot <- ggplot() + 
  geom_sf(data = CORN, fill = "lightblue") +
  geom_point(data = CORN.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = CORN.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
CORN.plot

# Beaver # 
BEAV <- st_read(paste0(to.carto, "Lake_shapes/Beaver.shp"))

BEAV.att <- attributes %>% filter(Lake == "Beaver")

BEAV.plot <- ggplot() + 
  geom_sf(data = BEAV, fill = "lightblue") +
  geom_point(data = BEAV.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = BEAV.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
BEAV.plot

# Montaubois #
MONT <- st_read(paste0(to.carto, "Lake_shapes/Montaubois.shp"))

MONT.att <- attributes %>% filter(Lake == "Montaubois")

MONT.plot <- ggplot() + 
  geom_sf(data = MONT, fill = "lightblue") +
  geom_point(data = MONT.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = MONT.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
MONT.plot

# Tracy #
TRAC <- st_read(paste0(to.carto, "Lake_shapes/Tracy.shp"))

TRAC.att <- attributes %>% filter(Lake == "Tracy")

TRAC.plot <- ggplot() + 
  geom_sf(data = TRAC, fill = "lightblue") +
  geom_point(data = TRAC.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = TRAC.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
TRAC.plot

# Coeur #
COEU <- st_read(paste0(to.carto, "Lake_shapes/Coeur.shp"))

COEU.att <- attributes %>% filter(Lake == "Coeur")

COEU.plot <- ggplot() + 
  geom_sf(data = COEU, fill = "lightblue") +
  geom_point(data = COEU.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = COEU.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
COEU.plot

# Pin rouge #
PINR <- st_read(paste0(to.carto, "Lake_shapes/Pin_rouge.shp"))

PINR.att <- attributes %>% filter(Lake == "Pin_rouge")

PINR.plot <- ggplot() + 
  geom_sf(data = PINR, fill = "lightblue") +
  geom_point(data = PINR.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = PINR.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
PINR.plot

# St-Onge #
STON <- st_read(paste0(to.carto, "Lake_shapes/St-Onge.shp"))

STON.att <- attributes %>% filter(Lake == "St-Onge")

STON.plot <- ggplot() + 
  geom_sf(data = STON, fill = "lightblue") +
  geom_point(data = STON.att, aes(x = Longitude, y = Latitude, size = prev_LeGi, color = Sampling_method)) + 
  scale_color_manual(values = col.pal) +
  geom_text(data = STON.att, aes(x = Longitude, y = Latitude, label = Sampling_ID), size = 2) +
  theme_void()
STON.plot

#### SBL cartography ####

TRIT <- st_read(paste0(to.carto, "Lake_shapes/Triton.shp"))
LONG <- st_read(paste0(to.carto, "Lake_shapes/Long.shp"))
PrevalenceLP <- read.csv2("~/Desktop/PrevalenceLP.csv", sep = ";")

SBL.lat <- c( "45 58 38", "45 59 34", "45 59 21", "45 59 50", "45 59 16")
SBL.long <- c("-74 00 03",  "-74 00 34", "-73 59 55", "-74 00 26", "-74 00 29")
SBL.lat <- conv_unit(SBL.lat, from = "deg_min_sec", to = "dec_deg") #Coordinates conversion
SBL.long <- conv_unit(SBL.long, from = "deg_min_sec", to = "dec_deg") #Coordinates conversion


# Mean prevalence per lake #
prev.lake <- PrevalenceLP %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = Prevalence, mean))

SBL.map <- rbind(CORR, CROC, CROM, LONG, TRIT) #Data frame
SBL.map <- cbind(SBL.map, prev_LeGi = prev.lake$Prevalence)
SBL.map <- cbind(SBL.map, SBL.lat, SBL.long)

# Background hydrologic data #
creeks <- st_read(paste0(to.carto, "Attribute_templates/Template_creeks.shp"))
lakes <- st_read(paste0(to.carto, "Attribute_templates/Template_lacs.shp"))
batiments <- st_point(st_read(dsn = "carto/Attribute_templates", layer = "Template_batiments_points.shp"))

crop.frame <- c(xmin = -74.022, 
                 ymin = 45.975,
                 xmax = -73.992,
                 ymax = 46.00)
cropped.creeks <- st_crop(creeks, crop.frame)
cropped.lakes <- st_crop(lakes, crop.frame)
cropped.batiments <- st_crop(batiments, crop.frame)

# Plot #
SBL.plot <- ggplot() + 
  geom_sf(data = cropped.lakes, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks, alpha = 0.5, color = "lightblue") +
  geom_sf(data = SBL.map, aes(fill = (prev_LeGi))) +
  theme(legend.position = c(0.14, 0.16),
        legend.text = element_text(color = "#4e4d47", size = 8),
        legend.title = element_text(color = "#4e4d47", size = 9, face = "bold"), 
        legend.background = element_rect(fill = NA, color = "#4e4d47", size = 0.4), 
        panel.background = element_rect(fill = "#f5f5f2", color = "black"), 
        panel.grid = element_line(NA), 
        axis.ticks = element_blank()) + 
  annotation_scale(location = "tr",
                   pad_x = unit(1, "cm"),
                   pad_y = unit(0.5, "cm"),
                   bar_cols = c("grey60", "white")) + 
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         pad_x = unit(2.3, "cm"), 
                         pad_y = unit(1, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "#4e4d47")) +
  scale_fill_continuous_sequential(palette = "YlOrBr") +
  geom_sf_label(data = SBL.map, 
                aes(label = NOM_ENTITE), 
                size = 3,
                position = position_nudge(x = -0.0008, y = -0.0008), 
                label.size = 0.2) +
  labs(fill = "Infection \nprevalence (%)", y = "", x = "")

SBL.plot

#### Study area cartography ####
## Attributes table ##
df.Lake <- CombinedData[c(1, 9:42)]
df.Lake <- df.Lake %>% #Sum abundances per lake
  group_by(Lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

lake.fish.tot <- df.Lake %>% select(starts_with("tot")) %>% rowSums() 
lake.fish.inf <- df.Lake %>% select(starts_with("inf")) %>% rowSums()
lake.fish.prev <- (lake.fish.inf/lake.fish.tot)*100

Lake.prev <- attributes %>% 
  group_by(Lake) %>% 
  summarise(across(.cols = prev_LeGi, mean, na.rm = TRUE)) #Lake mean prevalence

# Lake coordinates #
Lake.lat <- c("45 56 34", "45 55 30", "45 58 06", "45 52 53", "45 58 38", "45 59 34", "45 59 21", "45 53 14", "45 54 53", "45 55 20", "45 55 40", "45 57 39", "45 54 52", "45 55 38", "45 59 16") #GPS data from bathymetric maps
Lake.long <- c("-73 58 41", "-74 03 50", "-74 00 36", "-74 00 02", "-74 00 03", "-74 00 34", "-73 59 55", "-74 01 24", "-74 02 28", "-74 04 23", "-74 02 09", "-74 02 28", "-73 57 44", "-74 03 57", "-74 00 29") #GPS data from bathymetric maps

Lake.lat <- conv_unit(Lake.lat, from = "deg_min_sec", to = "dec_deg") #Coordinates conversion
Lake.lat <- as.numeric(Lake.lat)

Lake.long <- conv_unit(Lake.long, from = "deg_min_sec", to = "dec_deg")  #Coordinates conversion
Lake.long <- as.numeric(Lake.long)

# Data frame #
Lake.attributes <- data.frame(Lake.prev, Lake.lat, Lake.long)
Study.map <- rbind(ACHI, BEAV, COEU, CORN, CORR, CROC, CROM, ECHO, FOUR, MONT, MORE, PINR, STON, TRAC, TRIT)

Study.map2 <- Study.map %>% mutate(prev_LeGi = Lake.attributes$prev_LeGi) %>% mutate(prev_fish = lake.fish.prev) #Dataframe including mean prevalence data 

crop.frame2 <- c(xmin = -74.08, 
                ymin = 45.87,
                xmax = -73.94,
                ymax = 46.00)
cropped.creeks2 <- st_crop(creeks, crop.frame2)
cropped.lakes2 <- st_crop(lakes, crop.frame2)

# Bubble map #
Study.plot <- ggplot() + 
  geom_sf(data = Study.map, 
          fill = "lightblue") +
  theme_void() +
  geom_point(data = Lake.attributes, 
             aes(x = Lake.long, y = Lake.lat, size = prev_LeGi), 
             color = "chocolate", 
             alpha = 0.8) +
  labs(size = "Infection \nprevalence") +
  theme(plot.background = element_rect(fill = "#f5f5f2", color = "black"), 
        legend.position = c(0.87, 0.15),
        legend.text = element_text(color = "#4e4d47", size = 8), 
        legend.title = element_text(color = "#4e4d47", size = 9)) + 
  annotation_scale(location = "bl", 
                   bar_cols = c("grey60", "white")) + 
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         pad_x = unit(0.5, "cm"), 
                         pad_y = unit(0.3, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "#4e4d47")) +
  geom_sf_label(data = Study.map, 
               aes(label = NOM_ENTITE), 
               size = 3,
               position = position_nudge(x = -0.0008, y = -0.0008), 
               label.size = 0.2)
Study.plot

# LeGi Final map #
Study.plot2 <- ggplot() + 
  geom_sf(data = cropped.lakes2, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks2, color = "lightblue", alpha = 0.5) +
  geom_sf(data = Study.map2, aes(fill = (prev_LeGi*100))) +
  theme(legend.position = c(0.845, 0.17),
        legend.text = element_text(color = "#4e4d47", size = 8),
        legend.title = element_text(color = "#4e4d47", size = 9, face = "bold"), 
        legend.background = element_rect(fill = NA, color = "#4e4d47", size = 0.4), 
        panel.background = element_rect(fill = "#f5f5f2", color = "black"), 
        plot.background = element_rect(fill = "#FAFAFA"), 
        panel.grid = element_line(NA), 
        axis.ticks = element_blank()) +
  scale_fill_continuous_sequential(palette = "YlOrBr") +
  annotation_scale(location = "tl", 
                   bar_cols = c("grey60", "white")) + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true", 
                         pad_x = unit(0.5, "cm"), 
                         pad_y = unit(0.7, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "#4e4d47")) +
  labs(fill = "Infection \nprevalence (%)")
Study.plot2

# Fish Final map #
Study.plot2 <- ggplot() + 
  geom_sf(data = cropped.lakes2, fill = "lightblue", alpha = 0.5, color = "lightblue") +
  geom_sf(data = cropped.creeks2, color = "lightblue", alpha = 0.5) +
  geom_sf(data = Study.map2, aes(fill = prev_fish)) +
  theme(legend.position = c(0.845, 0.17),
        legend.text = element_text(color = "#4e4d47", size = 8),
        legend.title = element_text(color = "#4e4d47", size = 9, face = "bold"), 
        legend.background = element_rect(fill = NA, color = "#4e4d47", size = 0.4), 
        panel.background = element_rect(fill = "#f5f5f2", color = "black"),
        plot.background = element_rect(fill = "#FAFAFA"),
        panel.grid = element_line(NA), 
        axis.ticks = element_blank()) +
  scale_fill_continuous_sequential(palette = "YlOrBr") +
  scale_continuous_identity(aesthetics = c(0, 0.75)) +
  annotation_scale(location = "tl", 
                   bar_cols = c("grey60", "white")) + 
  annotation_north_arrow(location = "tl", 
                         which_north = "true", 
                         pad_x = unit(0.5, "cm"), 
                         pad_y = unit(0.7, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "#4e4d47")) +
  labs(fill = "Infection \nprevalence (%)")
Study.plot2
