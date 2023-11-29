
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

#### SBL cartography ####

TRIT <- st_read(paste0(to.carto, "Lake_shapes/Triton.shp"))
LONG <- st_read(paste0(to.carto, "Lake_shapes/Long.shp"))
CORR <- st_read(paste0(to.carto, "Lake_shapes/Corriveau.shp"))
CROC <- st_read(paste0(to.carto, "Lake_shapes/Croche.shp"))
CROM <- st_read(paste0(to.carto, "Lake_shapes/Cromwell.shp"))

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
  #geom_sf(data = SBL.map, aes(fill = (prev_LeGi))) +
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
                         pad_x = unit(2.5, "cm"), 
                         pad_y = unit(1, "cm"), 
                         style = north_arrow_nautical(fill = c("grey60", "white"), line_col = "#4e4d47")) #+
  #scale_fill_continuous_sequential(palette = "YlOrBr") +
  #geom_sf_label(data = SBL.map, 
   #             aes(label = NOM_ENTITE), 
    #            size = 3,
     #           position = position_nudge(x = -0.0008, y = -0.0008), 
      #          label.size = 0.2) #+
  #labs(fill = "Infection \nprevalence (%)", y = "", x = "")

SBL.plot

ggsave(paste0(to.figs, "MapSBL.png"), plot = SBL.plot, dpi = 300, width = 10, height = 15, units = "cm")
