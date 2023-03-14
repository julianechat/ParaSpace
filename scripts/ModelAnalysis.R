# ---- Script setup ----
## R Setup ----

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"
to.carto <- "./carto/"

## Loading packages ----

## Loading data ----
CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))


# ---- Data exploration ----
## Cleaning data ----

lake.scale.fish <- CombinedData %>% filter(!Sampling_method == "Transect")

lake.exp.data <- lake.scale.fish %>% 
  group_by(Lake) %>% 
  select(c(1, 43:50, 51:62)) %>% 
  distinct() 

lake.comm.matrix <- CombinedData %>% 
  group_by(Lake) %>% 
  select(starts_with("tot") | starts_with("inf")) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))

lake_scale <- data.frame(lake.comm.matrix, lake.exp.data)[-36] 

## Exploration of variables ##
# Response variables #
lake.fish.tot <- lake.comm.matrix %>% select(starts_with("tot")) %>% rowSums()
lake.fish.inf <- lake.comm.matrix %>% select(starts_with("inf")) %>% rowSums()

lake.prev.fish = (lake.fish.inf) / (lake.fish.tot)
lake.prev.LeGi = (lake.comm.matrix$inf_LeGi) / (lake.comm.matrix$tot_LeGi)
lake.prev.df = as.data.frame(cbind(Lake = LakesName, Prev_fish = lake.prev.fish, Prev_LeGi = lake.prev.LeGi))

## Outliers ----

## Collinearity ----

## Relationships ----

# ---- Data analysis ----
## 



