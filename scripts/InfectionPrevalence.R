## Infection prevalence ##

# ----- R Setup ----- #

to.data <- "./data/"
to.script <- "./scripts/"
to.output <- "./output/"
to.figs <- "./figs/"
to.R <- "./R/"

# ----- Loading packages ----- #

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(writexl)
library(splitstackshape)

# ----- Loading data ----- #

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv")) 

# --------------------------- #

#### Prevalence per lake ####
LakesName <- c("Achigan", "Beaver","Coeur", "Cornu", "Corriveau", "Croche", "Cromwell", "Echo", "Fournelle", "Montaubois", "Morency", "Pin_rouge", "St-Onge", "Tracy", "Triton")
df.Lake <- CombinedData[c(1, 9:42)]

df.Lake <- df.Lake %>% #Sum abundances per lake
group_by(Lake) %>%
summarise(across(.cols = everything(), sum, na.rm = TRUE))

attach(df.Lake)
prev.Lake <- df.Lake %>% transmute(Lake = LakesName) %>%
  mutate(prev_AmRu = inf_AmRu/tot_AmRu) %>% 
  mutate(prev_FuDi = inf_FuDi/tot_FuDi) %>%
  mutate(prev_MiDo = inf_MiDo/tot_MiDo) %>%
  mutate(prev_Centrarchidae = inf_Centrarchidae/tot_Centrarchidae) %>%
  mutate(prev_LeGi = inf_LeGi/tot_LeGi) %>%
  mutate(prev_PeFl = inf_PeFl/tot_PeFl) %>%
  mutate(prev_PiPr = inf_PiPr/tot_PiPr) %>%
  mutate(prev_ChrosomusSp. = inf_ChrosomusSp./tot_ChrosomusSp.) %>%
  mutate(prev_PiNo = inf_PiNo/tot_PiNo) %>%
  mutate(prev_Cyprinidae = inf_Cyprinidae/tot_Cyprinidae) %>%
  mutate(prev_SeAt = inf_SeAt/tot_SeAt) %>%
  mutate(prev_LuCo = inf_LuCo/tot_LuCo) %>%
  mutate(prev_AmNe = inf_AmNe/tot_AmNe) %>%
  mutate(prev_CaCo = inf_CaCo/tot_CaCo) %>%
  mutate(prev_EsMa = inf_EsMa/tot_EsMa) %>%
  mutate(prev_UmLi = inf_UmLi/tot_UmLi) %>%
  mutate(prev_RhAt = inf_RhAt/tot_RhAt)

#### Prevalence per Sampling_ID ####
tot.matrix <- CombinedData[9:25]
inf.matrix <- CombinedData[26:42]
SamplingIDs <- CombinedData[2]

prev.matrix <- inf.matrix/tot.matrix 
prev.Names <- str_replace_all(colnames(prev.matrix), "inf_", '')
prev.Sampling <- `colnames<-`(prev.matrix, prev.Names)
prev.Sampling <- cbind(SamplingIDs, prev.Sampling)

#### LeGi prevalence per lake by method ####
## Minnow trap ##
MTdata <-  CombinedData %>% filter(Sampling_method == "Minnow_trap") #Selecting minnow trap data
MTLeGi <- MTdata[c(1, 13, 30)] #Selecting LeGi data

prev.MT <- MTLeGi %>% #Sum abundance per lake
  group_by(Lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

prev.MT <-  prev.MT %>% mutate(prev_LeGi = inf_LeGi/tot_LeGi) #Creating prevalence column

# Intra lake standard deviation #
sdMT <- MTLeGi %>% mutate(prev_LeGi2 = inf_LeGi/tot_LeGi) # Intra lake standard deviation
attach(sdMT)
sdMT <- tapply(prev_LeGi2, Lake , sd, na.rm= TRUE)

## Seine ##
Sdata <- CombinedData %>% filter(Sampling_method == "Seine")
SLeGi <- Sdata[c(1, 13, 30)]

prev.S <- SLeGi %>% 
  group_by(Lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

prev.S <-  prev.S %>% mutate(prev_LeGi = inf_LeGi/tot_LeGi)

# Intra lake standard deviation # 
sdS <- SLeGi %>% mutate(prev_LeGi2 = inf_LeGi/tot_LeGi)
attach(sdS)
sdS <- tapply(prev_LeGi2, Lake , sd, na.rm= TRUE)

## Transect ##
Tdata <-  CombinedData %>% filter(Sampling_method == "Transect")
TLeGi <- Tdata[c(1, 13, 30)]

prev.T <- TLeGi %>% 
  group_by(Lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

prev.T <-  prev.T %>% mutate(prev_LeGi = inf_LeGi/tot_LeGi)

# Intra lake standard deviation #
sdT <- TLeGi %>% mutate(prev_LeGi2 = inf_LeGi/tot_LeGi)
attach(sdT)
sdT <- tapply(prev_LeGi2, Lake , sd, na.rm= TRUE)

## Fishing ##
Fdata <- CombinedData %>% filter(Sampling_method == "Minnow_trap" | Sampling_method == "Seine")
FLeGi <- Fdata[c(1, 13, 30)]

prev.F <- FLeGi %>% 
  group_by(Lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

prev.F <-  prev.F %>% mutate(prev_LeGi = inf_LeGi/tot_LeGi)

# Intra lake standard deviation #
sdF <- FLeGi %>% mutate(prev_LeGi2 = inf_LeGi/tot_LeGi)
attach(sdF)
sdF <- tapply(prev_LeGi2, Lake , sd, na.rm= TRUE)

## All ##
ALeGi <- CombinedData[c(1, 13, 30)]

prev.A <- ALeGi %>% 
  group_by(Lake) %>%
  summarise(across(.cols = everything(), sum, na.rm = TRUE))

prev.A <-  prev.A %>% mutate(prev_LeGi = inf_LeGi/tot_LeGi)

# Intra lake standard deviation #
sdA <- ALeGi %>% mutate(prev_LeGi2 = inf_LeGi/tot_LeGi)
attach(sdA)
sdA <- tapply(prev_LeGi2, Lake , sd, na.rm= TRUE)

## Method comparison ##
PrevNames <- c("Lake", "Minnow_Trap", "Seine", "Transect", "Minnow_Trap+Seine", "All")
df.Prev <- cbind(prev.MT[c(1,4)], prev.S[4], prev.T[4], prev.F[4], prev.A[4])
df.Prev <- `colnames<-`(df.Prev, PrevNames)

write_xlsx(df.Prev, paste0(to.output, "LeGi_PrevMethod.xlsx")) #Exporting data frame
write.csv(df.Prev, paste0(to.output, "LeGi_PrevMethod.csv", row.names = FALSE))

df.plot <- pivot_longer(df.Prev, cols = 2:6, names_to = "Method", values_to = "Prevalence") #Preparing data frame for plotting 
df.plot <- arrange(df.plot, Method)
df.plot$sd <- c(sdA, sdMT, sdF, sdS, sdT)

attach(df.plot)
 plot.PrevMethod <- ggplot(df.plot) + 
  geom_point(aes(x = Lake, y = Prevalence, color = Method), size = 2, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(x = Lake, y = Prevalence, ymin = Prevalence - sd, ymax = Prevalence + sd, color = Method), width = 0.3, position = position_dodge(width = 0.5)) + 
  coord_cartesian(ylim = c(0, 1.2))
plot.PrevMethod
ggsave(paste0(to.figs, "Prevalence_Methods.png"), plot = last_plot(), dpi = 300, width = 20, height = 10)  

#### Overall prevalence by species ####
inf.sum <- colSums(inf.matrix, na.rm = TRUE)
tot.sum <- colSums(tot.matrix, na.rm = TRUE)
Sp.Names <- c("AmRu", "FuDi", "MiDo", "Cenrtrarchidae", "LeGi", "PeFl", "PiPr", "ChrosomusSp.", "PiNo", "Cyprinidae", "SeAt", "LuCo", "AmNe", "CaCo", "EsMa", "UmLi", "RhAt")

prev.overall <- data.frame(inf.sum/tot.sum, row.names = (Sp.Names))
prev.overall <- `colnames<-`(prev.overall, "Overall_Prevalence")

#### Regional prevalence

inf <- inf.matrix %>% 
  adorn_totals(where = c("row", "col"))

tot <- tot.matrix %>% 
  adorn_totals(where = c("row", "col"))
