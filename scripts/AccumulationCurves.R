## Script name : Accumulation curves

## Authors : Juliane Vigneault & Éric Harvey
## Date created : January 11, 2023

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
to.rédaction <- "./rédaction/"

## Loading packages ----

library(vegan)
library(ggplot2)
library(colorspace)
library(patchwork)
library(dplyr)
library(gt)
library(broom)
library(tidyr)
library(dunn.test)
library(scales)
library(tibble)

## Loading data ----

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))

# ---- Data preparation ----

CombinedData <- CombinedData %>% #Muskellunge and brown bullhead individuals are excluded from the prevalence calculus since they are not host of the black spot disease
  select(!(c(tot_EsMa, inf_EsMa, tot_AmNe, inf_AmNe)))

## Combined methods ----

Cdata <- CombinedData %>% 
  select(Lake, starts_with(c("inf", "tot"))) %>% #Select total and infected community matrix
  na.omit()

Cdata.inf <- Cdata %>% 
  select(starts_with("inf")) #Select infected community matrix

Cdata.tot <- Cdata %>% 
  select(starts_with("tot")) #Select total community matrix

#LeGi
CLeGi.inf <- Cdata %>% 
  select(inf_LeGi) 

CLeGi.tot <- Cdata %>% 
  select(tot_LeGi)

CLeGi.prev <- CLeGi.inf/CLeGi.tot 

CLeGi <- data.frame(CLeGi.tot, CLeGi.prev)
CLeGi <- na.omit(CLeGi)

#Infection prevalence data
Infected <- rowSums(Cdata.inf)
Total <- rowSums(Cdata.tot)
Prevalence <- Infected/Total
Lake <- Cdata$Lake

Cdata <- data.frame(Lake, Infected, Total, Prevalence) %>% 
  mutate(Method = "Combined", .after = "Lake")

## Minnow trap ----

MTdata <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") %>% #Select minnow trap method
  select("Lake", starts_with(c("inf", "tot"))) %>% #Select total and infected community matrix
  na.omit()

MTdata.inf <- MTdata %>% 
  select(starts_with("inf"))  #Select infected community matrix

MTdata.tot <- MTdata %>% 
  select(starts_with("tot")) #Select total community matrix

#LeGi
MTLeGi.inf <- MTdata %>% 
  select(inf_LeGi) 

MTLeGi.tot <- MTdata %>% 
  select(tot_LeGi)

MTLeGi.prev <- MTLeGi.inf/MTLeGi.tot 

MTLeGi <- data.frame(MTLeGi.tot, MTLeGi.prev)
MTLeGi <- na.omit(MTLeGi)

#Infection prevalence data
Infected <- rowSums(MTdata.inf)
Total <- rowSums(MTdata.tot)
Prevalence <- Infected/Total
Lake <- MTdata$Lake

MTdata <- data.frame(Lake, Infected, Prevalence, Total) %>% 
  mutate(Method = "Minnow trap", .after = "Lake")

## Seine net ----

Sdata <- CombinedData %>% 
  filter(Sampling_method == "Seine") %>% #Select seine method
  select("Lake", starts_with(c("inf", "tot"))) %>% #Select total and infected community matrix
  na.omit()

Sdata.inf <- Sdata %>% 
  select(starts_with("inf")) #Select infected community matrix

Sdata.tot <- Sdata %>% 
  select(starts_with("tot")) #Select total community matrix

#LeGi
SLeGi.inf <- Sdata %>% 
  select(inf_LeGi) 

SLeGi.tot <- Sdata %>% 
  select(tot_LeGi)

SLeGi.prev <- SLeGi.inf/SLeGi.tot 

SLeGi <- data.frame(SLeGi.tot, SLeGi.prev)
SLeGi <- na.omit(SLeGi)

#Infection prevalence data
Infected <- rowSums(Sdata.inf)
Total <- rowSums(Sdata.tot)
Prevalence <- Infected/Total
Lake <- Sdata$Lake

Sdata <- data.frame(Lake, Infected, Total, Prevalence) %>% 
  mutate(Method = "Seine net", .after = "Lake")

## Transect ----

Tdata <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% #Select transect method
  select("Lake", starts_with(c("inf", "tot"))) %>% #Select total and infected community matrix
  na.omit()

Tdata.inf <- Tdata %>% 
  select(starts_with("inf")) #Select infected community matrix

Tdata.tot <- Tdata %>% 
  select(starts_with("tot")) #Select total community matrix

#LeGi
TLeGi.inf <- Tdata %>% 
  select(inf_LeGi) 

TLeGi.tot <- Tdata %>% 
  select(tot_LeGi)

TLeGi.prev <- TLeGi.inf/TLeGi.tot 

TLeGi <- data.frame(TLeGi.tot, TLeGi.prev)
TLeGi <- na.omit(TLeGi)

#Infection prevalence data
Infected <- rowSums(Tdata.inf)
Total <- rowSums(Tdata.tot)
Prevalence <- Infected/Total
Lake <- Tdata$Lake

Tdata <- data.frame(Lake, Infected, Total, Prevalence) %>% 
  mutate(Method = "Transect", .after = "Lake")

# ---- Species accumulation cruves ----

## Minnow trap ----

MTdata.tot2 <- MTdata.tot %>% 
  select(!(tot_Centrarchidae)) %>% 
  select(!(tot_Cyprinidae))

acc.MT <- specaccum(MTdata.tot2, method = "random", permutations = 1000, gamma = "jack1") #Accumulation curve function
MT.plot <- plot(acc.MT, col = "#2A5676", xlab = "sampling", ylab = "species")

## Seine net ----

Sdata.tot2 <- Sdata.tot %>% 
  select(!(tot_Centrarchidae)) %>% 
  select(!(tot_Cyprinidae))

acc.S <- specaccum(Sdata.tot2, method = "random", permutations = 1000, gamma = "jack1") #Accumulation curve function
S.plot <- plot(acc.S, col = "#999600", xlab = "sampling", ylab = "species")

## Transect ----

acc.T <- specaccum(Tdata.tot, method = "random", permutations = 1000, gamma = "jack1") #Accumulation curve function
T.plot <- plot(acc.T, col = "#966F1E", xlab = "sampling", ylab = "species")

## Comparative plot ----

pdf(paste0(to.figs, "AccumulationCurves_species.pdf"), width = 15, height = 10)

plot(acc.S, col = "#999600", xlab = "sampling", ylab = "species", cex = 1)
plot(acc.MT, add = TRUE, col = "#2A5676", xlab = "sampling", ylab = "species")
plot(acc.T, add = TRUE, col = "#966F1E", xlab = "sampling", ylab = "species")

legend("bottomright", legend = c("Seine net", "Minnow trap", "Transect"),
       fill = c("#999600", "#2A5676", "#966F1E"))

dev.off()

# ---- Infected individuals accumulation curves ----

#Loop parameters
N <- 35 #Maximum number of samples (i)
Resampling <- 999 #Number of times each i is repeated 

## Combined methods ----

C.inf <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    Infected <- sum(sample(Cdata$Infected, i)) #Sample i lines randomly
    output <- data.frame(N = i, Resampling = j, Infected) #Save output in temporary data.frame (changed at each iterations)
    C.inf <- rbind(C.inf, output)
  }
}

C.inf <- C.inf %>% 
  mutate(Method = "Combined", .before = "N")

boxplot.inf.C <- boxplot(Infected ~ N, data = C.inf)
    
## Minnow trap ----

MT.inf <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    Infected <- sum(sample(MTdata$Infected, i)) #Sample i lines randomly
    output <- data.frame(N = i, Resampling = j, Infected) #Save output in temporary data.frame (changed at each iterations)
    MT.inf <- rbind(MT.inf, output)
  }
}

MT.inf <- MT.inf %>% 
  mutate(Method = "Minnow trap", .before = "N")

boxplot.inf.MT <- boxplot(Infected ~ N, data = MT.inf)

## Seine net ----

S.inf <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    Infected <- sum(sample(Sdata$Infected, i)) #Sample i lines randomly
    output <- data.frame(N = i, Resampling = j, Infected) #Save output in temporary data.frame (changed at each iterations)
    S.inf <- rbind(S.inf, output)
  }
}

S.inf <- S.inf %>% 
  mutate(Method = "Seine net", .before = "N")

boxplot.inf.S <- boxplot(Infected ~ N, data = S.inf)

## Transect ----

T.inf <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    Infected <- sum(sample(Tdata$Infected, i)) #Sample i lines randomly
    output <- data.frame(N = i, Resampling = j, Infected) #Save output in temporary data.frame (changed at each iterations)
    T.inf <- rbind(T.inf, output)
  }
}

T.inf <- T.inf %>% 
  mutate(Method = "Transect", .before = "N")

boxplot.inf.T <- boxplot(Infected ~ N, data = T.inf)

## Comparative plot ----

#Binding data
df.inf <- rbind(C.inf, MT.inf, S.inf, T.inf)

#Plot
inf.acc.plot <- ggplot(df.inf) + 
  stat_summary(aes(x = N, y = Infected, group = Method, color = Method, shape = Method), fun = mean, size = 1) +
  geom_smooth(aes(x= N, y = Infected, group = Method, color = Method, fill = Method), method = "lm", se = TRUE, level = 0.95,lineend = "round") +
  labs(x = "Number of samples", y = "Infected fish abundance") +
  scale_color_manual(values = c("#7E7E7E", "#2A5676", "#999600", "#966F1E"),
                     aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(0,5,2,1)) +
  guides(fill = guide_legend(override.aes = list(fill = NA, linetype = 0))) +
theme(text = element_text(size = 20, family = "Calibri Light", color = "black"),
      panel.background = element_blank(),
      legend.key = element_rect(fill = NA),
      axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
      axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.line.x = element_line(color = "black", lineend = "round"),
      axis.line.y = element_line(color = "black", lineend = "round"),
      plot.tag = element_text(face = "bold"))

inf.acc.plot

ggsave(paste0(to.figs, "AccumulationCurves_infection.png"), plot = inf.acc.plot, dpi = 300, width = 15, height = 10) #Saving plot  

# ---- Individuals accumulation curves ---- 

N <- 35 #Maximum number of samples (i)
Resampling <- 999 #Number of times each i is repeated 

## Combined methods ----

C.tot <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    Total <- sum(sample(Cdata$Total, i)) #Sample i lines randomly
    output <- data.frame(N = i, Resampling = j, Total) #Save output in temporary data.frame (changed at each iterations)
    C.tot <- rbind(C.tot, output)
  }
}

C.tot <- C.tot %>% 
  mutate(Method = "Combined", .before = "N")

boxplot.tot.C <- boxplot(Total ~ N, data = C.tot)

## Minnow trap ----

MT.tot <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    Total <- sum(sample(MTdata$Total, i)) #Sample i lines randomly
    output <- data.frame(N = i, Resampling = j, Total) #Save output in temporary data.frame (changed at each iterations)
    MT.tot <- rbind(MT.tot, output)
  }
}

MT.tot <- MT.tot %>% 
  mutate(Method = "Minnow trap", .before = "N")

boxplot.tot.MT <- boxplot(Total ~ N, data = MT.tot)

## Seine net ----

S.tot <- data.frame()

for(i in 1:N) {
  for(j in 1:Resampling) {
    Total <- sum(sample(Sdata$Total, i)) #Sample i lines randomly
    output <- data.frame(N = i, Resampling = j, Total) #Save output in temporary data.frame (changed at each iterations)
    S.tot <- rbind(S.tot, output)
  }
}

S.tot <- S.tot %>% 
  mutate(Method = "Seine net", .before = "N")

boxplot.tot.S <- boxplot(Total ~ N, data = S.tot)

## Transect method ----

T.tot <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    Total <- sum(sample(Tdata$Total, i)) #Sample i lines randomly
    output <- data.frame(N = i, Resampling = j, Total) #Save output in temporary data.frame (changed at each iterations)
    T.tot <- rbind(T.tot, output)
  }
}

T.tot <- T.tot %>% 
  mutate(Method = "Transect", .before = "N")

boxplot.tot.T <- boxplot(Total ~ N, data = T.tot)

## Comparative plot ----

#Binding data
df.tot <- rbind(C.tot, MT.tot, S.tot, T.tot)

#Plot
tot.acc.plot <- ggplot(df.tot) + 
  stat_summary(aes(x = N, y = Total, group = Method, color = Method, shape = Method), fun = mean, size = 1) +
  geom_smooth(aes(x= N, y = Total, group = Method, color = Method, fill = Method), method = "lm", se = TRUE, lineend = "round") +
  labs(x = "Number of samples", y = "Total fish abundance") +
  scale_color_manual(values = c("#7E7E7E", "#2A5676", "#999600", "#966F1E"),
                     aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(0, 5, 2, 1)) +
  guides(fill = guide_legend(override.aes = list(fill = NA, linetype = 0))) +
  theme(text = element_text(size = 20, family = "Calibri Light", color = "black"),
        panel.background = element_blank(),
        legend.key = element_rect(fill = NA),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line.x = element_line(color = "black", lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"),
        plot.tag = element_text(face = "bold"))

tot.acc.plot

ggsave(paste0(to.figs, "AccumulationCurves_individuals.png"), plot = tot.acc.plot, dpi = 300, width = 15, height = 10)  

# ---- Prevalence accumulation curves ----

N <- 35 #Maximum number of samples (i)
Resampling <- 999 #Number of times each i is repeated 

## Combined methods ----

C.prev <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    line <- sample(1:nrow(Cdata), i) #sample i lines randomly
    prop.samp <- Cdata[line, "Total"]/sum(Cdata[line, "Total"])
    w_prev <- Cdata[line, "Prevalence"]*prop.samp
    Prevalence <- sum(na.omit(w_prev))
    output <- data.frame(N = i, Resampling = j, Prevalence) #save output in temporary data.frame (changed at each iterations)
    C.prev <- rbind(C.prev, output)
  }
}

C.prev <- C.prev %>% 
  mutate(Method = "Combined", .before = "N")

boxplot.prev.C <- boxplot(Prevalence ~ N, data = C.prev)

## Minnow trap ----

MT.prev <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    line <- sample(1:nrow(MTdata), i) #sample i lines randomly
    prop.samp <- MTdata[line, "Total"]/sum(MTdata[line, "Total"])
    w_prev <- MTdata[line, "Prevalence"]*prop.samp
    Prevalence <- sum(na.omit(w_prev))
    output <- data.frame(N = i, Resampling = j, Prevalence) #save output in temporary data.frame (changed at each iterations)
    MT.prev <- rbind(MT.prev, output)
  }
}

MT.prev <- MT.prev %>% 
  mutate(Method = "Minnow trap", .before = "N")

boxplot.prev.MT <- boxplot(Prevalence ~ N, data = MT.prev)

## Seine ----

S.prev <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    line <- sample(1:nrow(Sdata), i) #sample i lines randomly
    prop.samp <- Sdata[line, "Total"]/sum(Sdata[line, "Total"])
    w_prev <- Sdata[line, "Prevalence"]*prop.samp
    Prevalence <- sum(na.omit(w_prev))
    output <- data.frame(N = i, Resampling = j, Prevalence) #save output in temporary data.frame (changed at each iterations)
    S.prev <- rbind(S.prev, output)
  }
}

S.prev <- S.prev %>% 
  mutate(Method = "Seine net", .before = "N")

boxplot.prev.S <- boxplot(Prevalence ~ N, data = S.prev)

## Transect ----

T.prev <- data.frame()

#Loop
for(i in 1:N) {
  for(j in 1:Resampling) {
    line <- sample(1:nrow(Tdata), i) #sample i lines randomly
    prop.samp <- Tdata[line, "Total"]/sum(Tdata[line, "Total"])
    w_prev <- Tdata[line, "Prevalence"]*prop.samp
    Prevalence <- sum(na.omit(w_prev))
    output <- data.frame(N = i, Resampling = j, Prevalence) #save output in temporary data.frame (changed at each iterations)
    T.prev <- rbind(T.prev, output)
  }
}

T.prev <- T.prev %>% 
  mutate(Method = "Transect", .before = "N")

boxplot.prev.T <- boxplot(Prevalence ~ N, data = T.prev)

## Comparative plots ----

df.prev <- rbind(C.prev, MT.prev, S.prev, T.prev)
df.prev$N <- as.factor(df.prev$N)
df.prev$Method <- as.factor(df.prev$Method)

### Boxplots ----

boxplot.prev.summary <- df.prev %>% 
  group_by(Method) %>% 
  ggplot(aes(y = Prevalence, x = N, color = Method)) +
  geom_boxplot(aes(fill = Method), alpha = 0.3) + 
  facet_wrap(vars(Method)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Number of samples", y = "Mean infection prevalence") +
  scale_color_manual(values = c("#7E7E7E", "#2A5676", "#999600", "#966F1E"),
                     aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(0, 5, 2, 1)) +
  theme(text = element_text(size = 20, family = "Helvetica", color = "black"),
        panel.background = element_blank(),
        legend.key = element_rect(fill = NA),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line.x = element_line(color = "black", lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"),
        plot.tag = element_text(face = "bold"))

boxplot.prev.summary

ggsave(paste0(to.figs, "AccumulationCurves_prevalence_boxplot.png"), plot = boxplot.prev.summary, dpi = 300, width = 15, height = 10)


### Tendency curves ----

prev.acc.plot <- ggplot() + 
  stat_summary(aes(x = N, y = Prevalence, group = Method, color = Method, shape = Method), fun = "mean", size = 0.5, data = df.prev) +
  #geom_point(aes(x = N, y = mean, group = Method, color = Method, shape = Method), data = prev.intrervals) +
  #geom_smooth(aes(x = N, y = mean, group = Method, color = Method, fill = Method), method = "loess", se = FALSE, lineend = "round", alpha = 0.3, data = prev.intrervals) +
  #geom_ribbon(aes(x = N, ymin = lower.bound, ymax = upper.bound, group = Method, color = Method, fill = Method), alpha = 0.3, data = prev.intrervals) +
  stat_smooth(aes(x = N, y = Prevalence, group = Method, color = Method, fill = Method), method = "loess", se = FALSE, level = 0.95, lineend = "round", alpha = 0.3, data = df.prev) +
  #geom_ribbon(aes(x = N, ymin = ymin*100, ymax = ymax*100, group = Method, color = Method), data = prev.intrervals) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(breaks = c(5,10,15,20,25,30,35), labels = c("5", "10", "15", "20", "25", "30", "35")) +
  labs(x = "Number of samples", y = "Mean infection prevalence") +
  scale_color_manual(values = c("#7E7E7E", "#2A5676", "#999600", "#966F1E"),
                     aesthetics = c("color", "fill")) +
  scale_shape_manual(values = c(0, 5, 2, 1)) +
  guides(fill = guide_legend(override.aes = list(fill = NA, linetype = 0))) +
  theme(text = element_text(size = 16, family = "Helvetica", color = "black"),
        plot.background = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.key = element_rect(fill = NA, colour = NA),
        legend.background = element_blank(),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line.x = element_line(color = "black", lineend = "round"),
        axis.line.y = element_line(color = "black", lineend = "round"))

prev.acc.plot

ggsave(paste0(to.figs, "AccumulationCurves_prevalence_RangeAxis.png"), plot = prev.acc.plot, dpi = 500, width = 10, height = 6.5)  
ggsave(paste0(to.rédaction, "Figures/Figure6_PrevRemsampling.png"), plot = prev.acc.plot, dpi = 300, width = 15, height = 10)

## Resampled mean values extraction ----

prev.resamp <- df.prev %>% #Selecting N samples for each method
  select(Method, N, Prevalence) %>% 
  group_by(Method) %>% 
  filter(N == "5" | N == "10"| N == "15" | N == "20" | N == "25" | N == "30" | N == "35" ) 

prev.resamp <- prev.resamp %>% #Extraction of mean prevalence values at each chosen N for each sampling method
  group_by(Method, N) %>% 
  summarise(across(.cols = Prevalence, mean))

prev.resamp$Prevalence <- (prev.resamp$Prevalence)*100 #Converting prevalence in percentage

prev.resamp <- prev.resamp %>% #Formating data frame
  pivot_wider(names_from = N, values_from = Prevalence) 
  
Observed = c(29.62, 19.46, 20.45, 35.55) #Adding observed prevalence colum
prev.resamp <- prev.resamp %>% 
  cbind(Observed = Observed)

#Comparative table
S2.S9 <- gt(prev.resamp, groupname_col = NA) %>% 
  tab_header(md("**TABLE S9.** Landscape observed and resampled prevalence estimated by each sampling method. All values are given in percentage. N stands for the number of according sampling effort.")) %>% 
  cols_label("5" = md("N<sub>5</sub>"), "10" = md("N<sub>10</sub>"), "15" = md("N<sub>15</sub>"), "20" = md("N<sub>20</sub>"), "25" = md("N<sub>25</sub>"), "30" = md("N<sub>30</sub>"), "35" = md("N<sub>35</sub>")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              table.width = pct(100)) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style= cell_borders(sides = c("bottom"), weight = px(2)), 
            location = list(cells_body(rows = 4))) %>% 
  fmt_number(decimals = 2, columns = c(2:8)) %>% 
  cols_move(columns = "Observed", after = "Method")

S2.S9

S2.S9 %>% #Saving gt tab
  gtsave("Tab_ResampledPrev_Methods.png", paste0(to.figs))
S2.S9 %>% 
  gtsave("AppendixS2_TableS9.png", paste0(to.rédaction, "./Support_information/"))

## Confidence intervals  ----

#Combined
C.prev$Prevalence <- C.prev$Prevalence*100

C.intervals <- C.prev %>% 
  group_by(N) %>% 
  summarise(mean = mean(Prevalence),
            sd = sd(Prevalence)) %>% 
  mutate(Method = "Combined")

C.intervals <- C.intervals %>% 
  mutate(se = sd/sqrt(999)) %>% 
  mutate(t.score = qt(p=0.05/2, df=998,lower.tail=F)) %>% 
  mutate(margin.error = t.score * se) %>% 
  mutate(lower.bound = mean - margin.error) %>% 
  mutate(upper.bound = mean + margin.error)

#Minnow trap 
MT.prev$Prevalence <- MT.prev$Prevalence*100

MT.intervals <- MT.prev %>% 
  group_by(N) %>% 
  summarise(mean = mean(Prevalence),
            sd = sd(Prevalence)) %>% 
  mutate(Method = "Minnow trap")

MT.intervals <- MT.intervals %>% 
  mutate(se = sd/sqrt(999)) %>% 
  mutate(t.score = qt(p=0.05/2, df=998,lower.tail=F)) %>% 
  mutate(margin.error = t.score * se) %>% 
  mutate(lower.bound = mean - margin.error) %>% 
  mutate(upper.bound = mean + margin.error)

#Seine
S.prev$Prevalence <- S.prev$Prevalence*100

S.intervals <- S.prev %>% 
  group_by(N) %>% 
  summarise(mean = mean(Prevalence),
            sd = sd(Prevalence)) %>% 
  mutate(Method = "Seine net")

S.intervals <- S.intervals %>% 
  mutate(se = sd/sqrt(999)) %>% 
  mutate(t.score = qt(p=0.05/2, df=998,lower.tail=F)) %>% 
  mutate(margin.error = t.score * se) %>% 
  mutate(lower.bound = mean - margin.error) %>% 
  mutate(upper.bound = mean + margin.error)

#Transect
T.prev$Prevalence <- T.prev$Prevalence*100

T.intervals <- T.prev %>% 
  group_by(N) %>% 
  summarise(mean = mean(Prevalence),
            sd = sd(Prevalence)) %>% 
  mutate(Method = "Transect")

T.intervals <- T.intervals %>% 
  mutate(se = sd/sqrt(999)) %>% 
  mutate(t.score = qt(p=0.05/2, df=998,lower.tail=F)) %>% 
  mutate(margin.error = t.score * se) %>% 
  mutate(lower.bound = mean - margin.error) %>% 
  mutate(upper.bound = mean + margin.error)

#Table
prev.intrervals <- rbind(C.intervals, MT.intervals, S.intervals, T.intervals)

prev.intrervals <- prev.intrervals %>% 
  relocate(Method, .before = N)

S2.S10 <- gt(prev.intrervals, groupname_col = "Method") %>% 
  tab_header(md("**TABLE S10.** Summary statistics of 999 resampling analyses. All values are given in percentage. N stands for the number of sampling effort implemented in the random resampling loop. Values are grouped according to sampling method")) %>% 
  cols_label("t.score" = md("t score"), "margin.error" = md("margin error"), "lower.bound" = md("lower bound"), "upper.bound" = md("upper bound")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_options(table.border.top.style = "hidden",
              heading.border.bottom.color = "black",
              row.striping.include_table_body = TRUE,
              page.orientation = "paysage",
              table.width = pct(100)) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>% 
  tab_style(style= cell_borders(sides = c("bottom"), weight = px(2)), 
            location = list(cells_body(rows = 140))) %>% 
  fmt_number(decimals = 2) %>% 
  fmt_number(decimals = 0, columns = 2)

S2.S10

S2.S10 %>% #Saving gt tab
  gtsave("Tab_ResampledStats.png", paste0(to.figs))
S2.S10 %>% 
  gtsave("AppendixS2_TableS10.png", paste0(to.rédaction, "./Support_information/"))

## Minimum sampling effort (N) ----

## Comparison final (N = 35) and observed prevalence ----

CompFinal <- df.prev %>% 
  filter(N == "35")

### Combined methods ----

CompFinal.C <- CompFinal %>% 
  filter(Method == "Combined")

wilcox.test(CompFinal.C$Prevalence, Cdata$Prevalence, alternative = "greater", paired = FALSE)
#N35 prevalence is not different than observed prevalence

### Minnow trap ----

CompFinal.MT <- CompFinal %>% 
  filter(Method == "Minnow trap")

wilcox.test(CompFinal.MT$Prevalence, MTdata$Prevalence, alternative = "greater", paired = FALSE)
#N35 prevalence is not different than observed prevalence

### Seine net ----

CompFinal.S <- CompFinal %>% 
  filter(Method == "Seine net")

wilcox.test(CompFinal.S$Prevalence, Sdata$Prevalence, alternative = "greater", paired = FALSE)
#N35 prevalence is not different than observed prevalence

### Transect ----

CompFinal.T <- CompFinal %>% 
  filter(Method == "Transect")

wilcox.test(CompFinal.T$Prevalence, Tdata$Prevalence, alternative = "greater", paired = FALSE)
#N35 prevalence is not different than observed prevalence

## Comparison final (N = 35) prevalence between methods ----

kruskal.test(CompFinal$Prevalence, CompFinal$Method)
#Significant difference of methods distribution

dunn.resamp <- dunn.test(CompFinal$Prevalence, CompFinal$Method, method = "bh")
#Difference between all methods

#Table 
resamp.method.dunn <- data.frame(cbind("Comparison" = dunn.resamp$comparison, "Adjusted p-value" = dunn.resamp$P.adjusted))
resamp.method.dunn$Adjusted.p.value <- as.numeric(resamp.method.dunn$Adjusted.p.value)

S2.S8 <- gt(resamp.method.dunn) %>% 
  cols_label(Adjusted.p.value = md("Adjusted p-value")) %>% 
  tab_options(table.border.top.style = "hidden",
              row.striping.include_table_body = TRUE,
              heading.border.bottom.color = "black") %>% 
  tab_header(md("**TABLE S8.** Method comparison of landscape final resampled prevalence estimates (N =35). Kruskal-Wallis chi-squared = 1288, p-value = 0. A Benjamini-Hochberg correction is applied on the dunn test.")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
            locations = cells_body()) %>% 
  tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
            location = list(cells_column_labels())) %>%           
  tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "black"),
            locations =  cells_body(rows = 6)) %>% 
  fmt_number(decimals = 3, columns = 2)

S2.S8

S2.S8 %>% #Saving gt tab
  gtsave("Tab_Resampled35_Methods.png", paste0(to.figs))
S2.S8 %>% 
  gtsave("AppendixS2_TableS8.png", paste0(to.rédaction, "./Support_information/"))

##Comparison of observed prevalence by methods ----

kruskal.test(CompFinal$Prevalence, CompFinal$Method)
dunn.obs <- dunn.test(CompFinal$Prevalence, CompFinal$Method, method = "bh")

obs.prev.compared <- data.frame(cbind("Comparison" = dunn.obs$comparison, "Adjusted p-value" = dunn.obs$P.adjusted))

obs.prev.compared.tbl <- gt(obs.prev.compared) %>% 
  cols_label(Adjusted.p.value = md("Adjusted p-value")) %>% 
  tab_options(table.border.top.style = "hidden",
              row.striping.include_table_body = TRUE,
              heading.border.bottom.color = "black") %>% 
  tab_header(md("**TABLE SX.** Method comparison of landscape prevalence estimates")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri Light", size = 9, align = "left"),
            locations = cells_title("title")) %>% 
  tab_style(cell_text(color = "black", font = "Calibri light", weight = "bold", size = 9, align = "center", v_align = "middle"),
            locations = cells_column_labels()) %>% 
    tab_style(cell_text(color = "black", font = "Calibri light", size = 9, align = "center", v_align = "middle"),
              locations = cells_body()) %>% 
    tab_style(style= cell_borders(sides = c("bottom", "top"), weight = px(2)), 
              location = list(cells_column_labels())) %>%           
    tab_style(style = cell_borders(sides = "bottom", weight = px(2), color = "black"),
              locations =  cells_body(rows = 6))
            
# ---- Summary accumulation panel ----

summary.acc.plot <- inf.acc.plot + tot.acc.plot + prev.acc.plot +
  plot_layout(ncol = 3,
              nrow = 1, 
              guides = "collect",
              tag_level = "new") &
  theme(legend.position = "bottom",
        text = element_text(size = 32, family = "Calibri Light", color = "black"),
        axis.title.x = element_text(margin = unit(c(10, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 10, 0, 0), "mm")),
        legend.margin = margin(unit(c(25, 0, 0, 0), "mm")),
        legend.key.size = unit(20, "mm")) &
  guides(color = guide_legend(override.aes = list(size = 2)))

summary.acc.plot 

ggsave(paste0(to.figs, "AccumulationCurves_summary.png"), plot = summary.acc.plot, dpi = 300, width = 30, height = 12)

