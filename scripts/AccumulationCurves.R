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

## Loading data ----

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))

# ---- Data preparation ----

## Combined methods ----

Cdata <- CombinedData %>% 
  select(Lake, starts_with(c("inf", "tot"))) %>% #Select total and infected community matrix
  na.omit()

Cdata.inf <- Cdata %>% 
  select(starts_with("inf")) #Select infected community matrix

Cdata.tot <- Cdata %>% 
  select(starts_with("tot")) #Select total community matrix

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

#Infection prevalence data
Infected <- rowSums(Tdata.inf)
Total <- rowSums(Tdata.tot)
Prevalence <- Infected/Total
Lake <- Tdata$Lake

Tdata <- data.frame(Lake, Infected, Total, Prevalence) %>% 
  mutate(Method = "Transect", .after = "Lake")

# ---- Species accumulation cruves ----

## Minnow trap ----

acc.MT <- specaccum(MTdata.tot, method = "random", permutations = 1000, gamma = "jack1") #Accumulation curve function
MT.plot <- plot(acc.MT, col = "#2A5676", xlab = "sampling", ylab = "species")

## Seine net ----

acc.S <- specaccum(Sdata.tot, method = "random", permutations = 1000, gamma = "jack1") #Accumulation curve function
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

## Seine method ----

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

## Transect method ----

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

boxplot.prev.summary

ggsave(paste0(to.figs, "AccumulationCurves_prevalence_boxplot.png"), plot = boxplot.prev.summary, dpi = 300, width = 15, height = 10)  

### Tendency curves ----

prev.acc.plot <- ggplot(df.prev) + 
  stat_summary(aes(x = N, y = Prevalence, group = Method, color = Method, shape = Method), fun = "mean", size = 1) +
  stat_smooth(aes(x= N, y = Prevalence, group = Method, color = Method, fill = Method), method = "loess", se = TRUE, level = 0.95, lineend = "round", alpha = 0.3) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Number of samples", y = "Mean infection prevalence") +
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
        axis.line.y = element_line(color = "black", lineend = "round"))

prev.acc.plot

ggsave(paste0(to.figs, "AccumulationCurves_prevalence_RangeAxis.png"), plot = prev.acc.plot, dpi = 300, width = 15, height = 10)  
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
  
Observed = c(29.55, 19.20, 20.44, 35.55) #Adding observed prevalence colum
prev.resamp <- prev.resamp %>% 
  cbind(Observed = Observed)

#Comparative table
Table.S15 <- gt(prev.resamp, groupname_col = NA) %>% 
  tab_header(md("**TABLE S15.** Landscape observed and resampled prevalence estimated by each sampling method. All values are given in percentage.")) %>% 
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

Table.S15 %>% #Saving gt tab
  gtsave("Tab_ResampledPrev_Methods.png", paste0(to.figs))
Table.S15 %>% 
  gtsave("Table_S15.png", paste0(to.rédaction, "./Support_information/"))

## Minimum sampling effort (N) ----

prev.test.data <- df.prev %>%
  group_by(Method) %>% 
  filter(N =="1" | N == "5" | N == "10"| N == "15" | N == "20" | N == "25" | N == "30" | N == "35" ) 

library(dunn.test)

### Combined ----

prev.test.C <- prev.test.data %>% 
  filter(Method == "Combined")

hist(prev.test.C$Prevalence)

dunn.test(prev.test.C$Prevalence, prev.test.C$N, method = "bh")

### Minnow trap ----

prev.test.MT <- prev.test.data %>% 
  filter(Method == "Minnow trap")

hist(prev.test.MT$Prevalence)

dunn.test(prev.test.MT$Prevalence, prev.test.MT$N, method = "bh")

### Seine net ----

prev.test.S <- prev.test.data %>% 
  filter(Method == "Seine net")

hist(prev.test.S$Prevalence)

dunn.test(prev.test.S$Prevalence, prev.test.S$N)

### Transect ----

prev.test.T <- prev.test.data %>% 
  filter(Method == "Transect")

hist(prev.test.T$Prevalence)

dunn.test(prev.test.T$Prevalence, prev.test.T$N, method = "bh")

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

