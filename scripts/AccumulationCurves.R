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

## Loading data ----

CombinedData <- read.csv(paste0(to.output, "CombinedData.csv"))

# ---- Data preparation ----

## All ----
Adata <- CombinedData %>% 
  select("Lake", starts_with(c("inf", "tot"))) %>% 
  na.omit()

Adata.inf <- Adata %>% 
  select(starts_with("inf")) 

Adata.tot <- Adata %>% 
  select(starts_with("tot"))

Infected <- rowSums(Adata.inf)
Total <- rowSums(Adata.tot)
Prevalence <- Infected/Total
Lake <- Adata$Lake

Adata <- data.frame(Lake, Infected, Total, Prevalence) %>% 
  mutate(Method = "All", .after = "Lake")

## Minnow trap ----

MTdata <- CombinedData %>% 
  filter(Sampling_method == "Minnow_trap") %>% 
  select("Lake", starts_with(c("inf", "tot"))) %>% 
  na.omit()

MTdata.inf <- MTdata %>% 
  select(starts_with("inf")) 

MTdata.tot <- MTdata %>% 
  select(starts_with("tot"))

Infected <- rowSums(MTdata.inf)
Total <- rowSums(MTdata.tot)
Prevalence <- Infected/Total
Lake <- MTdata$Lake

MTdata <- data.frame(Lake, Infected, Prevalence, Total) %>% 
  mutate(Method = "Minnow trap", .after = "Lake")

## Seine net ----

Sdata <- CombinedData %>% 
  filter(Sampling_method == "Seine") %>% 
  select("Lake", starts_with(c("inf", "tot"))) %>% 
  na.omit()

Sdata.inf <- Sdata %>% 
  select(starts_with("inf")) 

Sdata.tot <- Sdata %>% 
  select(starts_with("tot"))

Infected <- rowSums(Sdata.inf)
Total <- rowSums(Sdata.tot)
Prevalence <- Infected/Total
Lake <- Sdata$Lake

Sdata <- data.frame(Lake, Infected, Total, Prevalence) %>% 
  mutate(Method = "Seine net", .after = "Lake")

## Transect ----

Tdata <- CombinedData %>% 
  filter(Sampling_method == "Transect") %>% 
  select("Lake", starts_with(c("inf", "tot"))) %>% 
  na.omit()

Tdata.inf <- Tdata %>% 
  select(starts_with("inf")) 

Tdata.tot <- Tdata %>% 
  select(starts_with("tot"))

Infected <- rowSums(Tdata.inf)
Total <- rowSums(Tdata.tot)
Prevalence <- Infected/Total
Lake <- Tdata$Lake

Tdata <- data.frame(Lake, Infected, Total, Prevalence) %>% 
  mutate(Method = "Transect", .after = "Lake")

# ---- Species accumulation cruves ----

## Minnow traps ----

acc.MT <- specaccum(MTdata.tot, method = "random", permutations = 1000, gamma = "jack1") #Accumulation curve function
MT.plot <- plot(acc.MT, col = "#2A5676", xlab = "sampling", ylab = "species")

## Seine net ----

acc.S <- specaccum(Sdata.tot, method = "random", permutations = 1000, gamma = "jack1") #Accumulation curve function
S.plot <- plot(acc.S, col = "#999600", xlab = "sampling", ylab = "species")

## Transect ----

acc.T <- specaccum(Tdata.tot, method = "random", permutations = 1000, gamma = "jack1") #Accumulation curve function
T.plot <- plot(acc.T, col = "#966F1E", xlab = "sampling", ylab = "species")

## Comparison plot ----

pdf(paste0(to.figs, "AccumulationCurves_species.pdf"), width = 15, height = 10)

plot(acc.S, col = "#999600", xlab = "sampling", ylab = "species", cex = 1)
plot(acc.MT, add = TRUE, col = "#2A5676", xlab = "sampling", ylab = "species")
plot(acc.T, add = TRUE, col = "#966F1E", xlab = "sampling", ylab = "species")

legend("bottomright", legend = c("Seine", "Minnow Trap", "Transect"),
       fill = c("#999600", "#2A5676", "#966F1E"))

dev.off()

# ---- Infected individuals accumulation curves ----

#Simulation parameters
N <- 35 #nb of samples (i)
Resampling <- 999 #nb of times each i is repeated 

## All methods ----

A.inf <- data.frame()

for(i in 1:N) {
  
  for(j in 1:Resampling) {
    Infected <- sum(sample(Adata$Infected, i)) #sample i lines randomly
    
    output <- data.frame(N = i, Resampling = j, Infected) #save output in temporary data.frame (changed at each iterations)
    
    A.inf <- rbind(A.inf, output)
    
  }

}

A.inf <- A.inf %>% 
  mutate(Method = "All", .before = "N")

boxplot(Infected ~ N, data = A.inf)
    
## Minnow trap ----

MT.inf <- data.frame()

for(i in 1:N) {
  
  for(j in 1:Resampling) {
    Infected <- sum(sample(MTdata$Infected, i)) #sample i lines randomly
    
    output <- data.frame(N = i, Resampling = j, Infected) #save output in temporary data.frame (changed at each iterations)
    
    MT.inf <- rbind(MT.inf, output)
    
  }
  
}

MT.inf <- MT.inf %>% 
  mutate(Method = "Minnow trap", .before = "N")

boxplot(Infected ~ N, data = MT.inf)

## Seine method ----

S.inf <- data.frame()

for(i in 1:N) {
  
  for(j in 1:Resampling) {
    Infected <- sum(sample(Sdata$Infected, i)) #sample i lines randomly
    
    output <- data.frame(N = i, Resampling = j, Infected) #save output in temporary data.frame (changed at each iterations)
    
    S.inf <- rbind(S.inf, output)
    
  }
  
}

S.inf <- S.inf %>% 
  mutate(Method = "Seine net", .before = "N")

boxplot(Infected ~ N, data = S.inf)

## Transect method ----

T.inf <- data.frame()

for(i in 1:N) {
  
  for(j in 1:Resampling) {
    Infected <- sum(sample(Tdata$Infected, i)) #sample i lines randomly
    
    output <- data.frame(N = i, Resampling = j, Infected) #save output in temporary data.frame (changed at each iterations)
    
    T.inf <- rbind(T.inf, output)
    
  }
  
}

T.inf <- T.inf %>% 
  mutate(Method = "Transect", .before = "N")

boxplot(Infected ~ N, data = T.inf)

## Plotting simulation ----

col.pal <- c("#7E7E7E", "#2A5676", "#999600", "#966F1E")
#col.pal4 <- c("#7E7E7E", "#005260", "#A4473D", "#A57E00")

#Binding data
df.inf <- rbind(A.inf, MT.inf, S.inf, T.inf)

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

ggsave(paste0(to.figs, "AccumulationCurves_infection.png"), plot = inf.acc.plot, dpi = 300, width = 15, height = 10)  

# ---- Individuals accumulation curves ---- 

## All methods ----

A.tot <- data.frame()

for(i in 1:N) {
  
  for(j in 1:Resampling) {
    Total <- sum(sample(Adata$Total, i)) #sample i lines randomly
    
    output <- data.frame(N = i, Resampling = j, Total) #save output in temporary data.frame (changed at each iterations)
    
    A.tot <- rbind(A.tot, output)
    
  }
  
}

A.tot <- A.tot %>% 
  mutate(Method = "All", .before = "N")

boxplot(Total ~ N, data = A.tot)

## Minnow trap method ----

MT.tot <- data.frame()

for(i in 1:N) {
  
  for(j in 1:Resampling) {
    Total <- sum(sample(MTdata$Total, i)) #sample i lines randomly
    
    output <- data.frame(N = i, Resampling = j, Total) #save output in temporary data.frame (changed at each iterations)
    
    MT.tot <- rbind(MT.tot, output)
    
  }
  
}

MT.tot <- MT.tot %>% 
  mutate(Method = "Minnow trap", .before = "N")

boxplot(Total ~ N, data = MT.tot)

## Seine method ----

S.tot <- data.frame()

for(i in 1:N) {
  
  for(j in 1:Resampling) {
    Total <- sum(sample(Sdata$Total, i)) #sample i lines randomly
    
    output <- data.frame(N = i, Resampling = j, Total) #save output in temporary data.frame (changed at each iterations)
    
    S.tot <- rbind(S.tot, output)
    
  }
  
}

S.tot <- S.tot %>% 
  mutate(Method = "Seine net", .before = "N")

boxplot(Total ~ N, data = S.tot)

## Transect method ---

T.tot <- data.frame()

for(i in 1:N) {
  
  for(j in 1:Resampling) {
    
    Total <- sum(sample(Tdata$Total, i)) #sample i lines randomly
    
    output <- data.frame(N = i, Resampling = j, Total) #save output in temporary data.frame (changed at each iterations)
    
    T.tot <- rbind(T.tot, output)
    
  }
  
}

T.tot <- T.tot %>% 
  mutate(Method = "Transect", .before = "N")

boxplot(Total ~ N, data = T.tot)

## Plotting simulation ----

df.tot <- rbind(A.tot, MT.tot, S.tot, T.tot)

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

ggsave(paste0(to.figs, "AccumulationCurves_individuals.png"), plot = tot.acc.plot, dpi = 300, width = 15, height = 10)  

# ---- Prevalence accumulation curves ----

## All ----

A.prev <- data.frame()

for(i in 1:N) {
  
  for(j in 1:Resampling) {
    
    line <- sample(1:nrow(Adata), i) #sample i lines randomly
    prop.samp <- Adata[line, "Total"]/sum(Adata[line, "Total"])
    w_prev <- Adata[line, "Prevalence"]*prop.samp
    Prevalence <- sum(na.omit(w_prev))
    
    output <- data.frame(N = i, Resampling = j, Prevalence) #save output in temporary data.frame (changed at each iterations)
    
    A.prev <- rbind(A.prev, output)
    
  }
  
}

A.prev <- A.prev %>% 
  mutate(Method = "All", .before = "N")

boxplot(Prevalence ~ N, data = A.prev)



## Minnow trap ----

MT.prev <- data.frame()

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

boxplot(Prevalence ~ N, data = MT.prev)


## Seine method ----

S.prev <- data.frame()

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

boxplot(Prevalence ~ N, data = S.prev)

## Transect method ----

T.prev <- data.frame()

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

boxplot(Prevalence ~ N, data = T.prev)

## Plotting simulation ----

df.prev <- rbind(A.prev, MT.prev, S.prev, T.prev)
df.prev$N <- as.factor(df.prev$N)
df.prev$Method <- as.factor(df.prev$Method)

box.plot <- df.prev %>% 
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

ggsave(paste0(to.figs, "AccumulationCurves_prevalence_boxplot.png"), plot = box.plot, dpi = 300, width = 15, height = 10)  


#library(plyr)
#df.mean <- ddply(df.prev, c("Method", "N"), summarise,
 #     n    = length(Prevalence),
  #    mean = mean(Prevalence),
   #   sd   = sd(Prevalence),
    #  se   = sd / sqrt(n),
     # df = n-1,
     # t.score = qt(p = 0.05/2, df = df, lower.tail = FALSE),
    #  margin = t.score*se,
    #  lower = mean-margin,
     # upper = mean+margin)

#df.mean$N <- as.numeric(df.mean$N)

#prev.mean.plot <- ggplot(df.mean) +
 # geom_point(aes(x = N, y = mean, group = Method, color = Method, shape = Method), size = 1) +
  #geom_smooth(aes(x = N, y = mean, group = Method, color = Method, fill = Method), se = FALSE) +
  #geom_ribbon(aes(x = N, ymin = lower, ymax = upper, color = Method, fill = Method), alpha = 0.3, lineend = "round") + 
  #scale_y_continuous(labels = scales::percent) +
  #labs(x = "Number of samples", y = "Mean infection prevalence") +
  #scale_color_manual(values = c("#7E7E7E", "#2A5676", "#999600", "#966F1E"),
   #                  aesthetics = c("color", "fill")) +
  #scale_shape_manual(values = c(0, 5, 2, 1)) +
  #guides(fill = guide_legend(override.aes = list(fill = NA, linetype = 0))) +
  #theme(text = element_text(size = 20, family = "Calibri Light", color = "black"),
   #     panel.background = element_blank(),
    #    legend.key = element_rect(fill = NA),
     #   axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
      #  axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
       # axis.text.x = element_text(color = "black"),
        #axis.text.y = element_text(color = "black"),
        #axis.line.x = element_line(color = "black", lineend = "round"),
        #axis.line.y = element_line(color = "black", lineend = "round"))

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

ggsave(paste0(to.figs, "AccumulationCurves_prevalence.png"), plot = prev.acc.plot, dpi = 300, width = 15, height = 10)  
ggsave(paste0(to.rédaction, "Figures/Figure3_PrevSimulations.png"), plot = prev.acc.plot, dpi = 300, width = 15, height = 10)

# ---- Summary figure ----

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

ggsave(paste0(to.figs, "AccumulationCurves_summary.png"), plot = summary.acc.plot, dpi = 300, width = 30, height = 12)

## Extraction of final prevalence value ----

### All ----

prev.stab.A <- df.prev %>% 
  filter(Method == "All") %>% 
  filter(N == "35")

prev.stab.A <- mean(prev.stab.A$Prevalence)

### Minnow trap ----

prev.stab.MT <- df.prev %>% 
  filter(Method == "Minnow trap") %>% 
  filter(N == "35")

prev.stab.MT <- mean(prev.stab.MT$Prevalence)

### Seine net ----

prev.stab.S <- df.prev %>% 
  filter(Method == "Seine net") %>% 
  filter(N == "35")

prev.stab.S <- mean(prev.stab.S$Prevalence)

### Transect ----

prev.stab.T <- df.prev %>% 
  filter(Method == "Transect") %>% 
  filter(N == "35")

prev.stab.T <- mean(prev.stab.T$Prevalence)
