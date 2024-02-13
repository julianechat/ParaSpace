library(ggplot2)
Cplot <- ggplot() +
  geom_point(aes(x = tot_fish, y = prev_fish), color = "grey", shape = 1,  data = Site.pool.prev.C) +
  geom_smooth(aes(x = tot_fish, y = prev_fish), method = "lm", se = FALSE, color = "grey", data = Site.pool.prev.C) +
  scale_y_continuous(limits = c(0,100))
Tplot <- ggplot() +
  geom_point(aes(x = tot_fish, y = prev_fish), color = "brown", shape = 2, data = Site.pool.prev.T) +
  geom_smooth(aes(x = tot_fish, y = prev_fish), method = "lm", se = FALSE, color = "brown", data = Site.pool.prev.T) +
  scale_y_continuous(limits = c(0,100))
MTplot <- ggplot() +
  geom_point(aes(x = tot_fish, y = prev_fish), color = "skyblue", shape = 3, data = Site.pool.prev.MT) +
  geom_smooth(aes(x = tot_fish, y = prev_fish), method = "lm", se = FALSE, color = "skyblue", data = Site.pool.prev.MT) +
  scale_y_continuous(limits = c(0,100))
Splot <- ggplot() +
  geom_point(aes(x = tot_fish, y = prev_fish), color = "darkgreen", shape = 4, data = Site.pool.prev.S) + 
  geom_smooth(aes(x = tot_fish, y = prev_fish), method = "lm", se = FALSE, color = "darkgreen", data = Site.pool.prev.S) +
  scale_y_continuous(limits = c(0,100))

ft <- Cplot + Tplot + MTplot + Splot
ft
