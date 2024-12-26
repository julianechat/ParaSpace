#Perimeter curve
#Without Achigan
mod.data2 <- mod.data %>% 
  filter(!(Lake == "Achigan"))
PERI.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Perimeter, bs = "cs") + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data2, method = "ML")
summary(PERI.GAMM) 
#Still significant

PERI.sm <- smooth_estimates(PERI.GAMM) %>%
  add_confint()
PERI.pr <- mod.data2 %>%
  add_partial_residuals(PERI.GAMM)
PERI.pe <- PERI.sm %>%
  filter(.smooth == "s(Perimeter)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Perimeter),
           data = PERI.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Perimeter), fill ="#682714", alpha = 0.5) +
  geom_line(aes(x = Perimeter, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Perimeter (m)", y = "Partial effect (prevalence)", tag = "I") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank())
PERI.pe

#Without Achigan and Echo
mod.data3 <- mod.data2 %>% 
  filter(!(Lake == "Echo"))

PERI.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Perimeter, bs = "cs", k=9) + s(Lake, bs = "re"),
                 family = quasibinomial, data = mod.data3, method = "ML")
summary(PERI.GAMM) 
#Still significant

PERI.sm <- smooth_estimates(PERI.GAMM) %>%
  add_confint()
PERI.pr <- mod.data3 %>%
  add_partial_residuals(PERI.GAMM)
PERI.pe <- PERI.sm %>%
  filter(.smooth == "s(Perimeter)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Perimeter),
           data = PERI.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Perimeter), fill ="#682714", alpha = 0.5) +
  geom_line(aes(x = Perimeter, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Perimeter (m)", y = "Partial effect (prevalence)", tag = "I") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank())
PERI.pe

#Area:Perimeter
#Without achigan
AREAPERI.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Area_Perimeter, bs = "cs") + s(Lake, bs = "re"),
                     family = quasibinomial, data = mod.data2, method = "ML")
summary(AREAPERI.GAMM) 

AREAPERI.sm <- smooth_estimates(AREAPERI.GAMM) %>%
  add_confint()
AREAPERI.pr <- mod.data2 %>%
  add_partial_residuals(AREAPERI.GAMM)
AREAPERI.pe <- AREAPERI.sm %>%
  filter(.smooth == "s(Area_Perimeter)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Area_Perimeter),
           data = AREAPERI.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Area_Perimeter), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Area_Perimeter, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Area:Perimeter (m)", y = "Partial effect (prevalence)", tag = "H") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank())
AREAPERI.pe

#Without achigan and Echo

AREAPERI.GAMM <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Area_Perimeter, bs = "cs", k =9) + s(Lake, bs = "re"),
                     family = quasibinomial, data = mod.data3, method = "ML")
summary(AREAPERI.GAMM) 

AREAPERI.sm <- smooth_estimates(AREAPERI.GAMM) %>%
  add_confint()
AREAPERI.pr <- mod.data3 %>%
  add_partial_residuals(AREAPERI.GAMM)
AREAPERI.pe <- AREAPERI.sm %>%
  filter(.smooth == "s(Area_Perimeter)") %>%
  ggplot(unconditional = TRUE, overall_uncertainty = TRUE) +
  geom_rug(aes(x = Area_Perimeter),
           data = AREAPERI.pr,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = Area_Perimeter), fill = "#682714", alpha = 0.5) +
  geom_line(aes(x = Area_Perimeter, y = .estimate), color = "#682714", lwd = 1.2) +
  scale_y_continuous(trans = inverse_logit_trans) +
  labs(x = "Area:Perimeter (m)", y = "Partial effect (prevalence)", tag = "H") +
  theme(text = element_text(size = 20, 
                            family = "Calibri Light", 
                            color = "black"),
        axis.title.x = element_text(margin = unit(c(7, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 7, 0, 0), "mm")),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_blank(),
        axis.line.x = element_line(color = "black", 
                                   lineend = "round"),
        axis.line.y = element_line(color = "black", 
                                   lineend = "round"),
        plot.caption = element_blank(),
        plot.title = element_blank())
AREAPERI.pe
