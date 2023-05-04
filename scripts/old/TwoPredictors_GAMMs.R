#Two predictors GAMS
# 05 MAY 

#morpho
lake.morpho.gam1 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Area_Perimeter, bs = "cr") + s(Mean_depth, bs = "cr") + s(Lake, bs = "re"), 
                        family = quasibinomial, data = mod.data2, method = "ML")
summary(lake.morpho.gam1) #both significative
draw(lake.morpho.gam1)
#space
lake.space.gam1 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Elevation, bs = "cr") + s(Drainage_area, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(lake.space.gam1)

lake.space.gam2 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Elevation, bs = "cr") + s(WRT, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(lake.space.gam2)#nope

# ---- Transect scale GAMs ----
#nutrients
trans.nutrient.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(TOC.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "REML")
summary(trans.nutrient.gam) #TN_TP significative

#physico
trans.physico.gam1 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cr") + s(Temp.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam1) #Turb > Temp significative

trans.physico.gam2 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cr") + s(pH.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam2) #Turb > pH significative

trans.physico.gam3 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.T, bs = "cr") + s(pH.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam3) #Temp significative
draw(trans.physico.gam3)
appraise(trans.physico.gam3)

trans.physico.gam4 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cr") + s(DO.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam4) #Turb significative

trans.physico.gam5 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cr") + s(Cond.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam5) #Turb significative

trans.physico.gam6 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.T, bs = "cr") + s(DO.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam6) #Temp significative

trans.physico.gam7 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.T, bs = "cr") + s(Cond.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam7) #Temp significative

trans.physico.gam8 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Cond.T, bs = "cr") + s(DO.T, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.physico.gam8) #DO significative

#Habitat
trans.habitat.gam1 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Sub1, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam1) #Macrophyte & Sub1 significative

trans.habitat.gam2 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Sub2, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam2) #Macrophyte & Sub2 significative

trans.habitat.gam3 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Depth, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam3) #Macrophyte significative

trans.habitat.gam4 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Trunk, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam4) #Macrophyte significative

trans.habitat.gam5 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub1, bs = "cr") + s(Depth, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam5) #Non significative

trans.habitat.gam6 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub1, bs = "cr") + s(Trunk, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam6) #Non significative

trans.habitat.gam7 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub2, bs = "cr") + s(Depth, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam7) #Sub2 & Depth lightly significative

trans.habitat.gam8 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Sub2, bs = "cr") + s(Trunk, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam8) #Non significative

trans.habitat.gam9 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Depth, bs = "cr") + s(Trunk, bs = "cr") + s(Lake, bs = "re"), 
                          family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.habitat.gam9) #Depth significative

#biotic
trans.biotic.gam1 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Diversity.T, bs = "cr") + s(Species_richness.T, bs = "cr", k = 5) + s(Lake, bs = "re"), 
                         family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.biotic.gam1) #Non significative

trans.biotic.gam2 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Diversity.T, bs = "cr") + s(Centrarchids.T, bs = "cr") + s(Lake, bs = "re"), 
                         family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.biotic.gam2) #Diversity & Centrarchids significative

trans.biotic.gam3 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Species_richness.T, bs = "cr", k = 5) + s(Centrarchids.T, bs = "cr") + s(Lake, bs = "re"), 
                         family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.biotic.gam3) #Non significative



## Two predictor GAMs ----
# Mixed variables models ----
trans.mix.gam1 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Macrophyte, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam1)  

trans.mix.gam2 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Temp.T, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam2)  

trans.mix.gam3 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Turb.T, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam3)
draw(trans.mix.gam3)
appraise(trans.mix.gam3, method = "simulate")

trans.mix.gam4 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(pH.T, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam4)  

trans.mix.gam5 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(DO.T, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam5)  

trans.mix.gam6 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Cond.T, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam6)  

trans.mix.gam7 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Area_Perimeter, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam7)  

trans.mix.gam8 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam8)  

trans.mix.gam9 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Temp.T, bs = "cr") + s(Lake, bs = "re"), 
                      family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam9)  

trans.mix.gam10 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Turb.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam10)  

trans.mix.gam11 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(pH.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam11)  

trans.mix.gam12 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(DO.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam12)  

trans.mix.gam13 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Cond.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam13)  

trans.mix.gam14 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Area_Perimeter, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam14)  

trans.mix.gam15 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Macrophyte, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam15)  

trans.mix.gam16 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.T, bs = "cr") + s(Area_Perimeter, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam16)  

trans.mix.gam17 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Temp.T, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam17)  

trans.mix.gam18 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cr") + s(Area_Perimeter, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam18)  

trans.mix.gam19 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Turb.T, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam19)  

trans.mix.gam20 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(pH.T, bs = "cr") + s(Area_Perimeter, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam20)  

trans.mix.gam21 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(pH.T, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam21)  

trans.mix.gam22 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(DO.T, bs = "cr") + s(Area_Perimeter, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam22)  

trans.mix.gam23 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(DO.T, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam23)  

trans.mix.gam24 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Cond.T, bs = "cr") + s(Area_Perimeter, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam24)  

trans.mix.gam25 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Cond.T, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam25)  

trans.mix.gam26 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(Area_Perimeter, bs = "cr") + s(Diversity.T, bs = "cr") + s(Lake, bs = "re"), 
                       family = quasibinomial, data = mod.data2, method = "ML")
summary(trans.mix.gam26)  