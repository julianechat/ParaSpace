### General additive models ###
#Binomial gam - TN_TP smooth
test1.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T), family = binomial, data = mod.data, method = "REML")
summary(test1.gam)
#Donne adj. R-sq (0.114) et deviance explained (27.4%)
#Significatif
plot(test1.gam)
appraise(test1.gam, method = "simulate")

#Binomial gam - TN_TP cubic regression smooth
test2.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr"), family = binomial, data = mod.data, method = "REML")
summary(test2.gam)
#Adj. R-sq = 0.13
#Deviance explained = 28.4%
plot(test2.gam)
appraise(test2.gam, method = "simulate")

#Binomial gam - TN_TP & TOC cubic regression smooth
test3.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(TOC.T, bs = "cr"), family = binomial, data = mod.data, method = "REML")
summary(test3.gam)
#Adj. R-sq = 0.625
#Deviance explained = 75.4%
plot(test3.gam)
check_overdispersion(test3.gam)
appraise(test3.gam, method = "simulate")

#Quasibinomial gam - TN_TP & TOC cubic regression smooth
test4.gam <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(TOC.T, bs = "cr"), family = quasibinomial, data = mod.data, method = "REML")
summary(test4.gam)
#Adj. R-sq = 0.536
#Deviance explained = 57.1%
#Est-ce qu'il faut regarder sur dispersion dans un gam ?
#REML bcp plus petit (mieux) que test3.gam
plot(test4.gam)
gam.check(test4.gam) #Interprétation ?

check_overdispersion(test4.gam) #Améliore un peu la sudispersion...
appraise(test4.gam, method = "simulate")

#Betabinomial gam - TN_TP & TOC cubic regression smooth
test5.gam <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), family = BB, data = mod.data2)
summary(test5.gam)
#Pas sur de coprendre la sortie
#AIC = 334.3 (comme betabin3)
plot(test5.gam)

#Binomial gamm (no random effect)
test1.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~1, family = binomial, data = mod.data, method = "REML")
summary(test1.gamm)
#All significatives
#Adj. R-sq. = 0.302
#Deviance explained = 29.8
appraise(test1.gamm, method = "simulate")

#Binomial gamm (random = Lake)
test2.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~Lake, family = binomial, data = mod.data, method = "REML")
summary(test2.gamm)
#All significatives
#Adj. R-sq. = 0.302
#Deviance explained = 29.8
#Exactement la même sortie...
check_overdispersion(test2.gamm)
appraise(test2.gamm, method = "simulate")

#Quasibinomial gamm (no random effect)
test3.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~1, family = quasibinomial, data = mod.data, method = "REML")
summary(test3.gamm)
#TOC significatif
#Adj. R-sq. = 0.302
#Deviance explained = 29.8%
#MAIIS REML beaucoup plus bas (donc better fit ?)
appraise(test3.gamm, method = "simulate")

#Quasibinomial gamm (random effect = lake)
test4.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(TOC.T, bs = "cr"), random = ~Lake, family = quasibinomial, data = mod.data2, method = "REML")
summary(test4.gamm)
#TOC significatif
#Adj. R-sq. = 0.302
#Deviance explained = 29.8
#Comme test3.gamm
check_overdispersion(test4.gamm)
#mmmh dispersion ratio encore plus haut que binomial sans effet aléatoire. Bizzare
gam.check(test4.gamm)
appraise(test4.gamm, method = "simulate")

#Betabinomial gamm (no random effect)
test5.gamm <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~Lake, family = BB, data = mod.data2)
summary(test5.gamm)
plot(test5.gamm)
#No significatif
#AIC = 334.3
#Exactement même sortie que sans l'effet aléatoire (test5.gam)

#Binomial gamm (OLRE random effect)
test6.gamm <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T) + cs(TOC.T), random = ~Lake + Transect_ID, family = quasibinomial, data = mod.data, method = "REML")
summary(test6.gamm)
#Adj. R-sq. = 0.309
#Deviance explained =29.8%
#Significatif
appraise(test6.gamm, method = "simulate")

#BetaBinomial gam
TNTP.GAMM <- gamlss(cbind(inf_fish, tot_fish - inf_fish) ~ cs(TN_TP.T), random = ~Lake, 
                    family = BB, data = mod.data2, REML = TRUE)
summary(TNTP.GAMM)
#AIC = 342.88
#No significative

#Binomial gam 
TNTP.GAMM2 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Lake, bs = "re"),
                  family = binomial, data = mod.data2, method = "ML")
summary(TNTP.GAMM2)
appraise(TNTP.GAMM2, method = "simulate")
testDispersion(TNTP.GAMM2)
check_overdispersion(TNTP.GAMM2)
gam.check(TNTP.GAMM2)

#Quasibinomial gam - correcting for overdispersion
TNTP.GAMM3 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Lake, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "ML")
summary(TNTP.GAMM3)
appraise(TNTP.GAMM3, method = "simulate")
check_overdispersion(TNTP.GAMM3)
gam.check(TNTP.GAMM3)
#It appears to be the best model so far
anova(TNTP.GAMM2, TNTP.GAMM3)

#Quaisbionmial random slope doesnt really help
TNTP.GAMM4 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Lake, bs = "re") + s(Lake, TN_TP.T, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(TNTP.GAMM4)
appraise(TNTP.GAMM4)

TNTP.GAMM5 <- gam(cbind(inf_fish, tot_fish - inf_fish) ~ s(TN_TP.T, bs = "cr") + s(Lake, TN_TP.T, bs = "re"),
                  family = quasibinomial, data = mod.data2, method = "REML")
summary(TNTP.GAMM5)
appraise(TNTP.GAMM5)

anova(TNTP.GAMM3, TNTP.GAMM5, TNTP.GAMM4)
#Random slope and intercept seems to be the best model 
