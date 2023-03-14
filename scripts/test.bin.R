# Comparaison de modèles #
library(glmmTMB)
library(aod)

#Proportion data on discrete count (count based proportion) can be analyzed with beta regression or logistic regression

# Lake scale
## LeGi population
lake.mod.data2 <- lake.mod.data[c(1:10),]
lake.mod.data2 <- lake.mod.data2 %>% mutate(prev_LeGi = inf_LeGi/tot_LeGi)

### Binomial 
bin1 <-  glm(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC, data = lake.mod.data, family = "binomial")
summary(bin1) #p-value significative, mais forte surdispersion - AIC 1836.9
check_overdispersion(bin1) #Dispersion ratio = 198.093

#Effet du bassin versant
#Même si 4/5 n'ont qu'un seul lac, on peut observer des variation substentielles.
#Utiliser bassin versant comme facteur aléatoire afin de réduire la surdispersion
random <- rstandard(bin1)
plot(random ~ as.factor(lake.mod.data$Watershed),
     xlab = "Watershed", ylab = "Standardized residuals")
abline(0, 0, lty = 2)
# On ne peut pas scale la variable réponse parce valeurs négatives ne rentrent pas dans distribution binomiale

#Modèle avec l'effet aléatoire est mieux. Confirme le choix du modèle mixte
#GLMM sont bons avec petits échantillons
#Si on utilise sur lake.mod.data2, p-value significatves et dispersion ration = 4.978
bin2 <- glmer(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC + (1|Watershed), data = lake.mod.data2, family = "binomial")
summary(bin2) #p-value significative - AIC 910.4
overdisp_fun(bin2) #Dispersion ratio = 114

##option1
bin3 <- glmer(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC + (1|Lake), data = lake.mod.data, family = "binomial")
summary(bin3) #p-value non significatives - AIC 136.4
overdisp_fun(bin3) #Dispersion ratio = 0.283. Pas de surdispersion. 

### Quasi-binomial
qbin1 <- glm(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC, data = lake.mod.data, family = "quasibinomial")
summary(qbin1) #Presque significatif
check_overdispersion(qbin1) #Dispersion ratio = 198.093. N'a pas changé par rapport à binomial

library(MASS)
qbin2 <- glmmPQL(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC, random = ~1|Watershed, data = lake.mod.data, family = "quasibinomial")
summary(qbin2) #p-value non significatives
sum(residuals(qbin2, type="pearson")^2)/df.residual(qbin2) #Incapable de calculer dispersion ratio
overdisp_fun(qbin2)
check_overdispersion(qbin2)

###Beta regression 
install.packages("betareg")
library(betareg)

#Option 2 (sans Triton)
betareg1 <- betareg(prev_LeGi ~ TN_TP + TOC, data = lake.mod.data2)
summary(betareg1) #Valeurs non significatives
sum(residuals(betareg1, type="pearson")^2)/df.residual(betareg1) #Dispersion ratio = 1.848

###Beta binomial regression
install.packages("aod")
library(aod)

betabin1 <- betabin(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC, ~1, data = lake.mod.data, link = "logit")
summary(betabin1) #Valeurs non significatives - AIC 133 - AICc 139 - Overdispersion coefficient 0.403
sum(residuals(betabin1, type="pearson")^2)/df.residual(betabin1) #1.711

#Possible convergence problem
#NA for overdispersion coefficient on 3 Watershed
betabin2 <- betabin(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC, ~ Watershed, data = lake.mod.data, link = "logit")
summary(betabin2) #Valeurs non significatives - AIC 126 - AICc 306...

#Possible convergence problem  
#Retourne même pas de p-value
betabin3 <- betabin(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC, ~ Lake, data = lake.mod.data, link = "logit")
summary(betabin3) #Retourne même pas de p-value - AIC 131.5 - AICc 165...
sum(residuals(betabin2, type="pearson")^2)/df.residual(betabin2)

#Changement du link n'a pas d'influence
betabin4 <- betabin(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC, ~ Watershed, data = lake.mod.data, link = "cloglog")
summary(betabin4) #Valeurs non significatives (mais presque) - AIC 126 - AICc 306...

library(glmmTMB)
betabin5 <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC, family = betabinomial(link = "logit"), data = lake.mod.data)
summary(betabin5) #p-value non significatives - AIC 133.2 (comme betabin1) - BIC 134.8 - Paramètre de dispersion 1.48

betabin6 <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC, family = betabinomial(link = "cloglog"), data = lake.mod.data)
summary(betabin6) #p-value non significatives (mais un peu meilleur que pour betabin5) - AIC 133.2 (comme betabin1) - Paramètre de dispersion 1.48

betabin7 <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC + (1|Watershed), family = betabinomial(link = "logit"), data = lake.mod.data)
summary(betabin7) #p-value non significatives - AIC 135.2 - Paramètre de dispersion 1.48

betabin8 <- glmmTMB(cbind(inf_LeGi, tot_LeGi - inf_LeGi) ~ TN_TP + TOC + (1|Watershed), family = betabinomial(link = "cloglog"), data = lake.mod.data)
summary(betabin8) #p-value non significatives (comme betabin6) - AIC 135.2 - BIC 137.2-  Paramètre de dispersion 1.48

#Should I use BIC instead of AICc ?
# AICc is good for small sample
# BIC is good for unobserved heterogeneity

#Link function : link between the mean of Y and the fixed component used to fit other distributions (e.g. binomial) to linear form
#Logit vs. cloglog
#Logit is a symmetrical while cloglog is an asymmetrical function
#Comme mes données sembles asymétriques, est-ce que je dois choisir cloglog?
hist(lake.mod.data$inf_LeGi/lake.mod.data$tot_LeGi)
#When the probability of the binary or binomial response approaches to 0 at a different rate than it approaches to 1 (as a function of covariate), symmetric link functions cannot be appropriate (Prasetyo et al., 2019)

