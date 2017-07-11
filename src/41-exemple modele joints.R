#=====================
#=====================
# lcmm
#=====================
#=====================

install.packages("lcmm")
library(lcmm)

library(JMbayes)

#data de JMbayes
head(pbc2)
pbc <- pbc2[, c("id", "years", "year", "status", "drug", "age", "serBilir")]
head(pbc)

head(pbc2.id)
pbc.id <- pbc2.id[, c("id", "years", "year", "status", "drug", "age", "serBilir")]
head(pbc.id)

#comparaison avec le tableau de lcmm
head(data_lcmm)

#pbc contient d�j� toute l'info n�cessaire (X1 est par exemple age ou drug (cad var a baseline) et Ydep est serBilir(cad var longitudinale))

#def de l'evt
pbc2$status2 <- as.numeric(pbc2$status != "alive") #on veut status2 == 1 qd décès ou transplantation
pbc2.id$status2 <- as.numeric(pbc2.id$status!= "alive")

pbc$serBilir.log <- log(pbc$ser)
pbc$status2 <- as.numeric(pbc$status!= "alive")

#Jointlcmm(serBilir.log ~ ns(year, 2), random = ~ns(year, 2) | id, subject = "id", 
Join1 <- Jointlcmm(serBilir.log ~ ns(year, 2), random = ~ns(year, 2), subject = "id", 
          survival = Surv(years, status2) ~ drug * age, data = pbc)

sink("modelejointJointlcmm.txt")
summary(Join1)
sink()

JoinJM1 <- readRDS("../data/joinFit.pbc1.rds")
sink("modelejointJMbayes.txt")
summary(JoinJM1)
sink()
#=====================
#=====================
# JMbayes
#=====================
#=====================



install.packages("JMbayes")
library(JMbayes)
library(lattice)

head(pbc2)
head(pbc2.id)


#------------------------
#modele simple
#------------------------

#question : la bilirubinémie est-elle associé avec l'évolution de la cirrhose biliaire primaire?

table(pbc2$status2)
#def de l'evt
pbc2$status2 <- as.numeric(pbc2$status != "alive") #on veut status2 == 1 qd décès ou transplantation
pbc2.id$status2 <- as.numeric(pbc2.id$status!= "alive")

#plots for survival 
sfit <- survfit(Surv(years, status2)~drug, data = pbc2.id)
plot(sfit)
plot(sfit, lty = 1:2, lwd = 2, col = 1:2, mark.time = FALSE, xlab = "Time (years)", ylab = "Transplantation-free Survival")
legend("topright", levels(pbc2.id$drug), lty = 1:2, col = 1:2, lwd = 2, cex = 1.3, bty = "n")

#plot de longitudinales data
pbc2$status2f <- factor(pbc2$status2, levels = 0:1, labels = c("alive", "transplantated/dead"))
xyplot(serBilir ~ year | status2f, data = pbc2) #le | sépare la population en  vivant et transplante/mort
xyplot(serBilir ~ year | status2f, group = id, data = pbc2) #group permet de grouper par identifiant
#log permet de passer de 0-40 = -2 3
xyplot(log(serBilir) ~ year | status2f, group = id, data = pbc2, panel = function(x, y,...){ #...permet de conserver group (on ne peut pas le mettre dans panel)
  panel.xyplot(x, y, type = "l", col= 1, ...) #trace les trajectoires
  panel.loess(x, y, col = 2, lwd = 2) #trace un moyenne des trajectoires?
}, xlab = "Time (years)", ylab = "log(serum Bilirubin)")  

#modèle mixte linéaire (car var à expliquer quantitative) : mais forme non linéaire => spline(ns)
lmeFit.pbc1 <- lme(log(serBilir) ~ ns(year, 2), data = pbc2, random = ~ns(year, 2) | id)
#lmeFit.pbc12 <- lme(log(serBilir) ~ year, data = pbc2, random = ~ year | id)

#cox model
coxFit.pbc1 <- coxph(Surv(years, status2) ~ drug * age, data = pbc2.id, x = TRUE)#x = TRUE permet d'obtenir la matrice de design (?)

#model joint
joinFit.pbc1 <- jointModelBayes(lmeFit.pbc1, coxFit.pbc1, timeVar = "year", n.iter = 30000) # 30 000 dans l'article
summary(joinFit.pbc1, include.baseHazCoefs = TRUE)
#saveRDS(joinFit.pbc1, "data/joinFit.pbc1.rds")

#diag
plot(joinFit.pbc1) # paramètre du sous modèle longitudinal : param = c("betas", "sigma", "alphas", "gammas")
plot(joinFit.pbc1, which = "density")


#------------------------
#modele étendu
#------------------------


# Modèle joint en utilisant les memes modeles (var longtiudinale quantitative normale)

# fonction densité de probabilité de la loi de student généralisée
dLongST <- function(y, eta.y, scale, log = FALSE, data){
  dgt(x = y, mu = eta.y, sigma = scale, df = 4, log = log) #loi de student
}

joinFit.pbc2 <- jointModelBayes(lmeFit.pbc1, coxFit.pbc1, timeVar = "year", densLong = dLongST) 
summary(joinFit.pbc2)
#saveRDS(joinFit.pbc2, "data/joinFit.pbc2.rds")
#legerement different du modele simple mais pas tellement

#En utilisant une variable longitudinale binaire
#binarisation
pbc2$serBilirD <- as.numeric(pbc2$serBilir > 1.8)
#modele mixte
lmeFit.pbc2 <- glmmPQL(serBilirD ~ year, data = pbc2, random = ~ year | id, family = binomial) #package MASSS

#modele joint
#fonction densité de probabilité
dLongBin <-  function(y, eta.y, scale, log = FALSE, data){
  dbinom(x = y, size = 1L, prob = plogis(eta.y), log = log) #loi binomiale
}

joinFit.pbc3 <- jointModelBayes(lmeFit.pbc2, coxFit.pbc1, timeVar = "year", densLong = dLongBin)
#saveRDS(joinFit.pbc3, "data/joinFit.pbc3.rds")
summary(joinFit.pbc3)

#variable longitudinale censurée (atteint une valeur planchée ou plafond)
pbc2$CensInd <- as.numeric(pbc2$serBilir <= 0.8)
pbc2$serBilir2 <- pbc2$serBilir
pbc2$serBilir2[pbc2$serBilir2 <= 0.8] <- 0.8


