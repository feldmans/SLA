#=====================
#=====================
# modele joint SLA  ||
#=====================
#=====================


source("src/libraries_SLA.R")
source("src/02-fonctions_SLA.R")
library(lattice)
library(nlme)
#install.packages("WWGbook")
library(WWGbook)
#====================================================
#====================================================


#=======================
#chargement bases
#=======================

#-----------------------
#variables baselines
bl <- readRDS("data/bl.rds")
bdd_dates <- readRDS("data/bdd_dates.rds")

#-----------------------
#variables dont la valeur dépendant du temps
d <- readRDS("data/df_rep.rds")
d <- d[d$PATIENT %in% bl$PATIENT, ]

#-----------------------
#baseline recuperees

#BMI
bl_bmi <- readRDS("data/bl_bmi.rds")
table(bl_bmi$PATIENT %in% bl$PATIENT)

#=========================
#merge bl et bl_bmi

d <- d %>% filter(!(qui == "BMI" & x >= 45))
bl_bmi2 <- bind_rows(d, bl_bmi) %>% select(one_of(colnames(d))) %>% filter(qui == "BMI" & f==0) %>% 
  group_by(PATIENT) %>% arrange(desc(date)) %>% 
  slice(1)

#verif:
bind_rows(d, bl_bmi) %>% select(one_of(colnames(d))) %>% filter(qui == "BMI" & f==0) %>% 
  group_by(PATIENT) %>% arrange(desc(date)) %>% filter(PATIENT == "ID101") 
bind_rows(d, bl_bmi) %>% select(one_of(colnames(d))) %>% filter(qui == "BMI" & f==0) %>% 
  group_by(PATIENT) %>% arrange(desc(date)) %>% filter(PATIENT == "ID101") %>% 
  slice(1)
hist(bl_bmi2$x)

d <- bind_rows(d %>% filter(!(qui == "BMI" & f==0)), bl_bmi2) %>% filter(x < 80)

#donnees manquantes
npos0 <- d %>% filter(qui=="BMI") %>% group_by(PATIENT) %>% 
  summarise(n0 = sum(del<=0), npos = sum(del>0), n = n0+npos) %>% arrange(PATIENT) 

tab <- table(bl=npos0$n0, nbval=pmin(npos0$n,2))
res <- sum(tab[1,]) #nb de patients sans baseline (mais avec un suivi)
res2 <- sum(tab)
res3 <- round(res/res2 *100,2) #% de patients sans baseline (denominateur = patients avec au moins une valeur)
res <- data.frame(res2, res, res3 = paste0(res3, "%"))

d %>% filter(qui=="BMI") %>% group_by(PATIENT) %>% 
  summarise(n0 = sum(del<=0), npos = pmin(1, sum(del>0)), n = n0+npos) %>% arrange(PATIENT) 


#=======================
#selection des variables
#=======================

my_col <- read.csv("data/names_var_sel.csv",stringsAsFactors = FALSE)
bl <- bl %>% select(PATIENT, one_of(my_col$variables), -BMI) 
d <- d %>% filter(qui %in% my_col$variables) 

varbl.vec <- bl %>% select(one_of(my_col$variables)) %>% colnames()
#======================
#1 seul tableau longitudinal
#======================
head(bl)
da <- left_join(d %>% filter(qui=="BMI"), bl) %>% select(PATIENT, del, evt, qui, x, one_of(varbl.vec)) %>% na.omit
#====================================================
#====================================================


#=======================
#Modele longitudinal
#=======================
attach(autism)
sicdegp.f <- factor(sicdegp)
age.f <- factor(age)
autism.updated <- data.frame(autism, sicdegp.f, age.f)
age.2 <- age -2
age.2sq <- age.2*age.2
sicdegp2 <- cut(sicdegp, breaks = 0:3, labels = FALSE)
sicdegp2.f <- factor(sicdegp2)
autism.updated <- subset(data.frame(autism, sicdegp2.f, age.2), !is.na(vsae))
autism.grouped <- groupedData(vsae~age.2|childid, data = autism.updated, order.groups = FALSE)

model6.1.fit <- lme(vsae ~ age.2 +I(age.2^2) + sicdegp2.f + age.2:sicdegp2.f +I(age.2^2):sicdegp2.f,
                    random = ~ age.2 + I(age.2^2), method = "REML",
                    data = autism.grouped)
model6.2.fit <- lme(vsae ~ age.2 +I(age.2^2) + sicdegp2.f + age.2:sicdegp2.f +I(age.2^2):sicdegp2.f,
                    random = ~ age.2 + I(age.2^2) -1, method = "REML", #-1 retire ici l'intercept par enfant
                    data = autism.grouped)
summary(model6.2.fit)
model6.2a.fit <- update(model6.2.fit, random = ~ age.2 -1) #-1 ici retire l'effet de l'age au carre
summary(model6.2a.fit)
#Y a t il une difference significative entre ces deux modeles
dif <- logLik(model6.2.fit)*2 - logLik(model6.2a.fit)*2
h6.1.pvalue <- 0.5*(1-pchisq(dif,1) + 0.5*(1-pchisq(dif,2)))
#modeles significativement different, on prend l'AIC le plus petit cad 
AIC(model6.2.fit)
AIC(model6.2a.fit)

#-----------------------------
#plot de BMI en fonction du temps
d$evt <- as.factor(d$evt)
 d %>% filter(qui == "BMI" & PATIENT == "ID2215") 
 d %>% filter(qui == "BMI" & x>60) 
 d2 <- d %>% filter(qui=="BMI")
xyplot(x ~ del | evt, data = d2) #le | sépare la population en  vivant et transplante/mort
xyplot(x ~ del | evt, group = PATIENT, data = d2) #group permet de grouper par identifiant
xyplot(x ~ del | evt, group = PATIENT, data = da, panel = function(x, y,...){ #...permet de conserver group (on ne peut pas le mettre dans panel)
  panel.xyplot(x, y, type = "b", col= 1, ...) #trace les trajectoires
  panel.loess(x, y, col = 2, lwd = 2) #trace un moyenne des trajectoires?
}, xlab = "delai (jours)", ylab = "BMI(kg/m2)")  

#-----------------------------
#definition de la structure hierarchique
da.grp <- groupedData(x ~ del | PATIENT, data = da)


#-----------------------------
#selection des effets aleatoires
na.omit(da %>% select(PATIENT, evt:APPAREILLE_PP)) %>% count(PATIENT)

varbl <- paste(varbl.vec, collapse = " + ")
lmeBMI <- lme(fixed = as.formula(paste0("x ~ del + I(del^2) +", varbl)) , data = da.grp, random = ~ del + I(del^2), na.action = na.omit, method = "REML")
lmeBMI.2 <- update(lmeBMI, random = ~ del + I(del^2) - 1)#pas de random intercept
lmeBMI.2a <- update(lmeBMI.2, random = ~ del - 1)#pas de random del^2

lmeBMIb <- lme(fixed = as.formula(paste0("x ~ del +", varbl)) , data = da.grp, random = ~ del, na.action = na.omit, method = "REML")
lmeBMIb.2 <- update(lmeBMI, random = ~ del - 1)#pas de random intercept

AIC(lmeBMI)
AIC(lmeBMI.2)
AIC(lmeBMI.2a)
AIC(lmeBMIb)
AIC(lmeBMIb.2)


dif <- logLik(lmeBMI)*2 - logLik(lmeBMI.2)*2
h6.1.pvalue <- 0.5*(1-pchisq(dif,1) + 0.5*(1-pchisq(dif,2)))
dif <- logLik(lmeBMI.2)*2 - logLik(lmeBMI.2a)*2
h6.1.pvalue <- 0.5*(1-pchisq(dif,1) + 0.5*(1-pchisq(dif,2)))#pas de difference
dif <- logLik(lmeBMI)*2 - logLik(lmeBMIb)*2
h6.1.pvalue <- 0.5*(1-pchisq(dif,1) + 0.5*(1-pchisq(dif,2)))#difference, on garde lmeBMI

#-----------------------------
#selection des effets fixes
library(MASS)
lmeBMI.ml <- update(lmeBMI, method = "ML")
lmeBMI.ml2 <- update(lmeBMI.ml, fixed = method = "ML")
stepAIC(lmeBMI.ml, direction = "backward")
