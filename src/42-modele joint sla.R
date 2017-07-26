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
#NB: on avait nettoyé dr : si consult apres date de deces ce n'est pas un deces et time.vni==date de derniere consult - datevni
#il faudra la nettoyer ici#=> ok fait dans 00-datamanaegement

#=========================
#merge bl et bl_bmi

#Je supprime les BMI superieurs a 45
d <- d %>% filter(!(qui == "BMI" & x >= 45))

bl_bmi %>% filter(PATIENT =="ID140") %>% select(time.vni) # 597 au lieu de 598 : mais deja comme ca dans bdd_dates => c'est parce qu dr a ete change qd consult ulterieur a date deces, mais pas bdd_dates => je change aussi bdd_dates #ok MAJ dans 00-datamanegement

#je fait une nouvelle baseline en fusionnant les baseline de dr et bl_bmi et je prend la date la plus proche de datevni
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

#je merge les nouvelles baseline de BMI avec d
d <- bind_rows(d %>% filter(!(qui == "BMI" & f==0)), bl_bmi2) %>% filter(x < 80) %>% mutate(del = ifelse(del<0, 0, del))



#donnees manquantes
npos0 <- d %>% filter(qui=="BMI") %>% group_by(PATIENT) %>% 
  summarise(n0 = sum(del<=0), npos = sum(del>0), n = n0+npos) %>% arrange(PATIENT) 

tab <- table(bl=npos0$n0, nbval=pmin(npos0$n,2))
res <- sum(tab[1,]) #nb de patients sans baseline (mais avec un suivi)
res2 <- sum(tab) #bl et/ou suivi
res3 <- round(res/res2 *100,2) #% de patients sans baseline (denominateur = patients avec au moins une valeur)
data.frame(pat_noNA = res2, F1_noF0 = res, perc = paste0(res3, "%"))

#essqi dplyr (pas fini)
d %>% filter(qui=="BMI") %>% group_by(PATIENT) %>% 
  summarise(n0 = sum(del<=0), npos = pmin(1, sum(del>0)), n = n0+npos) %>% arrange(PATIENT) 


#=======================
#selection des variables
#=======================

my_col <- read.csv("data/names_var_sel.csv",stringsAsFactors = FALSE)
bl <- bl %>% select(PATIENT, one_of(my_col$variables)) 
d <- d %>% filter(qui %in% my_col$variables) 

varbl.vec <- bl %>% select(one_of(my_col$variables)) %>% colnames()
vardr.vec <- d %>% count(qui) %>% .$qui
#======================
#1 seul tableau longitudinal
#======================
head(bl)
da <- left_join(d %>% filter(qui=="BMI"), bl) %>% select(PATIENT, time.vni, del, evt, qui, x, one_of(varbl.vec)) %>% na.omit 
da$del<-da$del/30.4375
da$time.vni <- da$time.vni/30.4375
#======================
#tableau pour cox
#======================

da2 <- d %>% filter(PATIENT %in% da$PATIENT) %>% mutate(del = del/30.4375, time.vni = time.vni/30.4375)
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


#=====================================
#modele mixte sans covariables

#-----------------------------
#selection des effets aleatoires

lmeBMI.111 <- lme(fixed = x ~ del + I(del^2), data = da.grp, random = ~ del + I(del^2), na.action = na.omit, method = "REML")
lmeBMI.011 <- update(lmeBMI.111, random = ~ del + I(del^2) - 1)
lmeBMI.010 <- update(lmeBMI.111, random = ~ del - 1)
lmeBMI.100 <- update(lmeBMI.111, random = ~ 1)
lmeBMI.110 <- update(lmeBMI.111, random = ~ 1 + del)
#lmeBMI.110 <- update(lmeBMI.111, random = ~ del)
lmeBMI.101 <- update(lmeBMI.111, random = ~ 1 + I(del^2))
lmeBMI.001 <- update(lmeBMI.111, random = ~ I(del^2) - 1)

lmeBMI.11 <- lme(fixed = x ~ del, data = da.grp, random = ~ del, na.action = na.omit, method = "REML")
lmeBMI.10 <- update(lmeBMI.11, random = ~ 1)
lmeBMI.01 <- update(lmeBMI.11, random = ~ del - 1)

lmeBMI.1 <- lme(fixed = x ~ 1, data = da.grp, random = ~ 1, na.action = na.omit, method = "REML")

lmeBMI.1111 <- lme(fixed = x ~ del + I(del^2) + I(del^3), data = da.grp, random = ~ del + I(del^2) +I(del^3), na.action = na.omit, method = "REML") #ne tourne pas
lmeBMI.1110 <- lme(fixed = x ~ del + I(del^2) + I(del^3), data = da.grp, random = ~ del + I(del^2), na.action = na.omit, method = "REML")
lmeBMI.1100 <- update(lmeBMI.1110, random = ~ del)
lmeBMI.1000 <- update(lmeBMI.1110, random = ~ 1)
lmeBMI.0110 <- update(lmeBMI.1110, random = ~  del + I(del^2) - 1)
lmeBMI.0100 <- update(lmeBMI.1110, random = ~  del - 1)
lmeBMI.0001 <- update(lmeBMI.1110, random = ~  I(del^3) - 1)
lmeBMI.1001 <- update(lmeBMI.1110, random = ~  I(del^3))
lmes<-list(lmeBMI.111=lmeBMI.111, lmeBMI.011=lmeBMI.011, lmeBMI.010=lmeBMI.010, lmeBMI.100=lmeBMI.100, lmeBMI.110=lmeBMI.110,
           lmeBMI.101 = lmeBMI.101, lmeBMI.001 = lmeBMI.001,
           lmeBMI.11 = lmeBMI.11, lmeBMI.10 = lmeBMI.10, lmeBMI.01 = lmeBMI.01, 
           lmeBMI.1 = lmeBMI.1,
           lmeBMI.1110 = lmeBMI.1110, lmeBMI.1100 = lmeBMI.1100, lmeBMI.1000 = lmeBMI.1000, lmeBMI.0110 = lmeBMI.0110, 
           lmeBMI.0100 = lmeBMI.0100, lmeBMI.0001 = lmeBMI.0001, lmeBMI.1001 = lmeBMI.1001)
           
AIC.lme <- sapply(lmes, AIC)
AIC.lme <- data.frame(AIC.lme)
AIC.lme$variable <-  rownames(AIC.lme)
AIC.lme[order(AIC.lme$AIC.lme),]

#representation graphique
br<-sapply(lmes, function(l) l$coef$ran)
bf<-sapply(lmes, function(l) l$coef$fix)
nr<-lapply(br, function(x) dim(x)[2]);nr
bf2<-mapply(function(x,y) x[1:y], bf, nr);bf2
lv<-sapply(lmes, logLik);lv

pat<-unique(as.character(da.grp$PATIENT))
ti<-da.grp$time.vni[match(pat, da$PATIENT)]/30.4375
head(ti)
head(da.grp)

x<-seq(0, 100, l=201)
im<-1;bf2i<-bf2[[im]];bf2i;nri<-nr[[im]];nri;bri<-br[[im]];head(bri)
names(lmes)[im]

y<-array(0, dim=c(length(x), 1+dim(bri)[1]))
for (i in 1:dim(bri)[1]) {
  vide<-x>ti[i]
  if (i==1) {
    bhat<-bf2i
    for (j in 1:nri) y[,i]<-y[,i]+bhat[j]*(x^(j-1))
  }
  bhat<-bf2i+bri[i,]
  for (j in 1:nri) y[,i+1]<-y[,i+1]+bhat[j]*(x^(j-1))
  y[vide,i+1]<-NA
}

ylim<-range(y, na.rm=T);ylim
xlim<-range(x)
coul<-rep("pink", dim(y)[2]);coul[1]<-"red"
lty<-rep(2, dim(y)[2]);lty[1]<-1
lwd<-rep(1, dim(y)[2]);lwd[1]<-2
matplot(x, y, col=coul, lty=lty, lwd=lwd, type="l")
lines(x, y[, 1], col=coul[1], lwd=lwd[1])
#ok jusqua 40 mois puis polus assez de donnees pour avoir des predictions fiables


#selection des effets fixes

#pas besoihyhn car on fait un modele sans covariable.
lmeBMI.111 <- lme(fixed = x ~ del + I(del^2), data = da.grp, random = ~ del + I(del^2), na.action = na.omit, method = "REML")

resid(lmeBMI.111)
summary(lmeBMI.111)


#selection de lq mqtrice des residus ?

#diqgnostic
#hypothese de la constance de la variance poour les residus. presence d'outlyers
plot(lmeBMI.111,
     resid(., type="p") ~ fitted(.),
     id = 0.05,
     abline = 0)
#la variance des residus est-elle constante au cours du temps.presence d'outlyer
plot(lmeBMI.111,
     resid(.)~ del,
     abline = 0)

#normalite des residus
qqnorm(lmeBMI.111,
       ~resid(.),
       id = 0.05)

#diqgnostic des random effects
qqnorm(lmeBMI.111,
       ~ranef(.),
       id = 0.10)
#il faut regarder les outlier pour verifier s'il n'y a pas une valeur aberrante
da.grp %>% filter(PATIENT == "ID8867")
pairs(lmeBMI.111, ~ ranef(.), id = ~ PATIENT == "ID8867")
#valeur observee vs valeur predite
plot(lmeBMI.111, x ~ fitted(.), id = 0.05)
#good agreement except some outlyers

#je compare les resultats sans ID8867
lmeBMI.111.out <- lme(fixed = x ~ del + I(del^2), data = da.grp[da.grp$PATIENT != "ID8867",], random = ~ del + I(del^2), na.action = na.omit, method = "REML")
summary(lmeBMI.111)
summary(lmeBMI.111.out)
#pas de grands changements

#=====================================
#modele avec covariables

#-----------------------------
#selection des effets aleatoires
na.omit(da %>% select(PATIENT, evt:APPAREILLE_PP)) %>% count(PATIENT)


varbl <- paste(varbl.vec, collapse = " + ")
lmeBMI <- lme(fixed = as.formula(paste0("x ~ del + I(del^2) +", varbl)) , data = da.grp, random = ~ del + I(del^2), na.action = na.omit, method = "REML")
lmeBMI0 <- lme(fixed = as.formula(paste0("x ~ del + I(del^2)")) , data = da.grp, random = ~ del + I(del^2), na.action = na.omit, method = "REML")
lmeBMI.2 <- update(lmeBMI, random = ~ del + I(del^2) - 1)#pas de random intercept
lmeBMI.2a <- update(lmeBMI, random = ~ del - 1)#pas de random del^2
lmeBMI.2b <- update(lmeBMI, random = ~ 1)#
lmeBMI.2c <- update(lmeBMI, random = ~ 1+del)#
lmeBMI.2d <- update(lmeBMI, random = ~ I(del^2))
lmeBMI.2e <- update(lmeBMI, random = ~ I(del^2)-1)


lmeBMIb <- lme(fixed = as.formula(paste0("x ~ del +", varbl)) , data = da.grp, random = ~ del, na.action = na.omit, method = "REML")
lmeBMIb.2 <- update(lmeBMIb, random = ~ del - 1)#pas de random intercept
lmeBMIb.2b <- update(lmeBMIb, random = ~ 1)

lmeBMIc <- lme(fixed = as.formula(paste0("x ~ +", varbl)) , data = da.grp, random = ~ 1, na.action = na.omit, method = "REML")

lmeBMI3 <- lme(fixed = as.formula(paste0("x ~ del + I(del^2) + I(del^3) +", varbl)) , data = da.grp, random = ~ del + I(del^2) + I(del^3), na.action = na.omit, method = "REML")
lmeBMI3.1000 <- update(lmeBMI3, random = ~ 1)
lmeBMI3.0100 <- update(lmeBMI3, random = ~ del-1)
lmeBMI3.0010 <- update(lmeBMI3, random = ~ I(del^2)-1)
lmeBMI3.0001 <- update(lmeBMI3, random = ~ I(del^3)-1)
lmeBMI3.1100 <- update(lmeBMI3, random = ~ del)
lmeBMI3.1010 <- update(lmeBMI3, random = ~ I(del^2))
lmeBMI3.1001 <- update(lmeBMI3, random = ~ I(del^3))

lmes<-list(d2alea.111=lmeBMI, d2alea.011=lmeBMI.2, d2alea.010=lmeBMI.2a, d2alea.100=lmeBMI.2b, d2alea.110=lmeBMI.2c, d2alea.101=lmeBMI.2d, d2alea.001=lmeBMI.2e,
           d1alea.11=lmeBMIb, d1alea.10=lmeBMIb.2b, d1alea.01=lmeBMIb.2, d0alea.1=lmeBMIc,
           d3alea.1111=lmeBMI3, d3alea.1000=lmeBMI3.1000, d3alea.0100=lmeBMI3.0100, d3alea.0010=lmeBMI3.0010, d3alea.0001=lmeBMI3.0001,
           d3alea.1100=lmeBMI3.1100, d3alea.1010=lmeBMI3.1010, d3alea.1001=lmeBMI3.1001)
sapply(lmes, AIC)


br<-sapply(lmes, function(l) l$coef$ran)
bf<-sapply(lmes, function(l) l$coef$fix)

nr<-lapply(br, function(x) dim(x)[2]);nr

bf2<-mapply(function(x,y) x[1:y], bf, nr);bf2

lv<-sapply(lmes, logLik);lv

pat<-unique(as.character(da.grp$PATIENT))
ti<-da.grp$time.vni[match(pat, da$PATIENT)]/30.4375
head(ti)
head(da.grp)

x<-seq(0, 100, l=201)
im<-1;bf2i<-bf2[[im]];bf2i;nri<-nr[[im]];nri;bri<-br[[im]];head(bri)
names(lmes)[im]

y<-array(0, dim=c(length(x), 1+dim(bri)[1]))
for (i in 1:dim(bri)[1]) {
  vide<-x>ti[i]
  if (i==1) {
    bhat<-bf2i
    for (j in 1:nri) y[,i]<-y[,i]+bhat[j]*(x^(j-1))
  }
  bhat<-bf2i+bri[i,]
  for (j in 1:nri) y[,i+1]<-y[,i+1]+bhat[j]*(x^(j-1))
  y[vide,i+1]<-NA
}

ylim<-range(y, na.rm=T);ylim
xlim<-range(x)
coul<-rep("pink", dim(y)[2]);coul[1]<-"red"
lty<-rep(2, dim(y)[2]);lty[1]<-1
lwd<-rep(1, dim(y)[2]);lwd[1]<-2
matplot(x, y, col=coul, lty=lty, lwd=lwd, type="l")
lines(x, y[, 1], col=coul[1], lwd=lwd[1])


#aléatoire
lv
unr<-unlist(nr);unr
M<-length(lv)
z<-expand.grid(list(im1=1:M, im2=1:M))
z<-z[z$im1<z$im2,]
z$m1<-names(lv)[z$im1]
z$m2<-names(lv)[z$im2]
z$lv1<-lv[z$im1]
z$lv2<-lv[z$im2]
z$n1<-unr[z$im1]
z$n2<-unr[z$im2]
z$rdv<-abs(2*(z$lv1-z$lv2))
z$ddl<-abs(z$n1-z$n2)
z$p<-(1-pchisq(z$rdv, z$ddl))/2+(1-pchisq(z$rdv, z$ddl+1))/2
z

#-----------------------------
#selection des effets fixes

lmes2<-lapply(lmes, update, method="ML")

library(MASS)
lmeBMI.ml <- update(lmeBMI, method = "ML")
mystep <- stepAIC(lmeBMI.ml, direction = "backward", scope=list(lower=~ del + I(del^2)))

lmeBMI.3 <- update(lmeBMI.ml, fixed = ~ del + I(del^2) + E_PHAR_LAR + TOUX_EFFICACE + CVF_ERS +
                     agevni + SEX + DYSP_DECUBI + FERM_BOUCHE)
resid(lmeBMI.3)
#par defaut pas de corrélation intra groupe des résidus. lire p.135. residual dans random effect donne l'es résidus qui restent.'erreur résiduel qui est normalement nulle. Si non nul les effets aléatoire n'ont pas capturé toute la corrélation entre les données. 
plot(predict(lmeBMI.3), resid(lmeBMI.3)) 
resid(lmeBMI.3)[resid(lmeBMI.3)>3] #faire les résidus et prédits en fonction du temps et ploter#marginal car n'inclut pas les effets aléatoires.
predict(lmeBMI.3)[resid(lmeBMI.3)>3]
tab=da.grp[da.grp$PATIENT=="ID8867", c("PATIENT", "x", "del")]

lmeBMI.4 <- update(lmeBMI.ml, fixed = ~ del + I(del^2) + E_PHAR_LAR + TOUX_EFFICACE + CVF_ERS +
                     agevni + SEX + DYSP_DECUBI + FERM_BOUCHE)
getVarCov(lmeBMI.3, type = "marginal")

lmeBMI.marg <- lme(fixed = x ~ del + I(del^2) , data = da.grp, random = ~ del + I(del^2), na.action = na.omit, method = "REML")
#Pas de variables de groupe de traitement donc on est sûr que la variance est la même pour tout l'échantillon.

#=======================
#Modele de survie
#=======================


#ces ti ne marchent pas pour les mois, ok pour les jours uniquement!
# ti<-0:max(d$time.vni)
# ti<-0:max(da$time.vni)
# ti<-0:max(da2$time.vni)
#ti <- sort(unique(da2$time.vni))
ti <- sort(unique(c(da2$time.vni, da2$del)))
length(ti)

vardr.vec
var <- "BMI"
#y<-da[da$qui==var,] %>% filter(PATIENT %in% c("ID101", "ID949")) %>% select(PATIENT:TOUX_EFFICACE)

y<-da2[da2$qui==var,]

#y<-d[d$qui==var,] %>% mutate(del = ifelse(del<0, 0, del)) #faut il mettre les delai negatifs à 0? =>oui, deja fait
y<-y[order(y$PATIENT, y$del),] 
z<-tapply(y$del, y$PATIENT, c)
zf<-tapply(y$time.vni, y$PATIENT, c)
zm<-mapply(function(x,y) c(x[-1], y[1]), z, zf)
names(z)[1:5] # il manque le 107 a cause du na.omit => c'est normal
zm[[2]]


z<-tapply(y$evt, y$PATIENT, c)
fct<-function (x) {
  x[-length(x)]<-0
  return(x)
}
ze<-sapply(z, fct)

y$delapres<-unlist(zm)
y$evt2<-unlist(ze)
yt<-y
yt$start<-yt$del
yt$stop<-yt$delapres
yt$etat<-yt$evt2

yt<-survSplit(data = yt, end="stop", start="start", cut=ti, event="etat")
yt<-yt[order(yt$PATIENT, yt$start),]

#des sujets meurt le jour de la visite (?)
yt[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni,]
yt$etat[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni]<-1

yt[ ,var] <- yt$x
Yt<-yt[, c("PATIENT", "start", "stop", "etat", var)]

#ajouter une variable répétée
var <- vr1[2]
var <- "ALS"
y<-d[d$qui==var,]

y<-y[order(y$PATIENT, y$del),]
z<-tapply(y$del, y$PATIENT, c)
zf<-tapply(y$time.vni, y$PATIENT, c)
zm<-mapply(function(x,y) c(x[-1], y[1]), z, zf)
names(z)[1:3]
zm[[1]]
z<-tapply(y$evt, y$PATIENT, c)
fct<-function (x) {
  x[-length(x)]<-0
  return(x)
}
ze<-sapply(z, fct)
y$delapres<-unlist(zm)
y$evt2<-unlist(ze)

yt<-y
yt$start<-yt$del
yt$stop<-yt$delapres
yt$etat<-yt$evt2
yt<-survSplit(yt, end="stop", start="start", cut=ti, event="etat")
yt<-yt[order(yt$PATIENT, yt$start),]
#des sujets meurt le jour de la visite
yt[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni,]
yt$etat[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni]<-1

yt[,var] <-yt$x

dim(Yt)
Yt<-merge(Yt, yt[, c("PATIENT", "start", var)], by=c("PATIENT", "start"), all=T)
Yt<-merge(Yt, yt, by=c("PATIENT", "start"), all=T)
dim(Yt)

head(Yt)

#mettre les variables initiales
y0<-bl[,  c("PATIENT", "SEX")]
y0 <- bl[,  c("PATIENT", vb1, vb2, vb3)]

dim(Yt)
Yt<-merge(Yt, y0, by="PATIENT", all.x=T, all.y=F)
dim(Yt)

#--------------------------------------
#version Sarah

# #version dplyr
# y3 <- da2 %>% filter(qui==var) %>% group_by(PATIENT) %>% arrange(PATIENT, del) %>% 
#   mutate(delapres = lead(del), 
#          delapres = ifelse(is.na(delapres), time.vni, delapres), 
#          rn = row_number(), n = n(), evt2 = ifelse(rn == n, evt, 0)) %>% 
#   mutate(start = del, stop = delapres, etat = evt2) %>% 
#   survSplit(., end="stop", start="start", cut=ti, event="etat") %>% 
#   arrange(PATIENT, start) %>% 
#   mutate(etat = ifelse(evt==1 & etat==0 & stop==time.vni, 1, etat)) %>%
#   select(PATIENT, start, stop, etat, x)
# colnames(y3)[colnames(y3)=="x"] <- var
# 
# 
# y3[y3$delapres!=y$delapres,]  #tout est ok
# y3[y3$evt2!=y$evt2,]  #tout est ok
# sapply(colnames(Yt), function(x)all(y3[,x] == Yt[,x]))

# y %>% filter(PATIENT =="ID140") %>% select(-(E_PHAR_LAR: APPAREILLE_PP))
# #le probleme vient des lignes ou time.vni bl n'est pas identique : pb au moment de la creation de time.vni a del = 0
# #ok pb regle : on navait change date incoherente dans dr mais pas dans bdd_date


get_split <- function(data, var){
  #browser()
  y3.2 <- data %>% filter(qui==var) %>% group_by(PATIENT) %>% arrange(PATIENT, del) %>% 
    mutate(delapres = lead(del), 
           delapres = ifelse(is.na(delapres), time.vni, delapres), 
           rn = row_number(), n = n(), evt2 = ifelse(rn == n, evt, 0)) %>% 
    mutate(start = del, stop = delapres, etat = evt2) %>% 
    survSplit(., end="stop", start="start", cut=ti, event="etat") %>% 
    arrange(PATIENT, start) %>% 
    mutate(etat = ifelse(evt==1 & etat==0 & stop==time.vni, 1, etat)) %>%
    #mutate(etat = ifelse(evt==1 & etat==0 & stop==time.vni, 1, etat)) %>%
    select(PATIENT, start, stop, etat, evt, del, time.vni, x)
  colnames(y3.2)[colnames(y3.2)=="x"] <- var
  return(y3.2)
}


y <- get_split(da2, vardr.vec[1])
for (.var in vardr.vec[-1]){
#for (.var in vardr.vec[2]){
  y2 <- get_split(da2, .var)
  print(head(y2))
  y <- full_join(y, y2 %>% select(PATIENT, start, stop, del, etat, evt, time.vni, one_of(.var)), by = c("PATIENT", "start", "stop", "etat", "evt", "time.vni"))
  print(head(y))
} 
y %>% filter(PATIENT == "ID101" ) %>% head #pb lignes 3 et 4 : vient du start stop qui est different a partir de MORPHO_PPCHOICE 1 #MAJ : ti en mois en time.vni en mois : plus de pb 
y %>% filter(PATIENT == "ID101" ) #%>% View #pb lignes 3 et 4 : vient du start stop qui est different a partir de MORPHO_PPCHOICE 1 #MAJ : ti en mois en time.vni en mois : plus de pb 
y %>% filter(PATIENT == "ID1052" ) %>% head #pb lignes 3 et 4 : vient du start stop qui est different a partir de MORPHO_PPCHOICE 1 #MAJ : ti en mois en time.vni en mois : plus de pb 
#verif que etat 1 au meme endroit
get_split(da2, "MORPHO_PP_CHOICE_1") %>% filter(PATIENT == "ID101" ) %>% head
get_split(da2, "BMI") %>% filter(PATIENT == "ID101" ) %>% head
da2 %>% filter(PATIENT == "ID101" & qui == "MORPHO_PP_CHOICE_1") 
da2 %>% filter(PATIENT == "ID101" & qui == "ALS") 


#merge dr splitte et bl
all.cox <- left_join(y, bl) 
all.cox <- all.cox %>% mutate(all_del = all.cox %>% select(starts_with("del", ignore.case=FALSE)) %>% mutate(all_del = apply(.,1, min, na.rm=T)) %>% .$all_del) %>% 
  select(-starts_with("del", ignore.case=FALSE)) %>% mutate(del = all_del, all_del = NULL) %>% 
  select(PATIENT, start, stop, etat, del, time.vni, everything())

saveRDS(all.cox, "data/all.cox_NOtimeTransf20170726.rds")

#-------------------------------------------------
#PB

#il y a des del evt etime.vni et ALS NA 3=> pb venait de ALS vide au depart pour ces patients. pb resolu en mergeant aussi evt del et time.vni
all.cox %>% filter(is.na(del) & is.na(evt) & is.na(time.vni)) %>% dim
all.cox %>% filter(is.na(del) & is.na(evt) & is.na(time.vni) & is.na(ALS)) %>% dim #=> pb vient de ALS
all.cox %>% filter(is.na(del) & is.na(evt) & is.na(time.vni) & is.na(ALS)) %>% count(PATIENT) #88 patient : c'est bien le nombre de aptient avec NA pour ces variables
#pb : il ya des start na : c'est parce que nettoyage au niveau de get_split n'est pas complet
all.cox %>% filter(is.na(start))
all.cox %>% filter(stop == del) %>% count(start) #quand stop = del alors tjr NA
all.cox %>% filter(is.na(start) & stop != del)  #start est na alors stop tjr == del
all.cox %>% filter(is.na(start) & evt==1)  #ID181 uniquement : on lui a bien mis etat == 1 a l'avant derniere ligne mais on a pas supprime derniere ligne
all.cox %>% filter(is.na(start) & evt==0)  #iD1081, 1208, 126 etc...

#NB MAJ de ti => pb des lignes avec infos supplementaires reglees
# Etude de ID181: 2 pb : le dernier start est NA mais aussi deux lignes avec des infos complémentaiers pour start = 1.54
all.cox %>% filter(PATIENT == "ID181" ) %>% View 
all.cox %>% filter(PATIENT == "ID181" & is.na(start))
all.cox %>% filter(PATIENT == "ID181" & stop>3)
#pb desdeux lignes supplémentaires vient du fait que ti prend toutes les valeurs de time.vni(delai entr epose de vni et deces) mais pas de del(qui est le délai entre la pose de vni et la consult)

# Etude de ID1081: 2 pb : le dernier start est NA mais aussi deux lignes avec des infos complémentaiers pour start = 11.92 et 37.22
all.cox %>% filter(PATIENT == "ID1081" ) %>% View 
all.cox %>% filter(PATIENT == "ID1081" & is.na(start)) #qd la derniere consult est la date de derniere nouvelle alors la derniere ligne est NA pour start => j'en fais quoi??
all.cox %>% filter(PATIENT == "ID1081" & stop>40)

# Etude de ID1208: 2 pb : le dernier start est NA mais aussi deux lignes avec des infos complémentaiers pour start = 11.92 et 37.22
all.cox %>% filter(PATIENT == "ID1208" ) %>% View 
all.cox %>% filter(PATIENT == "ID1208" & is.na(start)) #qd la derniere consult est la date de derniere nouvelle alors la derniere ligne est NA pour start => j'en fais quoi??
all.cox %>% filter(PATIENT == "ID1208" & stop>40)

#pb patient sans baseline=> c'est pourquoi pour BMI on a que 395 patients sans NA (alors qu'on a fait un na omit sur da qui est la base repetee BMI, avant split sur tous les evt)
all.cox %>% filter(is.na(BMI))
all.cox %>% filter(is.na(BMI) & !is.na(start))
all.cox %>% filter(PATIENT == "ID4930") #pas de baseline
all.cox %>% filter(PATIENT == "ID9415" & start>4) #pas de baseline
all.cox %>% filter(PATIENT == "ID9415" & start<4.66) %>% count(BMI) 
all.cox %>% filter(PATIENT == "ID1081" & stop >40) #le NA BMI vient de la ligne del = stop (cette consult n'a ete replie que pour certaines variables et les autres sont NA)
all.cox %>% filter(PATIENT == "ID7877" & stop >15) #le NA BMI vient de la ligne del = stop (cette consult n'a ete replie que pour certaines variables et les autres sont NA)
#essai pour comparer avec Yann notamment les bug
y <- get_split(da2, "ALS")
y2 <- get_split(da2, "BMI")
full_join(y, y2, by = c("PATIENT", "start", "stop", "etat")) %>% filter(PATIENT =="ID1067") %>% View
full_join(y, y2, by = c("PATIENT", "start", "stop", "etat")) %>% filter(PATIENT =="ID101") %>% View


#-------------------------------------------------
#Analyse des donnees manquantes

#combien de patient si on fait un NA omit?
all.cox %>% na.omit %>% count(PATIENT)#3

#combien de NA par variable
count_na <- function(x) sum(is.na(x))
any_NA <- function(x) any(is.na(x))
no_NA_atall <- function(x) all(!is.na(x))

#nb de NA par patient par variable
all.cox %>% group_by(PATIENT) %>% summarise_all(count_na) 

#pour chaque pqtient et pour chqaue variable, dit s'il y a au moins un NA
all.cox %>% group_by(PATIENT) %>% summarise_all(any_NA)
#pour chaque variable, combien de patient avec au moins 1 NA
all.cox %>% group_by(PATIENT) %>% select(-start) %>% summarise_all(any_NA) %>% #et pas mutate all qui donne TF pr chaque ligne du patient
  ungroup %>% select(-PATIENT) %>% summarise_all(sum) %>% t
#pour chaque variable, combien de patient sans NA
noNA_var <- all.cox %>% group_by(PATIENT) %>% #select(-start) %>%
  summarise_all(no_NA_atall) %>% #et pas mutate all qui donne TF pr chaque ligne du patient
  ungroup %>% select(-PATIENT) %>% summarise_all(sum) %>% t


#essai de na.omit em retirant des variables (le pb des start ne modifie pas le compte des patienst car na.omit retire des lignes)
select_var <- noNA_var %>% data.frame(NB=., var=row.names(.), stringsAsFactors = FALSE) %>% filter(NB>300) %>% .$var
all.cox %>% select(PATIENT, one_of(select_var)) %>% na.omit %>% count(PATIENT)#363 PATTIENT avec un seuil de 300
noNA_rows <- all.cox %>% select(PATIENT, one_of(select_var)) %>% na.omit #363 PATTIENT avec un seuil de 300
#Pb ces patients n'ont peut etre pas de baseline
noNA_rows %>% group_by(PATIENT) %>% summarise(mindel = min(del), minstart = min(start)) %>% filter(minstart!=0) #94 patients sans baseline

# all.cox %>% group_by(PATIENT) %>% summarise_all(any_NA) %>% select(-PATIENT) %>%  apply(.,1,sum)
# all.cox %>% group_by(PATIENT) %>% summarise_all(any_NA) %>% select(-PATIENT) %>%  summarise_all(sum)
# all.cox %>% group_by(PATIENT) %>% summarise_all(any_NA) %>% count






#-----------------------------------------------------------------

yt<-na.omit(Yt)
coxt<-coxph(Surv(start, stop, etat)~SEX+dyspnee+cephal+cluster(PATIENT), data=yt)
summary(coxt)


coxt0<-coxph(Surv(start, stop, etat)~SEX+dyspnee+cluster(PATIENT), data=yt)
#cephal
nbpar<-1
init<-c(coef(coxt0), rep(0, nbpar))
coxt1<-coxph(Surv(start, stop, etat)~SEX+dyspnee+cephal+cluster(PATIENT), data=yt, init=init)
summary(coxt1)

