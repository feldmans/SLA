library(mice)
library(dplyr)
library(survival)

#bl <- readRDS("data/bl_b.rds")
bl <- readRDS("data/bl_b.rds")
names(bl)
bl$nelsal <- nelsonaalen(bl, time.vni, evt)
#mice(bl)

#all.cox <- readRDS("data/all.cox_NOtimeTransf20170814.rds")
all.cox <- readRDS("data/all.cox20171029.rds")
#names(all.cox)

#----------------------------------------
#echantillon de patients
p<-unique(all.cox$PATIENT)
p0<-unique(bl$PATIENT)
p0<-p0[p0 %in%p]

ps<-sample(p0, 100, replace=F)

bl<-bl[bl$PATIENT%in%ps,]
all.cox<-all.cox[all.cox$PATIENT%in%ps,]

#-----------------------------------------
#retrait des variables inutiles pour l'imputation
bli <- bl %>% select(-ddn, -datevni, -dfin, -rilu, -date_dc, -fin_vni, 
                     -date_trach, -date_evt, -FIRSTSYMPTOM, -SLAtillvni_sup25mo,
                     -LIEUDEB_recode, -LIEUDEB_rec2)
# bli2 <- bl %>% select(-ddn, -datevni, -dfin, -time.vni, -rilu, -date_dc, -fin_vni, 
#                      -date_trach, -PATIENT, -date_evt, -FIRSTSYMPTOM, -SLAtillvni_sup25mo,
#                      -LIEUDEB_recode)

tail(md.pattern(bli))
sort(names(bli))

bli <- bl %>% select(-CVF)
#-----------------------------------------
#Choix de la méthode
mymet <- data.frame(name = names(bli))
mymet$methode <- ifelse(mymet$name %in% c("PATIENT", "evt", "time.vni", "nelsal"), '', NA)
mymet$methode <- ifelse(mymet$name %in% c("agevni", "SLAtillvni", "PAQUET_AN_PP", "MRC", "F_RESPIR", "CVF_ASSIS_ESR", "VEMS_OBSV", "VEMS_CVF", "CONSO_CIGARET_PP",
                                          "DEBIT_POINTE", "DEBIT_POINTE_T", "CVF_ERS", "CHUTE_CV", "PIMAX_THEO", "SNIP_THEO", "CVF_THEO", "CVF_ASSIS_THEO"),
                        "pmm", mymet$methode)
mymet$methode <- ifelse(mymet$name %in% c("LIEUDEB_rec3"), "polyreg", mymet$methode)
mymet$methode <- ifelse(is.na(mymet$methode), "logreg", mymet$methode)

for (var in mymet$name[mymet$methode=="logreg"]){bli[,var] <- as.factor(bli[,var])}

#-----------------------------------------
#choix des variables à imputer et prédictrices
mymat<-(1 - diag(1, ncol(bli)))
i<-match(c("PATIENT", "time.vni", "nelsal", "evt"), names(bli));i #variables qui ne sont pas a imputer
j<-match(c("PATIENT", "time.vni"), names(bli));j #variables qui ne sont pas des prédicteurs. je garde evt et nelsal (nelson aalen predictor) comme recommandé par Ian R white 2009
mymat[i,]<-0; #rows = variable à imputer
mymat[,j]<-0 #col = predictor => si col = 0, on n'utilise pas comme predicteur
head(mymat)

#-----------------------------------------
#imputation
M<-5

#bli <- bli %>% select(PATIENT, agevni, SLAtillvni, TABAGISME_PP, DYSP_EFFORT, DYSP_PAROX, MRC, SOM_REPAR)
imp <- mice(bli, m = M, maxit = 5, meth = mymet$methode, predictorMatrix=mymat, seed = 556) 
imp2 <- mice(bli, m = M, maxit = 50, seed = 556) #ca ne change rien en terme de NA
#im <- mice(bli, m = M, maxit = 5, meth = "pmm", predictorMatrix=m, printFlag = FALSE)
saveRDS(imp, "data/imp100-5-5.rds")
saveRDS(imp2, "data/imp100-5-50.rds")

#-----------------------------------------
#verif
#moyenne de la variable PAQUET_AN_PP pour chaque jeu d'imputation
x<-with(imp, mean(PAQUET_AN_PP))
#pour sortir uniquement les moyennes
unlist(x$analyses)
#ecart-types des moyennes (pour voir à quelle point l'imputation est fiable)
sd(unlist(x$analyses))
#on compare à l'écart-type de PAQUET_AN_PP non imputé ?
sd(bli$PAQUET_AN_PP, na.rm=T)

#idem avec agevni (sd(mean(X)) vs sd(X))
x<-with(imp, mean(agevni))
sd(unlist(x$analyses))
sd(bli$agevni, na.rm=T)

#j'extrait la première base imputée
#bli_i1<-mice::complete(imp2, action = 1)
bli_i1<-mice::complete(imp, action = 1)
summary(bli_i1)
#j'extrait la deuxième base imputée
bli_i2<-mice::complete(imp, action = 2)
summary(bli_i2)

#imp (et impt) contient l'ensemble des 5 imputations
impt<-imp
names(impt$imp)
impt$chainVar

##############
# 2 variables

#je fais tourner modele de cox sur les 5 bases imputées à la fois => 5 resultats
fit0 <- with(data=imp, exp=coxph(Surv(time.vni, evt)~agevni+PAQUET_AN_PP))
fit0 <- with(data=imp, exp=coxph(Surv(time.vni, evt)~DYSP_EFFORT+SEX))
#je pool les résultats pour en avoir un seul à la fin
summary(pool(fit0))
summary(coxph(Surv(time.vni, evt)~agevni+PAQUET_AN_PP, bl))
summary(coxph(Surv(time.vni, evt)~DYSP_EFFORT+SEX, bl))

#-------
# calculs pour essayer de savoir comment sont poolés le résultats
fit<-fit0
a<-fit$analyses
#coefficients (est) obtenus pour chacune des 5 bases
bm<-sapply(a, coef)
bc<-if(!is.null(nrow(bm))) rowMeans(bm) else mean(bm) ;bc
#je compare avec le pool
data.frame(summary(pool(fit0)))$est #ok idem

#Variance (se) pour chacune des 5 bases #probleme : ne marache que pour 2 variables
vbm<-var(t(bm))
vm<-sapply(a, vcov)
vm
vm <- if(!is.null(nrow(vm))) rowMeans(vm) else mean(vm)
vbc<-array(vm, dim=c(2,2))+vbm*(1+1/M);vbc
sqrt(diag(vbc))
#je compare au pool
data.frame(summary(pool(fit0)))$se #ok idem

g <- summary(pool(fit0))
solve(g[1,"se"], g[2,"se"])

W <- ((rbind(bc))%*%solve(vbc))%*%cbind(bc) 
#http://www.statisticshowto.com/wp-content/uploads/2016/09/2101f12WaldWithR.pdf

r <- 1 # nb de parametres dans le model
pval = 1-pchisq(W,r); pval

tc<-bc/sqrt(diag(vbc))
#ne marche pas
2*(1-pt(abs(tc)))

##########
#Une seule variable
fit0 <- with(data=imp, exp=coxph(Surv(time.vni, evt)~agevni))
fit0 <- with(data=imp, exp=coxph(Surv(time.vni, evt)~ENC_BRONCHIQ))
fit0 <- with(data=imp, exp=coxph(Surv(time.vni, evt)~DYSP_EFFORT))
summary(coxph(Surv(time.vni, evt)~DYSP_EFFORT, bl))
summary(coxph(Surv(time.vni, evt)~agevni, bl))
summary(coxph(Surv(time.vni, evt)~ENC_BRONCHIQ, bl))

# calculs pour essayer de savoir comment sont poolés le résultats
fit<-fit0
a<-fit$analyses
#coefficients (est) obtenus pour chacune des 5 bases
bm<-sapply(a, coef)
bc<-if(!is.null(nrow(bm))) rowMeans(bm) else mean(bm) ;bc
#je compare avec le pool
data.frame(summary(pool(fit0)))$est #ok idem
#Variance (se) pour chacune des 5 bases #probleme : ne marache que pour 2 variables
vm<-sapply(a, vcov) #pas de covariance quand il n'y a qu'une var, on ne recupere que la variance
vm <- if(!is.null(nrow(vm))) rowMeans(vm) else mean(vm)
sqrt(vm)
#je compare au pool
data.frame(summary(pool(fit0)))$se #pas idem pour DYSP_EFFORT, ok idem pour agevni
W <- ((rbind(bc))%*%solve(vm))%*%cbind(bc) #ok ca marche pour 1 variable : on retrouve la meme chose que le pool(et on voit avec ENC_BRONCHIQ qui n'a pas de NA que le pool prend le test de Wald)
r <- 1 # nb de parametres dans le model
pval = 1-pchisq(W,r); pval
data.frame(summary(pool(fit0)))["Pr...t.."]#meme ordre de grandeur pour DYSP_EFFORT, quasi idem pour agevni et enc bronchique
#-----------------------------------------
#merge

impc<-complete(imp, action="long", include=TRUE)
names(impc)
table(impc$.imp)
impc$.imp <- as.numeric(levels(impc$.imp))[impc$.imp]

impct<-merge(impc, all.cox[, c("PATIENT", "start", "stop", "etat")], by="PATIENT", all=F)
head(impct)
impct$.id <- as.numeric(levels(impct$.id))[impct$.id]
row.names(impct)<-NULL

impct<-impct[order(impct$.imp, impct$.id, impct$start),]

names(impct)
c(length(unique(impct$.id)), length(unique(impct$PATIENT)))

N<-table(impct$.id[impct$.imp==0])
N<-data.frame(.id=as.numeric(names(N)), n=as.numeric(N))
#N$plus<-N$N-N$n
head(N)

z<-expand.grid(list(.id=N$.id, ligne=1:max(N$n)))
dim(z)

z<-merge(z, N, by=".id", all=T)
head(z)
z<-z[order(z$.id, z$ligne),]
z$plus<-ifelse(z$ligne<=z$n, 0, 1)
head(z)

I<-0
impcti<-impct[impct$.imp==I,]
impcti$ligne<-sequence(N$n)
impcti<-merge(impcti, z, by=c(".id", "ligne"), all=T)
i<-unique(impcti[!is.na(impcti$PATIENT), c("PATIENT", ".id", ".imp")])
impcti$PATIENT<-as.character(i$PATIENT)[match(impcti$.id, z$.id)]
impcti$.imp<-I
impcti[125:134, c("PATIENT",".imp", ".id", "ligne", "plus")]
Impct<-impcti

for (I in 1:M) {
  cat(I, "\n")
  impcti<-impct[impct$.imp==I,]
  impcti$ligne<-sequence(N$n)
  impcti<-merge(impcti, z, by=c(".id", "ligne"), all=T)
  i<-unique(impcti[!is.na(impcti$PATIENT), c("PATIENT", ".id", ".imp")])
  impcti$PATIENT<-as.character(i$PATIENT)[match(impcti$.id, z$.id)]
  impcti$.imp<-I
  impcti[125:134, c("PATIENT",".imp", ".id", "ligne", "plus")]
  Impct<-rbind(Impct, impcti)
}
table(table(Impct$.id, Impct$.imp))

saveRDS(Impct, "data/Impct.rds")
Impct <- readRDS("data/Impct.rds")

#---------------------------------------------
#retransformer en mids (Est-ce qu'on l'utilise?? ne marche pas)

# impcti$ligne
# 
# impcti[125:134, c("PATIENT",".imp", ".id", "ligne")]
# impcti[895:905, c("PATIENT",".imp", ".id", "ligne")]
#impct<-merge(impct, N, by=".id", all=T)


as.mids2 <- function(Data2, .imp=1, .id=2){
#Data2<-Impct;.imp<-4;.id<-1
  cat("init\n")
  ini <- mice(Data2[Data2[, .imp] == 0, -c(.imp, .id)], m = 
                max(as.numeric(Data2[, .imp])), maxit=0)
  names  <- names(ini$imp)
  if (!is.null(.id)){
    rownames(ini$data) <- Data2[Data2[, .imp] == 0, .id]
  }
  for (i in 1:length(names)){
    cat(names[i], "\n")
    for(m in 1:(max(as.numeric(Data2[, .imp])))){
      if(!is.null(ini$imp[[i]])){
        indic <- Data2[, .imp] == m & is.na(Data2[Data2[, .imp]==0, names[i]])
        ini$imp[[names[i]]][m] <- Data2[indic, names[i]]
      }
    } 
  }
  return(ini)
}

head(Impct[Impct$.imp==0, c("PATIENT",".imp", ".id", "ligne")])
range(Impct$.id[Impct$.imp==0])


Impct$.jd<-paste(Impct$.id, Impct$ligne, sep="-")

summary(Impct$.id)
row.names(Impct)<-NULL
dim(Impct)
head(Impct, 2)

Impct1<-as.mids2(Impct, .id=56, .imp=4)
#ne marche pas "duplicate 'row.names' are not allowed"


#Comment a-t-on pu faire la suite??

#fit1 <- with(data=Impct1, exp=coxph(Surv(start, stop, etat)~agevni+PAQUET_AN_PP+cluster(PATIENT)))
fit1 <- with(data=Impct1, exp=coxph(Surv(start, stop, etat)~agevni+PAQUET_AN_PP, robust=T))

fit<-fit1
a<-fit$analyses
bm<-sapply(a, coef)
bc<-rowMeans(bm);bc
vbm<-var(t(bm))

vm<-sapply(a, vcov)
vm
vbc<-array(rowMeans(vm), dim=c(2,2))+vbm*(1+1/M);vbc

summary(pool(fit))
bc
sqrt(diag(vbc))




#----------------------------
#merger et analyser un a un

# le but est de reproduire le pool, sauf qu'on vu plus haut qu'en mergeant l'imputation avec all.cox, on perd la structure mids qu'on n'a pas reussi a reproduire.
bm<-array(0, dim=c(2, M)) #b comme coef
v0m<-array(0, dim=c(4, M)) #v comme vcov
v1m<-array(0, dim=c(4, M))

#pour chacun des jeux m, on merge avec les variables longitudinales, on fait le modele de cox, on recupere coef et vcov 
for (m in 1:M) {
  print(m)
  b<-mice::complete(imp, action = m)
  bt<-merge(b, all.cox[, c("PATIENT", "start", "stop", "etat")], by="PATIENT", all=F)
  f <- coxph(Surv(start, stop, etat)~agevni+PAQUET_AN_PP+cluster(PATIENT), data=bt)
  v1<-vcov(f) #c'est bien la variance robuste : comparer sqrt(vcov(f)[1]) a data.frame(summary(f)["coefficients"])["agevni","coefficients.robust.se"]
  bm[,m]<-coef(f)
  v1m[,m]<-c(v1)
  
  #Pourquoi c'etait commente?? c'est quoi f0? pourquoi coef = 0 mais pas se
  f0 <- coxph(Surv(start, stop, etat)~agevni+PAQUET_AN_PP+cluster(PATIENT), data=bt, control=coxph.control(iter.max=0))
  v0<-vcov(f0)
  v0m[,m]<-c(v0)
}

#pool des coefficients
bm # Ce sont les coef obtenus pour agevni et PAQUET_AN_PP avec chacune des 5 imputations
bc<-rowMeans(bm);bc #On avait vu avec les verif que c'etait la facon dont le pool fonctionnait le coef est la moyenne des coef

#pool des variances (robustes?)
#matrice de variance covariance des coef obtenus dans les 5 jeux soit la variance des coef pour agevni, la variance des coef pour paquet_an et la covariance des 2 (qui est donnee deux fois)
vbm<-var(t(bm)) 
# les matrice de covariance des beta obtenus separement pour chaque jeu est stocke dans v1m et v0m
#on fait la moyenne de chaque variance ou varcovar  
v0bc<-array(rowMeans(v0m), dim=c(2,2))+vbm*(1+1/M);v0bc
v1bc<-array(rowMeans(v1m), dim=c(2,2))+vbm*(1+1/M);v1bc
#donc vbm est la covariance des moyennes des coefficients et V0bc et v1bc est la moyenne des covariances
#se des 2 coefficients:
sqrt(diag(v1bc)) #racine des variances des coefficients

#calcul du test de Wald
b0<-bc*0;b0 #bc est la moyenne des coefficients 

b<-(bc-b0) #Pourquoi on retire 0? b0 vaudra toujours 0 #Parce que je compare les beta a 0
#Test de Wald robuste (robuste car on utilise la variance robuste)
((rbind(b))%*%solve(v1bc))%*%cbind(b) 

HRIC <- round(c(exp(m), exp(m + qnorm(0.975)*sqrt(variance) * c(-1,1))),3)

#--------------------------------------------------
#binaire avec modif du temps

bm<-array(0, dim=c(2, M)) #b comme coef
v0m<-array(0, dim=c(4, M)) #v comme vcov
v1m<-array(0, dim=c(4, M))
var_m <- array(0, dim=c(1, M))
coef_m <- array(0, dim=c(1, M))

for (m in 1:M) {
  print(m)
  b<-mice::complete(imp, action = m)
  bt<-merge(b, all.cox[, c("PATIENT", "start", "stop", "etat")], by="PATIENT", all=F)
  bt <- bt %>% mutate(ENC_BRONCHIQ = as.numeric(as.character(ENC_BRONCHIQ)),
                ENC_BRONCHIQ_t = ENC_BRONCHIQ * log(stop))
  #bt %>% filter(row_number() %in% 1:5) %>% mutate(ENC_BRONCHIQ = as.numeric(as.character(ENC_BRONCHIQ)), ENC_BRONCHIQ_t = ENC_BRONCHIQ * log(stop))
  # tmp <- bt %>% filter(row_number() == 1:5) %>% mutate(ENC_BRONCHIQ_t = ENC_BRONCHIQ * log(stop)) %>% select(ENC_BRONCHIQ, stop)
  # tmp %>% mutate(a = log (stop) * ENC_BRONCHIQ)
  # str(tmp) #pb = ENC_BRONCHIQ est un facteur
  f <- coxph(Surv(start, stop, etat)~ENC_BRONCHIQ+ENC_BRONCHIQ_t+cluster(PATIENT), data=bt)
  v1<-vcov(f)
  bm[,m]<-coef(f)
  v1m[,m]<-c(v1)
  
  t_t <- log(12)
  var_m[,m] <-  v1[1,1] + v1[2,2] * t_t^2 + 2*v1[1,2]*t_t
  coef_m[,m] <- coef(f)[1] + coef(f)[2]*t_t

  #Pourquoi c'etait commente?? c'est quoi f0? pourquoi coef = 0 mais pas se
  f0 <- coxph(Surv(start, stop, etat)~ENC_BRONCHIQ+ENC_BRONCHIQ_t+cluster(PATIENT), data=bt, control=coxph.control(iter.max=0))
  v0<-vcov(f0)
  v0m[,m]<-c(v0)
}

coef_m
var_m 

#pb les coeff ne change pas selon les jeux...:c'est parce que pas de NA! (permet de faire les verif!)

#Calcul de HRIC
#pool des coefficients
bc<-rowMeans(coef_m);bc #On avait vu avec les verif que c'etait la facon dont le pool fonctionnait le coef est la moyenne des coef
#pool des variances (robustes?)
#matrice de variance covariance des coef obtenus dans les 5 jeux soit la variance des coef pour agevni, la variance des coef pour paquet_an et la covariance des 2 (qui est donnee deux fois)
vm <- mean(var_m)
sqrt(vm) #impossible car vm negatif
HRIC <- round(c(exp(bc), exp(bc + qnorm(0.975)*sqrt(vm) * c(-1,1))),3); HRIC
# les matrice de covariance des beta obtenus separement pour chaque jeu est stocke dans v1m et v0m
#on fait la moyenne de chaque variance ou varcovar  

#calcul du test de Wald
#Tets de Wald robuste (car utilise variance robuste) : ON VEUT TESTER L'APPORT DES 2 VARIABLES COMPARE A AUCUNE
bc<-rowMeans(bm);bc
vbm<-var(t(bm)) 
# les matrice de covariance des beta obtenus separement pour chaque jeu est stocke dans v1m et v0m
#on fait la moyenne de chaque variance ou varcovar  
v1bc<-array(rowMeans(v1m), dim=c(2,2))+vbm*(1+1/M);v1bc #c'est bien la variance robuste
sqrt(diag(v1bc))
b0<-bc*0;b0 #bc est la moyenne des coefficients 
b<-(bc-b0) #Pourquoi on retire 0? b0 vaudra toujours 0 #Parce que je compare les beta a 0
W <- ((rbind(b))%*%solve(v1bc))%*%cbind(b) #ok ca marche pour 1 variable : on retrouve la meme chose que le pool(et on voit avec ENC_BRONCHIQ qui n'a pas de NA que le pool prend le test de Wald)
r <- 2 # nb de parametres dans le model
pval = 1-pchisq(W,r); pval #ok

bt<-merge(bl, all.cox[, c("PATIENT", "start", "stop", "etat")], by="PATIENT", all=F)
bt <- bt %>% mutate(ENC_BRONCHIQ = as.numeric(as.character(ENC_BRONCHIQ)),
                ENC_BRONCHIQ_t = ENC_BRONCHIQ * log(stop))
f <- coxph(Surv(start, stop, etat)~ENC_BRONCHIQ+ENC_BRONCHIQ_t+cluster(PATIENT), data=bt)
v1<-vcov(f)
t_t <- log(12)
var_m <-  v1[1,1] + v1[2,2] * t_t^2 + 2*v1[1,2]*t_t
coef_m <- coef(f)[1] + coef(f)[2]*t_t
HRIC <- round(c(exp(coef_m), exp(coef_m + qnorm(0.975)*sqrt(var_m) * c(-1,1))),3)
summary(f)
  

#--------------------------------------------------
#binaire avec decoupe du temps 

bm<-array(0, dim=c(2, M)) #b comme coef
v0m<-array(0, dim=c(4, M)) #v comme vcov
v1m<-array(0, dim=c(4, M))

for (m in 1:M) {
  print(m)
  b<-mice::complete(imp, action = m)
  bt<-merge(b, all.cox[, c("PATIENT", "start", "stop", "etat")], by="PATIENT", all=F)
  bt <- bt %>% mutate(ENC_BRONCHIQ = as.numeric(as.character(ENC_BRONCHIQ)),
                      ENC_BRONCHIQ_t1 = ENC_BRONCHIQ * ifelse(stop <= 4, 1, 0),
                      ENC_BRONCHIQ_t2 = ENC_BRONCHIQ * ifelse(stop > 4, 1, 0))
  
  wat <- c("ENC_BRONCHIQ_t1", "ENC_BRONCHIQ_t2")
  x<-bt[, wat]
  sx<-colSums(x) #interval de temps sans evt
  wat<-wat[sx>0] #on supprime interval de temps quand pas d'evenement
  form <-paste("Surv(start, stop, evt) ~ ", paste(wat, collapse="+"),"+cluster(PATIENT)", sep="")  #on ne met pas a_recode car les at couvre deja  toutes les perdiodes
  f <- coxph(as.formula(form), data=bt)
  
  v1<-vcov(f)
  bm[,m]<-coef(f)
  v1m[,m]<-c(v1)
  
  #Pourquoi c'etait commente?? c'est quoi f0? pourquoi coef = 0 mais pas se
  f0 <- coxph(as.formula(form), data=bt, control=coxph.control(iter.max=0))
  v0<-vcov(f0)
  v0m[,m]<-c(v0)
}

bc<-rowMeans(bm);bc
vbm<-var(t(bm))#que des 0 quand la variable n'a pas de NA 
# les matrice de covariance des beta obtenus separement pour chaque jeu est stocke dans v1m et v0m
#on fait la moyenne de chaque variance ou varcovar  
v1bc<-array(rowMeans(v1m), dim=c(2,2))+vbm*(1+1/M);v1bc #c'est bien la variance robuste
sqrt(diag(v1bc))

#HRIC
.tps_clinique = 12
vec_int <- c(4)
i <- findInterval(.tps_clinique, vec_int) + 1 #findInterval commence à 0...
HRIC <- round(c(exp(bc)[i], exp(bc[i] + qnorm(0.975)*sqrt(diag(v1bc)[i]) * c(-1,1))),4)

#pvalue
b0<-bc*0;b0 #bc est la moyenne des coefficients 
b<-(bc-b0) #Pourquoi on retire 0? b0 vaudra toujours 0 #Parce que je compare les beta a 0
W <- ((rbind(b))%*%solve(v1bc))%*%cbind(b) #ok ca marche pour 1 variable : on retrouve la meme chose que le pool(et on voit avec ENC_BRONCHIQ qui n'a pas de NA que le pool prend le test de Wald)
r <- 2 # nb de parametres dans le model
pval = 1-pchisq(W,r); pval #ok


#verif
wat <- c("ENC_BRONCHIQ_t1", "ENC_BRONCHIQ_t2")
x<-bt[, wat]
sx<-colSums(x) #interval de temps sans evt
wat<-wat[sx>0] #on supprime interval de temps quand pas d'evenement
form <-paste("Surv(start, stop, evt) ~ ", paste(wat, collapse="+"),"+cluster(PATIENT)", sep="")  #on ne met pas a_recode car les at couvre deja  toutes les perdiodes
coxt <- coxph(as.formula(form), data=bt)
test <- summary(coxt)
coefbeta <- round(test$coefficients[ ,"coef"], 5)
serob <- round(test$coefficients[ ,"robust se"], 5)
name_param <- rownames(test$coefficients)
pwald <- test$waldtest["pvalue"]
.tps_clinique = 12
vec_int <- c(4)
i <- findInterval(.tps_clinique, vec_int) + 1 #findInterval commence à 0...
HRIC <- round(exp(cbind(coef(coxt)[i], qnorm(0.025, coef(coxt)[i], sqrt(diag(vcov(coxt))[i])), qnorm(1-0.025, coef(coxt)[i], sqrt(diag(vcov(coxt))[i])))),4)
HRIC <- paste0(HRIC[1], " [", HRIC[2], " - ", HRIC[3],"]")
verif <- data.frame(t(c(paste(name_param, collapse = ";"), beta = paste(coefbeta, collapse = ";"), 
                        se_rob = paste(serob, collapse = ";"), pwald = pwald, HRIC=HRIC)))
