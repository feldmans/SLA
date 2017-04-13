#########################
#    SURVIE UNIVARIEE   #
#########################


source("src/libraries_SLA.R")
source("src/fonctions_SLA.R")
bl <- readRDS("data/bl.rds")

#=======================
#=======================
#variables baselines

#data management
d <- bl #base avec les colonnes qui ne sont pas dans bdd_dates
d[ ,c("LIEUDEB", "DOB", "FIRSTSYMPTOM")] <- NULL

#raison de la mise en place de la vni :
for (i in (1:20)){
  d[ ,paste0("crit", i)] <- ifelse((d$CRIT_1_VNI==i & !is.na(d$CRIT_1_VNI==i)) | (d$CRIT_2_VNI==i & !is.na(d$CRIT_2_VNI==i)) | (d$CRIT_3_VNI==i & !is.na(d$CRIT_3_VNI==i)), 1, 0)
}
d$CRIT_1_VNI <- NULL
d$CRIT_2_VNI <- NULL
d$CRIT_3_VNI <- NULL
d$VNI_ON <- NULL
d$ECHEC_MEO_VENT <- NULL

d [d$SEX == 1, "SEX"] <- 0
d [d$SEX == 2, "SEX"] <- 1

bl$extraction <- as_date("2015-08-27")
bl2 <- bl[bl$extraction - bl$datevni >= 365, ]


#----------------
#selection des variables
d2 <- d[ ,names(d)[!names(d) %in% names(bdd_dates)]] #base avec les colonnes qui ne sont pas dans bdd_dates
d2$FIRSTSYMPTOM <- NULL #base avec les colonnes qui ne sont pas dans bdd_dates
n_val <- apply(d2, 2, function(x) sum(!is.na(x))) #nb de non NA par variable
n_length <- apply(d2, 2, function(x) length(names(table(x)))) #nombre de valeurs différentes par variables
min_level <-  apply(d2, 2, function(x)min(as.numeric(table(x)))) #nombre minimum de personnes par catégorie 
ncu <- data.frame(var=names(d2), np_noNA=as.numeric(n_val), nval=as.numeric(n_length), min_level = min_level)

#Je supprime variable si moins de 25% des sujets l'ont renseignée 
#n pour 25% des sujets
n10 <- nrow(bl)*0.25
ncu <- ncu[ncu$np_noNA > n10, ]

#Je supprime les sous échelle de E_BULBAIRE et de ALSFRS
ncu <- ncu[!grepl("E_BULBAIRE", ncu$var), ]
ncu <- ncu[!grepl("ALS", ncu$var), ]

#pour regarder les différentes valeurs prises
apply(d2, 2, unique)
apply(d2, 2, unique)["FERM_BOUCHE"]#moins de 25% des sujets l'ont renseignée => pas dans binaire
apply(d2, 2, unique)["OXY_THERAP"]#moins de 25% des sujets l'ont renseignée => pas dans binaire


binaire <- as.character(ncu[ncu$nval == 2 & ncu$min_level > 1, "var"])
quanti  <- as.character(ncu[ncu$nval > 6, "var"])
quali <- as.character(ncu[ncu$nval > 2 & ncu$nval <= 6, "var"])
as.character(ncu[ncu$nval == 1, "var"])

#=======================
#analyses binaires

#----
#courbes de survie 
.l1 <- lapply(binaire[1:2], function(x)draw_surv_bin(x, data = d, .time = "time.vni", .evt = "evt", vec_time_IC= c(1, 3), recode = FALSE, surv_only=FALSE, pvalue = TRUE, dep_temps=FALSE, .transf=NULL))
ml <- marrangeGrob(.l1,ncol=1,nrow=1,top = NULL)
ggsave(file="binaire_bl.pdf", ml)

#----
#survie à 1 et 3 ans
.l2 <- lapply(binaire[1:3], function(x)draw_surv_bin(x, data = d, .time = "time.vni", .evt = "evt", vec_time_IC= c(1, 3), recode  = FALSE, surv_only=TRUE, pvalue = TRUE, dep_temps=FALSE, .transf=NULL))
df_surv <- do.call(rbind, .l2)
saveRDS(df_surv, "data/analyses/df_surv_bl.rds")
df_surv

#----
#hypothèse des risques proportionnels
pdf(paste0("data/analyses/RP_bin.pdf"))
.l <- lapply(binaire, function(x)check_RP(var=x, data=d, .time="time.vni", .evt="evt", recode = FALSE))
dev.off()

#NB j'ai rempli dans un tableau excel au regard des courbes si l'hypothèse des risques proportionnels était respecté(TRUE) ou non (FALSE)
write.table(print(binaire), file="clipboard", sep="\t")
rp <- read.csv2("data/RP_bin.csv")

#separation var selon respect RP ou non
binRP <-  as.character(rp[rp$RP, "variable"]) #RP ok, pas de var dépendante du temps
binNRP <- as.character(rp[!rp$RP, "variable"]) #RP pas ok, il faut rajouter var dépendante du temps

#----
#tranformation des variables ne respectant pas les RP

#####
#transformation du temps
pdf(paste0("data/analyses/RP_bin.pdf"))
.l <- lapply(binNRP, function(i){
   tmp <- lapply(c("log","sqrt","*t","/t","*t^2","*t^0.7", "log10", "*t^0.3", "*t^3"), function(x)add_vart_and_check(data=d, .time="time.vni", .evt="evt", recode = FALSE, var=i, .transf=x))
  #tmp <- lapply(c("log"), function(x)add_vart_and_check(data=d, .time="time.vni", .evt="evt", type="quali", var=i, .transf=x))
  .a <- do.call(rbind, tmp)
})
df_bint <- do.call(rbind, .l)
dev.off()

#remplissage de curve si test de beta_at(t) significatif
#remplissage à la main
write.table(print(df_bint[df_bint$beta_at==TRUE,]), file="clipboard", sep="\t", row.names=F) 
rpt <- read.csv2("data/RP dep du temps beta ok.csv")

#selection de la transformation qd curve et Harrell ok
rpts <- rpt[rpt$curve==TRUE,]
AICmin <- tapply(as.numeric(as.character(rpts$AIC)), as.character(rpts$variable), min)
AICmin <- data.frame(variable = names(AICmin), AICmin = as.numeric(AICmin))
rpts <- merge(rpts, AICmin, by="variable")

binNRPt <- unique(rpts[rpts$AIC==rpts$AICmin, c("variable", "transf")])

df_dept <- merge(binRPT, df_bint, by = c("variable","transf"), all=F)

#####
#Decoupe du temps pour les variables dont on n'a pas trouvé la bonne transformation:
tmp <- binNRP [!binNRP %in% binRPt$variable]
b <- c(6,18,50) #ok on garde 6 18 50 pour toutes les variables non corrigées par transformation du temps
pdf(paste0("data/analyses/RP_bin_decoup_", paste(b, collapse='-'), ".pdf"))
.l <- lapply(tmp, function(i){
  add_vart_and_check(data=d, .time="time.vni", .evt="evt", recode = FALSE, var=i, vec_cut = b)
})
df_bint <- do.call(rbind, .l)
dev.off()
df_cutt <- df_bint
binNRPcut <- unique(df_bint[ ,c("variable", "transf")])

#####
#J'ajoute une colonne transf NA pour les variables ok
binRP1 <- data.frame(variable = binRP, transf = NA)

#all var et transf
allbin <- rbind(binNRPt, binNRPcut, binRP1)
rownames(allbin) <- 1:nrow(allbin)
allbin$variable <- as.character(allbin$variable) 

#----
#HR [IC] et test du score (ou robuste)

.l <- lapply(1 : nrow(allbin), function(num){
HR_score (var=allbin$variable[num], data = d, .time="time.vni", .evt="evt", recode=FALSE, .transf=allbin$transf[num], .tps_clinique=12)
})
df_HR <- do.call(rbind,.l)

#=======================
#analyses quanti


#----
#hypothèse de loglinearité
pdf(paste0("data/analyses/Log_lin_bl.pdf"))
.l <- lapply(quanti, function(x){
  a <- check_loglin (var=x, data=d, .time="time.vni", .evt="evt")  
})
quanti_ln <- do.call(rbind, .l) #TRUE qd hypothèse non respectée et qu'il faut recoder
dev.off()

#----
#hypothèse des risques proportionnels : prend en compte la log lin vérifiée plus haut quanti_ln$recode 
pdf(paste0("data/analyses/RP_quanti_bl.pdf"))
.l <- lapply(1 : nrow(quanti_ln), function(x)check_RP(var=quanti_ln$variable[x], data=d, .time="time.vni", .evt="evt", recode = quanti_ln$recode[x]))
RP_quanti <- do.call(rbind, .l) #NB cette sortie est inutile, j'ai déjà l'info sur le graphe...
dev.off()
RP_quanti$RP <- NA

#Je rempli colonne RP en fonction de la courbe : si y=0 est toujours contenu par les bornes de l'IC et pvalue non signif, RP = T(l'hyp des RP est repectée), sinon RP = F 
write.table(print(RP_quanti), file="clipboard", sep="\t")
rp <- read.csv2("data/RP_quanti_bl.csv") #RP = TRUE pour hyopthèses respectée

#j'ajoute les info concernant la loglin
rp <- merge(quanti_ln, rp, by="variable")
rp <- rp[ , c("variable", "recode", "RP")]

#separation var selon respect RP ou non
QRP <-  rp[rp$RP, ] #RP ok, pas de var dépendante du temps
QN_RP <- rp[!rp$RP, ] #RP pas ok, il faut rajouter var dépendante du temps

#----
#tranformation des variables ne respectant pas les RP : variables QN_RP

#####
#transformation du temps
pdf(paste0("data/analyses/RP_quanti_bl_transft.pdf"))
.l <- lapply(1 : nrow(QN_RP), function(i){
  tmp <- lapply(c("log","sqrt","*t","/t","*t^2","*t^0.7", "log10", "*t^0.3", "*t^3"), function(x){
  #tmp <- lapply(c("log"), function(x){
    add_vart_and_check(data=d, .time="time.vni", .evt="evt", recode = QN_RP$recode[i], var=QN_RP$variable[i], .transf=x)
  })
  .a <- do.call(rbind, tmp)
})
df_quant <- do.call(rbind, .l)
dev.off()
dim(df_quant[df_quant$beta_at==TRUE & df_quant$param == "at",])

#Je ne regarde que les transformations pour lesquelles la pvalue de at est inférieure à 0.05 (beta_at = TRUE) (la courbe n'est de toute façon pas tracée quand sup à 0.05) 

#remplissage de curve si test de beta_at(t) significatif
#remplissage à la main de la colonne curve à l'aide du fichier "data/analyses/RP_quanti_bl_transft.pdf"
write.table(print(df_bint[df_bint$beta_at==TRUE & df_quant$param == "at", c("variable", "transf", "curve")]), file="clipboard", sep="\t", row.names=F) 
rpt <- read.csv2("data/RP quanti dept beta ok.csv")
rpt <- merge(rpt, subset(df_quant, select=-curve), by=c("variable", "transf"), all= F)

#selection de la transformation qd curve et Harrell ok (en fait quand curve ok, harrell est forcémen ok aussi)
rpts <- rpt[rpt$curve==TRUE,]
#Je prend la transformation avec le plus petit AIC
AICmin <- tapply(as.numeric(as.character(rpts$AIC)), as.character(rpts$variable), min)
AICmin <- data.frame(variable = names(AICmin), AICmin = as.numeric(AICmin))
rpts <- merge(rpts, AICmin, by="variable")

quanti_NRPt <- unique(rpts[rpts$AIC==rpts$AICmin, c("variable", "recode", "transf")])
quanti_NRPt$variable <- as.character(quanti_NRPt$variable)

#####
#Decoupe du temps pour les variables dont on n'a pas trouvé la bonne transformation:
tmp <- QN_RP [!QN_RP$variable %in% quanti_NRPt$variable, ]

#b <- c(6,18,50) #ok on garde 6 18 50 pour toutes les variables non corrigées par transformation du temps
#b <- c(6, 17, 34, 60) #ok on garde 6 18 50 pour toutes les variables non corrigées par transformation du temps
b <- c(7, 17, 34, 60) #ok on garde 6 18 50 pour toutes les variables non corrigées par transformation du temps
#add_vart_and_check(data=d, .time="time.vni", .evt="evt", recode = tmp$recode[1], var=tmp$variable[1], vec_cut = b)

pdf(paste0("data/analyses/RP_quanti_bl_decoup_", paste(b, collapse='-'), ".pdf"))
.l <- lapply(1: nrow(tmp), function(i){
  #add_vart_and_check(data=d, .time="time.vni", .evt="evt", type="quali", var=i, vec_cut = b)
  add_vart_and_check(data=d, .time="time.vni", .evt="evt", recode = tmp$recode[i], var=tmp$variable[i], vec_cut = b)
})
df_quanti_cutt <- do.call(rbind, .l)
dev.off()
QN_RPcut <- unique(df_quanti_cutt[ ,c("variable", "recode","transf")])

#####
#J'ajoute une colonne transf NA pour les variables ok
QRP$transf <- NA

#all var et transf
allquant <- rbind(subset(QRP, select=-RP), QN_RPcut, quanti_NRPt)
rownames(allquant) <- 1:nrow(allquant)
allquant$variable <- as.character(allquant$variable) 

#----
#HR [IC] et test du score (ou robuste)

.l <- lapply(1 : nrow(allquant), function(num){
  HR_score (var=allquant$variable[num], data = d, .time="time.vni", .evt="evt", recode=allquant$recode[num], .transf=allquant$transf[num], .tps_clinique=12)
})
df_HR_quanti_bl <- do.call(rbind,.l)

#----------------------
#Survie pour les quanti recodées

#courbe
df <- allquant[allquant$recode==TRUE, ]
.l <- lapply(df$variable, function(x)draw_surv_bin(var=x, data = d, .time="time.vni", .evt="evt", vec_time_IC= c(1, 3), 
                  recode=TRUE, surv_only=FALSE, pvalue = TRUE))
ml <- marrangeGrob(.l,ncol=1,nrow=1,top = NULL)
ggsave(file="courbe_survie_quanti_recode_bl.pdf", ml)

#survie à 1 et 3 ans
.l <- lapply(df$variable, function(x)draw_surv_bin(var=x, data = d, .time="time.vni", .evt="evt", vec_time_IC= c(1, 3), 
                                                   recode=TRUE, surv_only=TRUE))
df_surv_quant_bl <- do.call(rbind, .l)

#=======================
#analyses quali plus de 2 classes

test_musc <- grepl("TEST_MUSC", quali)
.l <- lapply(quali[!test_musc], function(x) draw_surv_qualisup2(var=x, data = d, .time = "time.vni", .evt = "evt", vec_time_IC= c(1, 3), surv_only=FALSE, pvalue=TRUE))
ml <- marrangeGrob(.l,ncol=1,nrow=1,top = NULL)
ggsave(file="courbe_survie_qualisup2_bl.pdf", ml)


#check RP
pdf(paste0("data/analyses/RP_quali_bl.pdf"))
.l <- lapply(quali[!test_musc], function(x)check_RP(var=x, data=d, .time="time.vni", .evt="evt", quali = TRUE))
dev.off()
RP_quanti$RP <- NA

#modif Non RP

#HRIC


#=======================
#=======================
#variables dont la valeur dépendant du temps


s <- d
var <- "SEX"
pdf(paste0("data/analyses/RP_bin", var, ".pdf"))
s$a_recode <- s[ ,var]
s <- s[!is.na(s$a_recode),]
s$tps <- (s[ ,"time.vni"]/365.25*12) + 0.001
ti <- sort(unique(c(0,s$tps[s$evt==1])))
slat <- s
slat$start <- 0
slat$stop <- slat$tps
slat$evt <- slat$evt
slat <- survSplit(Surv(stop,evt)~.,slat,start="start",cut=ti)


b<-c(6,12,18)
for (i in 1:(length(b)+1)){
  if (i == 1) tmp <-  slat$a_recode * ifelse(slat$stop<=b[1], 1, 0)
  if(i <= length(b) & i!= 1) tmp <- slat$a_recode * ifelse(slat$stop>b[i-1] & slat$stop<=b[i], 1, 0)
  if(i == (length(b)+1)) tmp <-  slat$a_recode * ifelse(slat$stop>b[i-1], 1, 0)
  slat[ ,paste0("at",i)] <- tmp
}
vat<-paste("at",  1:(length(b)+1), sep="")
x<-slat[, vat]
sx<-colSums(x) #interval de temps sans evt
wat<-vat[sx>0] #on supprime interval de temps quand pas d'evenement
f<-paste("Surv(start, stop, evt) ~ ", paste(wat, collapse="+"),"+cluster(PATIENT)", sep="")  #on ne met pas a_recode car les at couvre deja  toutes les perdiodes


coxt <- coxph(as.formula(f), data=slat)
coxt

t0<- 6
i <- findInterval(t0, b)
exp(coef(coxt)[i])
exp(cbind(coef(coxt), qnorm(0.025, coef(coxt), sqrt(diag(vcov(coxt)))), qnorm(1-0.025, coef(coxt), sqrt(diag(vcov(coxt))))))



bhat<-coef(coxt)
.AIC <- extractAIC(coxt)[2] #la premiere est le model vide, la deuxième le modèle avec les variables d'intéret
zit <- cox.zph(coxt, transform = "rank")
pval <- round(zit$table[,3],3)
zt <- cox.zph(coxt, transf="identity")
for (i in 1:(nrow(zt$table)-1)){
  iz<-i
  plot(zt[iz], resid = FALSE, main = paste0("plot shoenfeld for", var, "transformation\nHarrell ok\nAIC = ", .AIC))
  abline(h=0, col="red")
  abline(h=bhat[i], col="blue", lty=2)
  abline(v=b)
}

summary(survfit(coxt), time=seq(0, 80, by=10))
#-------------

b<-c(6, 18, 50, 50)
slat$at1 <- slat$a_recode * ifelse(slat$stop<=b[1], 1, 0)
slat$at2 <- slat$a_recode * ifelse(slat$stop>b[1] & slat$stop<=b[2], 1, 0)
slat$at3 <- slat$a_recode * ifelse(slat$stop>b[2] & slat$stop<=b[3], 1, 0)
slat$at4 <- slat$a_recode * ifelse(slat$stop>b[3] & slat$stop<=b[4], 1, 0)
slat$at5 <- slat$a_recode * ifelse(slat$stop>b[4], 1, 0)








vat<-paste("at", 1:5, sep="")
x<-slat[, vat]
sx<-colSums(x)
wat<-vat[sx>0]
f<-paste("Surv(start, stop, evt) ~ ", paste(wat, collapse="+"),"+cluster(PATIENT)", sep="")
coxt <- coxph(as.formula(f), data=slat)
coxt
bhat<-coef(coxt)
.AIC <- extractAIC(coxt)[2] 
zit <- cox.zph(coxt, transform = "rank")
pval <- round(zit$table[,3],3)
zt <- cox.zph(coxt, transf="identity")
for (i in 1:(nrow(zt$table)-1)){
  iz<-i
  plot(zt[iz], resid = FALSE, main = paste0("plot shoenfeld for", var, "transformation\nHarrell ok\nAIC = ", .AIC))
  abline(h=0, col="red")
  abline(h=bhat[i], col="blue", lty=2)
  abline(v=b)
}



summary(survfit(coxt), time=seq(0, 80, by=10))

b<-c(5,18)
slat$at1 <- slat$a_recode * ifelse(slat$stop<=b[1], 1, 0)
slat$at2 <- slat$a_recode * ifelse(slat$stop>b[1] & slat$stop<=b[2], 1, 0)
coxt <- coxph(Surv(start, stop, evt) ~ a_recode + at1 + at2, data=slat)
coxt

# b<-c(6)
# slat$at1 <- slat$a_recode * ifelse(slat$stop<=b[1], 1, 0)
# coxt <- coxph(Surv(start, stop, evt) ~ a_recode + at1, data=slat)
# coxt


# #coxt <- coxph(Surv(start, stop, evt) ~ a_recode + at, data=slat)
# coxt <- coxph(Surv(start, stop, evt) ~ a_recode + at1 + at2 + at3, data=slat)
# coxt
bhat<-coef(coxt)
bhat<-c(bhat[-1],0)+bhat[1];bhat

.AIC <- extractAIC(coxt)[2] 
zit <- cox.zph(coxt, transform = "rank")
pval <- round(zit$table[,3],3)
zt <- cox.zph(coxt, transf="identity")
for (i in 1:(nrow(zt$table)-1)){
  iz<-i
  plot(zt[iz], resid = FALSE, main = paste0("plot shoenfeld for", var, "transformation\nHarrell ok\nAIC = ", .AIC))
  abline(h=0, col="red")
  abline(v=b)
}
dev.off()
test <- summary(coxt)
pval_at <- test$coefficients["at","Pr(>|z|)"]
