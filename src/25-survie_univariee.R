#########################
#    SURVIE UNIVARIEE   #
#########################


source("src/libraries_SLA.R")
#source("src/01-fonctions_SLA.R")
source("src/02-fonctions_SLA.R")

#=======================
#=======================
#variables baselines
bl <- readRDS("data/bl.rds")
bdd_dates <- readRDS("data/bdd_dates.rds")


bl$extraction <- as_date("2015-08-27")
bl2 <- bl[bl$extraction - bl$datevni >= 365, ]

d <- bl
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
ncu <- ncu[ncu$nval>1 , ] #non informatif si une seule valeur possible 
ncu <- ncu[ncu$nval>2 | (ncu$nval==2 & ncu$min_level>1) , ] #non informatif si seulement 1 patient à le nivaeu pour variable binaire

# #Je supprime les sous échelle de E_BULBAIRE et de ALSFRS
# ncu <- ncu[!grepl("E_BULBAIRE", ncu$var), ]
# ncu <- ncu[!grepl("ALS", ncu$var), ]

#pour regarder les différentes valeurs prises
# apply(d2, 2, unique)
# apply(d2, 2, unique)["FERM_BOUCHE"]#moins de 25% des sujets l'ont renseignée => pas dans binaire
# apply(d2, 2, unique)["OXY_THERAP"]#moins de 25% des sujets l'ont renseignée => pas dans binaire


binaire <- as.character(ncu[ncu$nval == 2, "var"])
quanti  <- as.character(ncu[ncu$nval > 6, "var"])
quali <- as.character(ncu[ncu$nval > 2 & ncu$nval <= 6, "var"])
#MRC est en fait quanti de 1 à 4, seul liuedeb est quali
quanti <- c(quanti, "MRC") 
quali <- "LIEUDEB_recode"

#=======================
#description des données manquantes
head(d)
missbl.df <- data.frame(variable=c(binaire,quanti, quali), missing = sapply(c(binaire,quanti, quali), function(x)sum(is.na(d[,x]))))
rownames(missbl.df) <- NULL
missbl.df$missing_perc <- paste0(round(missbl.df$missing/nrow(d),2)*100, "%")
write.table(print(missbl.df), file="clipboard", sep = "\t", row.names = FALSE)
saveRDS(missbl.df, "data/missingbl.rds")

# donnees manquantes pour criteres de decision de ventilation (0% est artificiel : on met 0 si critère non coché)
df1 <- data.frame(apply(bl[ ,c("CRIT_1_VNI", "CRIT_2_VNI", "CRIT_3_VNI")], 2, is.na))
df1$nbNA <- rowSums(df1)
df1$allNA <- df1$nbNA == 3
table(df1$allNA) #236 patient n'ont aucune information concernant le critère de mise en place de la vni
prop.table(table(df1$allNA))
#=======================
#analyses binaires

#----
#courbes de survie
.l1 <- lapply(binaire, function(x){
  print(x)
  draw_surv_bin(x, data = d, .time = "time.vni", .evt = "evt", vec_time_IC= 1, recode = FALSE, surv_only=FALSE, pvalue = TRUE, dep_temps=FALSE, .transf=NULL)
  })
ml <- marrangeGrob(.l1,ncol=1,nrow=1,top = NULL)
ggsave(file="data/analyses/courbes/binaire_bl.pdf", ml)

#----
#survie à 1 et 3 ans
.l2 <- lapply(binaire, function(x)draw_surv_bin(x, data = d, .time = "time.vni", .evt = "evt", vec_time_IC= 1, recode  = FALSE, surv_only=TRUE, pvalue = TRUE, dep_temps=FALSE, .transf=NULL))
df_surv <- do.call(rbind, .l2)
df_surv$survIC <- paste0(df_surv$survival, " [", df_surv$LCI, "-", df_surv$UCI, "]")
df_surv <- df_surv[ , c("variable", "group", "time", "survIC")]
saveRDS(df_surv, "data/analyses/df_surv_bl.rds")
df_surv <- df_surv[df_surv$time==1, ]

#----
#hypothèse des risques proportionnels
pdf(paste0("data/analyses/RP_bin.pdf"))
.l <- lapply(binaire, function(x)check_RP(var=x, data=d, .time="time.vni", .evt="evt", recode = FALSE))
dev.off()

#NB j'ai rempli dans un tableau excel au regard des courbes si l'hypothèse des risques proportionnels était respecté(TRUE) ou non (FALSE)
.df <- data.frame(variable = binaire, RP = NA)
write.table(print(.df), file="clipboard", sep="\t", row.names = FALSE)
#rp <- read.csv2("data/RP_bin.csv")
rp <- read.csv2("data/analyses/RP_bin.csv")

#separation var selon respect RP ou non
binRP <-  as.character(rp[rp$RP, "variable"]) #RP ok, pas de var dépendante du temps
binNRP <- as.character(rp[!rp$RP, "variable"]) #RP pas ok, il faut rajouter var dépendante du temps

#----
#tranformation des variables ne respectant pas les RP

#####
#transformation du temps
pdf(paste0("data/analyses/RP_bin_transft.pdf"))
.l <- lapply(binNRP, function(i){
  tmp <- lapply(c("log","sqrt","*t","/t","*t^2","*t^0.7", "log10", "*t^0.3", "*t^3"), function(x)add_vart_and_check(data=d, .time="time.vni", .evt="evt", recode = FALSE, var=i, .transf=x))
  #tmp <- lapply(c("log"), function(x)add_vart_and_check(data=d, .time="time.vni", .evt="evt", type="quali", var=i, .transf=x))
  .a <- do.call(rbind, tmp)
})
df_bint <- do.call(rbind, .l)
dev.off()

#remplissage de curve si test de beta_at(t) significatif
#remplissage à la main
write.table(print(df_bint[df_bint$beta_at==TRUE & df_bint$param == "at", c("variable", "transf", "curve")]), file="clipboard", sep="\t", row.names=F)
#rpt <- read.csv2("data/RP dep du temps beta ok.csv")
rpt <- read.csv2("data/analyses/RP_bin_transft_betaok.csv")
rpt <- merge(rpt, subset(df_bint, select=-curve), by=c("variable", "transf"), all= F)

#selection de la transformation par AICmin qd curve et Harrell ok
rpts <- rpt[rpt$curve==TRUE,]
AICmin <- tapply(as.numeric(as.character(rpts$AIC)), as.character(rpts$variable), min)
AICmin <- data.frame(variable = names(AICmin), AICmin = as.numeric(AICmin))
rpts <- merge(rpts, AICmin, by="variable")

binNRPt <- unique(rpts[rpts$AIC==rpts$AICmin, c("variable", "transf")])

df_dept <- merge(binNRPt, df_bint, by = c("variable","transf"), all=F)
df_dept$curve <- TRUE

#####
#Decoupe du temps pour les variables dont on n'a pas trouvé la bonne transformation:
tmp <- binNRP [!binNRP %in% binNRPt$variable]
#b <- c(6,18,50) #ok on garde 6 18 50 pour toutes les variables non corrigées par transformation du temps
#b <- c(4,18,50) ##tout est ok avec 6 18 50 sauf crit1 sauf crit1 qui est mieux avec 4 18 50 (les auters sont ok aussi)
b <- c(6, 18, 36)
#b <- c(6, 18, 36, 48)#pas mieux que 6 18 36
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
allbin$transf <- as.character(allbin$transf)
#saveRDS(allbin, "data/all_var_bin_bl.rds")
saveRDS(allbin, "data/analyses/all_var_bin_bl.rds")
#----
#HR [IC] et test du score (ou robuste)

.l <- lapply(1 : nrow(allbin), function(num){
  HR_score (var=allbin$variable[num], data = d, .time="time.vni", .evt="evt", recode=FALSE, .transf=allbin$transf[num], .tps_clinique=12)
})
df_HR <- do.call(rbind,.l)

#saveRDS(df_HR, "data/HRIC_bin_bl.rds")
saveRDS(df_HR, "data/analyses/HRIC_bin_bl.rds")
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
write.table(print(RP_quanti[ ,c("variable", "RP")]), file="clipboard", sep="\t", row.names =F)
#rp <- read.csv2("data/RP_quanti_bl.csv") #RP = TRUE pour hyopthèses respectée
rp <- read.csv2("data/analyses/RP_quanti_bl.csv") #RP = TRUE pour hyopthèses respectée

#j'ajoute les info concernant la loglin
rp <- merge(quanti_ln, rp, by="variable")
rp <- rp[ , c("variable", "recode", "RP")]

#separation var selon respect RP ou non
QRP <-  rp[rp$RP, ] #RP ok, pas de var dépendante du temps
QRP$transf <- NA
QRP <- QRP[ , c("variable", "recode", "transf")]
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
#aucun ne convient(pvalue de at >0.05=> aucune courbe tracee) 30/05/2017 12062017

# #Je ne regarde que les transformations pour lesquelles la pvalue de at est inférieure à 0.05 (beta_at = TRUE) (la courbe n'est de toute façon pas tracée quand sup à 0.05)
# 
# #remplissage de curve si test de beta_at(t) significatif
# #remplissage à la main de la colonne curve à l'aide du fichier "data/analyses/RP_quanti_bl_transft.pdf"
# write.table(print(df_quant[df_quant$beta_at==TRUE & df_quant$param == "at", c("variable", "transf", "curve")]), file="clipboard", sep="\t", row.names=F)
# rpt <- read.csv2("data/RP quanti dept beta ok.csv")
# rpt <- merge(rpt, subset(df_quant, select=-curve), by=c("variable", "transf"), all= F)
# 
# #selection de la transformation qd curve et Harrell ok (en fait quand curve ok, harrell est forcémen ok aussi)
# rpts <- rpt[rpt$curve==TRUE,]
# #Je prend la transformation avec le plus petit AIC
# AICmin <- tapply(as.numeric(as.character(rpts$AIC)), as.character(rpts$variable), min)
# AICmin <- data.frame(variable = names(AICmin), AICmin = as.numeric(AICmin))
# rpts <- merge(rpts, AICmin, by="variable")
# #pb : log 10 a le même AIC => je supprime log10
# 
# QN_RPt <- unique(rpts[rpts$AIC==rpts$AICmin, c("variable", "recode", "transf")])
# QN_RPt$variable <- as.character(QN_RPt$variable)
# QN_RPt <- QN_RPt[QN_RPt$transf != "log10", ]

#####
#Decoupe du temps pour les variables dont on n'a pas trouvé la bonne transformation:
#tmp <- QN_RP [!QN_RP$variable %in% QN_RPt$variable, ]
tmp <- QN_RP 

#b <- c(7, 17, 34, 60) #utilisé pour présentation fin stage 
b <- c(6, 18, 36, 48) #c'est mieux et plus facilement interprêtable
# i=5
# add_vart_and_check(data=d, .time="time.vni", .evt="evt", recode = tmp$recode[i], var=tmp$variable[i], vec_cut = b)

pdf(paste0("data/analyses/RP_quanti_bl_decoup_", paste(b, collapse='-'), ".pdf"))
.l <- lapply(1: nrow(tmp), function(i){
  #add_vart_and_check(data=d, .time="time.vni", .evt="evt", type="quali", var=i, vec_cut = b)
  add_vart_and_check(data=d, .time="time.vni", .evt="evt", recode = tmp$recode[i], var=tmp$variable[i], vec_cut = b)
})
df_quanti_cutt <- do.call(rbind, .l)
dev.off()
QN_RPcut <- unique(df_quanti_cutt[ ,c("variable", "recode", "transf")])
#tout est ok sauf 48 et plus pour age debut

#####
#J'ajoute une colonne transf NA pour les variables ok


#all var et transf
#allquant <- rbind(QRP, QN_RPcut, QN_RPt)
allquant <- rbind(QRP, QN_RPcut)
rownames(allquant) <- 1:nrow(allquant)
allquant$variable <- as.character(allquant$variable)
saveRDS(allquant, "data/analyses/all_var_quanti_bl.rds")

#----
#HR [IC] et test du score (ou robuste)

.l <- lapply(1 : nrow(allquant), function(num){
  HR_score (var=allquant$variable[num], data = d, .time="time.vni", .evt="evt", recode=allquant$recode[num], .transf=allquant$transf[num], .tps_clinique=12)
})
df_HR_quanti_bl <- do.call(rbind,.l)
#df_HR_quanti_bl <- rbind(df_HR_quanti_bl, MRC)
saveRDS(df_HR_quanti_bl, "data/analyses/HRIC_quanti_bl.rds")

#----------------------
#Survie pour les quanti recodées
allquant <- readRDS("data/analyses/all_var_quanti_bl.rds")
#courbe
df <- allquant[allquant$recode==TRUE, ]
.l <- lapply(df$variable, function(x)draw_surv_bin(var=x, data = d, .time="time.vni", .evt="evt", vec_time_IC= 1,
                                                   recode=TRUE, surv_only=FALSE, pvalue = TRUE))
ml <- marrangeGrob(.l,ncol=1,nrow=1,top = NULL)
ggsave(file="data/analyses/courbes/courbe_survie_quanti_recode_bl.pdf", ml)

#survie à 1 et 3 ans
.l <- lapply(df$variable, function(x)draw_surv_bin(var=x, data = d, .time="time.vni", .evt="evt", vec_time_IC= 1,
                                                   recode=TRUE, surv_only=TRUE))
df_surv_quant_bl <- do.call(rbind, .l)

df_surv_quant_bl$survIC <- paste0(df_surv_quant_bl$survival, " [", df_surv_quant_bl$LCI, "-", df_surv_quant_bl$UCI, "]")
df_surv_quant_bl <- df_surv_quant_bl[ , c("variable", "group", "time", "survIC")]
saveRDS(df_surv_quant_bl, "data/analyses/df_surv_quanti_bl.rds")
df_surv_quant_bl <- df_surv_quant_bl[df_surv_quant_bl$time==1, ]


#=======================
#analyses quali plus de 2 classes

quali<-"LIEUDEB_recode"
#courbes de survie
.l <- lapply(quali, function(x) draw_surv_qualisup2(var=x, data = d, .time = "time.vni", .evt = "evt", vec_time_IC = 1 , surv_only=FALSE, pvalue=TRUE))
ml <- marrangeGrob(.l,ncol=1,nrow=1,top = NULL)
ggsave(file="data/analyses/courbes/courbe_survie_qualisup2_bl.pdf", ml)

#survie
df_surv_q_bl <- draw_surv_qualisup2(var="LIEUDEB_recode", data = d, .time = "time.vni", .evt = "evt", vec_time_IC = 1 , surv_only=TRUE, pvalue=TRUE)
df_surv_q_bl$survIC <- paste0(df_surv_q_bl$survival, " [", df_surv_q_bl$LCI, "-", df_surv_q_bl$UCI, "]")
df_surv_q_bl <- df_surv_q_bl[ , c("variable", "group", "time", "survIC")]
saveRDS(df_surv_q_bl, "data/analyses/df_surv_q_bl.rds")

#Yann
d$t<-d$time.vni/(365.25/12)
cox<-coxph(Surv(t, evt)~LIEUDEB_recode, data=d)
summary(cox)
b<-coef(cox)
zr<-cox.zph(cox, "rank")
zr
zi<-cox.zph(cox, "identity")
z<-zi
par(mfrow=c(2,2))
for (iz in 1:4) {
plot(z[iz], col="darkgrey", ylim=c(-4,4))
abline(h=b[iz], col="red")
}
#fin Yann


#check RP
pdf(paste0("data/analyses/RP_quali_bl.pdf"))
.l <- lapply(quali, function(x) check_RP(var=x, data=d, .time="time.vni", .evt="evt", quali = TRUE))
dev.off()

quali2.df <- data.frame(variable = quali2, recode=FALSE, .transf = NA)

s <- d
s$tps <- (s$time.vni/(365.25/12)) + 0.001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner
s <- s[!is.na(s$LIEUDEB_recode),]

#modif Non RP : lieudeb recode : a faire
s$cerv <- ifelse(s$LIEUDEB_recode=="cervical",1,0)
s$llimb <- ifelse(s$LIEUDEB_recode=="lower limb", 1, 0)
s$resp <- ifelse(s$LIEUDEB_recode=="respiratory", 1, 0)
s$ulimb <- ifelse(s$LIEUDEB_recode=="upper limb", 1, 0)

ti <- sort(unique(c(0,s$tps[s$evt==1])))
slat <- s
slat$start <- 0
slat$stop <- slat$tps
slat <- survSplit(slat, end="stop", event="evt", start="start",cut=ti) #Yann
#slat <- survSplit(Surv(stop,evt)~.,slat,start="start",cut=ti)           #Sarah

coxt <- coxph(Surv(start, stop, evt) ~ cerv + llimb + resp + ulimb+cluster(PATIENT), data=slat)
summary(coxt)$coefficients

# #Yann
# slat$cerv_t <- slat$cerv * (slat$stop) #non graphe mauvais
# slat$llimb_t <- slat$llimb * log(slat$stop)*slat$stop^(1/2) #non graphe mauvais
# slat$llimb_t <- slat$llimb * log(slat$stop) #non graphe mauvais
# slat$resp_t <- slat$resp * (slat$stop) #non graphe mauvais
# slat$ulimb_t <- slat$ulimb * (slat$stop) #non graphe mauvais
# slat$cerv_t2 <- slat$cerv * (slat$stop)^(2) #non graphe mauvais
# slat$llimb_t2 <- slat$llimb * (slat$stop)^(1/2) #non graphe mauvais
# slat$resp_t2 <- slat$resp * (slat$stop)^(2) #non graphe mauvais
# slat$ulimb_t2 <- slat$ulimb * (slat$stop)^(2) #non graphe mauvais
# 
# coxt2 <- coxph(Surv(start, stop, evt) ~ cerv + llimb + resp + ulimb+cerv_t + llimb_t + resp_t + ulimb_t+cerv_t2 + llimb_t2 + resp_t2 + ulimb_t2+cluster(PATIENT), data=slat)
# summary(coxt2)$coefficients
# #coxt2b <- coxph(Surv(start, stop, evt) ~ cerv + llimb + resp + ulimb+cerv_t + llimb_t + resp_t + ulimb_t+cerv_t2 + llimb_t2 + resp_t2 + ulimb_t2+cluster(PATIENT), 
# #                data=slat, init=c(coef(coxt), rep(0, 8)))
# coxt2b <- coxph(Surv(start, stop, evt) ~ cerv + llimb + resp + ulimb+ llimb_t+cluster(PATIENT), 
#                 data=slat, init=c(coef(coxt), rep(0, 1)))
# summary(coxt2b)
# 
# 
# b<-coef(coxt)
# bb<-coef(coxt2b)
# 
# ddl<-length(bb)-length(b)
# 1-pchisq(coxt2b$rscore, ddl)
# 
# zr<-cox.zph(coxt2b, "rank")
# zr
# zi<-cox.zph(coxt2b, "identity")
# zi
# 
# z<-zi
# par(mfrow=c(2,2))
# for (iz in 1:12) {
# #N  plot(z[iz], col="lightblue", ylim=c(-4,4))
#   plot(z[iz], col="lightblue")
#   abline(h=bb[iz], col="red")
# }


# slat$cerv_t <- slat$cerv * (slat$stop^0.7) #non graphe mauvais
# slat$llimb_t <- slat$llimb * (slat$stop^0.7) #non graphe mauvais
# slat$resp_t <- slat$resp * (slat$stop^0.7) #non graphe mauvais
# slat$ulimb_t <- slat$ulimb * (slat$stop^0.7) #non graphe mauvais
# slat$cerv_t <- slat$cerv * log(slat$stop) #non graphe mauvais
# slat$llimb_t <- slat$llimb * log(slat$stop) #non graphe mauvais
# slat$resp_t <- slat$resp * log(slat$stop) #non graphe mauvais
# slat$resp_t <- slat$resp * (slat$stop)^2 #non graphe mauvais
# slat$resp_t <- slat$resp * (slat$stop) #non graphe mauvais
slat$resp_t <- slat$resp / (slat$stop) #beta ok et graphe ok qd seul
# slat$ulimb_t <- slat$ulimb * log(slat$stop) #non graphe mauvais
# slat$cerv_t <- slat$cerv * (slat$stop^3) #non graphe mauvais
# slat$llimb_t <- slat$llimb * (slat$stop^3) #non graphe mauvais
# slat$resp_t <- slat$resp * (slat$stop^3) #non graphe mauvais
# slat$ulimb_t <- slat$ulimb * (slat$stop^3) #non graphe mauvais
# slat$cerv_t <- slat$cerv * sqrt(slat$stop) #non graphe mauvais
# slat$llimb_t <- slat$llimb * sqrt(slat$stop) #non graphe mauvais
# slat$resp_t <- slat$resp * sqrt(slat$stop) #non graphe mauvais
# slat$ulimb_t <- slat$ulimb * sqrt(slat$stop) #non graphe mauvais

#coxt <- coxph(Surv(start, stop, evt) ~ cerv + llimb + resp + ulimb + cerv_t + llimb_t + resp_t + ulimb_t +cluster(PATIENT), data=slat)
coxt <- coxph(Surv(start, stop, evt) ~ cerv + llimb + resp + ulimb + resp_t +cluster(PATIENT), data=slat)
test <- summary(coxt)
test$coefficients
b<-coef(coxt)
zi<-cox.zph(coxt, "identity")
z<-zi
par(mfrow=c(2,3))
for (iz in 1:5) {
  #N  plot(z[iz], col="lightblue", ylim=c(-4,4))
  plot(z[iz], col="lightblue")
  abline(h=b[iz], col="red")
}
#Correction de resp en 1/t fonctionne

#revoir avec Yann interpretation des quali a plus de 2 classes 
var <- "LIEUDEB_recode"
.tps_clinique <- 12
#HRIC pour les levels non modifiés
coefbeta <- round(test$coefficients[ ,"coef"], 5)
stat <- round(test$robscore["test"],2)
pval <- round(test$robscore["pvalue"],4)
tps_clinique <- NA
HRIC <- round(data.frame(IC=test$conf.int[,1], l=test$conf.int[,3], u=test$conf.int[,4]),3)
HRIC <-  HRIC[c("cerv", "llimb", "ulimb"), ]
HR <- HRIC$IC
HRIC <- paste0(HRIC[,1], "[", HRIC[,2], "-", HRIC[,3], "]")
df <- data.frame(param = c("cerv", "llimb", "ulimb"), beta = coefbeta[c("cerv", "llimb", "ulimb")], HRIC = HRIC)

#HRIC pour resp
S <- vcov(coxt)
t <- .tps_clinique 
t_t <- 1/t
S <- S[grep("resp", rownames(S)), grep("resp", colnames(S))]
b2 <- b[grep("resp", names(b))]
variance <- S[1,1]+S[2,2]*t_t^2+2*S[1,2]*t_t
m <- b2[1]+b2[2]*t_t #coef de l'HR
HRIC2 <- round(c(exp(m), exp(m + qnorm(0.975)*sqrt(variance) * c(-1,1))),3)
HRIC2 <- paste0(HRIC2[1], " [", HRIC2[2], " - ", HRIC2[3],"]")
df <- rbind(df, data.frame(param = "resp; resp t", beta = paste(round(b2,3), collapse = ";"), HRIC = HRIC2))

df_HR_quali_bl <-data.frame(variable = "LIEUDEB_recode", recode = FALSE, RP = FALSE, 
                            transf = c(NA, NA, NA, "/t"), tps_clinique = .tps_clinique, HRIC = df$HRIC, test = "robust",
                            statistic = stat, pvalue = pval, param = df$param, beta = df$beta)#HRIC : sans transformation uniquement pour l'instant


# # Decoupe du temps
# b <- c(6)
# name_cut <- paste(b, collapse = "-")
# vec_lieud <- c("cerv", "llimb", "resp", "ulimb") 
# for (j in vec_lieud){
#   for (i in 1:(length(b)+1)){
#     if (i == 1) tmp <-  slat[ ,j] * ifelse(slat$stop<=b[1], 1, 0)
#     if(i <= length(b) & i!= 1) tmp <- slat[ ,j] * ifelse(slat$stop>b[i-1] & slat$stop<=b[i], 1, 0)
#     if(i == (length(b)+1)) tmp <-  slat[ ,j] * ifelse(slat$stop>b[i-1], 1, 0)
#     slat[ ,paste0(j,i)] <- tmp
#   }
# }
# 
# vat<-apply(expand.grid(list(v=vec_lieud, r=1:(length(b)+1))),1, paste, collapse = "")#paste("at",  1:(length(b)+1), sep="")
# x <- slat[, vat]
# sx<-colSums(x) #interval de temps sans evt
# wat<-vat[sx>0] #on supprime interval de temps quand pas d'evenement
# f<-paste("Surv(start, stop, evt) ~ ", paste(wat, collapse=" + "),"+ cluster(PATIENT)", sep="")  #on ne met pas a_recode car les at couvre deja  toutes les perdiodes
# 
# coxt <- coxph(as.formula(f), data=slat)
# 
# #defaut de convergence pour certaines variables
# 
# #on ne fait que la courbe et pas de test de Harrell pour la découpe du temps
# zt <- cox.zph(coxt, transf="identity")
# for (i in 1:(nrow(zt$table)-1)){
#   iz<-i
#   plot(zt[iz], resid = FALSE, main = paste0("plot shoenfeld for LIEUDEB cut ", name_cut))
#   abline(h=0, col="red")
#   abline(h=coef(coxt)[iz], col="blue", lty=2)
#   abline(v=b)
# }
# 
# #interpretation pour la decoupe du temps
# test <- summary(coxt)
# coefbeta <- round(test$coefficients[ ,"coef"], 5)
# name_param <- rownames(test$coefficients)
# stat <- round(test$robscore["test"],2)
# pval <- round(test$robscore["pvalue"],4)
# .tps_clinique <- 12
# i <- findInterval(.tps_clinique, b) + 1 #findInterval commence à 0...
# HRIC <- round(exp(cbind(coef(coxt)[i], qnorm(0.025, coef(coxt)[i], sqrt(diag(vcov(coxt))[i])), qnorm(1-0.025, coef(coxt)[i], sqrt(diag(vcov(coxt))[i])))),3)
# HRIC <- paste0(HRIC[1], " [", HRIC[2], " - ", HRIC[3],"]")


# df_HR_quali_bl <-data.frame(variable = "LIEUDEB_recode", recode = FALSE, RP = FALSE, 
#                             transf = b, tps_clinique = .tps_clinique, HRIC = HRIC, test = "robust",
#                             statistic = stat, pvalue = pval, param = name_param, beta = coefbeta)#HRIC : sans transformation uniquement pour l'instant
# .l <- lapply("LIEUDEB_recode", function(.var){
#   HR_score (var=.var , data = d, .time="time.vni", .evt="evt", quali = TRUE, recode=FALSE, .transf=NA, .tps_clinique=12)
# })
# df_HR_quali_bl <- do.call(rbind,.l)
saveRDS(df_HR_quali_bl, "data/analyses/HRIC_quali_bl.rds")



#=======================
#=======================
#variables dont la valeur dépendant du temps
d <- readRDS("data/df_rep.rds")
d <- d[d$PATIENT %in% bl$PATIENT, ] #inutile c'est deja le cas


#----------------
#selection des variables
d2 <- d
nb_pat_byvar <- lapply(tapply(d2$PATIENT, d2$qui, c), function(x)length(unique(x)))
allx_byvar <- tapply(d2$x, d2$qui, c)
nrow_byvar <- lapply(allx_byvar, length) #nb de rang par variable
nval_byvar <- lapply(allx_byvar, function(x)length(unique(x))) #nb de valeurs différentes par variables
min_level <-  lapply(allx_byvar, function(x)min(as.numeric(table(x)))) #nombre minimum de personnes par catégorie
ncu <- data.frame(var = names(allx_byvar), nbrow = as.numeric(nrow_byvar), nval = as.numeric(nval_byvar), npat = as.numeric(nb_pat_byvar), min_level=as.numeric(min_level))

#Je supprime variable si moins de 25% des sujets l'ont renseignée
#n pour 25% des sujets
n25 <- nrow(bl)*0.25
ncu <- ncu[ncu$npat > n25, ]
ncu <- ncu[ncu$nval>1 , ] #non informatif si une seule valeur possible 
ncu <- ncu[ncu$nval>2 | (ncu$nval==2 & ncu$min_level>1) , ] #non informatif si seulement 1 patient à le nivaeu pour variable binaire

#Je supprime var :
# sup <- c("DCD", "DIAG", "DIAGPROBA")
# ncu <- ncu[!ncu$var %in% sup, ]

#Pour savoir qui est quali et qui est quanti:
ncu[order(ncu$nval),]
lapply(allx_byvar, table)["EVAL__ALS_FRS_R"]

#autres var à supprimer car hors sujet
ncu <- ncu [!ncu$var %in% c("TEST_MUSCUL","EVAL_COMPL", "EVAL__ALS_FRS_R", "MODIF_PARAM_SV","S_HOFFMANN_CHOICE_1", "S_HOFFMANN_CHOICE_2", "TREPIDATION_PIED_CHOICE_1", "TREPIDATION_PIED_CHOICE_2"), ]

#Les TTT ne converge pas bien et est probablement mal renseignée (seulement 30 ttt par kiné...)
ncu <- ncu [!ncu$var %in% paste0("TTT_CHOICE_", 1:10), ]

binaire <- data.frame(var = as.character(ncu[ncu$nval == 2, "var"]), stringsAsFactors = FALSE)
binaire$type <- "binaire"
quanti  <- data.frame(var = as.character(ncu[ncu$nval > 6, "var"]), stringsAsFactors = FALSE)
quanti$type <- "quanti"
quali <- data.frame(var = as.character(ncu[ncu$nval > 2 & ncu$nval <= 6, "var"]), stringsAsFactors = FALSE)
quali$type <- "quali"
#as.character(ncu[ncu$nval == 1, "var"])
s_i <- d2

#all_var <- rbind(binaire, quali, quanti)
all_var <- rbind(binaire, quanti)
all_var$recode <- FALSE
all_var$Harrell_test <- NA

#------------------------
#nombre de NA

#liste des variables
var <- as.character(ncu$var)
#nb de consult 
ncs <- d %>% group_by(PATIENT) %>% count(date)
ncs <- sum(table(ncs$PATIENT))

#nb total de valeurs renseignees par variable /nb total de consult
perc_var <- paste0(round(unlist(lapply(var, function(x) nrow(d[d$qui == x, ])/ncs)),2)*100, "%")

#nb de patients sans aucune valeur
#ntot patients
ntot <- length(names(table(d$PATIENT)))
#ntot <- length(names(table(bl$PATIENT))) #idem 747
#npatient avec au moins une valeur (par variable)
.l <- lapply(var, function (.var){
  nNO <- d %>% filter(qui==.var) %>% count(PATIENT)
  nNO <- ntot - length(names(table(nNO$PATIENT)))
  #nNO <- paste0(round((ntot - length(names(table(nNO$PATIENT))))/ntot , 2)* 100, "%")
})
#perc_pat <- unlist(.l)
N_pat_missing <- unlist(.l)
perc_pat

missingrep <- data.frame(variable = var, perc_var, N_pat_missing)
missingrep$perc_pat <- paste0(round(missingrep$N_pat_missing/ntot , 2)* 100, "%")

saveRDS(missingrep, "data/missingrep.rds")
#   
# d2 <- d
# nb_pat_byvar <- lapply(tapply(d2$PATIENT, d2$qui, c), function(x)length(unique(x)))
# missrep <- data.frame(variable = names(nb_pat_byvar), n_pat_nonNA = as.numeric(nb_pat_byvar))
# missrep$n_patNA <- length(unique(d2$PATIENT)) - missrep$n_pat_nonNA
# missrep$perc_NA <-  paste0(round(missrep$n_patNA/ length(unique(d2$PATIENT))*100,0), "%")

#----------------
#QUANTI et BINAIRE
#----------------
#verif loglin et RP

pdf("data/analyses/RP_var_rep_Q_bin.pdf")
for (nr in 1:nrow(all_var)) {#ce sera le debut de la boucle
#for (nr in 21) {#ce sera le debut de la boucle
  #for (nr in 64) {#ce sera le debut de la boucle
  var <- all_var$var[nr]
  print(var)
  s <- s_i[s_i$qui==var, ]
  s$time <- ifelse(s$del<0, 0, s$del) #del est le délai entre la date de receuil de la variable(date) et la date de vni (datevni). le del peut ê négatif pour les variables baseline
  names(s)
  #en mois
  s$time <-(s$time/365.25*12) + 0.001
  s$time.vni <- (s$time.vni /365.25*12) + 0.001
  
  idu<-sort(unique(s$PATIENT))
  
  #s[, "x"]<-as.numeric(as.character(s[, "x"]))
  s$x<-as.numeric(as.character(s$x))
  
  #La boucle suivante permet de réaliser ce que ferait automatiquement un survsplit mais en découpant chaque patient par ses valeurs uniquement
  for (id in idu) {
    s1 <- s[s$PATIENT==id, c("PATIENT", "time.vni", "evt", "time", "x")]
    s1 <- s1[!is.na(s1$time),]
    s1 <- s1[order(s1$time),]
    ns1<-dim(s1)[1] #nombre de changement de valeurs pour un patient
    #si x à la valeur NA, alors je remplace par la dernière valeur connue (ce cas existe-t-il vraiment?)
    if (ns1>1) {
      if (is.na(s1[1, "x"])) s1[1, "x"] <-s1[2, "x"]
      for (x in 2:nrow(s1)){
        if (is.na(s1[x, "x"])) s1[x, "x"] <-s1[(x-1), "x"]
      }
    }
    d<-s1
    #creation de la table avec start stop, la valeur prise dans chaque interval et l'evt dans le bon interval (survsplit mais à la main)
    if (ns1>=1) {
      d$id<-d$PATIENT
      d$del<-d$time.vni
      d$n<-ns1
      t<-sort(unique(d$time));t
      
      dt<-d[, c("id", "del", "evt", "time", "n", "x")]
      n <-dim(d)[1]
      dt$start<-c(d$time)
      dt$stop<-c(d$time[-1], d$del[n])
      dt$etat<-0;dt$etat[ns1]<-dt$evt[ns1]
      dt$w<-c(d[, "x"])
    } else { #si le patient n'a pas de ligne, le tableau est vide (? ce cas peut-il vraiment exister? si oui pourquoi ne pas faire dt <- NULL ?)
      dt<-s0
    }
    #je merge le tableau du patient idu[id] à celui du patient idu[id-1]
    if (id==idu[1]) {
      Dt<-dt
    } else {
      Dt<-rbind(Dt, dt)
    }
  }
  
  #Je remet dans l'ordre : par patient et par temps de start
  Dt<-Dt[order(Dt$id, Dt$start),]
  #head(Dt[Dt$n==5,]) #montre les lignes des patients qui ont au moins 5 intervalles (5 changements de valeur pour la variable var)
  #Dt[is.na(Dt$evt),]
  ttab<-table(tab<-table(Dt$id));ttab
  #sum(ttab)
  #table(Dt$n)
  
  #data management sur ce fichier long découpé par intervalles de changement de valeurs
  #gestion du cas start=stop : survient uniquement pour le dernier intervalle d'un patient, lorsqu'une nouvelle valeur est enregistrée
  #lors d'une visite, mais que cette visite correspond à la date de dernière nouvelle (etat=0) ou à la date de décès (état=1)
  #afficher cas etat=0
  Dt$i<-1:dim(Dt)[1] #numérote les lignes
  ote<-ifelse(Dt$start==Dt$stop & Dt$etat==0, 1, 0)
  io<-Dt$i[ote==1] #recupere lignes pour lesquels dernier interval : start=stop et etat=0
  jo<-sort(unique(c(io, io-1)))#juste pour verif qu'on peut supprimer derniere ligne : on regarde ensemble derniere ligneet avant derniere
  head(Dt[Dt$i %in% jo,])
  #afficher cas etat=1
  remp<-ifelse(Dt$start==Dt$stop & Dt$etat==1, 1, 0)
  ir<-Dt$i[remp==1]
  jr<-sort(unique(c(ir, ir-1)))
  head(Dt[Dt$i %in% jr,])
  
  #gérer cas etat=1 : c'est la ligne d'avant qui devient evt=1(la date d'evt correspond à la date du stop, donc la date d'evt est juste) et la ligne start=stop est supprimée
  Dt$etat[Dt$i %in% ir]
  Dt$etat[Dt$i %in% (ir-1)]<-1
  #gérer cas etat=1 ou etat=0 : on supprime la dernière ligne de ces 2 cas possibles du tableau Dt
  Dt<-Dt[!(Dt$i %in% c(io, ir)),]
  head(Dt[Dt$i %in% c(jo),])
  head(Dt[Dt$i %in% c(jr),])
  #NB : les lignes start=stop sont de toutes façons eliminées par coxph, mais là au moins on récupère l'info du décès.
  
  # Dt[Dt$start>=Dt$stop,]
  # bd[bd$time.vni==0,]
  # s[s$time.vni==0,]
  
  #======================================
  #3/verif des hypothèses pour une variable donnée
  #NB : il faudra faire une grande boucle qui va jusqu'à la vérif des risques proportionnels => creation de HRok (boucle sur le nom des variables)
  #puis continuer la boucle jusqu'à test du score et HRIC (rbind de res)
  
  .title <- paste0("RP of ", var)
  #------------
  #si quanti, verif de la loglin
  if (all_var$type[nr] == "quanti"){
    #3.1/ Loglin
    cox<-coxph(Surv(start, stop, etat)~poly(x, df=2, raw=T) + cluster(id), data=Dt)
    tabcox <- summary(cox)
    pval_loglin <- round(tabcox$coefficients[2,"Pr(>|z|)"],3)
    if (is.na(pval_loglin)) all_var$recode[nr] <- FALSE #?
    else{
      if (pval_loglin <= 0.05) {
        #Dt$x <- ifelse(Dt$x < median(Dt$x), 0, 1)
        Dt$x <- ifelse(Dt$x <= median(Dt[Dt$time==0.001, "x"]), 0, 1)#pb pour util diurn de vni : valeur imputee a 0 donc median a start = 0 : 0 : resolu en mettant <= au lieu de <
        all_var$recode[nr] <- TRUE
        .title <- paste0 ("RP of ", var, " superior to ", round(median(Dt[Dt$time==0.001, "x"]),0))
      } else { #pval_loglin non signif => Log lin respectée
        all_var$recode[nr] <- FALSE
      }
    }
  }
  
  #--------------
  #3.2/ vérif hypoth des risques proportionnels
  mod <- coxph(Surv(start, stop, etat)~x+cluster(id), data=Dt) #deja recode ou non en 0/1 selon étape précédente
  if (var %in% c("SPO2_AVT_PP", "SPO2_APR_PP")){
    z <- cox.zph(cox, transform = "rank")
    .rp <- round(z$table[,3],3)
    plot (0,0, main=paste0("RP of ", var, "\nHarrell test p = ", pval))
  } else {
    
    #Test de Harrell
    z <- cox.zph(mod, transform = "rank")
    cat("Test de Harrell\n\n")
    print(z)
    pval <- round(z$table[,3],3)
    cat(paste0("\nTest de Harrell p value: ", pval))
    #non signif si p>=0.05
    
    #résidus de Shoenfeld
    z <- cox.zph(mod, transf="identity")
    plot(z, main=paste0(.title, "\nHarrell test p = ", pval), resid = FALSE, ylab = paste0("Beta(t) for ", var),
         xlab = "Time, months")
    abline(h=0, col="red")
    abline(h=coef(mod), col="blue")
    #non significatif si l'IC contient a tout moment la courbe rouge
    
  }
  all_var$Harrell_test[nr] <- pval
  all_var$RP
  print(var)
}
dev.off()

#=================
#Modif quand RP non respectées

#Je rempli colonne RP en fonction de la courbe : si y=0 est toujours contenu par les bornes de l'IC et pvalue non signif, RP = T(l'hyp des RP est repectée), sinon RP = F
all_var$RP <- NA
#1 je copie le tableau dans excel
write.table(print(all_var), file="clipboard", sep="\t", row.names=F)
#2 je rempli à la main et j'enregistre sous csv
#3 j'importe le tableau
#all_var <- read.csv2("data/RP_all_rep.csv")
all_var <- read.csv2("data/analyses/RP_var_rep_Q_bin.csv")
all_var$var <- as.character(all_var$var)
all_var$type <- as.character(all_var$type)


#faire un tableau avec les coef pour chaque variable et sortir un schéma pour chaque variable
#puis générer un vecteur HR : si le risque prop est vérifié, HR=1 sinon HR=0
#Faire un tableau avec : HRok <- var, HR0/1
rRPok <- all_var[all_var$RP==TRUE, ]#HR est à modifier en fonction des résultats aux risques proportionnels
rRPok$transf <- NA


rNok <- all_var[all_var$RP==FALSE, ]

#pdf("data/analyses/RP_var_rep_findtransf.pdf")
pdf("data/analyses/RP_var_rep_Q_bin_transft.pdf")
.l <- lapply(1:nrow(rNok), function(nr){
  #.l <- lapply(1, function(nr){
  #------------------
  #gestion du changement de valeur en fonction du temps
  
  var <- rNok$var[nr]
  recode <- rNok$recode[nr]
  
  print(var)
  s <- s_i[s_i$qui==var, ]
  s$time <- ifelse(s$del<0, 0, s$del) #del est le délai entre la date de receuil de la variable(date) et la date de vni (datevni). le del peut ê négatif pour les variables baseline
  
  #en mois
  s$time <-(s$time/365.25*12) + 0.001
  s$time.vni <- (s$time.vni /365.25*12) + 0.001
  
  idu<-sort(unique(s$PATIENT))
  s[, "x"]<-as.numeric(as.character(s[, "x"]))
  
  #La boucle suivante permet de réaliser ce que ferait automatiquement un survsplit mais en découpant chaque patient par ses valeurs uniquement
  for (id in idu) {
    s1 <- s[s$PATIENT==id, c("PATIENT", "time.vni", "evt", "time", "x")]
    s1 <- s1[!is.na(s1$time),]
    s1 <- s1[order(s1$time),]
    ns1<-dim(s1)[1] #nombre de changement de valeurs pour un patient
    #si x à la valeur NA, alors je remplace par la dernière valeur connue (ce cas existe-t-il vraiment?)
    if (ns1>1) {
      if (is.na(s1[1, "x"])) s1[1, "x"] <-s1[2, "x"]
      for (x in 2:nrow(s1)){
        if (is.na(s1[x, "x"])) s1[x, "x"] <-s1[(x-1), "x"]
      }
    }
    d<-s1
    #creation de la table avec start stop, la valeur prise dans chaque interval et l'evt dans le bon interval (survsplit mais à la main)
    if (ns1>=1) {
      d$id<-d$PATIENT
      d$del<-d$time.vni
      d$n<-ns1
      t<-sort(unique(d$time));t
      
      dt<-d[, c("id", "del", "evt", "time", "n", "x")]
      n <-dim(d)[1]
      dt$start<-c(d$time)
      dt$stop<-c(d$time[-1], d$del[n])
      dt$etat<-0
      dt$etat[ns1]<-dt$evt[ns1]
      dt$w<-c(d[, "x"])
    } else { #si le patient n'a pas de ligne, le tableau est vide (? ce cas peut-il vraiment exister? si oui pourquoi ne pas faire dt <- NULL ?)
      dt<-s0
    }
    #je merge le tableau du patient idu[id] à celui du patient idu[id-1]
    if (id==idu[1]) {
      Dt<-dt
    } else {
      Dt<-rbind(Dt, dt)
    }
  }
  
  #Je remet dans l'ordre : par patient et par temps de start
  Dt<-Dt[order(Dt$id, Dt$start),]
  
  #data management
  #gestion du cas start=stop : survient uniquement pour le dernier intervalle d'un patient, lorsqu'une nouvelle valeur est enregistrée
  #lors d'une visite, mais que cette visite correspond à la date de dernière nouvelle (etat=0) ou à la date de décès (état=1)
  #afficher cas etat=0
  Dt$i<-1:dim(Dt)[1] #numérote les lignes
  ote<-ifelse(Dt$start==Dt$stop & Dt$etat==0, 1, 0)
  io<-Dt$i[ote==1] #recupere lignes pour lesquels dernier interval : start=stop et etat=0
  jo<-sort(unique(c(io, io-1)))#juste pour verif qu'on peut supprimer derniere ligne : on regarde ensemble derniere ligneet avant derniere
  
  #afficher cas etat=1
  remp<-ifelse(Dt$start==Dt$stop & Dt$etat==1, 1, 0)
  ir<-Dt$i[remp==1]
  jr<-sort(unique(c(ir, ir-1)))
  
  #gérer cas etat=1 : c'est la ligne d'avant qui devient evt=1(la date d'evt correspond à la date du stop, donc la date d'evt est juste) et la ligne start=stop est supprimée
  Dt$etat[Dt$i %in% (ir-1)]<-1
  #gérer cas etat=1 ou etat=0 : on supprime la dernière ligne de ces 2 cas possibles du tableau Dt
  Dt<-Dt[!(Dt$i %in% c(io, ir)),]
  
  #------------------
  #si loglin non vérifiée, je recoupe à la médiane
  if (recode == TRUE) {
    Dt$x <- ifelse(Dt$x <= median(Dt[Dt$time==0.001, "x"]), 0, 1)
    .title <- paste0 ("RP of ", var, " superior to ", round(median(Dt[Dt$time==0.001, "x"]),0))
  } else {
    .title <- paste0 ("RP of ", var)
  }
  
  #------------------
  #RP non vérifiés, ajout variable dépendante du temps
  
  #on split une deuxième fois
  dt<-Dt
  dt$evt<-dt$etat
  ti <- sort(unique(dt$stop[dt$etat==1]))
  dt<-survSplit(dt, end="stop", start="start", cut=ti, event="evt")
  dt<-dt[order(dt$id, dt$start),]
  
  
  #transformation :
  #tmp <- lapply(c("log","sqrt","*t","/t","*t^2","*t^0.7", "log10", "*t^0.3", "*t^3"),function(x){
  tmp <- lapply(c("log","sqrt","*t","/t","*t^2", "*t^0.3", "*t^3"),function(x){
    #tmp <- lapply("/t",function(x){
    # tmp <- lapply(c("*t^0.7"),function(x){
    add_vart_and_check_dt(data_split=dt, var=var, recode = recode, title = .title, .transf=x)#[[1]] #Evite que ça ne bloque si le modèle ne converge pas
    #add_vart_and_check_dt(data_split=dt, var=var, .transf="*t")#[[1]] #Evite que ça ne bloque si le modèle ne converge pas
  })
  a <- do.call(rbind, tmp)
  
  return(a)
})

dev.off()

repRP <- do.call(rbind, .l)
repRP <- repRP[repRP$beta_at == TRUE , ]
repRP <- repRP[repRP$Harrell == "ok", ]

write.table(print(repRP[repRP$param == "at", c("variable", "transf", "curve")]), file="clipboard", sep="\t", row.names=F)
#rNRP_t <- read.csv2("data/RP vart transfo t.csv")
rNRP_t <- read.csv2("data/analyses/RP_var_rep_Q_bin_transft.csv")
rNRP_t <- merge(rNRP_t, subset(repRP, select=-curve), by=c("variable", "transf"), all= F)

rNRP_t <- rNRP_t[rNRP_t$curve==TRUE, ]
#Je prend la transformation avec le plus petit AIC
AICmin <- tapply(as.numeric(as.character(rNRP_t$AIC)), as.character(rNRP_t$variable), min)
AICmin <- data.frame(variable = names(AICmin), AICmin = as.numeric(AICmin))
rNRP_t <- merge(rNRP_t, AICmin, by="variable")

rNRP_t <- unique(rNRP_t[rNRP_t$AIC==rNRP_t$AICmin, c("variable", "recode", "transf")])
rNRP_t$variable <- as.character(rNRP_t$variable)
rNRP_t <- rNRP_t[!duplicated(rNRP_t$variable),]

rNok[rNok$var %in% rNRP_t$variable, "transf"] <- rNRP_t$transf

#-----------------------------------------------
#Decoupe du temps pour les dernieres variables non corrigées

rNRP_cut <- rNok[is.na(rNok$transf), ]

# pdf(paste0("data/analyses/RP_var_rep_cut_", b, ".pdf"))
# .l <- lapply(1:nrow(rNok_cut), function(nr){
# b <- c(6,18,50)
# b <- c(6,20,60)
# b <- c(4, 10, 30)
# b <- c(5, 7)
#b <- c(7,17,34)
b <- c(3, 6, 18, 36, 60) #e (6, 18, 36,48 marche aussi mais plus long)
b <- c(6, 18, 36, 48) #e (6, 18, 36,48 marche aussi mais plus long)
for (i in seq_along(rNRP_cut$var)) {cut_rep(rNRP_cut$var[i], b, rNRP_cut$recode[i])}
# dev.off()
#
# dfrepcut <- do.call(rbind, .l)
# rNRP_cut <- dfrepcut

#rNRP_cut$transf <- "7-17-34"
rNRP_cut$transf <- paste(b, collapse = "-")
rNRP_cut$variable <- rNRP_cut$var
rNRP_cut <- rNRP_cut[ , c("variable", "recode", "transf")]
#binNRPcut <- unique(df_bint[ ,c("variable", "transf")])

#pour renommer var de RPok en variable mais je laisse ça ici car p-e pb de code si je le déplace
rRPok$variable <- rRPok$var
rRPok <- rRPok[ , c("variable", "recode", "transf")]
#-------------------------
#analyses
# all_var <- read.csv2("data/RP_all_rep.csv")
# all_var$var <- as.character(all_var$var)
# all_var$type <- as.character(all_var$type)
# all_var$transf <- NA

all_var <- rbind(rRPok, rNRP_t, rNRP_cut)


.l <- lapply(1:nrow(all_var), function(nr){
  #.l <- lapply(80:88, function(nr){
  # .l <- lapply(88, function(nr){
  tps_clinique = 12
  var.df <- all_var
  data.df <- s_i
  
  var <- as.character(var.df$variable[nr])
  recode <- var.df$recode[nr]
  transf <- as.character(var.df$transf[nr])
  RP <- ifelse(is.na(var.df$transf[nr]), TRUE, FALSE)
  
  #------------------
  #gestion du changement de valeur en fonction du temps
  print(var)
  s <- data.df[data.df$qui==var, ]
  s$time <- ifelse(s$del<0, 0, s$del) #del est le délai entre la date de receuil de la variable(date) et la date de vni (datevni). le del peut ê négatif pour les variables baseline
  
  #en mois
  s$time <-(s$time/365.25*12) + 0.001
  s$time.vni <- (s$time.vni /365.25*12) + 0.001 #délai entre la date de vni et la mort
  
  idu<-sort(unique(s$PATIENT))
  s[, "x"]<-as.numeric(as.character(s[, "x"]))
  
  #La boucle suivante permet de réaliser ce que ferait automatiquement un survsplit mais en découpant chaque patient par ses valeurs uniquement
  for (id in idu) {
    s1 <- s[s$PATIENT==id, c("PATIENT", "time.vni", "evt", "time", "x")]
    s1 <- s1[!is.na(s1$time),]
    s1 <- s1[order(s1$time),]
    ns1<-dim(s1)[1] #nombre de changement de valeurs pour un patient
    #si x à la valeur NA, alors je remplace par la dernière valeur connue (ce cas existe-t-il vraiment?)
    if (ns1>1) {
      if (is.na(s1[1, "x"])) s1[1, "x"] <-s1[2, "x"]
      for (x in 2:nrow(s1)){
        if (is.na(s1[x, "x"])) s1[x, "x"] <-s1[(x-1), "x"]
      }
    }
    d<-s1
    #creation de la table avec start stop, la valeur prise dans chaque interval et l'evt dans le bon interval (survsplit mais à la main)
    if (ns1>=1) {
      d$id<-d$PATIENT
      d$del<-d$time.vni
      d$n<-ns1
      t<-sort(unique(d$time));t
      
      dt<-d[, c("id", "del", "evt", "time", "n", "x")]
      n <-dim(d)[1]
      dt$start<-c(d$time)
      dt$stop<-c(d$time[-1], d$del[n])
      dt$etat<-0
      dt$etat[ns1]<-dt$evt[ns1]
      dt$w<-c(d[, "x"])
    } else { #si le patient n'a pas de ligne, le tableau est vide (? ce cas peut-il vraiment exister? si oui pourquoi ne pas faire dt <- NULL ?)
      dt<-s0
    }
    #je merge le tableau du patient idu[id] à celui du patient idu[id-1]
    if (id==idu[1]) {
      Dt<-dt
    } else {
      Dt<-rbind(Dt, dt)
    }
  }
  
  #Je remet dans l'ordre : par patient et par temps de start
  Dt<-Dt[order(Dt$id, Dt$start),]
  
  #------
  #data management
  #gestion du cas start=stop : survient uniquement pour le dernier intervalle d'un patient, lorsqu'une nouvelle valeur est enregistrée
  #lors d'une visite, mais que cette visite correspond à la date de dernière nouvelle (etat=0) ou à la date de décès (état=1)
  #afficher cas etat=0
  Dt$i<-1:dim(Dt)[1] #numérote les lignes
  ote<-ifelse(Dt$start==Dt$stop & Dt$etat==0, 1, 0)
  io<-Dt$i[ote==1] #recupere lignes pour lesquels dernier interval : start=stop et etat=0
  jo<-sort(unique(c(io, io-1)))#juste pour verif qu'on peut supprimer derniere ligne : on regarde ensemble derniere ligneet avant derniere
  
  #afficher cas etat=1
  remp<-ifelse(Dt$start==Dt$stop & Dt$etat==1, 1, 0)
  ir<-Dt$i[remp==1]
  jr<-sort(unique(c(ir, ir-1)))
  
  #gérer cas etat=1 : c'est la ligne d'avant qui devient evt=1(la date d'evt correspond à la date du stop, donc la date d'evt est juste) et la ligne start=stop est supprimée
  Dt$etat[Dt$i %in% (ir-1)]<-1
  #gérer cas etat=1 ou etat=0 : on supprime la dernière ligne de ces 2 cas possibles du tableau Dt
  Dt<-Dt[!(Dt$i %in% c(io, ir)),]
  
  #------------------
  #si loglin non vérifiée, je recoupe à la médiane
  if (recode == TRUE) {
    .title <- paste0 ("RP of ", var, " superior to ", round(median(Dt[Dt$time==0.001, "x"]),0))
    Dt$x <- ifelse(Dt$x <= median(Dt[Dt$time==0.001, "x"]), 0, 1)
  } else {
    .title <- paste0 ("RP of ", var)
  }
  
  #------------------
  #HR IC valeur des coef beta et test (analyses en fonction de la transformation qui convient)
  
  #1- version HP ok, on ne transforme pas
  if(is.na(transf)){
    Dt$evt <- Dt$etat #NB : evt = DC à la fin du suivi, etat = DC dans l'intervalle de temps considéré
    coxt<-coxph(Surv(start, stop, evt)~x+cluster(id), data=Dt)
    a <- summary(coxt)
    stat <- round(a$robscore["test"],2)
    pval <- round(a$robscore[3],3)
    coefx <- round(a$coefficients[1],4)
    HRIC <- round(data.frame(IC=a$conf.int[1], l=a$conf.int[3], u=a$conf.int[4]),3)
    HR <- HRIC$IC
    HRIC <- paste0(HRIC[1], "[", HRIC[2], "-", HRIC[3], "]")
    #HRIC95 <- paste0("[", HRIC[2], "-", HRIC[3], "]")
    test <- "robust"
    #res <- data.frame(var, RP, transf, tps_clinique, HR, HRIC95, coefx, coefxt, test, pval)
    res <- data.frame(variable = var, recode, RP, transf, tps_clinique = NA, HRIC, test, statistic = stat, pvalue = pval, param = "x", beta = coefx)
    return(res)
  
    #2- version HP non ok, je transforme (transfo temps ou cut)
    } else {
    #avant de transformer, on split une deuxième fois
    dt<-Dt
    dt$evt<-dt$etat
    ti <- sort(unique(dt$stop[dt$etat==1]))
    dt<-survSplit(dt, end="stop", start="start", cut=ti, event="evt")
    dt<-dt[order(dt$id, dt$start),]
    
    #2-a transfo du temps
    if(transf %in% c("log","sqrt","*t","/t","*t^2","*t^0.7", "log10", "*t^0.3", "*t^3")){
      
      #ajout var xt dont la valeure dépendant du temps
      if (transf=="log") dt$xt<-dt$x*log(dt$stop)
      if (transf=="sqrt")dt$xt<-dt$x*sqrt(dt$stop)
      if (transf=="*t")dt$xt<-dt$x*(dt$stop)
      if (transf=="/t")dt$xt<-dt$x/(dt$stop)
      if (transf=="*t^2") dt$xt <-dt$x*(dt$stop^2)
      if (transf=="*t^0.7") dt$xt <-dt$x*(dt$stop^0.7)
      if (transf=="log10") dt$xt <-dt$x*log10(dt$stop)
      if (transf=="*t^0.3") dt$xt <-dt$x*(dt$stop^0.3)
      if (transf=="*t^3") dt$xt <-dt$x*(dt$stop^3)
      
      f <- "Surv(start, stop, evt) ~ x + xt + cluster(id)"
      coxt <- coxph(as.formula(f), data=dt)
      
      #significativité
      test <- summary(coxt)
      stat <- round(test$robscore["test"],2)
      pval <- round(test$robscore["pvalue"],4)
      
      #Intervalle de confiance
      S <- vcov(coxt)
      b <- coef(coxt)
      t <- tps_clinique #choisir le temps en mois
      if (transf=="*t^0.7") t_t <- t^0.7
      if (transf=="log") t_t <- log(t)
      if (transf=="*t^2") t_t <- t^2
      if (transf=="*t") t_t <- t
      if (transf=="*t^3") t_t <- t^3
      if (transf=="/t") t_t <- 1/t
      if (transf=="*t^0.3") t_t <- t^0.3
      if (transf=="sqrt") t_t <- sqrt(t)
      
      variance <- S[1,1]+S[2,2]*(t_t)^2+2*S[1,2]*(t_t)
      m <- b[1]+b[2]*(t_t) #coef de l'HR
      HRIC <- round(c(exp(m), exp(m + qnorm(0.975)*sqrt(variance) * c(-1,1))),3)
      HRIC <- paste0(HRIC[1], " [", HRIC[2], " - ", HRIC[3],"]")
      
      #valeur des paramètres beta
      name_param <- rownames(test$coefficients)
      coefbeta <- round(test$coefficients[ ,"coef"], 5)
      
      
      return(data.frame(variable = var, recode, RP = FALSE, transf = transf, tps_clinique = tps_clinique, HRIC = HRIC, test = "robust",
                        statistic = stat, pvalue = pval, param = name_param, beta = coefbeta))
      #2-b cut du temps
    } else {
      
      print(paste(var, transf, sep="-"))
      b <- as.numeric(unlist(strsplit(transf, "-")))
      name_cut <- transf
      for (i in 1:(length(b)+1)){
        if (i == 1) tmp <-  dt$x * ifelse(dt$stop<=b[1], 1, 0)
        if(i <= length(b) & i!= 1) tmp <- dt$x * ifelse(dt$stop>b[i-1] & dt$stop<=b[i], 1, 0)
        if(i == (length(b)+1)) tmp <-  dt$x * ifelse(dt$stop>b[i-1], 1, 0)
        dt[ ,paste0("xt",i)] <- tmp
      }
      vat<-paste("xt",  1:(length(b)+1), sep="")
      x<-dt[, vat]
      sx<-colSums(x) #interval de temps sans evt
      wat<-vat[sx>0] #on supprime interval de temps quand pas d'evenement
      f<-paste("Surv(start, stop, evt) ~ ", paste(wat, collapse="+")," + cluster(id)", sep="")  #on ne met pas x car les at couvre deja  toutes les perdiodes
      
      coxt <- coxph(as.formula(f), data=dt)
      test <- summary(coxt)
      coefbeta <- round(test$coefficients[ ,"coef"], 5)
      name_param <- rownames(test$coefficients)
      stat <- round(test$robscore["test"],2)
      pval <- round(test$robscore["pvalue"],4)
      
      i <- findInterval(tps_clinique, b) + 1 #findInterval commence à 0...
      HRIC <- round(exp(cbind(coef(coxt)[i], qnorm(0.025, coef(coxt)[i], sqrt(diag(vcov(coxt))[i])), qnorm(1-0.025, coef(coxt)[i], sqrt(diag(vcov(coxt))[i])))),3)
      HRIC <- paste0(HRIC[1], " [", HRIC[2], " - ", HRIC[3],"]")
      #browser()
      return(data.frame(variable = var, recode, RP = FALSE, transf, tps_clinique = tps_clinique, HRIC, test = "robust",
                        statistic = stat, pvalue = pval, param = name_param, beta = coefbeta))
      
      
    }
    
  }
})
a <- do.call(rbind, .l)
saveRDS(a, "data/analyses/HRIC_var_rep.rds")


#----------------
#QUALI > 2 classes
#----------------

quali
#Verif et analyse des variables quali répétées
# qualir <- c("DYSPN_SOUSVENT_SV", "DYSPN_SVENT_SV", "EVOL_SOMM_VNI_SV", "QUALIT_SOMM_VENT_SV", 
#   "STYLO_RADIAL_D", "STYLO_RADIAL_G", "TRICIPITAL_D", "TRICIPITAL_G")

qualir <-   c("DYSPN_SOUSVENT_SV", "DYSPN_SVENT_SV", "EVOL_SOMM_VNI_SV", "QUALIT_SOMM_VENT_SV", "LANGUE", "BMI_c", "UTIL_VENTIL_DIURN_SV_c")


dq <- s_i
dq$del <-  dq$del/30.4375
dq$time.vni <-  dq$time.vni/30.4375
#-------------
#"DYSPN_SOUSVENT_SV"

# #préparation de la base 
var <- "DYSPN_SOUSVENT_SV"
yt <- get_split(dq, var, no_name = TRUE)

# y0<-bl[,  c("PATIENT", "SEX")]
# y<-s_i[s_i$qui==var,]
# y$del <- ifelse(y$del<0, 0, y$del) #del est le délai entre la date de receuil de la variable(date) et la date de vni (datevni). le del peut ê négatif pour les variables baseline
# y$del <- (y$del /365.25*12) + 0.001#del est le délai entre la date de receuil de la variable(date) et la date de vni (datevni). le del peut ê négatif pour les variables baseline
# y$time.vni <- (y$time.vni /365.25*12) + 0.001
# #y$time.vni <- (y$time.vni /365.25*12) + 0.001 #pb vient du ti<-0:max(y$time.vni). Cependant en réglant ce pb on a qd meme un robsute un peu différent (3.99 au lieu de 0.1363)
# y<-merge(y, y0, by="PATIENT", all.x=T, all.y=F)
# y<-y[order(y$PATIENT, y$del),]
# z<-tapply(y$del, y$PATIENT, c)
# zf<-tapply(y$time.vni, y$PATIENT, c)
# zm<-mapply(function(x,y) c(x[-1], y[1]), z, zf)
# 
# z<-tapply(y$evt, y$PATIENT, c)
# fct<-function (x) {
#   x[-length(x)]<-0
#   return(x)
# }
# ze<-sapply(z, fct)
# 
# y$delapres<-unlist(zm)
# y$evt2<-unlist(ze)
# #ti<-0:max(y$time.vni)
# 
# yt<-y
# yt$start<-yt$del
# yt$stop<-yt$delapres
# yt$etat<-yt$evt2
# #essai d'un autre ti pour changer d'échelle:
# ti <- sort(unique(yt$stop[yt$etat==1]))
# yt<-survSplit(yt, end="stop", start="start", cut=ti, event="etat")
# yt<-yt[order(yt$PATIENT, yt$start),]
# 
# #des sujets meurt le jour de la visite, et cette visite a été retirée (par la boucle ou le survsplit)=> on indique que le patient est bien mort au stop
# yt[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni,]
# yt$etat[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni]<-1

#analyse var qual 
yt$x<-factor(yt$x) # dans ce tableau s_i[s_i$qui==var,] x est la valeur de var
table(yt$x)
yt$x1<-ifelse(yt$x=="1", 1, 0)
yt$x2<-ifelse(yt$x=="2", 1, 0)
yt$x3<-ifelse(yt$x=="3", 1, 0)

cox<-coxph(Surv(start, stop, etat)~x2+x3+cluster(PATIENT), data=yt) #on ne met que 2 des 3 niveaux. le niveaux 1 est la ref
summary(cox)
b<-coef(cox)
#verif hypotheses
zr<-cox.zph(cox, "rank")
zr
zi<-cox.zph(cox, "identity")
plot(zi[1])
abline(h=b[1], col = "blue")
plot(zi[2])
abline(h=b[2], col= "blue")
#transformation du temps
#yt$x2t<-log(yt$stop)*yt$x2
yt$x3t<-log(yt$stop)*yt$x3
# yt$x2t<-(yt$stop^2)*yt$x2
 yt$x3t<-(yt$stop^2)*yt$x3*yt$stop
# yt$x2t<-(yt$stop)*yt$x2
# yt$x3t<-(yt$stop)*yt$x3
#coxt<-coxph(Surv(start, stop, etat)~x2+x2t+x3+x3t+cluster(PATIENT), data=yt)
coxt<-coxph(Surv(start, stop, etat)~x2+x3+x3t+cluster(PATIENT), data=yt)
summary(coxt) #pq var dep du temps non signif mais graphe ok?
bt<-coef(coxt)
ztr<-cox.zph(coxt, "rank")
ztr
zti<-cox.zph(coxt, "identity")
par(mfrow=c(2,2))
for (i in 1:4) {
  plot(zti[i], resid = FALSE)
  title(names(bt)[i])
  abline(h=bt[i], col= "blue")
}
#log not ok (at non signif)


#decoupage du temps

# m<-365.25/12 #pour transformer en mois
# yt$x2t1<-ifelse(yt$stop<=6*m, yt$x2, 0)
# yt$x2t2<-ifelse(yt$stop>6*m & yt$stop<=18*m, yt$x2, 0)
# yt$x2t3<-ifelse(yt$stop>18*m & yt$stop<=36*m, yt$x2, 0)
# yt$x2t4<-ifelse(yt$stop>36*m, yt$x2, 0)
# yt$x3t1<-ifelse(yt$stop<=6*m, yt$x3, 0)
# yt$x3t2<-ifelse(yt$stop>6*m & yt$stop<=18*m, yt$x3, 0)
# yt$x3t3<-ifelse(yt$stop>18*m & yt$stop<=36*m, yt$x3, 0)
# yt$x3t4<-ifelse(yt$stop>36*m, yt$x3, 0)
# #summary(yt)
# coxt<-coxph(Surv(start, stop, etat)~x2t1+x2t2+x2t3+x2t4+x3t1+x3t2+x3t3+x3t4+cluster(PATIENT), data=yt)
# summary(coxt)
# bt<-coef(coxt)
# ztr<-cox.zph(coxt, "rank")
# zti<-cox.zph(coxt, "identity")
# ztr
# #par(mfrow=c(3,3))
# for (i in 1:8) {
#   plot(zti[i])
#   title(names(bt)[i])
#   abline(h=bt[i], col="blue")
# }

#####
#avec boucle 
transf <- "6-18-36"
b <- as.numeric(unlist(strsplit(transf, "-")))
name_cut <- transf

for (j in 3){
  for (i in 1:(length(b)+1)){
    if (i == 1) tmp <-  ifelse(yt$stop<=b[1], yt[ ,paste0("x",j)], 0)
    if(i <= length(b) & i!= 1) tmp <-ifelse(yt$stop>b[i-1] & yt$stop<=b[i], yt[ ,paste0("x",j)], 0)
    if(i == (length(b)+1)) tmp <-  ifelse(yt$stop>b[i-1], yt[ ,paste0("x",j)], 0)
    yt[ ,paste0("x",j,"t",i)] <- tmp
  } 
}

# 
# m<-365.25/12 #pour transformer en mois
# tps_clinique <- 12 #12mois 
# #for (j in 2:3){
# for (j in 3){
#   for (i in 1:(length(b)+1)){
#     if (i == 1) tmp <-  ifelse(yt$stop<=b[1]*m, yt[ ,paste0("x",j)], 0)
#     if(i <= length(b) & i!= 1) tmp <-ifelse(yt$stop>b[i-1]*m & yt$stop<=b[i]*m, yt[ ,paste0("x",j)], 0)
#     if(i == (length(b)+1)) tmp <-  ifelse(yt$stop>b[i-1]*m, yt[ ,paste0("x",j)], 0)
#     yt[ ,paste0("x",j,"t",i)] <- tmp
#   } 
# }

vat<-apply(expand.grid(list("x", 2:3,"t",  1:(length(b)+1))),1, paste, collapse="")
vat <- vat [vat %in% names(yt)]
x<-yt[, vat]
sx<-colSums(x) #interval de temps sans evt
wat<-vat[sx>0] #on supprime interval de temps quand pas d'evenement
#f<-paste("Surv(start, stop, etat) ~ ", paste(wat, collapse="+")," + cluster(PATIENT)", sep="")  #on ne met pas x car les at couvre deja  toutes les perdiodes
f<-paste("Surv(start, stop, etat) ~ ", paste(wat, collapse="+")," + x2 + cluster(PATIENT)", sep="")  #on ne met pas x car les at couvre deja  toutes les perdiodes
#f<-paste("Surv(start, stop, etat) ~ ", paste(wat, collapse="+")," + cluster(PATIENT)", sep="")  #on ne met pas x car les at couvre deja  toutes les perdiodes

#model
coxt <- coxph(as.formula(f), data=yt)

#verif
bt<-coef(coxt)
ztr<-cox.zph(coxt, "rank")
zti<-cox.zph(coxt, "identity")
ztr
for (i in seq_along(bt)) {
  plot(zti[i])
  title(names(bt)[i])
  abline(h=bt[i], col="blue")
}

#interpretation
test <- summary(coxt)
coefbeta <- round(test$coefficients[ ,"coef"], 5)
name_param <- rownames(test$coefficients)
stat <- round(test$robscore["test"],2)
pval <- round(test$robscore["pvalue"],4)

i <- findInterval(tps_clinique, b) + 1 #findInterval commence à 0...
HRIC <- round(exp(cbind(coef(coxt)[i], qnorm(0.025, coef(coxt)[i], sqrt(diag(vcov(coxt))[i])), qnorm(1-0.025, coef(coxt)[i], sqrt(diag(vcov(coxt))[i])))),3)
HRIC <- paste0(HRIC[1], " [", HRIC[2], " - ", HRIC[3],"]")

df <- data.frame(param = name_param, beta = coefbeta)
df <- df[order(df$param),]
myParam <- rbind(paste(df$param[grepl("x2",df$param)], sep="", collapse="; "), paste(df$param[grepl("x3",df$param)], sep="", collapse="; "))
mybeta <- rbind(paste(df$beta[grepl("x2",df$param)], sep="", collapse="; "), paste(df$beta[grepl("x3",df$param)], sep="", collapse="; "))
df1 <- data.frame(variable = var, recode = FALSE, RP = FALSE, transf, tps_clinique = tps_clinique, HRIC, test = "robust",
                  statistic = stat, pvalue = pval, param = myParam, beta = mybeta)


#-------------
#"DYSPN_SVENT_SV"

#préparation de la base 
var <- "DYSPN_SVENT_SV"
yt <- get_split(dq, var, no_name = TRUE)

# y0<-bl[,  c("PATIENT", "SEX")]
# y<-s_i[s_i$qui==var,]
# y<-merge(y, y0, by="PATIENT", all.x=T, all.y=F)
# y<-y[order(y$PATIENT, y$del),]
# z<-tapply(y$del, y$PATIENT, c)
# zf<-tapply(y$time.vni, y$PATIENT, c)
# zm<-mapply(function(x,y) c(x[-1], y[1]), z, zf)
# 
# z<-tapply(y$evt, y$PATIENT, c)
# fct<-function (x) {
#   x[-length(x)]<-0
#   return(x)
# }
# ze<-sapply(z, fct)
# 
# y$delapres<-unlist(zm)
# y$evt2<-unlist(ze)
# ti<-0:max(y$time.vni)
# 
# yt<-y
# yt$start<-yt$del
# yt$stop<-yt$delapres
# yt$etat<-yt$evt2
# yt<-survSplit(yt, end="stop", start="start", cut=ti, event="etat")
# yt<-yt[order(yt$PATIENT, yt$start),]
# 
# #des sujets meurt le jour de la visite
# yt[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni,]
# yt$etat[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni]<-1
# 
# yt$start<-yt$start/(365.25/12)
# yt$stop<-yt$stop/(365.25/12)

#analyse var qual 
yt$x<-factor(yt$x) # dans ce tableau s_i[s_i$qui==var,] x est la valeur de var
myLev <- levels(yt$x)
mySeqLev <- seq_along(myLev)[-1]
myVar <- paste0("x",myLev)[mySeqLev]
for (i in myLev){
  yt[ ,paste0("x",i)] <-ifelse(yt$x==i, 1, 0)
}

f <- paste("Surv(start, stop, etat) ~ ", paste(myVar, collapse = "+"), " + cluster(PATIENT)", sep="") 
cox <- coxph(as.formula(f), data=yt) #on ne met que 2 des 3 niveaux. le niveaux 1 est la ref
b<-coef(cox)
#verif hypotheses
zr<-cox.zph(cox, "rank")
zr
zi<-cox.zph(cox, "identity")
for (i in seq_along(mySeqLev)){
  plot(zi[i])
  abline(h=b[i], col = "blue")
}

#transformation du temps
 yt$x2t<-1/(yt$stop)*yt$x2
 yt$x2t<-(yt$stop)*yt$x2
 yt$x2t<-(yt$stop)^0.3*yt$x2
# yt$x3t<-1/(yt$stop)*yt$x3
yt$x2t<-log(yt$stop)*yt$x2
#yt$x3t<-log(yt$stop)*yt$x3
#coxt<-coxph(Surv(start, stop, etat)~x2+x2t+x3+x3t+cluster(PATIENT), data=yt)
coxt<-coxph(Surv(start, stop, etat)~x2+x2t+x3+cluster(PATIENT), data=yt)
summary(coxt)
bt<-coef(coxt)
ztr<-cox.zph(coxt, "rank")
ztr
zti<-cox.zph(coxt, "identity")
#par(mfrow=c(2,2))
for (i in seq_along(bt)) {
  plot(zti[i], resid = FALSE)
  title(names(bt)[i])
  abline(h=bt[i])
}
#log, sqrt ^2 not ok. 1/t not ok(beta(t) pas signif)


#decoupage du temps
#####
#avec boucle 
transf <- "3-6-18-36"
b <- as.numeric(unlist(strsplit(transf, "-")))
rank_interv <- seq_len(length(b)+1)
name_cut <- transf
#m<-365.25/12 #pour transformer mois en jours
#for (j in myVar){
for (j in "x2"){
  for (i in rank_interv){
    if (i == 1) tmp <-  ifelse(yt$stop<=b[1], yt[ ,j], 0)
    if(i <= length(b) & i!= 1) tmp <-ifelse(yt$stop>b[i-1] & yt$stop<=b[i], yt[ ,j], 0)
    if(i == (length(b)+1)) tmp <-  ifelse(yt$stop>b[i-1], yt[ ,j], 0)
    # if (i == 1) tmp <-  ifelse(yt$stop<=b[1]*m, yt[ ,j], 0)
    # if(i <= length(b) & i!= 1) tmp <-ifelse(yt$stop>b[i-1]*m & yt$stop<=b[i]*m, yt[ ,j], 0)
    # if(i == (length(b)+1)) tmp <-  ifelse(yt$stop>b[i-1]*m, yt[ ,j], 0)
    yt[ ,paste0(j,"t",i)] <- tmp
  } 
}

vat<-apply(expand.grid(list("x", 2:3,"t",  1:(length(b)+1))),1, paste, collapse="")
vat <- vat [vat %in% names(yt)]
x<-yt[, vat]
sx<-colSums(x) #interval de temps sans evt
wat<-vat[sx>0] #on supprime interval de temps quand pas d'evenement
#f<-paste("Surv(start, stop, etat) ~ ", paste(wat, collapse="+")," + cluster(PATIENT)", sep="")  #on ne met pas x car les at couvre deja  toutes les perdiodes
f<-paste("Surv(start, stop, etat) ~ ", paste(wat, collapse="+")," + x3 + cluster(PATIENT)", sep="")  #on ne met pas x car les at couvre deja  toutes les perdiodes

#model
coxt <- coxph(as.formula(f), data=yt)

#verif
bt<-coef(coxt)
ztr<-cox.zph(coxt, "rank")
zti<-cox.zph(coxt, "identity")
ztr
for (i in seq_along(bt)) {
  plot(zti[i])
  title(names(bt)[i])
  abline(h=bt[i], col = "blue")
}

#interpretation
test <- summary(coxt)
coefbeta <- round(test$coefficients[ ,"coef"], 5)
name_param <- rownames(test$coefficients)
stat <- round(test$robscore["test"],2)
pval <- round(test$robscore["pvalue"],4)

tps_clinique <- 12 #12mois
i <- findInterval(tps_clinique, b) + 1 #findInterval commence à 0...
HRIC <- round(exp(cbind(coef(coxt)[i], qnorm(0.025, coef(coxt)[i], sqrt(diag(vcov(coxt))[i])), qnorm(1-0.025, coef(coxt)[i], sqrt(diag(vcov(coxt))[i])))),3)
HRIC <- paste0(HRIC[1], " [", HRIC[2], " - ", HRIC[3],"]")


df <- data.frame(param = name_param, beta = coefbeta)
df <- df[order(df$param),]
myParam <- rbind(paste(df$param[grepl("x2",df$param)], sep="", collapse="; "), paste(df$param[grepl("x3",df$param)], sep="", collapse="; "))
mybeta <- rbind(paste(df$beta[grepl("x2",df$param)], sep="", collapse="; "), paste(df$beta[grepl("x3",df$param)], sep="", collapse="; "))
df <- data.frame(variable = var, recode = FALSE, RP = FALSE, transf, tps_clinique = tps_clinique, HRIC, test = "robust",
                  statistic = stat, pvalue = pval, param = myParam, beta = mybeta)

df1 <- rbind(df1, df)



#-------------
#"EVOL_SOMM_VNI_SV"

#préparation de la base 
var <- "EVOL_SOMM_VNI_SV"
yt <- get_split(dq, var, no_name = TRUE)

# y0<-bl[,  c("PATIENT", "SEX")]
# y<-s_i[s_i$qui==var,]
# y<-merge(y, y0, by="PATIENT", all.x=T, all.y=F)
# y<-y[order(y$PATIENT, y$del),]
# z<-tapply(y$del, y$PATIENT, c)
# zf<-tapply(y$time.vni, y$PATIENT, c)
# zm<-mapply(function(x,y) c(x[-1], y[1]), z, zf)
# 
# z<-tapply(y$evt, y$PATIENT, c)
# fct<-function (x) {
#   x[-length(x)]<-0
#   return(x)
# }
# ze<-sapply(z, fct)
# 
# y$delapres<-unlist(zm)
# y$evt2<-unlist(ze)
# ti<-0:max(y$time.vni)
# 
# yt<-y
# yt$start<-yt$del
# yt$stop<-yt$delapres
# yt$etat<-yt$evt2
# yt<-survSplit(yt, end="stop", start="start", cut=ti, event="etat")
# yt<-yt[order(yt$PATIENT, yt$start),]
# 
# #des sujets meurt le jour de la visite
# yt[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni,]
# yt$etat[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni]<-1

#analyse var qual 
yt$x<-factor(yt$x) # dans ce tableau s_i[s_i$qui==var,] x est la valeur de var
myLev <- levels(yt$x)
mySeqLev <- seq_along(myLev)[-1]
myVar <- paste0("x",myLev)[mySeqLev]

for (i in myLev){
  yt[ ,paste0("x",i)] <-ifelse(yt$x==i, 1, 0)
}

f <- paste("Surv(start, stop, etat) ~ ", paste(myVar, collapse = "+"), " + cluster(PATIENT)", sep="") 
cox <- coxph(as.formula(f), data=yt) #on ne met que 2 des 3 niveaux. le niveaux 1 est la ref
b<-coef(cox)
#verif hypotheses
zr<-cox.zph(cox, "rank")
zr
zi<-cox.zph(cox, "identity")
for (i in seq_along(mySeqLev)){
  plot(zi[i])
  abline(h=b[i], col = "blue")
}

#interpretation
#RP ok => pas de modif
coxt <- cox
test <- summary(coxt)
coefbeta <- round(test$coefficients[ ,"coef"], 5)
name_param <- rownames(test$coefficients)
stat <- round(test$robscore["test"],2)
pval <- round(test$robscore["pvalue"],4)

transf <- NA
tps_clinique <- NA
HRIC <- round(data.frame(IC=test$conf.int[,1], l=test$conf.int[,3], u=test$conf.int[,4]),3)
HR <- HRIC$IC
HRIC <- paste0(HRIC[,1], "[", HRIC[,2], "-", HRIC[,3], "]")

df <- data.frame(param = name_param, beta = coefbeta)
df <- df[order(df$param),]
myParam <- rbind(paste(df$param[grepl("x2",df$param)], sep="", collapse="; "), paste(df$param[grepl("x3",df$param)], sep="", collapse="; "))
mybeta <- rbind(paste(df$beta[grepl("x2",df$param)], sep="", collapse="; "), paste(df$beta[grepl("x3",df$param)], sep="", collapse="; "))
df <- data.frame(variable = var, recode = FALSE, RP = TRUE, transf, tps_clinique = tps_clinique, HRIC, test = "robust",
                 statistic = stat, pvalue = pval, param = myParam, beta = mybeta)

df1 <- rbind(df1,df)


#-------------
#"QUALIT_SOMM_VENT_SV"

#préparation de la base 
var <- "QUALIT_SOMM_VENT_SV"
yt <- get_split(dq, var, no_name = TRUE)

# y0<-bl[,  c("PATIENT", "SEX")]
# y<-s_i[s_i$qui==var,]
# y<-merge(y, y0, by="PATIENT", all.x=T, all.y=F)
# y<-y[order(y$PATIENT, y$del),]
# z<-tapply(y$del, y$PATIENT, c)
# zf<-tapply(y$time.vni, y$PATIENT, c)
# zm<-mapply(function(x,y) c(x[-1], y[1]), z, zf)
# 
# z<-tapply(y$evt, y$PATIENT, c)
# fct<-function (x) {
#   x[-length(x)]<-0
#   return(x)
# }
# ze<-sapply(z, fct)
# 
# y$delapres<-unlist(zm)
# y$evt2<-unlist(ze)
# ti<-0:max(y$time.vni)
# 
# yt<-y
# yt$start<-yt$del
# yt$stop<-yt$delapres
# yt$etat<-yt$evt2
# yt<-survSplit(yt, end="stop", start="start", cut=ti, event="etat")
# yt<-yt[order(yt$PATIENT, yt$start),]
# 
# #des sujets meurt le jour de la visite
# yt[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni,]
# yt$etat[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni]<-1

#analyse var qual 
yt$x<-factor(yt$x) # dans ce tableau s_i[s_i$qui==var,] x est la valeur de var
myLev <- levels(yt$x)
mySeqLev <- seq_along(myLev)[-1]
myVar <- paste0("x",myLev)[mySeqLev]

for (i in myLev){
  yt[ ,paste0("x",i)] <-ifelse(yt$x==i, 1, 0)
}

f <- paste("Surv(start, stop, etat) ~ ", paste(myVar, collapse = "+"), " + cluster(PATIENT)", sep="") 
cox <- coxph(as.formula(f), data=yt) #on ne met que 2 des 3 niveaux. le niveaux 1 est la ref
b<-coef(cox)
#verif hypotheses
zr<-cox.zph(cox, "rank")
zr
zi<-cox.zph(cox, "identity")
for (i in seq_along(mySeqLev)){
  plot(zi[i])
  abline(h=b[i], col = "blue")
}

#transformation du temps
yt$x2t<-log(yt$stop)*yt$x2
yt$x3t<-log(yt$stop)*yt$x3
coxt<-coxph(Surv(start, stop, etat)~x2+x2t+x3+x3t+cluster(PATIENT), data=yt)

b<-coef(coxt)
ztr<-cox.zph(coxt, "rank")
ztr
zti<-cox.zph(coxt, "identity")
#par(mfrow=c(2,2))
for (i in 1:4) {
  plot(zti[i], resid = FALSE)
  title(names(b)[i])
  abline(h=b[i])
}
#log ok
#HRIC pour resp

transf <- "log"
m_d <- 365.25/12 #pour transformer mois en jours
tps_clinique <- 12
S <- vcov(coxt)
#t <- tps_clinique * m_d #temps en jours
t <- tps_clinique #temps en mois
t_t <- log(t)

dfHR <- data.frame()
for (i in c("x2", "x3")){
  S2 <- S[grep(i, rownames(S)), grep(i, colnames(S))]
  b2 <- b[grep(i, names(b))]
  variance <- S2[1,1]+S2[2,2]*t_t^2+2*S2[1,2]*t_t
  m <- b2[1]+b2[2]*t_t #coef de l'HR
  HRIC <- round(c(exp(m), exp(m + qnorm(0.975)*sqrt(variance) * c(-1,1))),3)
  HRIC <- paste0(HRIC[1], " [", HRIC[2], " - ", HRIC[3],"]")
  dfHR <- rbind(dfHR,data.frame(param = paste0(i, "; ", i,"t"), beta = paste(round(b2,3), collapse = ";"), HRIC = HRIC))
}

test <- summary(coxt)
stat <- round(test$robscore["test"],2)
pval <- round(test$robscore["pvalue"],4)

df <- data.frame(variable = var, recode = FALSE, RP = FALSE, transf, tps_clinique = tps_clinique, HRIC = dfHR$HRIC, test = "robust",
                 statistic = stat, pvalue = pval, param = dfHR$param, beta = dfHR$beta)

df1 <- rbind(df1,df)


#-------------
#"LANGUE"

#préparation de la base 
var <- "LANGUE"
yt <- get_split(dq, var, no_name = TRUE)
# y0<-bl[,  c("PATIENT", "SEX")]
# y<-s_i[s_i$qui==var,]
# y<-merge(y, y0, by="PATIENT", all.x=T, all.y=F)
# y<-y[order(y$PATIENT, y$del),]
# z<-tapply(y$del, y$PATIENT, c)
# zf<-tapply(y$time.vni, y$PATIENT, c)
# zm<-mapply(function(x,y) c(x[-1], y[1]), z, zf)
# 
# z<-tapply(y$evt, y$PATIENT, c)
# fct<-function (x) {
#   x[-length(x)]<-0
#   return(x)
# }
# ze<-sapply(z, fct)
# 
# y$delapres<-unlist(zm)
# y$evt2<-unlist(ze)
# ti<-0:max(y$time.vni)
# 
# yt<-y
# yt$start<-yt$del
# yt$stop<-yt$delapres
# yt$etat<-yt$evt2
# yt<-survSplit(yt, end="stop", start="start", cut=ti, event="etat")
# yt<-yt[order(yt$PATIENT, yt$start),]
# 
# #des sujets meurt le jour de la visite
# yt[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni,]
# yt$etat[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni]<-1

#analyse var qual 
yt$x<-factor(yt$x) # dans ce tableau s_i[s_i$qui==var,] x est la valeur de var
myLev <- levels(yt$x)
mySeqLev <- seq_along(myLev)[-1]
myVar <- paste0("x",myLev)[mySeqLev]

for (i in myLev){
  yt[ ,paste0("x",i)] <-ifelse(yt$x==i, 1, 0)
}

f <- paste("Surv(start, stop, etat) ~ ", paste(myVar, collapse = "+"), " + cluster(PATIENT)", sep="") 
cox <- coxph(as.formula(f), data=yt) #on ne met que 2 des 3 niveaux. le niveaux 1 est la ref
b<-coef(cox)
#verif hypotheses
zr<-cox.zph(cox, "rank")
zr
zi<-cox.zph(cox, "identity")
for (i in seq_along(mySeqLev)){
  plot(zi[i])
  abline(h=b[i], col = "blue")
}

#transformation du temps pour x2
yt$x2t<- (yt$stop)^2*yt$x2
coxt<-coxph(Surv(start, stop, etat)~x2+x2t+x3+cluster(PATIENT), data=yt)

b<-coef(coxt)
ztr<-cox.zph(coxt, "rank")
ztr
zti<-cox.zph(coxt, "identity")
#par(mfrow=c(2,2))
for (i in 1:4) {
  plot(zti[i], resid = FALSE)
  title(names(b)[i])
  abline(h=b[i])
}
#^2 ok

#HRIC pour x2
transf <- "^2"
m_d <- 365.25/12 #pour transformer mois en jours
tps_clinique <- 12
S <- vcov(coxt)
#t <- tps_clinique * m_d #temps en jours
t <- tps_clinique #temps en mois
t_t <- t^2

dfHR <- data.frame()
for (i in "x2"){
  S2 <- S[grep(i, rownames(S)), grep(i, colnames(S))]
  b2 <- b[grep(i, names(b))]
  variance <- S2[1,1]+S2[2,2]*t_t^2+2*S2[1,2]*t_t
  m <- b2[1]+b2[2]*t_t #coef de l'HR
  HRIC <- round(c(exp(m), exp(m + qnorm(0.975)*sqrt(variance) * c(-1,1))),3)
  HRIC <- paste0(HRIC[1], " [", HRIC[2], " - ", HRIC[3],"]")
  dfHR <- rbind(dfHR,data.frame(param = paste0(i, "; ", i,"t"), beta = paste(round(b2,3), collapse = ";"), HRIC = HRIC))
}


#HRIC pour x3 : RP ok => pas de modif
test <- summary(coxt)
coefbeta <- b[grep("x3", names(b))]
stat <- round(test$robscore["test"],2)
pval <- round(test$robscore["pvalue"],4)

transf <- NA
tps_clinique <- NA
HRIC <- round(data.frame(IC=test$conf.int["x3",1], l=test$conf.int["x3",3], u=test$conf.int["x3",4]),3)
HR <- HRIC$IC
HRIC <- paste0(HRIC[,1], "[", HRIC[,2], "-", HRIC[,3], "]")
dfHR <- rbind(dfHR,data.frame(param = "x3", beta = paste(round(coefbeta,3), collapse = ";"), HRIC = HRIC))

df <- data.frame(variable = var, recode = FALSE, RP = FALSE, transf, tps_clinique = tps_clinique, HRIC = dfHR$HRIC, test = "robust",
                 statistic = stat, pvalue = pval, param = dfHR$param, beta = dfHR$beta)

df1 <- rbind(df1,df)

#-------------
#"BMI_c"

dq <- bind_rows(dq %>% filter(qui=="BMI") %>% mutate(qui = "BMI_c3", 
                                                     x = ifelse(x<18.5, 1, 
                                                                ifelse(x>=18.5 & x<25, 2, 3))), dq) 
#préparation de la base 
var <- "BMI_c3"
yt <- get_split(dq, var, no_name = TRUE)

#analyse var qual 
yt$x<-factor(yt$x) # dans ce tableau s_i[s_i$qui==var,] x est la valeur de var
myLev <- levels(yt$x)
mySeqLev <- seq_along(myLev)[-2]#2 est la reference pour BMI
myVar <- paste0("x",myLev)[mySeqLev]

for (i in myLev){
  yt[ ,paste0("x",i)] <-ifelse(yt$x==i, 1, 0)
}

f <- paste("Surv(start, stop, etat) ~ ", paste(myVar, collapse = "+"), " + cluster(PATIENT)", sep="") 
cox <- coxph(as.formula(f), data=yt) #on ne met que 2 des 3 niveaux. le niveaux 1 est la ref
b<-coef(cox)
#verif hypotheses
zr<-cox.zph(cox, "rank")
zr
zi<-cox.zph(cox, "identity")
for (i in seq_along(mySeqLev)){
  plot(zi[i])
  abline(h=b[i], col = "blue")
}

#interpretation
#RP ok => pas de modif
coxt <- cox
test <- summary(coxt)
coefbeta <- round(test$coefficients[ ,"coef"], 5)
stat <- round(test$robscore["test"],2)
pval <- round(test$robscore["pvalue"],4)

transf <- NA
tps_clinique <- NA
HRIC <- round(data.frame(IC=test$conf.int[,1], l=test$conf.int[,3], u=test$conf.int[,4]),3)
HR <- HRIC$IC
HRIC <- paste0(HRIC[,1], "[", HRIC[,2], "-", HRIC[,3], "]")

df <- data.frame(param = names(coefbeta), beta = as.numeric(coefbeta))
df <- df[order(df$param),]
len <- length(name_param)/2 #nb le nb de param final n'est pas forcément le meme que le nombre d'interval demandé au départ
mySeq <- seq_along(df$param)
myParam <- rbind(paste(df$param[mySeq<=len], sep="", collapse="; "), paste(df$param[mySeq>len], sep="", collapse="; "))
mybeta <- rbind(paste(df$beta[mySeq<=len], sep="", collapse="; "), paste(df$beta[mySeq>len], sep="", collapse="; "))
df <- data.frame(variable = var, recode = FALSE, RP = TRUE, transf, tps_clinique = tps_clinique, HRIC, test = "robust",
                 statistic = stat, pvalue = pval, param = myParam, beta = mybeta)

df1 <- rbind(df1,df)
#-------------
#"UTIL_VENTIL_DIURN_SV_c"

#préparation de la base 
var <- "UTIL_VENTIL_DIURN_SV_c"
yt <- get_split(dq, var, no_name = TRUE)

#analyse var qual 
yt$x<-factor(yt$x) # dans ce tableau s_i[s_i$qui==var,] x est la valeur de var
myLev <- levels(yt$x)
mySeqLev <- seq_along(myLev)[-1]
myVar <- paste0("x",myLev)[mySeqLev]

for (i in myLev){
  yt[ ,paste0("x",i)] <-ifelse(yt$x==i, 1, 0)
}

f <- paste("Surv(start, stop, etat) ~ ", paste(myVar, collapse = "+"), " + cluster(PATIENT)", sep="") 
cox <- coxph(as.formula(f), data=yt) #on ne met que 2 des 3 niveaux. le niveaux 1 est la ref
b<-coef(cox)
#verif hypotheses
zr<-cox.zph(cox, "rank")
zr
zi<-cox.zph(cox, "identity")
for (i in seq_along(mySeqLev)){
  plot(zi[i])
  abline(h=b[i], col = "blue")
}

#interpretation
#RP ok => pas de modif
coxt <- cox
test <- summary(coxt)
coefbeta <- round(test$coefficients[ ,"coef"], 5)
stat <- round(test$robscore["test"],2)
pval <- round(test$robscore["pvalue"],4)

transf <- NA
tps_clinique <- NA
HRIC <- round(data.frame(IC=test$conf.int[,1], l=test$conf.int[,3], u=test$conf.int[,4]),3)
HR <- HRIC$IC
HRIC <- paste0(HRIC[,1], "[", HRIC[,2], "-", HRIC[,3], "]")

df <- data.frame(param = names(coefbeta), beta = as.numeric(coefbeta))
df <- df[order(df$param),]
df <- data.frame(variable = var, recode = FALSE, RP = TRUE, transf, tps_clinique = tps_clinique, HRIC, test = "robust",
                 statistic = stat, pvalue = pval, param = names(coefbeta), beta = factor(coefbeta))

df1 <- rbind(df1,df)
#-------------

saveRDS(df1, "data/analyses/HRIC_rep_quali.rds")

paste0(missrep[missrep$variable %in% qualir, "n_patNA"], "(",missrep[missrep$variable %in% qualir, "perc_NA"], ")") 



#=========================
#=========================
#MISE EN FORME RESULTATS


s1 <- readRDS("data/analyses/df_surv_bl.rds")
s2 <- readRDS("data/analyses/df_surv_quanti_bl.rds")
s5 <- readRDS("data/analyses/df_surv_q_bl.rds")
write.table(print(s2), file="clipboard", sep= "\t", row.names = FALSE)

#HRIC
df1 <- readRDS("data/analyses/HRIC_bin_bl.rds")
df2 <- readRDS("data/analyses/HRIC_quanti_bl.rds")
df3 <- readRDS("data/analyses/HRIC_var_rep.rds")
df4 <- readRDS("data/analyses/HRIC_rep_quali.rds")
df5 <- readRDS("data/analyses/HRIC_quali_bl.rds")

#missing
missbl.df <- readRDS("data/missingbl.rds")
missrep.df <- readRDS("data/missingrep.rds")

n <- 0.3*nrow(bl)

#df1 : baseline binaire
#df1
data <- df1
data <- data %>% filter(!(variable == "ENC_BRONCHIQ" & transf == "log10")) %>% filter(!(variable == "FERM_BOUCHE" & transf == "log10"))
data <- data[ , c("variable", "recode", "RP", "transf", "param", "beta", "tps_clinique", "HRIC", "pvalue")]
#mise en ligne des coefficients
data$beta <- ifelse(data$beta > -0.001 & data$beta <0, "-0.001<beta<0", as.character(round(data$beta, 2)))
a1 <- tapply(data$beta, data$variable, paste, collapse = "; ")
a2 <- tapply(data$param, data$variable, paste, collapse = "; ")
a <- data.frame(variable = names(a1), param = a2, beta = a1)
a$variable <- as.character(a$variable)
data <- data[match(unique(data$variable), data$variable), ]#uniquement première ligne
data <- merge(data[ , !names(data) %in% c("param","beta")], a, by="variable")
#merge avec la survie et mise en ligne
data2 <- s1
data2$survIC <- paste0(data2$group, " : ", data2$survIC)
surv.t <- tapply(data2$survIC, data2$variable, paste, collapse = " ; ") 
a <- data.frame(variable = names(surv.t), survie = surv.t)
a$variable <- as.character(a$variable)
data <- merge(data, a, by="variable")
#merge avec missing value
data <- merge(missbl.df, data,  by = "variable", all.x = FALSE, all.y = TRUE)
#data$missing <- paste0(data$missing, " (", data$missing_perc, ")")
#data$missing_perc <- NULL
data$recode <- NA
data <- data[order(data$pvalue), ]
#data <- data[data$pvalue<0.05, ]
data <- data[data$missing<=n, ]
data <- data[data$pvalue<0.2, ]
data_bin <- data
write.table(print(data), file="clipboard", sep= "\t", row.names = FALSE)

#df2 : quanti baseline
data <- df2
data <- data[ , c("variable", "recode", "RP", "transf", "param", "beta", "tps_clinique", "HRIC", "pvalue")]
#mise en ligne des coefficients
data$beta <- ifelse(data$beta > -0.001 & data$beta <0, "-0.001<beta<0", as.character(round(data$beta, 2)))
a1 <- tapply(data$beta, data$variable, paste, collapse = "; ")
a2 <- tapply(data$param, data$variable, paste, collapse = "; ")
a <- data.frame(variable = names(a1), param = a2, beta = a1)
a$variable <- as.character(a$variable)
data <- data[match(unique(data$variable), data$variable), ]#uniquement première ligne
data <- merge(data[ , !names(data) %in% c("param","beta")], a, by="variable")
#merge avec la survie et mise en ligne
data2 <- s2
data2 <- data2[match(unique(paste0(data2$variable, data2$group)), paste0(data2$variable, data2$group)), ]
data2$survIC <- paste0(data2$group, " : ", data2$survIC)
surv.t <- tapply(data2$survIC, data2$variable, paste, collapse = " ; ") 
a <- data.frame(variable = names(surv.t), survie = surv.t)
a$variable <- as.character(a$variable)
data <- merge(data, a, by="variable", all.x=TRUE, all.y=FALSE)
#merge avec missing value
data <- merge(missbl.df, data,  by = "variable", all.x = FALSE, all.y = TRUE)
#data$missing <- paste0(data$missing, " (", data$missing_perc, ")")
#data$missing_perc <- NULL
data <- data[order(data$pvalue), ]
#data <- data[data$pvalue<0.05, ]
data <- data[data$missing<=n, ]
data <- data[data$pvalue<0.2, ]
data_Q <- data

#df5 : quali bl
data <- df5
data <- data[ , c("variable", "recode", "RP", "transf", "param", "beta", "tps_clinique", "HRIC", "pvalue")]
#merge avec missing value
data <- merge(missbl.df, data,  by="variable", all.x = FALSE, all.y = TRUE)
data$variable <- as.character(data$variable)
data$param <- as.character(data$param)
# #merge qvec survie
# s5$param <- s5$group
# s5$variable <- as.character(s5$variable)
# s5$param <- as.character(s5$param) # les niveaux de data ne sont pas les meme que survie
# data <- merge(data, s5, by = c("variable", "param"), all.x=TRUE, all.y=FALSE)
#data$missing <- paste0(data$missing, " (", data$missing_perc, ")")
#data$missing_perc <- NULL
data$recode <- NA
data <- data[order(data$pvalue), ]
#data <- data[data$pvalue<0.05, ]
data <- data[data$missing<=n, ]
data <- data[data$pvalue<0.2, ]
data$survie <- NA
data <- data[ , names(data_bin)]
data_q <- data


data <- rbind(data_bin, data_Q, data_q)
data <- data[order(data$pvalue), ]

write.table(print(data), file="clipboard", sep= "\t", row.names = FALSE)

#df3 : var repetees quanti et binaire melangees
data <- df3
data <- data[ , c("variable", "recode", "RP", "transf", "tps_clinique", "HRIC", "pvalue", "param", "beta")]
#mise en ligne des coef
data$beta <- ifelse(data$beta > -0.001 & data$beta <0, "-0.001<beta<0", as.character(round(data$beta, 3)))
a1 <- tapply(data$beta, data$variable, paste, collapse = "; ")
a2 <- tapply(data$param, data$variable, paste, collapse = "; ")
a <- data.frame(variable = names(a1), param = a2, beta = a1)
a$variable <- as.character(a$variable)
data <- data[match(unique(data$variable), data$variable), ]#uniquement première ligne
data <- merge(data[ , !names(data) %in% c("param","beta")], a, by="variable")
#merge avec missing value
#data <- merge(missrep[ ,c("variable", "n_patNA", "perc_NA"), ], data, by = "variable", all.x = FALSE, all.y = TRUE)
#data$n_patNA <- paste0(data$n_patNA, " (", data$perc_NA, ")")
#data$perc_NA <- NULL
data <- merge(missrep.df, data, by = "variable", all.x = FALSE, all.y = TRUE)
#retirer variables qui n'auraient pas du être sélectionnées
#data <- data[! data$variable %in% c("CAUSDCD_SLA", "CAUSE_DCD", "CONFCERTDCD", "CONFIRM"), ]
#retirer var quali qui ont ete analysees comme des quanti
#data <- data[! data$variable %in% c("DYSPN_SOUSVENT_SV", "DYSPN_SVENT_SV", "EVOL_SOMM_VNI_SV", "QUALIT_SOMM_VENT_SV", 
#                                   "STYLO_RADIAL_D", "STYLO_RADIAL_G", "TRICIPITAL_D", "TRICIPITAL_G"), ]
# #merge avec les noms
# namevar.df <- read.csv2("data/variables et signification.csv")
# namevar.df <- namevar.df[ , c("X", "variable")]
# data <- merge(namevar.df, data, by="variable", all.x=F, all.y=T)
data <- data[order(data$pvalue), ]
data <- data[data$N_pat_missing <= n, ]
#data <- data[data$pvalue<0.05, ]
data <- data[data$pvalue<0.2, ]
dataQbin <- data
write.table(print(data), file="clipboard", sep= "\t", row.names = FALSE)

#quali rep 
data <- df4
data <- merge(missrep.df, data, by = "variable", all.x = FALSE, all.y = TRUE)
data$variable <- as.character(data$variable)
data$param <- as.character(data$param)
data <- data[order(data$pvalue), ]
#data <- data[data$pvalue<0.05, ]
data <- data[data$pvalue<0.2, ]
data <- data[data$N_pat_missing <= n, ]
data <- data[ , names(dataQbin)]
data_q <- data

data <- rbind(dataQbin, data_q)
data <- data[order(data$pvalue), ]

write.table(print(data), file="clipboard", sep= "\t", row.names = FALSE)

#variables à binariser
"OXYM_VNI_SV"

min(d[d$qui== "TEST_MUSCUL", "x"])


write.table(print(data), file="clipboard", sep= "\t", row.names = FALSE)







#------------------------
# #yann split pour le multivarié (et pour analyse quali, meme si non obligatoire)
# 
# #préparation de la base 
# var <- "DYSPN_SOUSVENT_SV"
# y0<-bl[,  c("PATIENT", "SEX")]
# y<-d[d$qui==var,]
# y<-merge(y, y0, by="PATIENT", all.x=T, all.y=F)
# 
# #----
# #servira à la fin pour vérifier avec SEX qu'on a bien le bon nombre d'évènements
# id<-unique(y$PATIENT)
# yu<-y[match(id, y$PATIENT),]
# #----
# 
# y<-y[order(y$PATIENT, y$del),]
# z<-tapply(y$del, y$PATIENT, c)
# zf<-tapply(y$time.vni, y$PATIENT, c)
# zm<-mapply(function(x,y) c(x[-1], y[1]), z, zf)
# 
# z<-tapply(y$evt, y$PATIENT, c)
# fct<-function (x) {
#   x[-length(x)]<-0
#   return(x)
# }
# ze<-sapply(z, fct)
# 
# y$delapres<-unlist(zm)
# y$evt2<-unlist(ze)
# 
# #y[y$PATIENT=="ID7275", c("del", "delapres", "time.vni")]
# 
# ti<-0:max(y$time.vni)
# 
# yt<-y
# yt$start<-yt$del
# yt$stop<-yt$delapres
# yt$etat<-yt$evt2
# yt<-survSplit(yt, end="stop", start="start", cut=ti, event="etat")
# yt<-yt[order(yt$PATIENT, yt$start),]
# 
# 
# #des sujets meurt le jour de la visite
# yt[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni,]
# yt$etat[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni]<-1
# 
# 
# 
# #on vérifie avec la variable non dépendante du temps qu'on a bien le bon nombre d'évènement
# cox<-coxph(Surv(time.vni, evt)~SEX, data=yu)
# coxt<-coxph(Surv(start, stop, etat)~SEX, data=yt)
# cox
# coxt
# #ok c'est bon les résultats sont identiques, on a découpé sans encombre
# 
# 
# #---------------
# #analyse var qual(version Yann) 
# yt$x<-factor(yt$x)
# 
# table(yt$x)
# yt$x1<-ifelse(yt$x=="1", 1, 0)
# yt$x2<-ifelse(yt$x=="2", 1, 0)
# yt$x3<-ifelse(yt$x=="3", 1, 0)
# 
# 
# cox<-coxph(Surv(start, stop, etat)~x2+x3+cluster(PATIENT), data=yt) #on ne met que 2 des 3 niveaux. le niveaux 1 est la ref
# summary(cox)
# b<-coef(cox)
# zr<-cox.zph(cox, "rank")
# zi<-cox.zph(cox, "identity")
# zr
# 
# plot(zi[1])
# abline(h=b[1], col = "blue")
# plot(zi[2])
# abline(h=b[2], col= "blue")
# 
# #transformation du temps
# yt$x2t<-log(yt$stop)*yt$x2
# yt$x3t<-log(yt$stop)*yt$x3
# coxt<-coxph(Surv(start, stop, etat)~x2+x2t+x3+x3t+cluster(PATIENT), data=yt)
# summary(coxt)
# bt<-coef(coxt)
# ztr<-cox.zph(coxt, "rank")
# zti<-cox.zph(coxt, "identity")
# ztr
# par(mfrow=c(2,2))
# for (i in 1:4) {
#   plot(zti[i])
#   title(names(bt)[i])
#   abline(h=bt[i])
# }
# 
# #decoupage du temps
# m<-31.25 #pour transformer en mois
# yt$x2t1<-ifelse(yt$stop<=7*m, yt$x2, 0)
# yt$x2t2<-ifelse(yt$stop>7*m & yt$stop<=17*m, yt$x2, 0)
# yt$x2t3<-ifelse(yt$stop>17*m & yt$stop<=34*m, yt$x2, 0)
# yt$x2t4<-ifelse(yt$stop>34*m, yt$x2, 0)
# yt$x3t1<-ifelse(yt$stop<=7*m, yt$x3, 0)
# yt$x3t2<-ifelse(yt$stop>7*m & yt$stop<=17*m, yt$x3, 0)
# yt$x3t3<-ifelse(yt$stop>17*m & yt$stop<=34*m, yt$x3, 0)
# yt$x3t4<-ifelse(yt$stop>34*m, yt$x3, 0)
# #summary(yt)
# coxt<-coxph(Surv(start, stop, etat)~x2t1+x2t2+x2t3+x2t4+x3t1+x3t2+x3t3+x3t4+cluster(PATIENT), data=yt)
# summary(coxt)
# bt<-coef(coxt)
# ztr<-cox.zph(coxt, "rank")
# zti<-cox.zph(coxt, "identity")
# ztr
# par(mfrow=c(3,3))
# for (i in 1:8) {
#   plot(zti[i])
#   title(names(bt)[i])
#   abline(h=bt[i])
# }
# 
# #-----------------------
# #version Sarah
# #préparation de la base 
# var <- "DYSPN_SOUSVENT_SV"
# y0<-bl[,  c("PATIENT", "SEX")]
# y<-d[d$qui==var,]
# y<-merge(y, y0, by="PATIENT", all.x=T, all.y=F)
# y<-y[order(y$PATIENT, y$del),]
# z<-tapply(y$del, y$PATIENT, c)
# zf<-tapply(y$time.vni, y$PATIENT, c)
# zm<-mapply(function(x,y) c(x[-1], y[1]), z, zf)
# 
# z<-tapply(y$evt, y$PATIENT, c)
# fct<-function (x) {
#   x[-length(x)]<-0
#   return(x)
# }
# ze<-sapply(z, fct)
# 
# y$delapres<-unlist(zm)
# y$evt2<-unlist(ze)
# ti<-0:max(y$time.vni)
# 
# yt<-y
# yt$start<-yt$del
# yt$stop<-yt$delapres
# yt$etat<-yt$evt2
# yt<-survSplit(yt, end="stop", start="start", cut=ti, event="etat")
# yt<-yt[order(yt$PATIENT, yt$start),]
# 
# #des sujets meurt le jour de la visite
# yt[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni,]
# yt$etat[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni]<-1
# 
# #analyse var qual 
# yt$x<-factor(yt$x)
# yt[,var] <- yt$x
# q2b <- get_binaires(var, yt)
# vec <- grep(var, names(q2b))
# n1 <- names(q2b)[vec[-1]]
# ref <- names(table(yt[,var]))[1]
# f <- paste0("Surv(start, stop, etat) ~ ", paste(n1, collapse=" + "), "+cluster(PATIENT)")
# mod <- coxph(as.formula(f), data = q2b)
# .title <- paste0("RP of ", n1)
# #Test de Harrell
# z <- cox.zph(mod, transform = "rank")
# pval <- round(z$table[,3],3)[-nrow(z$table)]
# #résidus de Shoenfeld
# z <- cox.zph(mod, transf="identity")
# for (i in 1:(nrow(z$table)-1)){
#   plot(zi[i], main=paste0(.title[i], "\nref = ", ref, "\nHarrell test p = ",pval[i]), resid = FALSE, ylab = paste0("Beta(t) for ", n1[i]))
#   abline(h=0, col="red")
#   abline(h=coef(mod)[i], col="blue")
# }
# 
# #modification en transformant le temps
# for (i in 1:length(n1)){
#   q2b[ ,paste0(n1[i], "t")] <-log(q2b$stop)*q2b[ ,n1[i]]
# }
# f <- paste0("Surv(start, stop, etat) ~ ", paste(n1, collapse=" + "),
#             " + ", paste(paste0(n1, "t"), collapse=" + "), 
#             " + cluster(PATIENT)")
# coxt<-coxph(as.formula(f), data=q2b)
# summary(coxt)
# #Test de Harrell
# z <- cox.zph(coxt, transform = "rank")
# pval <- round(z$table[,3],3)[-nrow(z$table)]
# #résidus de Shoenfeld
# zi <- cox.zph(coxt, transf="identity")
# for (i in 1:(nrow(z$table)-1)){
#   plot(zi[i], main=paste0(rownames(zi$table)[i], "\nHarrell test p = ",pval[i]), resid = FALSE, ylab = paste0("Beta(t) for ", rownames(zi$table)[i]))
#   abline(h=0, col="red")
#   abline(h=coef(coxt)[i], col="blue")
# }
# 
# 
# 
# 
# 
# 





#===============================
#Lecture resultats

tmp <- readRDS("data/HRIC_quanti_bl.rds")
