#########################
#    SURVIE UNIVARIEE   #
#########################


source("src/libraries_SLA.R")
source("src/fonctions_SLA.R")

#=======================
#=======================
#variables baselines
bl <- readRDS("data/bl.rds")
bdd_dates <- readRDS("data/bdd_dates.rds")


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
# apply(d2, 2, unique)
# apply(d2, 2, unique)["FERM_BOUCHE"]#moins de 25% des sujets l'ont renseignée => pas dans binaire
# apply(d2, 2, unique)["OXY_THERAP"]#moins de 25% des sujets l'ont renseignée => pas dans binaire


binaire <- as.character(ncu[ncu$nval == 2 & ncu$min_level > 1, "var"])
quanti  <- as.character(ncu[ncu$nval > 6, "var"])
quali <- as.character(ncu[ncu$nval > 2 & ncu$nval <= 6, "var"])
as.character(ncu[ncu$nval == 1, "var"])

#=======================
#analyses binaires

#----
#courbes de survie
.l1 <- lapply(binaire, function(x)draw_surv_bin(x, data = d, .time = "time.vni", .evt = "evt", vec_time_IC= 1, recode = FALSE, surv_only=FALSE, pvalue = TRUE, dep_temps=FALSE, .transf=NULL))
ml <- marrangeGrob(.l1,ncol=1,nrow=1,top = NULL)
ggsave(file="binaire_bl.pdf", ml)

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
write.table(print(binaire), file="clipboard", sep="\t")
rp <- read.csv2("data/RP_bin.csv")

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
rpt <- read.csv2("data/RP dep du temps beta ok.csv")
rpt <- merge(rpt, subset(df_bint, select=-curve), by=c("variable", "transf"), all= F)

#selection de la transformation par AICmin qd curve et Harrell ok
rpts <- rpt[rpt$curve==TRUE,]
AICmin <- tapply(as.numeric(as.character(rpts$AIC)), as.character(rpts$variable), min)
AICmin <- data.frame(variable = names(AICmin), AICmin = as.numeric(AICmin))
rpts <- merge(rpts, AICmin, by="variable")

binNRPt <- unique(rpts[rpts$AIC==rpts$AICmin, c("variable", "transf")])

df_dept <- merge(binNRPt, df_bint, by = c("variable","transf"), all=F)

#####
#Decoupe du temps pour les variables dont on n'a pas trouvé la bonne transformation:
tmp <- binNRP [!binNRP %in% binNRPt$variable]
b <- c(6,18,50) #ok on garde 6 18 50 pour toutes les variables non corrigées par transformation du temps
#b <- c(6, 18, 36)
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
saveRDS(allbin, "data/all_var_bin_bl.rds")
#----
#HR [IC] et test du score (ou robuste)

.l <- lapply(1 : nrow(allbin), function(num){
HR_score (var=allbin$variable[num], data = d, .time="time.vni", .evt="evt", recode=FALSE, .transf=allbin$transf[num], .tps_clinique=12)
})
df_HR <- do.call(rbind,.l)

saveRDS(df_HR, "data/HRIC_bin_bl.rds")
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
write.table(print(RP_quanti[ ,c("variable", "RP")]), file="clipboard", sep="\t")
rp <- read.csv2("data/RP_quanti_bl.csv") #RP = TRUE pour hyopthèses respectée

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

#Je ne regarde que les transformations pour lesquelles la pvalue de at est inférieure à 0.05 (beta_at = TRUE) (la courbe n'est de toute façon pas tracée quand sup à 0.05)

#remplissage de curve si test de beta_at(t) significatif
#remplissage à la main de la colonne curve à l'aide du fichier "data/analyses/RP_quanti_bl_transft.pdf"
write.table(print(df_quant[df_quant$beta_at==TRUE & df_quant$param == "at", c("variable", "transf", "curve")]), file="clipboard", sep="\t", row.names=F)
rpt <- read.csv2("data/RP quanti dept beta ok.csv")
rpt <- merge(rpt, subset(df_quant, select=-curve), by=c("variable", "transf"), all= F)

#selection de la transformation qd curve et Harrell ok (en fait quand curve ok, harrell est forcémen ok aussi)
rpts <- rpt[rpt$curve==TRUE,]
#Je prend la transformation avec le plus petit AIC
AICmin <- tapply(as.numeric(as.character(rpts$AIC)), as.character(rpts$variable), min)
AICmin <- data.frame(variable = names(AICmin), AICmin = as.numeric(AICmin))
rpts <- merge(rpts, AICmin, by="variable")
#pb : log 10 a le même AIC => je supprime log10

QN_RPt <- unique(rpts[rpts$AIC==rpts$AICmin, c("variable", "recode", "transf")])
QN_RPt$variable <- as.character(QN_RPt$variable)
QN_RPt <- QN_RPt[QN_RPt$transf != "log10", ]

#####
#Decoupe du temps pour les variables dont on n'a pas trouvé la bonne transformation:
tmp <- QN_RP [!QN_RP$variable %in% QN_RPt$variable, ]

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
QN_RPcut <- unique(df_quanti_cutt[ ,c("variable", "recode", "transf")])

#####
#J'ajoute une colonne transf NA pour les variables ok


#all var et transf
allquant <- rbind(QRP, QN_RPcut, QN_RPt)
rownames(allquant) <- 1:nrow(allquant)
allquant$variable <- as.character(allquant$variable)
saveRDS(allquant, "data/all_var_quanti_bl.rds")

#----
#HR [IC] et test du score (ou robuste)

.l <- lapply(1 : nrow(allquant), function(num){
  HR_score (var=allquant$variable[num], data = d, .time="time.vni", .evt="evt", recode=allquant$recode[num], .transf=allquant$transf[num], .tps_clinique=12)
})
df_HR_quanti_bl <- do.call(rbind,.l)
saveRDS(df_HR_quanti_bl, "data/HRIC_quanti_bl.rds")

#----------------------
#Survie pour les quanti recodées
allquant <- readRDS("data/all_var_quanti_bl.rds")
#courbe
df <- allquant[allquant$recode==TRUE, ]
.l <- lapply(df$variable, function(x)draw_surv_bin(var=x, data = d, .time="time.vni", .evt="evt", vec_time_IC= 1,
                  recode=TRUE, surv_only=FALSE, pvalue = TRUE))
ml <- marrangeGrob(.l,ncol=1,nrow=1,top = NULL)
ggsave(file="courbe_survie_quanti_recode_bl.pdf", ml)

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

test_musc <- grepl("TEST_MUSC", quali)
quali2 <- quali[!test_musc]
quali2 <- c("LIEUDEB_recode", "DYSP_EFFORT", "DYSP_PAROX")

#courbes de survie
.l <- lapply(quali2, function(x) draw_surv_qualisup2(var=x, data = d, .time = "time.vni", .evt = "evt", vec_time_IC= c(1, 3), surv_only=FALSE, pvalue=TRUE))
ml <- marrangeGrob(.l,ncol=1,nrow=1,top = NULL)
ggsave(file="courbe_survie_qualisup2_bl.pdf", ml)


#check RP
pdf(paste0("data/analyses/RP_quali_bl.pdf"))
.l <- lapply(quali2, function(x)check_RP(var=x, data=d, .time="time.vni", .evt="evt", quali = TRUE))
dev.off()

#modif Non RP

#HRIC : à refaire
.l <- lapply(1 : nrow(quali2), function(num){
  HR_score (var=quali2$variable[num], data = d, .time="time.vni", .evt="evt", recode=quali2$recode[num], .transf=quali2$transf[num], .tps_clinique=12)
})
df_HR_quali_bl <- do.call(rbind,.l)
saveRDS(df_HR_quali_bl, "data/HRIC_quali_bl.rds")

HR_score (var="SEX", data=sla, .time="time.vni", .evt="evt", recode=FALSE, .transf=NULL, .tps_clinique = 12)

#=======================
#=======================
#variables dont la valeur dépendant du temps
d <- readRDS("data/df_rep.rds")

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

#Je supprime var :
sup <- c("DCD", "DIAG", "DIAGPROBA")
ncu <- ncu[!ncu$var %in% sup, ]

#Pour savoir qui est quali et qui est quanti:
ncu[order(ncu$nval),]
lapply(allx_byvar, unique)["TRICIPITAL_G"]

binaire <- data.frame(var = as.character(ncu[ncu$nval == 2 & ncu$min_level > 1, "var"]), stringsAsFactors = FALSE)
binaire$type <- "binaire"
quanti  <- data.frame(var = as.character(ncu[ncu$nval > 6, "var"]), stringsAsFactors = FALSE)
quanti$type <- "quanti"
quali <- data.frame(var = as.character(ncu[ncu$nval > 2 & ncu$nval <= 6, "var"]), stringsAsFactors = FALSE)
quali$type <- "quali"
#as.character(ncu[ncu$nval == 1, "var"])
s_i <- d2

all_var <- rbind(binaire, quali, quanti)
all_var$recode <- FALSE
all_var$Harrell_test <- NA
#=======================
#analyse

pdf("data/analyses/RP_var_rep.pdf")
for (nr in 1:nrow(all_var)) {#ce sera le debut de la boucle
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
        Dt$x <- ifelse(Dt$x < median(Dt$x), 0, 1)
        all_var$recode[nr] <- TRUE
        .title <- paste0 ("RP of ", var, " superior to ", round(median(Dt$x),0))
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
all_var <- read.csv2("data/RP_all_rep.csv")
all_var$var <- as.character(all_var$var)
all_var$type <- as.character(all_var$type)


#faire un tableau avec les coef pour chaque variable et sortir un schéma pour chaque variable
#puis générer un vecteur HR : si le risque prop est vérifié, HR=1 sinon HR=0
#Faire un tableau avec : HRok <- var, HR0/1
rRPok <- all_var[all_var$RP==TRUE, ]#HR est à modifier en fonction des résultats aux risques proportionnels
rRPok$transf <- NA


rNok <- all_var[all_var$RP==FALSE, ]

pdf("data/analyses/RP_var_rep_findtransf.pdf")
.l <- lapply(1:nrow(rNok), function(nr){
#.l <- lapply(5, function(nr){
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
    Dt$x <- ifelse(Dt$x < median(Dt$x), 0, 1)
    .title <- paste0 ("RP of ", var, " superior to ", round(median(Dt$x),0))
  } else {
    .title <- paste0 ("RP of ", var)
  }

  #------------------
  #RP non vérifiés, ajout variable dépendante du temps

  #on split une deuxième fois
  dt<-Dt
  dt$evt<-dt$etat
  dt<-survSplit(dt, end="stop", start="start", cut=t, event="evt")
  dt<-dt[order(dt$id, dt$start),]


  #transformation :
  tmp <- lapply(c("log","sqrt","*t","/t","*t^2","*t^0.7", "log10", "*t^0.3", "*t^3"),function(x){
  #tmp <- lapply("/t",function(x){
    # tmp <- lapply(c("*t^0.7"),function(x){
    add_vart_and_check_dt(data_split=dt, var=var, title = .title, .transf=x)#[[1]] #Evite que ça ne bloque si le modèle ne converge pas
  })
  a <- do.call(rbind, tmp)

  return(a)
})

dev.off()

repRP <- do.call(rbind, .l)
repRP <- repRP[repRP$beta_at == TRUE , ]
repRP <- repRP[repRP$Harrell == "ok", ]

write.table(print(repRP[repRP$param == "at", c("variable", "transf", "curve")]), file="clipboard", sep="\t", row.names=F)
repRPt <- read.csv2("data/RP vart transfo t.csv")
repRPt <- merge(repRPt, subset(repRP, select=-curve), by=c("variable", "transf"), all= F)

repRPt <- repRPt[repRPt$curve==TRUE, ]
#Je prend la transformation avec le plus petit AIC
AICmin <- tapply(as.numeric(as.character(repRPt$AIC)), as.character(repRPt$variable), min)
AICmin <- data.frame(variable = names(AICmin), AICmin = as.numeric(AICmin))
repRPt <- merge(repRPt, AICmin, by="variable")

repRPt <- unique(repRPt[repRPt$AIC==repRPt$AICmin, c("variable", "recode", "transf")])
repRPt$variable <- as.character(repRPt$variable)
repRPt <- repRPt[!duplicated(repRPt$variable),]

rNok[rNok$var %in% repRPt$variable, "transf"] <- repRPt$transf

#-----------------------------------------------
#Decoupe du temps pour les dernieres variables non corrigées

rNok_cut <- rNok[is.na(rNok$transf), ]

pdf(paste0("data/analyses/RP_var_rep_cut_", b, ".pdf"))
#.l <- lapply(1:nrow(rNok_cut), function(nr){
b <- c(6,18,50)
b <- c(6,20,60)
b <- c(4, 10, 30)
b <- c(5, 7)
cut_rep("TRONC", b)
dev.off()

dfrepcut <- do.call(rbind, .l)
repcut <- dfrepcut

#binNRPcut <- unique(df_bint[ ,c("variable", "transf")])

#=========================
#=========================
>>>>>>> 2c6114ea93b7ac3eb1cb3051f70a3cab85199115

#Je supprime variable si moins de 25% des sujets l'ont renseignée
#n pour 25% des sujets
n25 <- nrow(bl)*0.25
ncu <- ncu[ncu$npat > n25, ]

#Je supprime var :
sup <- c("DCD", "DIAG", "DIAGPROBA")
ncu <- ncu[!ncu$var %in% sup, ]

#Pour savoir qui est quali et qui est quanti:
ncu[order(ncu$nval),]
lapply(allx_byvar, unique)["TRICIPITAL_G"]

binaire <- data.frame(var = as.character(ncu[ncu$nval == 2 & ncu$min_level > 1, "var"]), stringsAsFactors = FALSE)
binaire$type <- "binaire"
quanti  <- data.frame(var = as.character(ncu[ncu$nval > 6, "var"]), stringsAsFactors = FALSE)
quanti$type <- "quanti"
quali <- data.frame(var = as.character(ncu[ncu$nval > 2 & ncu$nval <= 6, "var"]), stringsAsFactors = FALSE)
quali$type <- "quali"
#as.character(ncu[ncu$nval == 1, "var"])
s_i <- d2

all_var <- rbind(binaire, quali, quanti)
all_var$recode <- FALSE
all_var$Harrell_test <- NA

#=======================
#verif loglin et RP

pdf("data/analyses/RP_var_rep.pdf")
for (nr in 1:nrow(all_var)) {#ce sera le debut de la boucle
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
        Dt$x <- ifelse(Dt$x < median(Dt$x), 0, 1)
        all_var$recode[nr] <- TRUE
        .title <- paste0 ("RP of ", var, " superior to ", round(median(Dt$x),0))
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
all_var <- read.csv2("data/RP_all_rep.csv")
all_var$var <- as.character(all_var$var)
all_var$type <- as.character(all_var$type)


#faire un tableau avec les coef pour chaque variable et sortir un schéma pour chaque variable
#puis générer un vecteur HR : si le risque prop est vérifié, HR=1 sinon HR=0
#Faire un tableau avec : HRok <- var, HR0/1
rRPok <- all_var[all_var$RP==TRUE, ]#HR est à modifier en fonction des résultats aux risques proportionnels
rRPok$transf <- NA


rNok <- all_var[all_var$RP==FALSE, ]

pdf("data/analyses/RP_var_rep_findtransf.pdf")
.l <- lapply(1:nrow(rNok), function(nr){
#.l <- lapply(5, function(nr){
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
    Dt$x <- ifelse(Dt$x < median(Dt$x), 0, 1)
    .title <- paste0 ("RP of ", var, " superior to ", round(median(Dt$x),0))
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
  tmp <- lapply(c("log","sqrt","*t","/t","*t^2","*t^0.7", "log10", "*t^0.3", "*t^3"),function(x){
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
rNRP_t <- read.csv2("data/RP vart transfo t.csv")
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
 b <- c(7,17,34)
 cut_rep(rNRP_cut$var[1], b, rNRP_cut$recode[1])
# dev.off()
#
# dfrepcut <- do.call(rbind, .l)
# rNRP_cut <- dfrepcut

rNRP_cut$transf <- "7-17-34"
rNRP_cut$variable <- rNRP_cut$var
rNRP_cut <- rNRP_cut[ , c("variable", "recode", "transf")]
#binNRPcut <- unique(df_bint[ ,c("variable", "transf")])

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
    .title <- paste0 ("RP of ", var, " superior to ", round(median(Dt$x),0))
    Dt$x <- ifelse(Dt$x < median(Dt$x), 0, 1)
  } else {
    .title <- paste0 ("RP of ", var)
  }

  #------------------
  #HR IC valeur des coef beta et test (analyses en fonction de la transformation qui convient)
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
  } else {

    #avant de transformer, on split une deuxième fois
    dt<-Dt
    dt$evt<-dt$etat
    ti <- sort(unique(dt$stop[dt$etat==1]))
    dt<-survSplit(dt, end="stop", start="start", cut=ti, event="evt")
    dt<-dt[order(dt$id, dt$start),]

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
saveRDS(a, "data/HRIC_var_rep.rds")



#
#   #HRIC
#   coxt<-coxph(Surv(start, stop, evt)~x+cluster(id), data=dt)
#   a <- summary(coxt)
#   pval <- round(a$robscore[3],3)
#   coefx <- round(a$coefficients[1],4)
#   coefxt <- NA
#   HRIC <- round(data.frame(IC=a$conf.int[1], l=a$conf.int[3], u=a$conf.int[4]),3)
#   HR <- HRIC$IC
#   #HRIC95 <- paste0(HRIC[1], "[", HRIC[2], "-", HRIC[3], "]")
#   HRIC95 <- paste0("[", HRIC[2], "-", HRIC[3], "]")
#   test <- "robust"
#   tps_clinique <- "NA"
#   res <- data.frame(var, RP, transf, tps_clinique, HR, HRIC95, coefx, coefxt, test, pval)
#
#   return(res)
# })
# a <- do.call(rbind, .l)
# saveRDS(a, "data/analyses/HRIC_var_rep.rds")
# a <- a[order(a$pval, decreasing = FALSE), ]
# write.table(print(a), file="clipboard", sep="\t", row.names= F)
#=========================
#=========================



s1 <- readRDS("data/analyses/df_surv_bl.rds")
s2 <- readRDS("data/analyses/df_surv_quanti_bl.rds")
write.table(print(s2), file="clipboard", sep= "\t", row.names = FALSE)

#HRIC
df1 <- readRDS("data/HRIC_bin_bl.rds")
df2 <- readRDS("data/HRIC_quanti_bl.rds")
df3 <- readRDS("data/HRIC_var_rep.rds")

#df1
data <- readRDS("data/HRIC_bin_bl.rds")
data <- data[ , c("variable", "recode", "RP", "transf", "tps_clinique", "HRIC", "pvalue", "beta")]
data$beta <- ifelse(data$beta > -0.001 & data$beta <0, "-0.001<beta<0", as.character(round(data$beta, 2)))
a <- tapply(data$beta, data$variable, paste, collapse = "; ")
a <- data.frame(variable = names(a), beta = a)
a$variable <- as.character(a$variable)
data <- data[match(unique(data$variable), data$variable), ]
data <- merge(data[ , - ncol(data)], a, by="variable")
write.table(print(data), file="clipboard", sep= "\t", row.names = FALSE)

#df2
data <- readRDS("data/HRIC_quanti_bl.rds")
data <- data[ , c("variable", "recode", "RP", "transf", "tps_clinique", "HRIC", "pvalue", "beta")]
data$beta <- ifelse(data$beta > -0.001 & data$beta <0, "-0.001<beta<0", as.character(round(data$beta, 2)))
a <- tapply(data$beta, data$variable, paste, collapse = "; ")
a <- data.frame(variable = names(a), beta = a)
a$variable <- as.character(a$variable)
#uniquement première ligne
data <- data[match(unique(data$variable), data$variable), ]
data <- merge(data[ , - ncol(data)], a, by="variable")
write.table(print(data), file="clipboard", sep= "\t", row.names = FALSE)

#df3
data <- readRDS("data/HRIC_var_rep.rds")
data <- data[ , c("variable", "recode", "RP", "transf", "tps_clinique", "HRIC", "pvalue", "beta")]
data$beta <- ifelse(data$beta > -0.001 & data$beta <0, "-0.001<beta<0", as.character(round(data$beta, 2)))
a <- tapply(data$beta, data$variable, paste, collapse = "; ")
a <- data.frame(variable = names(a), beta = a)
a$variable <- as.character(a$variable)
#uniquement première ligne
data <- data[match(unique(data$variable), data$variable), ]
data <- merge(data[ , - ncol(data)], a, by="variable")
write.table(print(data), file="clipboard", sep= "\t", row.names = FALSE)


# s <- d
# var <- "SEX"
# pdf(paste0("data/analyses/RP_bin", var, ".pdf"))
# s$a_recode <- s[ ,var]
# s <- s[!is.na(s$a_recode),]
# s$tps <- (s[ ,"time.vni"]/365.25*12) + 0.001
# ti <- sort(unique(c(0,s$tps[s$evt==1])))
# slat <- s
# slat$start <- 0
# slat$stop <- slat$tps
# slat$evt <- slat$evt
# slat <- survSplit(Surv(stop,evt)~.,slat,start="start",cut=ti)
#
#
# b<-c(6,12,18)
# for (i in 1:(length(b)+1)){
#   if (i == 1) tmp <-  slat$a_recode * ifelse(slat$stop<=b[1], 1, 0)
#   if(i <= length(b) & i!= 1) tmp <- slat$a_recode * ifelse(slat$stop>b[i-1] & slat$stop<=b[i], 1, 0)
#   if(i == (length(b)+1)) tmp <-  slat$a_recode * ifelse(slat$stop>b[i-1], 1, 0)
#   slat[ ,paste0("at",i)] <- tmp
# }
# vat<-paste("at",  1:(length(b)+1), sep="")
# x<-slat[, vat]
# sx<-colSums(x) #interval de temps sans evt
# wat<-vat[sx>0] #on supprime interval de temps quand pas d'evenement
# f<-paste("Surv(start, stop, evt) ~ ", paste(wat, collapse="+"),"+cluster(PATIENT)", sep="")  #on ne met pas a_recode car les at couvre deja  toutes les perdiodes
#
#
# coxt <- coxph(as.formula(f), data=slat)
# coxt
#
# t0<- 6
# i <- findInterval(t0, b)
# exp(coef(coxt)[i])
# exp(cbind(coef(coxt), qnorm(0.025, coef(coxt), sqrt(diag(vcov(coxt)))), qnorm(1-0.025, coef(coxt), sqrt(diag(vcov(coxt))))))
#
#
#
# bhat<-coef(coxt)
# .AIC <- extractAIC(coxt)[2] #la premiere est le model vide, la deuxième le modèle avec les variables d'intéret
# zit <- cox.zph(coxt, transform = "rank")
# pval <- round(zit$table[,3],3)
# zt <- cox.zph(coxt, transf="identity")
# for (i in 1:(nrow(zt$table)-1)){
#   iz<-i
#   plot(zt[iz], resid = FALSE, main = paste0("plot shoenfeld for", var, "transformation\nHarrell ok\nAIC = ", .AIC))
#   abline(h=0, col="red")
#   abline(h=bhat[i], col="blue", lty=2)
#   abline(v=b)
# }
#
# summary(survfit(coxt), time=seq(0, 80, by=10))
# #-------------
#
# b<-c(6, 18, 50, 50)
# slat$at1 <- slat$a_recode * ifelse(slat$stop<=b[1], 1, 0)
# slat$at2 <- slat$a_recode * ifelse(slat$stop>b[1] & slat$stop<=b[2], 1, 0)
# slat$at3 <- slat$a_recode * ifelse(slat$stop>b[2] & slat$stop<=b[3], 1, 0)
# slat$at4 <- slat$a_recode * ifelse(slat$stop>b[3] & slat$stop<=b[4], 1, 0)
# slat$at5 <- slat$a_recode * ifelse(slat$stop>b[4], 1, 0)
#
#
#
# vat<-paste("at", 1:5, sep="")
# x<-slat[, vat]
# sx<-colSums(x)
# wat<-vat[sx>0]
# f<-paste("Surv(start, stop, evt) ~ ", paste(wat, collapse="+"),"+cluster(PATIENT)", sep="")
# coxt <- coxph(as.formula(f), data=slat)
# coxt
# bhat<-coef(coxt)
# .AIC <- extractAIC(coxt)[2]
# zit <- cox.zph(coxt, transform = "rank")
# pval <- round(zit$table[,3],3)
# zt <- cox.zph(coxt, transf="identity")
# for (i in 1:(nrow(zt$table)-1)){
#   iz<-i
#   plot(zt[iz], resid = FALSE, main = paste0("plot shoenfeld for", var, "transformation\nHarrell ok\nAIC = ", .AIC))
#   abline(h=0, col="red")
#   abline(h=bhat[i], col="blue", lty=2)
#   abline(v=b)
# }
#
#
#
# summary(survfit(coxt), time=seq(0, 80, by=10))
#
# b<-c(5,18)
# slat$at1 <- slat$a_recode * ifelse(slat$stop<=b[1], 1, 0)
# slat$at2 <- slat$a_recode * ifelse(slat$stop>b[1] & slat$stop<=b[2], 1, 0)
# coxt <- coxph(Surv(start, stop, evt) ~ a_recode + at1 + at2, data=slat)
# coxt
#
# # b<-c(6)
# # slat$at1 <- slat$a_recode * ifelse(slat$stop<=b[1], 1, 0)
# # coxt <- coxph(Surv(start, stop, evt) ~ a_recode + at1, data=slat)
# # coxt
#
#
# # #coxt <- coxph(Surv(start, stop, evt) ~ a_recode + at, data=slat)
# # coxt <- coxph(Surv(start, stop, evt) ~ a_recode + at1 + at2 + at3, data=slat)
# # coxt
# bhat<-coef(coxt)
# bhat<-c(bhat[-1],0)+bhat[1];bhat
#
# .AIC <- extractAIC(coxt)[2]
# zit <- cox.zph(coxt, transform = "rank")
# pval <- round(zit$table[,3],3)
# zt <- cox.zph(coxt, transf="identity")
# for (i in 1:(nrow(zt$table)-1)){
#   iz<-i
#   plot(zt[iz], resid = FALSE, main = paste0("plot shoenfeld for", var, "transformation\nHarrell ok\nAIC = ", .AIC))
#   abline(h=0, col="red")
#   abline(v=b)
# }
# dev.off()
# test <- summary(coxt)
# pval_at <- test$coefficients["at","Pr(>|z|)"]


#===============================
#Lecture resultats

tmp <- readRDS("data/HRIC_quanti_bl.rds")
