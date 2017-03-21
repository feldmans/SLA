who <- "Yann"
who <- "Sarah"

.dir <- getwd()
source(paste0(.dir,"/src/libraries_SLA.R"))
source(paste0(.dir,"/src/fonctions_SLA.R"))

 sla <- readRDS(paste0(.dir,"/data/BASE_SLA_allbl_withnames.rds"))
# sla$keep <- ifelse (sla$DATEVNI<=sla$ddn,1,0)
# sla <- sla[sla$keep==1, ]
# sla$time.vni <- as.numeric(sla$ddn - sla$DATEVNI)
# sla$time.sym <- as.numeric(sla$ddn - sla$FIRSTSYMPTOM)
# sla$censor <- ifelse (!is.na(sla$date_dc),1, 0)
# #sla$ddn <- ifelse(sla$ddn>as_date("2015-08-27"), as_date("2015-08-27"), sla$ddn) #voir avec Yann
# sla1 <- sla
# sla1$PATIENT <- sla1$PATIENT1
# sla1 <- sla1[ ,c("PATIENT","censor","time.vni")]
# 
# bdd_rep <- readRDS("data/essairep.rds")
# 
# s <- merge(sla1,bdd_rep, by="PATIENT", all.x=F, all.y=T)

bd <- readRDS("data/bdd_dates.rds")
#j'élimines les 9 patients mort avant la vni
bd <- bd[bd$datevni<=bd$fin_vni, ]
#bd$dfin <- as_date(ifelse(bd$dfin>as_date("2015-08-27"), as_date("2015-08-27"), bd$dfin)) #pb : 2 patients ont vni après 08 2015 
bd$time.vni <- as.numeric(bd$dfin - bd$datevni)
bd$censor <- ifelse (!is.na(bd$date_dc), 1, 0)

#--------
#--------
#--------
#=================================================================
#0/ Merge des tables et data management
#Yann 02/03/2017
bd <- readRDS("data/bdd_dates.rds")
#j'élimines les 9 patients mort avant la vni
bd <- bd[bd$datevni<=bd$fin_vni, ]
#bd$dfin <- as_date(ifelse(bd$dfin>as_date("2015-08-27"), as_date("2015-08-27"), bd$dfin)) #pb : 2 patients ont vni après 08 2015 
bd$time.vni <- as.numeric(bd$dfin - bd$datevni)
bd$evt <- ifelse (!is.na(bd$date_dc), 1, 0)

bdd_rep <- readRDS("data/df_rep_neuro.rds")
bdd_rep <- subset(bdd_rep, select =- datevni)
s_a <- merge(bd, bdd_rep, by="PATIENT", all.x=F, all.y=F)

bdd_rep <- readRDS("data/df_rep_nobl_pneumo_imput.rds")
bdd_rep <- subset(bdd_rep, select =- datevni)
s_b <- merge(bd, bdd_rep, by="PATIENT", all.x=F, all.y=F)

bdd_rep <- readRDS("data/df_rep_pneumo.rds")
bdd_rep <- subset(bdd_rep, select =- datevni)
s_c <- merge(bd, bdd_rep, by="PATIENT", all.x=F, all.y=F)

s <- rbind(s_a, s_b, s_c)
#s[s$PATIENT=="ATTIAN_ALAIN",]
#bd[bd$PATIENT=="ATTIAN_ALAIN",]
#bdd_rep[bdd_rep$PATIENT=="ATTIAN_ALAIN",]

#si décès avant vni, on supprime la ligne
s<-s[s$time.vni>0,]

#si date de visite après décès(et date inférieure à systeme date), ce n'est pas un décès et time.vni(follow up time à partir de la vni)=max date de visite 
s[s$PATIENT=="QUERE_PATRICK",]
s$date_dc[s$PATIENT=="QUERE_PATRICK"]<-NA
s$evt[s$PATIENT=="QUERE_PATRICK"]<-0
M<-max(s$date[s$PATIENT=="QUERE_PATRICK"]);M
m<-(s$datevni[s$PATIENT=="QUERE_PATRICK"][1]);m
s$time.vni[s$PATIENT=="QUERE_PATRICK"]<-as.numeric(M-m)
s[s$PATIENT=="QUERE_PATRICK",]

s[s$PATIENT=="OUARY_ROGER",]
s$date_dc[s$PATIENT=="OUARY_ROGER"]<-NA
s$evt[s$PATIENT=="OUARY_ROGER"]<-0
M<-max(s$date[s$PATIENT=="OUARY_ROGER"]);M
m<-(s$datevni[s$PATIENT=="OUARY_ROGER"][1]);m
s$time.vni[s$PATIENT=="OUARY_ROGER"]<-as.numeric(M-m)
s[s$PATIENT=="OUARY_ROGER",]


#si date de visite superieure à systeme date, on supprime la visite
s <- s[s$date<Sys.Date(), ]


#quasiment pas renseigné... je laisse tomber ces variables
# s_dur <- merge(s[s$qui %in% "DUREE_ENREG_H_PP", ], s[s$qui %in% "DUREE_ENREG_MIN_PP", c("PATIENT", "date", "x")], by=c("PATIENT", "date"), suffixes = c(".h",".min"), ALL=T)
# s_dur$x.h <- s_dur$x.h*60
# s_dur$x <- rowSums(s_dur[ ,c("x.h", "x.min")], na.rm=T)
s_i <- s
#=================================================================
#1/ selection des variable

#1.1 : var répétées 

#var non étudiées pour l'instant :
#var intéressante à analyser en descriptif : "CAUSDCD_SLA"
#%TTT_CHOICE = TTT_TEMPLATE dans CRF
nokeep <- c("SITE_LIMIT_ART_CHOICE_1", "SITE_LIMIT_ART_CHOICE_2", "SITE_LIMIT_ART_CHOICE_3", "SITE_LIMIT_ART_CHOICE_4", "TTT_CHOICE_1", "TTT_CHOICE_2", "TTT_CHOICE_3", "TTT_CHOICE_4",
            "TTT_CHOICE_5", "TTT_CHOICE_6", "TTT_CHOICE_7", "TTT_CHOICE_8", "TTT_CHOICE_9", "TTT_CHOICE_10", "TTT_CHOICE_11", "TTT_CHOICE_12", "EVAL_COMPL",
            "SITE_RETRACT_CHOICE_1", "SITE_RETRACT_CHOICE_2", "SITE_RETRACT_CHOICE_3", "SITE_RETRACT_CHOICE_4", "HEIGHT", "DIAG", "NEW_DIAG", "AUTRE1",
            "EVAL__ALS_FRS_R", "DIAGPROBA", "RETRACT", "AUTO_WC", "ACT_PHYS_ACTU", "CONFIRM", "CAUSDCD_SLA", "CONFCERTDCD", "CHUTE", "MARCHE_AIDE",
            "NEWDIAGPROBA", "CALORIMETRIE", "IMPEDENCEMET", "AB_BIPHOTONIQ", "ACHILEEN_D", "ACHILEEN_G" , "MEMBRE_SUP", "MEMBRE_INF", "MEMBRE_INF_D", "MEMBRE_INF_G",
            "MEMBRE_SUP_D", "MEMBRE_SUP_G", "RILUZ", "POSORILU", "POSOVIT_E", "VIT_E", "TREPIDATION_PIED_CHOICE_1", "TREPIDATION_PIED_CHOICE_2", "TRICIPITAL_D", "TRICIPITAL_G",
            "TRONC","MASSETER", "APATH_PAT", "ATT1PAT_GAST", "ATT1PAT_TRAC", "BICIPITAL_D", "BICIPITAL_G", "LANGUE", "APATHIE", "ATROPHIE", "ATT1PAT_VNI",
            "CAUSE_DCD", "DCD", "LIMIT_ART", "POSOAUTR1", "PRISE_ALIMENT", "R_CUT_PLANTAIRE_D", "R_CUT_PLANTAIRE_G",   "R_MASSETERIN",        "R_NASO_PALP",
            "ROTULIEN_D", "ROTULIEN_G", "S_HOFFMANN_CHOICE_1", "S_HOFFMANN_CHOICE_2", "STYLO_RADIAL_D",  "STYLO_RADIAL_G",  "TEST_MUSCUL",    "TTT_OXY",
            "TTT_TRACH", "TTT_VNI", "TTTETIOL", "BREF", "DUREE_ENREG_MIN_PP", "DUREE_ENREG_H_PP", "IDX_3_PP", "IDX_4_PP", "IDX_TOT_PP"  )#"IDX_TOT_PP" renseignée une seule fois pour un patient
quanti_neuro <- c("BMI", "DELTA_WEIGHT_DEBVNI", "DELTA_WEIGHT_REF", "WEIGHT_NUTRI", "WEIGHT", "PER_MARCHE", "ALS", "E_BULBAIRE", "CVF_OBS_THEO")
quanti_pneumo <- c("DUREE_SOMM_VENT_SV", "SATISF_VENTIL_SV", "UTIL_VENTIL_DIURN_SV", "UTIL_VENTIL_NOCT_SV", "DEP_TOUX_PP", "DESATUR_3_PP",
                   "DESATUR_4_PP", "DESATUR_5_PP", "SPO2_TPS_PP", "EPWORTH_PP", "HCO3_PP", "NB_5_PP", "NYCTURIE_PP",
                   "PACO2_PP", "PAO2_PP", "PH_PP", "SAO2_PP", "SPO2_AVT_PP", "SPO2_APR_PP", "SPO2_EVEIL_PP", "SPO2_MOYEN_PP")
bin_pneumo <- c("ASYNCHR_SV", "EVT_OBSTR_SV", "FUITE_VNI_SV", "MODIF_PARAM_SV", "ORTHOPN_SV", "OXYM_VNI_SV", "REVEIL_VENT_SV",
                "CEPHAL_PP", "ENCOMBR_PP", "MORPHO_PP_CHOICE_1", "MORPHO_PP_CHOICE_2", "QUALIT_SOM_PP", "SOMNOLENCE_PP")
quali_pneumo <- c("DYSPN_SOUSVENT_SV", "DYSPN_SVENT_SV", "EVOL_SOMM_VNI_SV", "QUALIT_SOMM_VENT_SV")

sort(unique(s$qui)[! unique(s$qui) %in% c(nokeep, quanti_neuro, quanti_pneumo, bin_pneumo,quali_pneumo) ])

#=================================================================
#2/ génération du tableau dépendant du temps pour une variable donnée

# pdf("writing/RP_quanti_neuro.pdf")
# par(mfrow=c(1,1))
# for (var in quanti_neuro) {#ce sera le debut de la boucle
  
vec <- "quanti_neuro"
vec <- "quanti_pneumo"

pdf(paste0("writing/RP_", vec, ".pdf"))
par(mfrow=c(1,1))
vec <- get(vec)
  
  
for (var in vec) {#ce sera le debut de la boucle

  print(var)
  #var="BREF"
  s <- s_i[s_i$qui==var, ]
  s$time <- ifelse(s$del<0, 0, s$del)
  names(s)
  
  idu<-sort(unique(s$PATIENT))
  
  s[, "x"]<-as.numeric(as.character(s[, "x"]))
  # s0<-s[1, c("PATIENT", "time.vni", "evt", "time", "x")]
  # s0$id<-s0$PATIENT
  # s0$del<-s0$time.vni
  # s0$etat<-s0$evt
  # s0$start<-0
  # s0$stop<-0
  # s0$n<-0
  # s0<-s0[-1,]
  
  
  #La boucle suivante permet de réaliser ce que ferait automatiquement un survsplit
  #cependant avec un survsplit, chaque patient serait découper en le nombre de temps total de changement de valeur
  #alors qu'ici chaque patient est découpé en le nombre de changement de valeurs pour lui même uniquement 
  
  for (id in idu) {
    #cat(id, "\n")
    s1 <- s[s$PATIENT==id, c("PATIENT", "time.vni", "evt", "time", "x")]
    s1 <- s1[!is.na(s1$time),]
    s1 <- s1[order(s1$time),]
    ns1<-dim(s1)[1] #nombre de changement de valeurs pour un patient
    ns1
    
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
      
      # s0<-s[1, c("PATIENT", "time.vni", "evt", "time", "x")]
      # s0$id<-s0$PATIENT
      # s0$del<-s0$time.vni
      # s0$etat<-s0$evt
      # s0$start<-0
      # s0$stop<-0
      # s0$n<-0
      # s0<-s0[-1,]
      
      dt<-s0
    }
    
    #Je merge les tableaux réalisés pour chaque patient
    if (id==idu[1]) {
      Dt<-dt
    } else {
      Dt<-rbind(Dt, dt)
    }
  }
  
  #Je remet dans l'ordre : par patient et par temps de start
  Dt<-Dt[order(Dt$id, Dt$start),]
  head(Dt[Dt$n==5,]) #montre les lignes des patients qui ont au moins 5 intervalles (5 changements de valeur pour la variable var)
  Dt[is.na(Dt$evt),]
  ttab<-table(tab<-table(Dt$id));ttab
  sum(ttab)
  table(Dt$n)
  
  s[s$PATIENT=="QUERE_PATRICK",]
  Dt[Dt$id=="QUERE_PATRICK",]
  
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
  # Dt[Dt$id=="COSSENET_CHANTAL",]
  # s[s$PATIENT=="ABGRALL_CLAUDE",]
  # bd[bd$time.vni==0,]
  # s[s$time.vni==0,]
  
  #======================================
  #3/verif des hypothèses pour une variable donnée
  #NB : il faudra faire une grande boucle qui va jusqu'à la vérif des risques proportionnels => creation de HRok (boucle sur le nom des variables)
  #puis continuer la boucle jusqu'à test du score et HRIC (rbind de res)
  
  
  #------------
  #si quanti, verif de la loglin
  
  #3.1/ Loglin
  cox<-coxph(Surv(start, stop, etat)~poly(x, df=2, raw=T) + cluster(id), data=Dt) 
  #cox<-coxph(Surv(start, stop, etat)~poly(x, df=2, raw=T) , data=Dt) 
  tabcox <- summary(cox)
  pval_loglin <- round(tabcox$coefficients[2,"Pr(>|z|)"],3)
  if (is.na(pval_loglin)) .recode <- FALSE
  else{
    if (pval_loglin <= 0.05) {
      Dt$x <- ifelse(Dt$x < median(Dt$x), 0, 1)
      .recode <- TRUE
    }
    if (pval_loglin > 0.05) .recode <- FALSE
  }
    
  #--------------
  #3.2/ vérif hypoth des risques proportionnels
  cox<-coxph(Surv(start, stop, etat)~x+cluster(id), data=Dt) #deja recode ou non en 0/1 selon étape précédente
  if (var %in% c("SPO2_AVT_PP", "SPO2_APR_PP")){
    z <- cox.zph(cox, transform = "rank")
    .rp <- round(z$table[,3],3)
    plot (0,0, main=paste0("RP of ", var, "\nHarrell test p = ", pval))
  } else {
    .rp <- check_RP_dt(cox, var=var)
  }
  
  .res <- data.frame(variable = var, Loglin = pval_loglin, recode = .recode, RP = .rp)
  if (which(vec==var) == 1) res_tot <- .res else res_tot <- rbind(res_tot, .res)
  print(var)
}

write.table(print(res_tot), file="clipboard", sep="\t")
dev.off()



#faire un tableau avec les coef pour chaque variable et sortir un schéma pour chaque variable
#puis générer un vecteur HR : si le risque prop est vérifié, HR=1 sinon HR=0
#Faire un tableau avec : HRok <- var, HR0/1
RPok <- data.frame(var=unique(bdd_rep$qui), RP=1)#HR est à modifier en fonction des résultats aux risques proportionnels
RP <- RPok[RPok$var==var, "RP"]

RP <- 0 #(1 : risque prop ok)
dep_temps <- ifelse(RP == 0, TRUE, FALSE)

if(dep_temps){
  #RP non vérifiés, ajout variable dépendante du temps
  dt<-Dt
  dt$evt<-dt$etat
  dt<-survSplit(dt, end="stop", start="start", cut=t, event="evt")
  dt<-dt[order(dt$id, dt$start),]

  
  #rajout et verif
  tmp <- lapply(c("log","sqrt","*t","/t","*t^2"),function(x){
    # tmp <- lapply(c("*t^0.7"),function(x){
    add_vart_and_check_dt(data_split=dt, var=var, .transf=x)[[1]]
  })
  cat(do.call(rbind, tmp))
  
  #liste des transformations ok:
  tmp <- lapply(c("log","sqrt","*t","/t","*t^2"),function(x){
    # tmp <- lapply(c("*t^0.7"),function(x){
    add_vart_and_check_dt(data_split=dt, var=var, .transf=x)[[2]]
  })
  tmp <- do.call(rbind, tmp)
  tmp <- tmp[!is.na(tmp)]
  

  if(length(tmp)!=0){
    transf <- tmp[1] #si plusieurs correspondent je prends la première par défaut
    corr <- "yes"
  } else {
    corr <- "no"
    transf <- NA
    cat("no transf found")
  }
}

#======================================
#4/interpretation du modele dependant du temps pour une variable donnée:

if(corr=="yes"){
  if (transf=="log") dt$xt<-dt$x*log(dt$stop)
  if (transf=="sqrt")dt$xt<-dt$x*sqrt(dt$stop)
  if (transf=="*t")dt$xt<-dt$x*(dt$stop)
  if (transf=="/t")dt$xt<-dt$x/(dt$stop)
  if (transf=="*t^2") dt$xt <-dt$x*(dt$stop^2)
  if (transf=="*t^0.7") dt$xt <-dt$x*(dt$stop^0.7)
  if (transf=="log10") dt$xt <-dt$x*log10(dt$stop)
  if (transf=="*t^0.3") dt$xt <-dt$x*(dt$stop^0.3)
  if (transf=="*t^3") dt$xt <-dt$x*(dt$stop^3)
  mod <-coxph(Surv(start, stop, evt)~x+xt+cluster(id), data=dt)
  
  S <- vcov(mod)
  b <- coef(mod)
  t <- 12 #12mois
  if (transf=="*t^0.7") t_t <- t^0.7
  if (transf=="log") t_t <- log(t)
  if (transf=="sqrt") t_t <- sqrt(t)
  if (transf=="*t^2") t_t <- t^2 
  if (transf=="*t") t_t <- t
  if (transf=="*t^3") t_t <- t^3
  
  variance <- S[1,1]+S[2,2]*(t_t)^2+2*S[1,2]*(t_t)
  m <- b[1]+b[2]*(t_t) #coef de l'HR
  
  HRIC <- c(round(exp(m),3), round(exp(m + qnorm(0.975)*sqrt(variance) * c(-1,1)),3))
  
  a <- summary(mod)
  pval <- a$robscore[3]
  coefx <- a$coefficients[1,1]
  coefxt <- a$coefficients[2,1]
  HRIC95 <- paste0(HRIC[1], "[", HRIC[2], "-", HRIC[3], "]")
  res <- data.frame(HR, transf, HRIC95, coefx, coefxt, pval)
  
  
} else {
  coxt<-coxph(Surv(start, stop, evt)~x+cluster(id), data=dt) 
  a <- summary(coxt)
  pval <- a$robscore[3]
  coefx <- a$coefficients[1]
  coefxt <- NA
  HRIC <- round(data.frame(IC=a$conf.int[1], l=a$conf.int[3], u=a$conf.int[4]),3)
  HRIC95 <- paste0(HRIC[1], "[", HRIC[2], "-", HRIC[3], "]")
  res <- data.frame(HR, transf, HRIC95, coefx, coefxt, pval)
}

#------------------------
#-------------------------

#----
#Yann

cox<-coxph(Surv(start, stop, etat)~x+cluster(id), data=Dt)
summary(cox)

t<-sort(unique(Dt$stop[Dt$etat==1]));t #ce sont les temps de décès


#je splite en fonction des temps de décès
dt<-Dt
dt$evt<-dt$etat
dt<-survSplit(dt, end="stop", start="start", cut=t, event="evt")
dt<-dt[order(dt$id, dt$start),]
head(dt)
dt[dt$id=="BEAUFRERE_BERNARD",]
dt$xt<-log(dt$stop)*dt$x

coxt<-coxph(Surv(start, stop, evt)~x+cluster(id), data=dt) # si on ne rajoute pas xt, c'est idem que cox <- coxph(Surv(start, stop, etat)~x+cluster(id), data=Dt)
summary(coxt)
coxtt<-coxph(Surv(start, stop, evt)~x+xt+cluster(id), data=dt)
summary(coxtt)

cox<-coxtt
z<-cox.zph(cox);z
iz<-1
plot(z[iz])
abline(h=coef(cox)[iz], col="blue")
abline(h=0, col="grey50", lwd=2, lty=2)


#------------------------
#-------------------------










s<-bd
s$time.vni<-s$time.vni+0.1
n<-dim(s)[1]
id<-unique(as.character(s$PATIENT))

#--
s1 <- s[s$PATIENT==id, c("PATIENT", "time.vni", "censor", "date", var)]
s1 <- s1[!is.na(s1$time),]
s1 <- s1[order(s1$time),]
ns1<-dim(s1)[1]
ns1

if (ns1>1) {
  if (is.na(s1[1, var])) s1[1, var] <-s1[2, var]
  for (x in 2:nrow(s1)){
    if (is.na(s1[x, var])) s1[x, var] <-s1[(x-1), var]
  }
} 
d<-s1
#--

for (i in id) {
  s1 <- s[s$PATIENT==id, c("PATIENT", "time.vni", "censor", "time", var)]
  # i-1
  k<-rbinom(1, 10, 0.5);k
  ti<-sort(sample(1:96, k, replace=F));ti[1]<-0
  xi<-round(rnorm(k, 0, 2), 2)
  cbind(ti, xi)
  
  di<-s[s$PATIENT ==id[i], c("PATIENT", "time.vni", "censor")]
  di$deb<-0
  di$fin<-di$time.vni
  
  xi<-xi[ti<=di$time.vni]
  ti<-ti[ti<=di$time.vni]
  cbind(ti, xi)
  if (who=="Yann") dit<-survSplit(di, cut=ti, end="fin", start="deb", event="censor")#Yann
  if (who=="Sarah") dit <- survSplit(di, cut=ti, end="fin", start="start", event="censor")#Sarah
  dit$xi<-xi
  
  if (i==1) {
    Dit<-dit
  } else {
    Dit<-rbind(Dit, dit)
  }
}

d<-Dit
ti<-sort(unique(c(0, d$fin[d$censor==1])))
d$start<-d$deb
d$stop<-d$fin
d$evt<-d$censor
dt<-survSplit(d, cut=ti, end="stop", start="start", event="evt")
dt<-dt[order(dt$PATIENT, dt$start),]
dt[dt$PATIENT==id[1],]



cox<-coxph(Surv(start, stop, evt)~xi+cluster(PATIENT), data=dt)  
summary(cox)
zi<-cox.zph(cox)

#---------------------
#version Yan originale
s<-bd
s$time.vni<-s$time.vni+0.1
n<-dim(s)[1]
id<-as.character(s$PATIENT)

for (i in 1:n) {
  # i-1
  k<-rbinom(1, 10, 0.5);k
  ti<-sort(sample(1:96, k, replace=F));ti[1]<-0
  xi<-round(rnorm(k, 0, 2), 2)
  cbind(ti, xi)
  
  di<-s[s$PATIENT ==id[i], c("PATIENT", "time.vni", "censor")]
  di$deb<-0
  di$fin<-di$time.vni
  
  xi<-xi[ti<=di$time.vni]
  ti<-ti[ti<=di$time.vni]
  cbind(ti, xi)
  if (who=="Yann") dit<-survSplit(di, cut=ti, end="fin", start="deb", event="censor")#Yann
  if (who=="Sarah") dit <- survSplit(di, cut=ti, end="fin", start="start", event="censor")#Sarah
  dit$xi<-xi
  
  if (i==1) {
    Dit<-dit
  } else {
    Dit<-rbind(Dit, dit)
  }
}

d<-Dit
ti<-sort(unique(c(0, d$fin[d$censor==1])))
d$start<-d$deb
d$stop<-d$fin
d$evt<-d$censor
dt<-survSplit(d, cut=ti, end="stop", start="start", event="evt")
dt<-dt[order(dt$PATIENT, dt$start),]
dt[dt$PATIENT==id[1],]



cox<-coxph(Surv(start, stop, evt)~xi+cluster(PATIENT), data=dt)  
summary(cox)
zi<-cox.zph(cox)
