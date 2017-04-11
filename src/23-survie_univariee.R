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
.l1 <- lapply(binaire, function(x)draw_surv_bin(x, data = d, .time = "time.vni", .evt = "evt", vec_time_IC= c(1, 3), type = "quali", surv_only=FALSE, pvalue = TRUE, dep_temps=FALSE, .transf=NULL))
ml <- marrangeGrob(.l1,ncol=1,nrow=1,top = NULL)
ggsave(file="binaire_bl.pdf", ml)

#----
#survie à 1 et 3 ans
.l2 <- lapply(binaire, function(x)draw_surv_bin(x, data = d, .time = "time.vni", .evt = "evt", vec_time_IC= c(1, 3), type = "quali", surv_only=TRUE, pvalue = TRUE, dep_temps=FALSE, .transf=NULL))
df_surv <- do.call(rbind, .l2)
saveRDS(df_surv, "data/analyses/df_surv_bl.rds")
df_surv

#----
#hypothèse des risques proportionnels
pdf(paste0("data/analyses/RP_bin.pdf"))
.l <- lapply(binaire, function(x)check_RP(var=x, data=d, .time="time.vni", .evt="evt", type="quali", recode = TRUE))
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
   tmp <- lapply(c("log","sqrt","*t","/t","*t^2","*t^0.7", "log10", "*t^0.3", "*t^3"), function(x)add_vart_and_check(data=d, .time="time.vni", .evt="evt", type="quali", var=i, .transf=x))
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
  add_vart_and_check(data=d, .time="time.vni", .evt="evt", type="quali", var=i, vec_cut = b)
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
HR_score (var=allbin$variable[num], data = d, .time="time.vni", .evt="evt", type="quali", recode=TRUE, .transf=allbin$transf[num], .tps_clinique=12)
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
.l <- do.call(rbind, .l)
dev.off()

#----
#hypothèse des risques proportionnels
pdf(paste0("data/analyses/RP_bin.pdf"))
.l <- lapply(binaire, function(x)check_RP(var=x, data=d, .time="time.vni", .evt="evt", type="quali", recode = TRUE))
dev.off()

#NB j'ai rempli dans un tableau excel au regard des courbes si l'hypothèse des risques proportionnels était respecté(TRUE) ou non (FALSE)
write.table(print(binaire), file="clipboard", sep="\t")
rp <- read.csv2("data/RP_bin.csv")

#separation var selon respect RP ou non
binRP <-  as.character(rp[rp$RP, "variable"]) #RP ok, pas de var dépendante du temps
binNRP <- as.character(rp[!rp$RP, "variable"]) #RP pas ok, il faut rajouter var dépendante du temps






#----------------------

tmp <- binNRP [!binNRP %in% binRPt$variable]
b <- c(6,18,50)
pdf(paste0("data/analyses/RP_bin_decoup_", paste(b, collapse='-'), ".pdf"))
.l <- lapply(tmp, function(i){
  add_vart_and_check(data=d, .time="time.vni", .evt="evt", type="quali", var=i, vec_cut = c(6,18,50))
})
df_bint <- do.call(rbind, .l)
dev.off()


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
