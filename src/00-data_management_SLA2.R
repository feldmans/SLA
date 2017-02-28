source("src/fonctions_SLA.R")
source("src/objects_SLA.R")


#-----------------------------
#-----------------------------
#Yann recup VNI 

listes_brut <- lapply(bdds, which_col,string1="DATE", string2="VNI",string3="STOP",type="merge")
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  #browser()
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}
bdd_debVNI <- get_min_max(data = res, fun = "min")
bdd_debVNI$DATEVNI <- manage_date_ND(bdd_debVNI$min)

bdd_debVNI <- unique(bdd_debVNI[,c("PATIENT","DATEVNI")])#seul les vrais doublons nom et date sont éliminés
#bdd_debVNI <- na.omit(bdd_debVNI)


#recup de la date de vni a partir des MEO etc...independamment du suivi

v<-c("VNI_ON_PP", "DAT_DECIS_VENT_PP", "DELAI_VENT_PP", "DAT_MISOVR_VENT_PP", "ECHEC_MEO_VENT_PP")
w<-gsub("PP", "PV", v)
v<-c(v, "DATE_RESP_PP", w, "DATE_PREVENT_PP")
r<-c("", paste("_F", 1:100, sep=""))
vr<-expand.grid(list(v=v, r=r))
vr$f<-as.numeric(gsub("_F", "", vr$r, fix=T))
vr$f[is.na(vr$f)]<-0
vr$vr<-paste(vr$v, vr$r, sep="")
vr<-vr[vr$vr%in%names(bdd7),]
dim(vr)
w<-c("vni", "datedec", "delai", "datemeo", "echec", "dateex")

vrs<-split(vr, vr$f)
vrs$"1"
#sapply(vrs, dim)

for (iv in 1:length(vrs)) {
  # iv<-1  
  y<-bdd7[, c("PATIENT", vrs[[iv]]$vr)]
  names(y)[-1]<-w
  y$f<-vrs[[iv]]$f[1]
  if (iv==1) {
    Y<-y
  } else {
    Y<-rbind(Y, y)
  }
}

Y<-Y[order(Y$PATIENT, Y$f),]
head(Y)


dim(Y)
Y<-Y[!is.na(Y$vni),]
Y<-Y[Y$vni==1,]
dim(Y)
Y$datedec<-as.Date(as.character(Y$datedec), "%d/%m/%Y")
Y$datemeo<-as.Date(as.character(Y$datemeo), "%d/%m/%Y")
Y$dateex<-as.Date(as.character(Y$dateex), "%d/%m/%Y")
summary(Y)

Y$echec[is.na(Y$echec)]<-0

head(Y[Y$echec==1,])

Y$del<-ifelse(Y$delai==1 & !is.na(Y$delai), 0, 0)
Y$del<-ifelse(Y$delai==2 & !is.na(Y$delai), 7, Y$del)
Y$del<-ifelse(Y$delai==3 & !is.na(Y$delai), 30, Y$del)
Y$date<-Y$datemeo
Y$date<-ifelse(is.na(Y$date), Y$datedec+Y$del, Y$date)
Y$date<-ifelse(is.na(Y$date), Y$dateex+Y$del, Y$date)
class(Y$date)<-"Date"

y<-bdd_debVNI
head(y)
Y<-merge(y, Y, by="PATIENT", all=T)
dim(Y)
length(unique((Y$PATIENT)))

length((Y$PATIENT))


vni<-Y
head(vni)

#---------
#pour garder ceux qui ont un suivi et qu'on garde dans la base (repechage)

v<-paste(c("DATE_SOUSVENT", "PACO2", "PAO2", "HCO3", "PH", "ORTHOPN", "CEPHAL"), "_SV", sep="") #rajouter des vairables pour repêcher d'autres personnes
r<-c("", paste("_F", 1:100, sep=""))
vr<-expand.grid(list(v=v, r=r))
vr$f<-as.numeric(gsub("_F", "", vr$r, fix=T))
vr$f[is.na(vr$f)]<-0
vr$vr<-paste(vr$v, vr$r, sep="")
vr<-vr[vr$vr%in%names(bdd7),]
dim(vr)
w<-c("datesv", "paco2", "pao2", "hco3","ph", "ortho", "cephal") #rajouter des vairables pour repêcher d'autres personnes

vrs<-split(vr, vr$f)
vrs$"2"

for (iv in 1:length(vrs)) {
  y<-bdd7[, c("PATIENT", vrs[[iv]]$vr)]
  names(y)[-1]<-w
  y$f<-vrs[[iv]]$f[1]
  if (iv==1) {
    Ysv<-y
  } else {
    Ysv<-rbind(Ysv, y)
  }
}
Ysv<-Ysv[order(Ysv$PATIENT, Ysv$f),]
Ysv$datesv<-as.Date(as.character(Ysv$datesv), "%d/%m/%Y")
Ysv$paco2<-as.numeric(as.character(Ysv$paco2))
Ysv$pao2<-as.numeric(as.character(Ysv$pao2))
Ysv$hco3<-as.numeric(as.character(Ysv$hco3))
Ysv$ph<-as.numeric(as.character(Ysv$ph))
Ysv$ortho<-as.numeric(as.character(Ysv$ortho))
Ysv$cephal<-as.numeric(as.character(Ysv$cephal))
x<-as.list(Ysv[, c("paco2", "pao2", "hco3","ph", "ortho", "cephal")]) #rajouter des vairables pour repêcher d'autres personnes
x<-sapply(x, as.character)
x[!is.na(x)]<-1
x[is.na(x)]<-0
x<-array(as.numeric(x), dim=dim(x))
sx<-rowSums(x)
Ysv$s<-pmin(1, sx)

table(Ysv$s)
summary(Ysv)

Ysv[Ysv$s==0 & !is.na(Ysv$datesv),]

Ysv<-Ysv[Ysv$s==1,]
dim(Ysv)

d<-tapply(Ysv$datesv, Ysv$PATIENT, min)
d<-data.frame(PATIENT=names(d), datesv1=as.numeric(d))
class(d$datesv1)<-"Date"
head(d)


sv<-tapply(Ysv$s, Ysv$PATIENT, sum)
sv<-data.frame(PATIENT=names(sv), sv=as.numeric(sv))
sv<-merge(sv, d, by="PATIENT", all=T)
head(sv)
dim(sv)

z<-merge(vni, sv, by="PATIENT", all=T)

dim(z)
length(unique(z$PATIENT))
summary(z)


s<-BASE_TOT[, c("PATIENT", "diag")]
s <- unique(s) #j'ai des doublons dans cette base alors que 2 fois meme info
tab <- table(s$PATIENT)
s <- s[! s$PATIENT %in% names(tab)[tab>1], ] #j'élimine doublons avec info discordante

i<-match(z$PATIENT, s$PATIENT);summary(i)
z[is.na(i), ]
z<-z[!is.na(i),]

i<-match(z$PATIENT, s$PATIENT);summary(i)
z$diag<-s$diag[i]
table(z$diag, exclude=NULL)

z$diag[is.na(z$diag)]<-0

z$sv1<-pmin(z$sv, 1);z$sv1[is.na(z$sv1)]<-0
table(z$diag, z$sv1)

z$vni2<-ifelse(!is.na(z$DATEVNI), 1, 0)
z$vni2<-z$vni2+2*ifelse(!is.na(z$date), 1, 0)
z$vni2<-z$vni2+4*z$sv1
addmargins(table(z$diag, z$vni2))


z$vni<-ifelse(!is.na(z$DATEVNI) | !is.na(z$date), 1, 0)
z$sla<-ifelse(z$diag==1, 1, 0)
addmargins(table(vni=z$vni, suivi=z$sv1, z$sla))

z$datevni<-ifelse(!is.na(z$date), z$date, z$DATEVNI) #datevni rassemble la date obtenue par algorythme et par datamining (si algo absent, alors on prend datamining)
summary(z)
class(z$datevni)<-"Date"


head(z)

#---------------------------------------------------------------------------------
#pour ne garder que les lignes vni==1 (qui sont tous des patients SLA)

#vnisla<-z[z$sla==1 & z$vni==1,]
vnisla<-z[z$vni==1,]
head(vnisla)
vnisla$n<-NULL
vnisla$nechec<-NULL
vnisla$nvni<-NULL

#Pour ne garder qu'une seule date de vni (si info concernant la vni a été renseignée plusieurs mois)

#date min de vni pour chaque patient:
#d<-tapply(vnisla$datevni, vnisla$PATIENT, min) # ATTENTION si la min est un echec, ça prend cette date quand meme!
d<-tapply(vnisla$datevni[vnisla$echec==0 | is.na(vnisla$echec)], vnisla$PATIENT[vnisla$echec==0 | is.na(vnisla$echec)], min) #ne prend pas en compte les echecs
d<-data.frame(PATIENT=names(d), datevni1=as.numeric(d))
vnisla<-merge(vnisla, d, by="PATIENT", all=T)
class(vnisla$datevni1)<-"Date"
head(vnisla)
#nb de ligne permettant de definir une date de vni par patient
x<-tapply(vnisla$vni, vnisla$PATIENT, sum) 
x<-data.frame(PATIENT=names(x), nvni=as.numeric(x))
vnisla<-merge(vnisla, x, by="PATIENT", all=T)
#nb d'échec de vni
vnisla$echec[is.na(vnisla$echec)]<-0 
x<-tapply(vnisla$echec, vnisla$PATIENT, sum)
x<-data.frame(PATIENT=names(x), nechec=as.numeric(x))
vnisla<-merge(vnisla, x, by="PATIENT", all=T)

# #nb de lignes avec une info sur la vni 
# n<-table(vnisla$PATIENT) 
# n<-data.frame(PATIENT=names(n), n=as.numeric(n))
# vnisla<-merge(vnisla, n, by="PATIENT", all=T)
# addmargins(table(vnisla$n))

#délai entre VNI et suivi? A recreer?


#creation d'une clé avec nb de lignes avec info sur la vni(succès et échec confondu), nb d'echec, suivi pneumo existant ou non.

vnisla$cle<-paste(vnisla$nvni, vnisla$nechec, vnisla$sv1, sep="|") #donne le nombre de lignes étant dans ce cas (attention doublons)
#pour explorer la base:
cbind(table(vnisla$cle))
vnisla[vnisla$cle %in% c("2|0|1") ,]

# 1|0|0 et 1|0|1 : 1 tentative, pas déchec : on garde la ligne (qui est unique pour ces patients)
#cas 1|1|0 : 1 info de vni, 1 échec, pas de suivi (aucune variable sous_vent) : on supprime la ligne
vnisla<-vnisla[vnisla$cle!="1|1|0",]
# cas 1|1|1 : on garde toutes les lignes
#cas 2|0|0 on prend datevni correspondant à la date min (n'ont pas toutes date de meo)
vnisla[vnisla$cle %in% c("2|0|0") ,]
vnisla<-vnisla[!(vnisla$cle=="2|0|0" & vnisla$datevni!=vnisla$datevni1 ),]
vnisla<-vnisla[!(vnisla$cle=="2|0|0" & vnisla$f<1),]
# cas 2|0|1: on garde la ligne qui a une date de meo ou une date de decision enfin on prend datevni correspondant à la date min
vnisla[vnisla$cle %in% c("2|0|1") ,]
vnisla<-vnisla[!(vnisla$cle=="2|0|1" & vnisla$datevni!=vnisla$datevni1 ), ]
vnisla<-vnisla[!(vnisla$cle=="2|0|1" & duplicated(vnisla$PATIENT)), ]
#cas "2|1|0" et "2|1|1" : 2 tentatives de vni, 1 est un échec=> on supprime l'échec
vnisla[vnisla$cle %in% c("2|1|0") ,]
vnisla<-vnisla[!(vnisla$cle=="2|1|0" & vnisla$echec==1),]
vnisla<-vnisla[!(vnisla$cle=="2|1|1" & vnisla$echec==1),]
#cas 2|2|0 : 2vni 2 echec pas de suivi : je suprrime
vnisla<-vnisla[vnisla$cle!="2|2|0",]
#cas "3|1|1" : 3 tentative 1 échec, suivi => on ne garde pas l'échec et on prend la plus petite des dates
vnisla[vnisla$cle %in% c("3|1|1") ,]
vnisla<-vnisla[!(vnisla$cle=="3|1|1" & vnisla$echec==1),]
vnisla<-vnisla[!(vnisla$cle=="3|1|1" & vnisla$datevni!=vnisla$datevni1),]
# cas 4|0|0 et 4|0|1 : je prend plus petite date de vni et j'enleve le doublon lié à la double date VNI
vnisla[vnisla$cle %in% c("4|0|1") ,]
vnisla<-vnisla[!(vnisla$cle=="4|0|0" & vnisla$datevni!=vnisla$datevni1),]
vnisla<-vnisla[!(vnisla$cle=="4|0|0" & vnisla$DATEVNI!=vnisla$datevni1),] #pour enlever le doublon lié à la double date VNI
vnisla<-vnisla[!(vnisla$cle=="4|0|1" & vnisla$datevni!=vnisla$datevni1),]
vnisla<-vnisla[!(vnisla$cle=="4|0|1" & vnisla$DATEVNI!=vnisla$datevni1),] #pour enlever le doublon lié à la double date VNI
#cas 4|2|0 : j'enlève l'échec et je supprime le doublon lié à fausse DATEVNI
vnisla[vnisla$cle %in% c("4|2|0") ,]
vnisla<-vnisla[!(vnisla$cle=="4|2|0" & vnisla$echec==1),]
vnisla<-vnisla[!(vnisla$cle=="4|2|0" & vnisla$DATEVNI!=vnisla$datevni1),] #pour enlever le doublon lié à la double date VNI
#cas 4|2|1 : j'enleve le doublon lié à la double date VNI et j'enleve l'echec
vnisla[vnisla$cle %in% c("4|2|1") ,]
vnisla<-vnisla[!(vnisla$cle=="4|2|1" & vnisla$echec==1), ]
vnisla<-vnisla[!(vnisla$cle=="4|2|1" & vnisla$DATEVNI!=vnisla$datevni1), ]
#cas 4|4|0 : que des échec sans suivi, je supprime
vnisla<-vnisla[!(vnisla$cle=="4|4|0"),]
# cas 6|2|1 : j'eneleve les echecs, j'enleve les doublons DATEVNI, je garde la plus petite des dates
vnisla[vnisla$cle %in% c("6|2|1") ,]
vnisla<-vnisla[!(vnisla$cle=="6|2|1" & vnisla$echec==1),]
vnisla<-vnisla[!(vnisla$cle=="6|2|1" & vnisla$DATEVNI!=vnisla$datevni1),]
vnisla<-vnisla[!(vnisla$cle=="6|2|1" & vnisla$datevni!=vnisla$datevni1),]


#plus de doublon datevni!
table(tab <- table(vnisla$PATIENT))

vnisla[vnisla$PATIENT%in%names(tab)[tab>1], ]
#792 patients avec une datevni (qui sont donc SLA quoi qu'en dise la base neuro)

saveRDS(vnisla, "data/vnisla.rds")

names_vnisla <- names(table(unique(vnisla$PATIENT)))


#-----------------------------
#-----------------------------
#CREATION DES TABLES:

#----
#VARIABLES PNEUMO


#selectionner tous les couples de variables pneumo existant
#attention certaines variables ne sont pas selectionnees a cause des CL1, voir var neuro p-e (rajoutees à la main dans tableau csv en attendant)
# [1] "PIMAX_PV_F1_CL1"      "PEMAX_PV_F1_CL1"      "SPO2_EVEIL_PV_F1_CL1" "PAO2_SV_F1_CL1"      
# [5] "SAO2_SV_F1_CL1"       "PACO2_SV_F1_CL1"      "HCO3_SV_F1_CL1"       "PH_SV_F1_CL1" 
#MODIF_PARAM_ITEMS_SV_CHOI_CL1à 100 et qq

v <- names(bdd7)          
v1 <- v[grep("_PP", v)] #existe car vient de names(bdd7)
v1 <- v1[- grep("DATE_PREVENT", v1)]
v1 <- v1[- grep("DATE_RESP", v1)]
v1 <- v1[str_sub(v1, -2, -1)=="PP"] 
v1 <- str_sub(v1, 1, -4)

vbis <- v[grep("_PV_F1", v)] #existe car vient de names(bdd7)
vbis <- vbis[str_sub(vbis, -3, -3)=="_"]
vbis <- str_sub(vbis, 1, -7)

vter <- v[grep("_SV_F1", v)] #existe car vient de names(bdd7)
vter <- vter[str_sub(vter, -3, -3)=="_"]
vter <- str_sub(vter, 1, -7)

v_unique <- unique(unlist(list(l1=v1, l2=vbis, l3=vter)))
#v <- data.frame(v = unique(c(v1, vbis, vter)))
v <- data.frame(v = v_unique)
v$v1 <- paste(v$v, "PP", sep="_")
v$exist_v <- v$v1%in%names(bdd7)
v$v1 <- ifelse (v$exist_v==FALSE, NA, v$v1)
v$v2 <- paste(v$v, "PV", "F1", sep="_")
v$exist_v <- v$v2%in%names(bdd7)
v$v2 <- ifelse (v$exist_v==FALSE, NA, v$v2)
v$v3 <- paste(v$v, "SV", "F1", sep="_")
v$exist_v <- v$v3%in%names(bdd7)
v$v3 <- ifelse (v$exist_v==FALSE, NA, v$v3)
v$exist_v <- NULL
#en resume :
#les variables existant uniquement en v1 sont les variables à baseline independantes de la VNI
#les variables existant en v1 et v2 sont les variables à baseline dependant de la date de vni
#les variables existant en v3 +/- v2 et v1 sont les variables repetees

#la sat en O2 est meilleure avec la var F1_CL1, c'est donc surement celle là qui est sous ventilation, l'autre est en air ambiant.
table(bdd7$SAO2_SV_F1>bdd7$SAO2_SV_F1_CL1)
table(bdd7$SAO2_SV_F2>bdd7$SAO2_SV_F2_CL1)
table(bdd7$PIMAX_PP_CL1)#bcp moins renseingée que PIMAX_PP, j'elimine
#je ne garde donc que les SV_Fx_CL1, pas les PP_CL1 qui sont des copies d'info

saveRDS (v, "data/variables_pneumobis.rds")#non utilisé

write.table(print(v), file="clipboard", sep="\t", row.names=F )
#---
#var pneumo repetees avec les 3 types de variables renseignées (PP PV SV)

#fonction
vnisla <- readRDS("data/vnisla.rds")
#get_var_bl_suivi_pneumo <- function(varPP, varPV, varSV){ #mapply ne marche pas
get_var_bl_suivi_pneumo <- function(vec){
  vs<-c("DATE_RESP_PP", "DATE_PREVENT_PP", "DATE_SOUSVENT_SV")
  #Ws <- c(varPP, varPV, varSV)
  Ws <- vec
  extvs<-c("PP", "PP", "SV")
  extws<-c("PP", "PV", "SV")
  print(Ws)
  for (ie in 1:length(extvs)) {
    #browser()
    # ie<-ie+1  
    v<-c(vs[ie], Ws[ie])
    #v<-paste(v, c(extvs[ie], extws[ie]), sep="_")
    r<-c("", paste("_F", 1:100, sep=""))
    vr<-expand.grid(list(v=v, r=r))
    vr$f<-as.numeric(gsub("_F", "", vr$r, fix=T))
    vr$f[is.na(vr$f)]<-0
    vr$vr<-paste(vr$v, vr$r, sep="")
    vr<-vr[vr$vr%in%names(bdd7),]
    dim(vr)
    w<-c("date", "x")
    vrs<-split(vr, vr$f)
    names(vrs)
    vrs$"0"
    vrs$"2"
    for (iv in 1:length(vrs)) { 
      #browser()
      y<-bdd7[, c("PATIENT", vrs[[iv]]$vr)]
      names(y)[-1]<-w
      #y$ext<-ext #erreur
      y$ext<-extws[ie]
      #y$qui<-W #erreur
      y$qui<-Ws[1]
      y$f<-vrs[[iv]]$f[1]
      if (iv==1) {
        Yv<-y
      } else {
        Yv<-rbind(Yv, y)
      }
    }
    #browser()
    Yv<-Yv[order(Yv$PATIENT, Yv$f),]
    Yv$date<-as.Date(as.character(Yv$date), "%d/%m/%Y")
    Yv$x<-as.numeric(as.character(Yv$x))
    Yv<-Yv[!is.na(Yv$x) & !is.na(Yv$date),]
    head(Yv)
    if (ie==1) {
      YYv<-Yv
    } else {
      YYv<-rbind(YYv, Yv)
    }
  }
  #y<-merge(YYv, vnisla[, c("PATIENT", "date")], by="PATIENT", all=F, suff=c("", ".vni"))
  y<-merge(YYv, vnisla[, c("PATIENT", "datevni")], by="PATIENT", all=F)
  
  #colonne delai(del) entre la date d'exam et la date de vni
  #y$del<-as.numeric(y$date-y$date.vni)
  y$del<-as.numeric(y$date-y$datevni)
  y<-y[!is.na(y$del),]
  
  y<-y[order(y$PATIENT, y$date),]
  head(y)
  
  #selection des variables de suivi : toutes
  yp<-y[y$del>0,] 
  head(yp)
  summary(yp)
  if(nrow(yp)!=0) yp$f<-1 #1 pour suivi (en opposition à baseline)
  
  #selection valeur de baseline
  yn<-y[y$del<=0,]
  head(yn)
  yn<-yn[order(yn$PATIENT, -yn$del),] #delai le plus petit en première ligne 
  head(yn)
  id<-unique(yn$PATIENT);id
  dim(yn)
  yn<-yn[match(id, yn$PATIENT),] #prend la première ligne de yn qui matche avec id (donc prend ligne correspondant au délai le plus petit pour chaque patient)
  dim(yn)
  head(yn)
  summary(yn)
  if(nrow(yn)!=0) yn$f<-0 #0 pour baseline
  
  #je rassemble les tableaux baseline(yn) et suivi(yp)
  y<-rbind(yp, yn)
  y<-y[order(y$PATIENT, y$del),]
  head(y)
  #browser()
  return(y)
}

#J'utilise le tableau de toutes les var pneumo (après nettoyage) pour obtenir le tableau repete 
vpr <- read.csv2("data/variables_suivi_pneumo_clean.csv")
vpr <- data.frame(lapply(vpr, as.character), stringsAsFactors=FALSE)
vpr <- vpr[!is.na(vpr$variables_PP) & !is.na(vpr$variables_PV) & !is.na(vpr$variables_SV), ]
vpr$variables_PV <- str_sub(vpr$variables_PV, 1, -4)
vpr$variables_SV <- str_sub(vpr$variables_SV, 1, -4)

lvar <- lapply(1:nrow(vpr), function(i) dput(as.character(vpr[i ,c("variables_PP", "variables_PV", "variables_SV")])))

# #je supprime les variables dates (sinon fait bugger la fonction)
# vnr <- rbind(vnr[! vnr$var_bdd6 %in% vnr$var_bdd6[grep("DAT", vnr$var_bdd6)], ], vnr[vnr$var_bdd6=="TREPIDATION_PIED_CHOICE_1" |vnr$var_bdd6=="TREPIDATION_PIED_CHOICE_2", ])

#je génère le tableau répété
#.l <- mapply(get_var_bl_suivi_pneumo, vpr$variables_PP[1], vpr$variables_PV[1], vpr$variables_SV[1])#ne marche pas
.l <- lapply(lvar, get_var_bl_suivi_pneumo)

df_rep_pneumo <- do.call(rbind, .l)

#Pour rajouter une variable sans tout refaire tourner:
# df_rep_pneumo <- rbind(df_rep_pneumo, get_var_bl_suivi_pneumo(c("NYCTURIE_PP", "NYCTURIE_PV",	"NYCTUR_SV")))

saveRDS(df_rep_pneumo, "data/df_rep_pneumo.rds")

#----------------
#var pneumo rep sans bl

#fonction
vnisla <- readRDS("data/vnisla.rds")
#get_var_bl_suivi_pneumo <- function(varPP, varPV, varSV){ #mapply ne marche pas
get_var_suivi_nobl_pneumo <- function(vec){
  vs<-c("DATE_SOUSVENT_SV")
  #Ws <- c(varPP, varPV, varSV)
  Ws <- vec
  extvs<-c("SV")
  extws<-c("SV")
  print(Ws)
  for (ie in 1:length(extvs)) {
    #browser()
    # ie<-ie+1  
    v<-c(vs[ie], Ws[ie])
    #v<-paste(v, c(extvs[ie], extws[ie]), sep="_")
    r<-c("", paste("_F", 1:100, sep=""))
    vr<-expand.grid(list(v=v, r=r))
    vr$f<-as.numeric(gsub("_F", "", vr$r, fix=T))
    vr$f[is.na(vr$f)]<-0
    vr$vr<-paste(vr$v, vr$r, sep="")
    vr<-vr[vr$vr%in%names(bdd7),]
    dim(vr)
    w<-c("date", "x")
    vrs<-split(vr, vr$f)
    names(vrs)
    vrs$"0"
    vrs$"2"
    for (iv in 1:length(vrs)) { 
      #browser()
      y<-bdd7[, c("PATIENT", vrs[[iv]]$vr)]
      names(y)[-1]<-w
      #y$ext<-ext #erreur
      y$ext<-extws[ie]
      #y$qui<-W #erreur
      y$qui<-Ws[1]
      y$f<-vrs[[iv]]$f[1]
      if (iv==1) {
        Yv<-y
      } else {
        Yv<-rbind(Yv, y)
      }
    }
    #browser()
    Yv<-Yv[order(Yv$PATIENT, Yv$f),]
    Yv$date<-as.Date(as.character(Yv$date), "%d/%m/%Y")
    Yv$x<-as.numeric(as.character(Yv$x))
    Yv<-Yv[!is.na(Yv$x) & !is.na(Yv$date),]
    head(Yv)
    if (ie==1) {
      YYv<-Yv
    } else {
      YYv<-rbind(YYv, Yv)
    }
  }
  #browser()
  #y<-merge(YYv, vnisla[, c("PATIENT", "date")], by="PATIENT", all=F, suff=c("", ".vni"))
  y<-merge(YYv, vnisla[, c("PATIENT", "datevni")], by="PATIENT", all=F)
  
  #colonne delai(del) entre la date d'exam et la date de vni
  #y$del<-as.numeric(y$date-y$date.vni)
  y$del<-as.numeric(y$date-y$datevni)
  y<-y[!is.na(y$del),]
  
  y<-y[order(y$PATIENT, y$date),]
  head(y)
  
  #selection des variables de suivi : toutes
  yp<-y[y$del>0,] 
  head(yp)
  summary(yp)
  if(nrow(yp)!=0) yp$f<-1 #1 pour suivi (en opposition à baseline)
  
  # #selection valeur de baseline
  # yn<-y[y$del<=0,]
  # head(yn)
  # yn<-yn[order(yn$PATIENT, -yn$del),] #delai le plus petit en première ligne 
  # head(yn)
  # id<-unique(yn$PATIENT);id
  # dim(yn)
  # yn<-yn[match(id, yn$PATIENT),] #prend la première ligne de yn qui matche avec id (donc prend ligne correspondant au délai le plus petit pour chaque patient)
  # dim(yn)
  # head(yn)
  # summary(yn)
  # if(nrow(yn)!=0) yn$f<-0 #0 pour baseline
  
  #je rassemble les tableaux baseline(yn) et suivi(yp)
  #y<-rbind(yp, yn)
  y <- yp
  y<-y[order(y$PATIENT, y$del),]
  head(y)
  #browser()
  return(y)
}


#J'utilise le tableau de toutes les var pneumo (après nettoyage) pour obtenir le tableau repete  
vpr <- read.csv2("data/variables_suivi_pneumo_clean.csv")
vpr <- data.frame(lapply(vpr, as.character), stringsAsFactors=FALSE)
vpr <- vpr[is.na(vpr$variables_PV) & !is.na(vpr$variables_SV), ]
vpr$variables_SV <- str_sub(vpr$variables_SV, 1, -4)
vpr <- vpr[vpr$variables_SV!="DATE_SOUSVENT_SV",]
#NB : 4 var pourlesquels il faudra une autre fonction pour les récupérer, à cause du codage:
#PAO2_SV_F1_CL1, SAO2_SV_F1_CL1, PACO2_SV_F1_CL1, HCO3_SV_F1_CL1, PH_SV_F1_CL1, MODIF_PARAM_ITEMS_SV

lvar <- lapply(1:nrow(vpr), function(i) dput(as.character(vpr[i ,c("variables_SV")])))
.l <- lapply(lvar, get_var_suivi_nobl_pneumo)

df_rep_nobl_pneumo <- do.call(rbind, .l)

saveRDS(df_rep_nobl_pneumo, "data/df_rep_nobl_pneumo.rds")


#imputer bl

tmp2 <- df_rep_nobl_pneumo %>% 
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="SATISF_VENTIL_SV") %>%
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 10, ext = "PV", f = 0, del = 0)) %>% #satisf = 10/10
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="UTIL_VENTIL_DIURN_SV") %>% #nb d'heures de ventilation diurne
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 0, ext = "PV", f = 0, del = 0)) %>% #0h diurne
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="UTIL_VENTIL_NOCT_SV") %>% #nb d'heures de ventilation nocturne
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 0, ext = "PV", f = 0, del = 0)) %>% #0h nocturne
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="DUREE_SOMM_VENT_SV") %>% #nb d'heures de sommeil sous ventilation
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 0, ext = "PV", f = 0, del = 0)) %>% #0h
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="QUALIT_SOMM_VENT_SV") %>% #qualité du sommeil sous ventilation (1=bonne, 3=mauvaise)
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 1, ext = "PV", f = 0, del = 0)) %>% #1: bonne qualité
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="EVOL_SOMM_VNI_SV") %>% #"Evolution de la qualité du sommeil par rapport au sommeil avant ventilation" (1: améliorée, 2: identique, 3: diminuée)
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 2, ext = "PV", f = 0, del = 0)) %>% #2: identique
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="REVEIL_VENT_SV") %>% #Réveils sous ventilation(1: oui, 0: non)
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 0, ext = "PV", f = 0, del = 0)) %>% #0 non
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="ORTHOPN_SV") %>% #orthopnée sous ventilation(1: oui, 0: non)
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 0, ext = "PV", f = 0, del = 0)) %>% #0 non
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="DYSPN_SVENT_SV") %>% #dyspnee SANS ventilation (1: stable, 2: majoree, 3: diminuee)
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 1, ext = "PV", f = 0, del = 0)) %>% #1: stable
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="DYSPN_SOUSVENT_SV") %>% #dyspnee SOUS ventilation (1: stable, 2: majoree, 3: diminuee)
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 1, ext = "PV", f = 0, del = 0)) %>% #1: stable
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="OXYM_VNI_SV") %>% #oxymétrie 1 normale 2 anormale 
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 1, ext = "PV", f = 0, del = 0)) %>% #1: normale
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="FUITE_VNI_SV") %>% #fuites 1 oui 0 non
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 0, ext = "PV", f = 0, del = 0)) %>%#0 non
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="EVT_OBSTR_SV") %>% #evt obstructif 1 oui 0 non
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 0, ext = "PV", f = 0, del = 0)) %>% #0 non
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="ASYNCHR_SV") %>% #asynchronisme 1 oui 0 non
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 0, ext = "PV", f = 0, del = 0)) %>% #0 non
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="ASYNCHR_SV") %>% #asynchronisme 1 oui 0 non
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 0, ext = "PV", f = 0, del = 0)) %>% #0 non
  bind_rows(df_rep_nobl_pneumo %>% filter(qui =="MODIF_PARAM_SV") %>% #modif param VNI 1 oui 0 non
              group_by(PATIENT) %>% filter(row_number()== 1) %>% 
              mutate(date = datevni, x = 0, ext = "PV", f = 0, del = 0)) #0 non

df_rep_nobl_pneumo_imput <- tmp2
saveRDS(tmp2, "data/df_rep_nobl_pneumo_imput.rds")
# 
# df_rep_nobl_pneumo %>% filter(qui =="SPO2_EVEIL_SUR_SV") %>% group_by(x) %>% summarise(n())
# lab_bdd7[lab_bdd7$var=="PAO2_SV_F1", ]
# lab_bdd7[grep("PIMAX_",lab_bdd7$var),]


#----------------
#var pneumo bl indep vni

#J'utilise le tableau de toutes les var pneumo (après nettoyage) pour obtenir le tableau repete  
vpr <- read.csv2("data/variables_suivi_pneumo_clean.csv")
vpr <- data.frame(lapply(vpr, as.character), stringsAsFactors=FALSE)
vpr$variables_PV <- str_sub(vpr$variables_PV, 1, -4)
vpr$variables_SV <- str_sub(vpr$variables_SV, 1, -4)
vpr <- vpr[!is.na(vpr$variables_PP) & is.na(vpr$variables_PV) & is.na(vpr$variables_SV), ]
vpr <- vpr[!vpr$variables%in% c("POURSUIVI", "DAT_ARRET"), ]

df_bl_pneumo <- bdd7[ ,c("PATIENT", vpr$variables_PP)]
df_bl_pneumo <- df_bl_pneumo[df_bl_pneumo$PATIENT %in% vnisla$PATIENT, ]
apply(apply(df_bl_pneumo,2,is.na),2,sum)
table(apply(apply(df_bl_pneumo,2,is.na),2,sum))
saveRDS(df_bl_pneumo, "data/df_bl_pneumo.rds")

#-----------------
#var pneumo bl dep vni

get_var_bl_depvni_pneumo <- function(vec){
  vs<-c("DATE_RESP_PP", "DATE_PREVENT_PP")
  #Ws <- c(varPP, varPV, varSV)
  Ws <- vec
  extvs<-c("PP", "PP")
  extws<-c("PP", "PV")
  print(Ws)
  
  for (ie in 1:length(extvs)) {
    #browser()
    # ie<-ie+1  
    v<-c(vs[ie], Ws[ie])
    #v<-paste(v, c(extvs[ie], extws[ie]), sep="_")
    r<-c("", paste("_F", 1:100, sep=""))
    vr<-expand.grid(list(v=v, r=r))
    vr$f<-as.numeric(gsub("_F", "", vr$r, fix=T))
    vr$f[is.na(vr$f)]<-0
    vr$vr<-paste(vr$v, vr$r, sep="")
    vr<-vr[vr$vr%in%names(bdd7),]
    dim(vr)
    w<-c("date", "x")
    vrs<-split(vr, vr$f)
    names(vrs)
    vrs$"0"
    vrs$"2"
    for (iv in 1:length(vrs)) { 
      #browser()
      y <- bdd7[, c("PATIENT", vrs[[iv]]$vr)]
      names(y)[-1] <- w
      #y$ext<-ext #erreur
      y$ext <- extws[ie]
      #y$qui<-W #erreur
      y$qui <- str_sub(Ws[1], 1, -4)
      y$f <- vrs[[iv]]$f[1]
      if (iv==1) {
        Yv<-y
      } else {
        Yv<-rbind(Yv, y)
      }
    }
    #browser()
    Yv<-Yv[order(Yv$PATIENT, Yv$f),]
    Yv$date<-as.Date(as.character(Yv$date), "%d/%m/%Y")
    Yv$x<-as.numeric(as.character(Yv$x))
    Yv<-Yv[!is.na(Yv$x) & !is.na(Yv$date),]
    head(Yv)
    if (ie==1) {
      YYv<-Yv
    } else {
      YYv<-rbind(YYv, Yv)
    }
  }
  #browser()
  #y<-merge(YYv, vnisla[, c("PATIENT", "date")], by="PATIENT", all=F, suff=c("", ".vni"))
  y<-merge(YYv, vnisla[, c("PATIENT", "datevni")], by="PATIENT", all=F)
  
  #colonne delai(del) entre la date d'exam et la date de vni
  #y$del<-as.numeric(y$date-y$date.vni)
  y$del<-as.numeric(y$date-y$datevni)
  y<-y[!is.na(y$del),]
  
  y<-y[order(y$PATIENT, y$date),]
  head(y)
  
  # #selection des variables de suivi : toutes
  # yp<-y[y$del>0,] 
  # head(yp)
  # summary(yp)
  # if(nrow(yp)!=0) yp$f<-1 #1 pour suivi (en opposition à baseline)
  
  #selection valeur de baseline
  yn<-y[y$del<=0,]
  head(yn)
  yn<-yn[order(yn$PATIENT, -yn$del),] #delai le plus petit en première ligne 
  head(yn)
  id<-unique(yn$PATIENT);id
  dim(yn)
  yn<-yn[match(id, yn$PATIENT),] #prend la première ligne de yn qui matche avec id (donc prend ligne correspondant au délai le plus petit pour chaque patient)
  dim(yn)
  head(yn)
  summary(yn)
  if(nrow(yn)!=0) yn$f<-0 #0 pour baseline
  y <- yn
  head(y)
  #browser()
  return(y)
}

#J'utilise le tableau de toutes les var pneumo (après nettoyage) pour obtenir le tableau repete  
vpr <- read.csv2("data/variables_suivi_pneumo_clean.csv")
vpr <- data.frame(lapply(vpr, as.character), stringsAsFactors=FALSE)
vpr$variables_PV <- str_sub(vpr$variables_PV, 1, -4)
vpr$variables_SV <- str_sub(vpr$variables_SV, 1, -4)
vpr <- vpr[!is.na(vpr$variables_PP) & !is.na(vpr$variables_PV) & is.na(vpr$variables_SV), ]

lvar <- lapply(1:nrow(vpr), function(i) dput(as.character(vpr[i ,c("variables_PP", "variables_PV")])))

.l <- lapply(lvar, get_var_bl_depvni_pneumo)
df_bl_depvni_pneumo <- do.call(rbind, .l)
#df_bl_depvni_pneumo <- df_bl_depvni_pneumo[df_bl_depvni_pneumo$qui!="NYCTURIE",]
tmp <- df_bl_depvni_pneumo
tmp <- reshape(tmp[ , c("PATIENT", "x", "qui", "datevni")], timevar = "qui", idvar=c("PATIENT", "datevni"), direction ="wide")

df_bl_depvni_pneumo_wide <- tmp 

saveRDS(df_bl_depvni_pneumo, "data/df_bl_depvni_pneumo.rds")
saveRDS(df_bl_depvni_pneumo_wide, "data/df_bl_depvni_pneumo_wide.rds")

#---------------
#var neuro répétées


Ws <- c("WEIGHT_NUTRI", "WEIGHT_NUTRI_V")
Ws <- c("DATE_NUTRI", "DATE_NUTRI_V")
get_var_blf0_suivif1_neuro(c("DATE_NUTRI", "DATE_NUTRI_V"))

get_var_blf0_suivif1_neuro <- function(Ws1, Ws2){
  #browser()
  Ws <- c(Ws1, Ws2)
  print(Ws)
  vs <- c("DATEXAM", "DATEXAM_V")
  extws <- c("base", "suivi")
  
  #base
  v <- c(vs[1], Ws[1])
  w<-c("date", "x")
  y<-bdd6[, c("PATIENT", v)]
  names(y)[-1]<-w
  y$ext <- extws[1]
  y$qui <- Ws[1]
  y$f<- 0 #f0 car base
  Yv<-y
  # Yv<-Yv[order(Yv$PATIENT, Yv$f),]
  Yv$date<-as.Date(as.character(Yv$date), "%d/%m/%Y")
  
  #option var non date
  Yv$x<-as.numeric(as.character(Yv$x))
  # #option verif_date :Ws <- c("DATE_NUTRI", "DATE_NUTRI_V") #pour verifier que la date nutri est la même que la date exam
  # Yv$x<-as.Date(as.character(Yv$x), "%d/%m/%Y")
  
  # Yv<-Yv[!is.na(Yv$x) & !is.na(Yv$date),]
  YYv<-Yv
  
  #suivi
  v <- c(vs[2], Ws[2])
  r<-c("", paste("_M", 1:100, sep=""))
  vr<-expand.grid(list(v=v, r=r))
  vr$f<-as.numeric(gsub("_M", "", vr$r, fix=T))
  vr$f[is.na(vr$f)]<-0
  vr$vr<-paste(vr$v, vr$r, sep="")
  vr<-vr[vr$vr%in%names(bdd9),]
  w<-c("date", "x")
  vrs<-split(vr, vr$f)
  
  for (iv in 1:length(vrs)) { 
    #browser()
    #print(iv)
    y<-bdd9[, c("PATIENT", vrs[[iv]]$vr)]
    names(y)[-1]<-w
    if (ncol(y)<3) next 
    y$ext<-extws[ie]
    #y$qui<-W #erreur
    y$qui<-Ws[1]
    y$f<-vrs[[iv]]$f[1]
    if (iv==1) {
      Yv<-y
    } else {
      Yv<-rbind(Yv, y)
    }
  }
  #browser()
  # Yv<-Yv[order(Yv$PATIENT, Yv$f),]
  Yv$date<-as.Date(as.character(Yv$date), "%d/%m/%Y")
  
  #option var non date
  Yv$x<-as.numeric(as.character(Yv$x))
  # #option verif_date :Ws <- c("DATE_NUTRI", "DATE_NUTRI_V") 
  # Yv$x<-as.Date(as.character(Yv$x), "%d/%m/%Y")
  
  # Yv<-Yv[!is.na(Yv$x) & !is.na(Yv$date),]
  head(Yv)
  YYv<-rbind(YYv, Yv)
  YYv<-YYv[order(YYv$PATIENT, YYv$f),]
  YYv<-YYv[!is.na(YYv$x) & !is.na(YYv$date),]
  if (nrow(YYv)==0) return(NULL)
  #y<-merge(YYv, vnisla[, c("PATIENT", "date")], by="PATIENT", all=F, suff=c("", ".vni"))
  y<-merge(YYv, vnisla[, c("PATIENT", "datevni")], by="PATIENT", all=F)
  #y$del<-as.numeric(y$date-y$date.vni)
  y$del<-as.numeric(y$date-y$datevni)
  y<-y[!is.na(y$del),]
  y<-y[order(y$PATIENT, y$date),]
  
  #selection des variables de suivi : toutes
  yp<-y[y$del>0,] 
  head(yp)
  summary(yp)
  if(nrow(yp)!=0) yp$f<-1 #1 pour suivi (en opposition à baseline)
  
  #selection valeur de baseline
  yn<-y[y$del<=0,]
  head(yn)
  yn<-yn[order(yn$PATIENT, -yn$del),] #delai le plus petit en première ligne 
  head(yn)
  id<-unique(yn$PATIENT);id
  dim(yn)
  yn<-yn[match(id, yn$PATIENT),] #prend la première ligne de yn qui matche avec id (donc prend ligne correspondant au délai le plus petit pour chaque patient)
  dim(yn)
  head(yn)
  summary(yn)
  if(nrow(yn)!=0)yn$f<-0 #0 pour baseline
  
  #je rassemble les tableaux baseline(yn) et suivi(yp)
  y<-rbind(yp, yn)
  y<-y[order(y$PATIENT, y$del),]
  head(y)
  return(y)
}




#selectionner tous les couples de variables neuro existant
v <- names(bdd6)
vbis <- names(bdd9)
vbis <- vbis[grep("_M1", vbis)]
vbis <- vbis[str_sub(vbis, -3,-3)=="_"]
vbis <- ifelse(str_sub(vbis, -4,-4)=="V", str_sub(vbis, 1,-6), str_sub(vbis, 1,-2))
v_unique <- unique(unlist(list(l1=v, l2=vbis)))
#v <- data.frame(v = unique(c(v, vbis)))
vn <- data.frame(v = v_unique)

vn$v1 <- as.character(vn$v)
vn$exist <- vn$v1 %in% names(bdd6)
vn$v1 <- ifelse(vn$exist==FALSE, NA, vn$v1)
vn$v2 <- ifelse(str_sub(vn$v, -2,-1)=="_M", paste0(vn$v,1), paste(vn$v, "V", "M1", sep="_"))
vn$exist <- vn$v2 %in% names(bdd9)
vn$v2 <- ifelse(vn$exist==FALSE, NA, vn$v2)
vn$exist <- NULL
colnames(vn) <- c("variables possibles", "bdd6", "bdd9")

saveRDS(vn, "data/variables_neuro_complet.rds")
write.table(print(vn), file="clipboard", sep="\t", row.names=F )

#Je nettoye ce tableau dans excel, je le sauvegarde sous csv : "data/variables_suivi_neuro_clean.csv"

#J'utilise ce tableau pour obtenir le tableau repete  
vrbis <- read.csv2("data/variables_suivi_neuro_clean.csv")
vrbis$var_bdd6 <- as.character(vrbis$var_bdd6)
vrbis$var_bdd9 <- as.character(vrbis$var_bdd9)
vrbis$var_possibles <- as.character(vrbis$var_possibles)
sample_n(vrbis, 10)
str(vrbis)
#je transforme pour que ça ressemble aux variables de vr
vrbis$var_bdd9 <- str_sub(vrbis$var_bdd9, 1, -4)
table(vr$base %in% vrbis$var_bdd6) #ok, les mêmes noms 
table(vr$suivi %in% vrbis$var_bdd9)#ok, les mêmes noms
#je selectionne les couples dispo dans les 2 bases (suivi et )
vnr <- vrbis[!is.na(vrbis$var_bdd9) & !is.na(vrbis$var_bdd6), ]
#je supprime les variables dates (sinon fait bugger la fonction)
vnr <- rbind(vnr[! vnr$var_bdd6 %in% vnr$var_bdd6[grep("DAT", vnr$var_bdd6)], ], vnr[vnr$var_bdd6=="TREPIDATION_PIED_CHOICE_1" |vnr$var_bdd6=="TREPIDATION_PIED_CHOICE_2", ])
#je génère le tableau répété
.l <- mapply(get_var_blf0_suivif1_neuro, Ws1 = vnr$var_bdd6, Ws2 = vnr$var_bdd9)
tab_rep_neuro <- do.call(rbind, .l)
# #REGLER ERREUR:
# # #"TREPIDATION_PIED_CHOICE_1"   "TREPIDATION_PIED_V_CHOICE_1"
# # which(vnr$var_bdd6=="TREPIDATION_PIED_CHOICE_1")
# .l <- mapply(get_var_blf0_suivif1_neuro, Ws1 = vnr$var_bdd6, Ws2 = vnr$var_bdd9)
#OK


#rajout variables variations de poids : par rapport à poids de forme et par rapport à poids du debut de vni
var_poids <- tab_rep_neuro %>% filter(qui=="WEIGHT_NUTRI") 
var_poids <- merge(var_poids, bdd6[, c("PATIENT", "WEIGHT_WB", "WEIGHT_REF")], by="PATIENT", all=F)
var_poids <- var_poids[!(is.na(var_poids$WEIGHT_WB) & is.na(var_poids$WEIGHT_REF)), ]
var_poids$WEIGHT_WB <- as.numeric(as.character(var_poids$WEIGHT_WB))
var_poids$WEIGHT_REF <- as.numeric(as.character(var_poids$WEIGHT_REF))
var_poids$x <- ifelse (!is.na(var_poids$WEIGHT_WB), var_poids$x - var_poids$WEIGHT_WB, var_poids$x - var_poids$WEIGHT_REF)
var_poids$qui <- "DELTA_WEIGHT_REF"
var_poids_ref <- var_poids[ , names(var_poids) %in% colnames(tab_rep_neuro)]

var_poids <- tab_rep_neuro %>% filter(qui=="WEIGHT_NUTRI")
poidsDEBVNI <- var_poids[var_poids$f==0, c("PATIENT", "x")] ; colnames(poidsDEBVNI)[2] <- "poids_pv"
var_poids <- merge (var_poids, poidsDEBVNI, by="PATIENT", all=F)
var_poids$x <- var_poids$poids_pv - var_poids$x
var_poids$qui <- "DELTA_WEIGHT_DEBVNI"
var_poids_debvni <- var_poids[ , names(var_poids) %in% colnames(tab_rep_neuro)]

tab_rep_neuro <- rbind(tab_rep_neuro, var_poids_ref, var_poids_debvni)
saveRDS(tab_rep_neuro, "data/df_rep_neuro.rds")

#-----------
#-----------
#var neuro baseline

#version tableau csv 
vrbis <- read.csv2("data/variables_suivi_neuro_clean.csv")
vrbis$var_bdd6 <- as.character(vrbis$var_bdd6)
vrbis$var_bdd9 <- as.character(vrbis$var_bdd9)
vrbis$var_possibles <- as.character(vrbis$var_possibles)
sample_n(vrbis, 10)
str(vrbis)
#je transforme pour que ça ressemble aux variables de vr
vrbis$var_bdd9 <- str_sub(vrbis$var_bdd9, 1, -4)
table(vr$base %in% vrbis$var_bdd6) #ok, les mêmes noms 
table(vr$suivi %in% vrbis$var_bdd9)#ok, les mêmes noms
#je selectionne les couples dispo dans la colonne base uniquement(var_bdd6)
vnr <- vrbis[is.na(vrbis$var_bdd9) & !is.na(vrbis$var_bdd6), ]
# #je supprime les variables dates (sinon fait bugger la fonction)
# vnr <- rbind(vnr[! vnr$var_bdd6 %in% vnr$var_bdd6[grep("DAT", vnr$var_bdd6)], ], vnr[vnr$var_bdd6=="TREPIDATION_PIED_CHOICE_1" |vnr$var_bdd6=="TREPIDATION_PIED_CHOICE_2", ])

for (i in vrbis[!is.na(vrbis$var_bdd9) & is.na(vrbis$var_bdd6), "var_bdd9"]) {
  print(i)
  print(table(bdd9[ , i], useNA = "a"))
}

df_bl_neuro <- bdd6[ ,c("PATIENT", vnr$var_bdd6)]
df_bl_neuro$DOB <- manage_date_ND(df_bl_neuro$DOB)
df_bl_neuro <- df_bl_neuro[df_bl_neuro$PATIENT %in% vnisla$PATIENT, ]
apply(apply(df_bl_neuro,2,is.na),2,sum)
table(apply(apply(df_bl_neuro,2,is.na),2,sum))
saveRDS(df_bl_neuro, "data/df_bl_neuro.rds")



#---------------
#---------------
#RESUME DES TABLEAUX CREES

#Base neuro répétée (format long):
df_rep_neuro <- readRDS("data/df_rep_neuro.rds")
#Base pneumo répétée avec bl imputée :
df_rep_nobl_pneumo_imput <- readRDS("data/df_rep_nobl_pneumo_imput.rds")
#Base pneumo répétée :
df_rep_pneumo <- readRDS("data/df_rep_pneumo.rds")
#Baseline neuro :
df_bl_neuro <- readRDS("data/df_bl_neuro.rds")
#Baseline pneumo :
df_bldep_pneumo <- readRDS("data/df_bl_depvni_pneumo_wide.rds")
df_bl_pneumo <- readRDS("data/df_bl_pneumo.rds")

#doublons
table(tab <- table(df_bl_neuro$PATIENT))
names(tab)[tab>1] #3 doublons
df_bl_neuro %>% filter (PATIENT%in% names(tab)[tab>1])
table(tab <- table(df_bldep_pneumo$PATIENT)) #les patients avec que des NA on été éliminé
table(tab <- table(df_bl_pneumo$PATIENT))
names(tab)[tab>1]#memes doublons
names_doublons <- names(tab)[tab>1]

#Je supprime ces 3 doublons des bases ci dessus
df_rep_neuro <- df_rep_neuro[! df_rep_neuro$PATIENT %in% names_doublons, ]
df_rep_nobl_pneumo_imput <- df_rep_nobl_pneumo_imput[!df_rep_nobl_pneumo_imput$PATIENT %in% names_doublons, ]
df_rep_pneumo <- df_rep_pneumo[!df_rep_pneumo$PATIENT %in% names_doublons, ]
df_bl_neuro <- df_bl_neuro[!df_bl_neuro$PATIENT %in% names_doublons, ]
df_bldep_pneumo <- df_bldep_pneumo[!df_bldep_pneumo$PATIENT %in% names_doublons, ]
df_bl_pneumo <- df_bl_pneumo[!df_bl_pneumo$PATIENT %in% names_doublons, ]

#verif dates
table(df_rep_neuro$date>Sys.Date())#0
table(df_rep_nobl_pneumo_imput$date>Sys.Date())#22
table(df_rep_pneumo$date>Sys.Date())#31
#Je supprime les dates incohérentes
df_rep_nobl_pneumo_imput <- df_rep_nobl_pneumo_imput[df_rep_nobl_pneumo_imput$date<Sys.Date(), ]
df_rep_pneumo <- df_rep_pneumo[df_rep_pneumo$date<Sys.Date(), ]


# #ddn
# sup_bdd <- rbind(df_rep_neuro, df_rep_nobl_pneumo_imput, df_rep_pneumo) 
# ddn <- sup_bdd %>% group_by(PATIENT) %>% arrange(desc(date)) %>% summarise(max(date))
# dim(sup_bdd %>% group_by(PATIENT) %>% arrange(desc(date)) %>% summarise(max(date))) #meme dimension que vnisla

#---------------------
#BASE  DE DONNEES DDN

#Boucle en commentaire car longue à faire tourner et résultat de la boucle déjà enregistrée dans data/ddn
# #Pour chercher la ddn de chaque base de donnée:
# for (i in .dir) {
#   num <- which(.dir==i)
#   print(i)
#   a <- get_ddn(i,"DAT")
#   saveRDS(a,paste0("data/ddn/ddn",num,".rds"))
#   assign(paste0("ddn",num),a)
# }

#Pour charger les ddn obtenues (pour éviter de relancer la boucle du dessus qui prend bcp de temps...)
for (i in .dir_csv[-2]) {
  num <- which(.dir_csv==i)
  a <- readRDS(paste0("data/ddn/ddn",num,".rds")) #NB : supprimer ddn2.Rds du dossier data/ddn car le fichier est vide
  a <- a[[1]]
  a <- aggregate(a,by=list(a$PATIENT),max,na.rm=T) #qd des noms sont rassemblé et que toutes leurs dates valent NA, alors le tableau vaut NA
  assign(paste0("ddn",num),a)
}


#Pour merger les ddn
#nom des dataframe ddn
dfddn <- str_sub(dir("data/ddn"),1,-5)[sapply(dir("data/ddn"),function(x)is.data.frame(get(str_sub(x,1,-5))))]
#merge
for (i in dfddn){
  num <- which(dfddn==i)
  .bd <- get(i)
  ddn_tot <- if(num==1) .bd[,c("PATIENT","max")] else merge(ddn_tot, .bd[,c("PATIENT","max")], by="PATIENT", suffixes= c(num-1,num),all=TRUE)
}
#transformation des colonnes en date si pas déjà fait

for (i in colnames(ddn_tot)[-1]){
  ddn_tot[,i] <- as_date(ddn_tot[,i])
  ddn_tot[,i] <- ifelse (ddn_tot[,i]>Sys.Date(), NA, ddn_tot[,i])
  ddn_tot[,i] <- as_date(ddn_tot[,i])
}

ddn_tot$ddn <- apply(ddn_tot[,grep("max", colnames(ddn_tot))],1,max,na.rm=T)

ddn_tot <- unique(ddn_tot[ ,c("PATIENT","ddn")]) #pas de duplicatat
ddn_tot$ddn <- as_date(ddn_tot$ddn)

dim(ddn_tot)

saveRDS(ddn_tot, "data/bdd_to_merge/ddn_tot.rds")

ddn_tot <- readRDS("data/bdd_to_merge/ddn_tot.rds")

#-------------
#BASE DE DONNEES DECES

lapply(bdds, which_col,"DAT","DCD",type="explo")
listes_brut <- lapply(bdds, which_col,"DAT","DCD",type="merge")

#tous les décès : res
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}

bdd_dcd <- get_min_max(data = res, fun = "min")
bdd_dcd$date_dc <- manage_date_ND(bdd_dcd$min)

bdd_dcd <- unique(bdd_dcd[ , c("PATIENT","date_dc")]) #seul les vrais doublons nom et date sont éliminés

saveRDS(bdd_dcd, "data/bdd_to_merge/bdd_dcd.rds")

bdd_dcd <- readRDS("data/bdd_to_merge/bdd_dcd.rds")

#-----------
#table avec infos essentielles : date de vni, ddn, date décès, date de fin, date de fin de vni

#date de fin 
bdd_dates <- merge(ddn_tot, bdd_dcd, by="PATIENT", all=T)
bdd_dates$censor <- ifelse (!is.na(bdd_dates$date_dc), 1, 0)
bdd_dates$dfin <- ifelse (!is.na(bdd_dates$date_dc), bdd_dates$date_dc, bdd_dates$ddn)

#date de fin de vni
poursuite <- get_var_suivi_nobl_pneumo("POURSUITE_VENT_SV")
poursuite[poursuite$x==0, "PATIENT"]
poursuite %>% filter(PATIENT %in% poursuite[poursuite$x==0, "PATIENT"]) #Attention le 0 n'est pas toujours la derniere info dispo!
fin_vni <- poursuite %>% group_by(PATIENT) %>% arrange(desc(date)) %>% top_n(1) %>% filter(x==0) %>% select(fin_vni=date)#Je sélectionne la date la plus récente puis je sélectionne poursuite =0

bdd_dates <- merge(bdd_dates, fin_vni, by="PATIENT", all=T)
bdd_dates$fin_vni <- ifelse(!is.na(bdd_dates$fin_vni), bdd_dates$fin_vni, bdd_dates$dfin)

#ne sélectionner que les patients vni et rajouter leur date de vni:
bdd_dates <- merge(vnisla[ , c("PATIENT", "datevni")], bdd_dates, by="PATIENT", all.x=T, all.y=F)
bdd_dates$dfin <- as_date(bdd_dates$dfin) #date de fin de suivi (ddn ou décès)
bdd_dates$fin_vni <- as_date(bdd_dates$fin_vni)

saveRDS(bdd_dates, "data/bdd_dates.rds")



#---------------
#---------------
#---------------
#---------------

#Pour fabriquer un cahier des variables à partir d'un fhichier sas 

get_lab <- function(num){
  file_dir <- .dir_sas[num]
  s <- scan (file_dir, what=as.character(), sep="\n", blank.lines.skip=FALSE)
  i1<-grep(" input", s)
  i1bis <- grep("proc FORMAT", s)
  i2<-grep(" Label", s)
  
  si<-s[i1:(i1bis-1)]
  sl<-s[i2:length(s)]
  
  x<-strsplit(sl, "= \"", fix=T)
  sl1<-sapply(x, function(x, i=1) x[i], i=1)
  sl2<-sapply(x, function(x, i=1) x[i], i=2)
  sl<-data.frame(var=sl1, lab=sl2, txt=sl)
  sl$var<-gsub(" ", "", as.character(sl$var))
  sl$lab<-gsub("\"", "", as.character(sl$lab))
  
  si1 <- sapply(si,function(x, i=1) x[i])
  si1 <- data.frame(input=si1)
  si1$var <-  gsub(" ", "", as.character(si1$input))
  si1$var <-  gsub("\\$", "", as.character(si1$var))
  
  lab_bdd <-merge(sl, si1, by="var", all=T)
  return(lab_bdd)
}
for (i in c(6,7,9)){
  lab_bdd <- get_lab(i)
  assign(paste0("lab_bdd",i), lab_bdd)
}

lab_bdd9[grep("LOOSEWEIGHT_REFN",lab_bdd9$var),]
lab_bdd9[grep("1_M1",lab_bdd9$var),]
lab_bdd9[lab_bdd9$var=="LOOSEWEIGHT_FV_V_M1",]
lab_bdd6[lab_bdd6$var=="TTT_SYMPT_LNUM_L3",]
lab_bdd7[lab_bdd7$var=="PAO2_SV_F1", ]
lab_bdd7[grep("SATISF_VENTIL_SV",lab_bdd7$var),]

table(bdd7$GAZOM_AIR_SV_F1)



