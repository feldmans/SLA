source("src/fonctions_SLA.R")


#------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#VERSION AVEC LES TABLES trt/PATIENT.csv et visite/PATIENT2.csv : OBTENTION TABLE BDDSLA


#vérifier si ce sont les même patients que les 202 de la version avec DIAG DCD VNI

#pat <-  read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/pat/PATIENT.csv") #non utilisé
ttt <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/trt/PATIENT.csv") #donne VNI
visite <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/visite/PATIENT2.csv")

#1 Selection du center SLA01
visite <- visite[visite$CENTRE_M1=="SLA01",]

#---------------------------------------------------------
#suppression des doublons
visite$PATIENT <- as.character(visite$PATIENT)
names2visite <- names(table(visite$PATIENT)[table(visite$PATIENT)>1])

ttt$PATIENT <- as.character(ttt$PATIENT)
names2ttt <- names(table(ttt$PATIENT)[table(ttt$PATIENT)>1])
arrange(ttt[ttt$PATIENT %in% names2ttt, 1:50],PATIENT) #voir plus tard pour les repêcher

#impression des doublons
print_double_unique (visite,"PETIT_BERNARD") #NB : on ne peut pas faire de lapply et do.call car pas meme nombre de colonne. Par conter on peut faire une boucle dans un fichier texte
print_double(visite,names2visite,500:700)
print_double(ttt,names2ttt,1:500)

visite <- visite [! visite$PATIENT %in% names2visite,]
ttt <- ttt [! ttt$PATIENT %in% names(names2ttt) ,]
#------------------------------
#SELECT SLA
#ALLDIAG <- visite[,colnames(visite)[grep("DIAG",colnames(visite))]] #Pas bon, sélectionne aussi les dates
ALLDIAG <-visite[,colnames(visite)[str_sub(colnames(visite),1,8)=="DIAG_V_M" | str_sub(colnames(visite),1,12) == "NEW_DIAG_V_M"]]
pick_diag <- lapply(1: nrow(ALLDIAG),function(.x){
  #browser()
  .l <- ALLDIAG[.x,]
  .l <- .l[!is.na(.l)]
  is.TRUE <- all(.l[1]==.l,na.rm=T) & length(.l)!=0
  if (is.TRUE) diag <- .l[1]
  else{ 
    if (length(.l)==0) diag <- NA 
    else diag <- "no identical diag" 
  }
  return(diag)
})
ALLDIAG$identical_diag <- do.call(rbind,pick_diag)
ALLDIAG$PATIENT <- as.character(visite$PATIENT)
no_identical_diag <- ALLDIAG[ALLDIAG$identical_diag=="no identical diag" & !is.na(ALLDIAG$identical_diag),]
ALLDIAG$diagSLA <- ifelse(as.numeric(ALLDIAG$identical_diag)==1,1,0) #1:SLA, 0: autre diagnostic ou non identical diagnostic, NA: pas de diagnostic
ALLDIAG <- ALLDIAG[, c("PATIENT","diagSLA","identical_diag")] 
#même si le new diag vaut 1 alors que les précédents étaient autre, je ne garde pas le diagnostic. (à discuter) 

#-----------------------------
#SELECT ONE DATE DIAG (AND ERASE NON IDENTICAL DATE DIAG)

ALLDATEDIAG <- visite[,colnames(visite)[grep("DATEDIAG_V_M",colnames(visite))]]
DATEDIAG_ND <- lapply(colnames(ALLDATEDIAG),function(.x){
  whoND <- who_is_date_ND(vec_name = visite$PATIENT,vec_date = visite[,.x])
  whoND <- cbind (whoND,.x)
  return(whoND)
})
DATEDIAG_ND <- do.call(rbind,DATEDIAG_ND)

for (i in colnames(ALLDATEDIAG)){
  ALLDATEDIAG[,i] <- manage_date_ND(ALLDATEDIAG[,i])
}
# identical_date_diag <- lapply(1: nrow(ALLDATEDIAG),function(.x){
#   #browser()
#   .l <- ALLDATEDIAG[.x,]
#   .l <- .l[!is.na(.l)]
#   .l <- as.Date(.l)
#   is.TRUE <- all(.l[1]==.l,na.rm=T) & length(.l)!=0
#   return(is.TRUE)
# })
# identical_date_diag <- do.call(rbind,identical_date_diag)

pick_date_diag <- lapply(1: nrow(ALLDATEDIAG),function(.x){
  #browser()
  .l <- ALLDATEDIAG[.x,]
  .l <- .l[!is.na(.l)]
  .l <- as.Date(.l)
  .l <- .l[!is.na(.l)]
  is.TRUE <- all(.l[1]==.l,na.rm=T) & length(.l)!=0
  if (is.TRUE) date <- as.Date(.l[1])
  else{ 
    if (length(.l)==0) date <- NA 
    else date <- "no identical date" 
  }
  return(date)
})
ALLDATEDIAG$identical_date_diag <- do.call(rbind,pick_date_diag)

table(ALLDATEDIAG$identical_date_diag=="no identical date")

ALLDATEDIAG$PATIENT <- as.character(visite$PATIENT)
no_identical_date_diag <- ALLDATEDIAG[ALLDATEDIAG$identical_date_diag =="no identical date" & !is.na(ALLDATEDIAG$identical_date_diag),]
ALLDATEDIAG$date_diag <- as.Date(as.numeric(ALLDATEDIAG$identical_date_diag),origin="1970-01-01") #les no identical date sont transformées en NA par coercion
ALLDATEDIAG <- ALLDATEDIAG[ , c("PATIENT","date_diag","identical_date_diag")]
#ceux avec un nouveau diagnostic sont suprrimés même si le nouveau diagnostic est une SLA => que faire de ces pateints?

#------------
#Date VNI
ALLVNI <- ttt[,colnames(ttt)[grep("VNI",colnames(ttt))]]
vni_ND <- who_is_date_ND(vec_name =ttt$PATIENT,vec_date = ttt$DATEVNI)
ALLVNI$DATEVNI <- manage_date_ND(ALLVNI$DATEVNI)
table(ALLVNI[!is.na(ALLVNI$DATEVNI),"TTT_VNI"])
head(ALLVNI)
ALLVNI$PATIENT <- as.character(ttt$PATIENT)
ALLVNI <- ALLVNI[ ,c("PATIENT","TTT_VNI","DATEVNI","DATEVNI_STOP") ]

#--------------
#Date Décès
ALLDC <- visite[,colnames(visite)[grep("DATEDCD",colnames(visite))]]
DC_ND <- lapply(colnames(ALLDC),function(.x){
  whoND <- who_is_date_ND(vec_name = visite$PATIENT,vec_date = visite[,.x])
  whoND <- cbind (whoND,.x)
  return(whoND)
  })
DC_ND <- do.call(rbind,DC_ND) 

for (i in colnames(ALLDC)){
  ALLDC[,i] <- manage_date_ND(ALLDC[,i])
}
# pick_date_DC <- lapply(1: nrow(ALLDC),function(.x){
#   #browser()
#   .l <- ALLDC[.x,]
#   .l <- .l[!is.na(.l)]
#   .l <- as.Date(.l)
#   .l <- .l[!is.na(.l)]
#   is.TRUE <- all(.l[1]==.l,na.rm=T) & length(.l)!=0
#   if (is.TRUE) date <- as.Date(.l[1])
#   else{ 
#     if (length(.l)==0) date <- NA 
#     else date <- "no identical date" 
#   }
#   return(date)
# })
pick_date_DC <- lapply(1: nrow(ALLDC),function(.x){
  #browser()
  .l <- ALLDC[.x,]
  .l <- .l[!is.na(.l)]
  .l <- as.Date(.l)
  .l <- .l[!is.na(.l)]
  #is.TRUE <- all(.l[1]==.l,na.rm=T) & length(.l)!=0
  is.TRUE <- length(.l)!=0 #si date de décès discordant, prendre la première
  #if (is.TRUE) date <- as.Date(.l[1])
  if (is.TRUE) date <- as.Date(min(.l,na.rm=T))
  else date <- NA 
  return(date)
})
ALLDC$identical_date_dc <- do.call(rbind,pick_date_DC)
ALLDC$PATIENT <- as.character(visite$PATIENT)
#no_identical_DC <- ALLDC[ALLDC$identical_date_dc=="no identical date" & !is.na(ALLDC$identical_date_dc),]
ALLDC$date_dc <- as.Date(as.numeric(ALLDC$identical_date_dc),origin="1970-01-01") #les no identical date sont transformées en NA par coercion
ALLDC <- ALLDC[ , c("PATIENT","date_dc")]

#-----------
#Date dernière nouvelle : DATEXAM_V_M
ALLCSinit <- visite[,colnames(visite)[grep("DATEXAM_V_M",colnames(visite))]]
CS_ND <- lapply(colnames(ALLCSinit),function(.x){
  whoND <- who_is_date_ND(vec_name = visite$PATIENT,vec_date = visite[,.x])
  whoND <- cbind (whoND,.x)
  return(whoND)
  })
CS_ND <- do.call(rbind,CS_ND)

for (i in colnames(ALLCSinit)){
  ALLCSinit[,i] <- manage_date_ND(ALLCSinit[,i])
}
ALLCSinit$PATIENT <- as.character(visite$PATIENT)
ALLCS <- merge(ALLCSinit,ALLDC,"PATIENT",all.x=T,all.y=F)
pick_date_CS <- lapply(1: nrow(ALLCS),function(.x){
  #browser()
  .l <- ALLCS[.x,!colnames(ALLCS)%in%("PATIENT")]
  if ( is.na(.l$date_dc) ){ 
    .l <- .l[!is.na(.l)]
    .l <- as.Date(.l) #utile s'il y a autre chose que des dates dans le tableau ALLCS : fonction tournera quand meme
    .l <- .l[!is.na(.l)]
    #.lsort <- sort(.l)
    #if (all(sort(.l)==.l) & length(.l)!=0) ddn <- max(.l) #$178 inconsistency
    if (length(.l)==0) ddn <- NA
    else {
      if ( max(sort(.l))==max(.l) ) ddn <- max(.l) #0 inconsistency
      else ddn <- "date inconsistency"
    }
  } else {
    ddn <- .l$date_dc
  }
  return(ddn)
})

ALLCS$identical_ddn <- do.call(rbind,pick_date_CS)
#ALLCS$PATIENT <- as.character(visite$PATIENT) #déjà dans ALLCS avec merge
no_identical_CS <- ALLCS[ALLCS$identical_ddn=="date inconsistency" & !is.na(ALLCS$identical_ddn),]
ALLCS$ddn <- as.Date(as.numeric(ALLCS$identical_ddn),origin="1970-01-01") #pas de date insconsistency donc pas de NA par coercion
#ALLCS <- ALLCS[ , c("PATIENT","ddn","identical_ddn")]
ALLCS <- ALLCS[ , c("PATIENT","ddn","date_dc")]

#----------------------
table(ALLDIAG$PATIENT)[table(ALLDIAG$PATIENT)>1]
table(ALLDATEDIAG$PATIENT)[table(ALLDATEDIAG$PATIENT)>1]
#merge
BDD <- merge(x=ALLDIAG,y=ALLDATEDIAG, by="PATIENT", all.x = T, all.y=T)
BDD <- merge(BDD, ALLVNI, by="PATIENT", all.x = T, all.y=T)
BDD <- merge(BDD, ALLCS, by="PATIENT", all.x = T, all.y=T) #ALLDC déjà mergé dans ALLCS

#-----------------------
#temps de suivi

#si il y a eu décès, ddn=décès
table((BDD$date_dc != BDD$ddn) & !is.na(BDD$date_dc))

BDD$TTT_VNI <- ifelse (!is.na(BDD$DATEVNI),1,BDD$TTT_VNI)
saveRDS(BDD,file="data/BDD.rds")

#selection
BDDSLA <- BDD[BDD$diagSLA==1 & !is.na(BDD$diagSLA) & !is.na(BDD$date_diag) & BDD$TTT_VNI==1 & !is.na(BDD$TTT_VNI) & !is.na(BDD$DATEVNI),]
BDDSLA <- BDDSLA [ , ! colnames(BDDSLA) %in% c("identical_diag","identical_date_diag")]
saveRDS(BDDSLA,file="data/BDDSLA.rds")

#----------------------------------------------------------------------


#cas pas de date de dernière nouvelle
# > ddn <- NA
# > dc <- as.Date("2010-01-01")
# > ddn>dc
# [1] NA
# > new <- ifelse (ddn>dc,1,0)
# > new
# [1] NA
#cas pas de décès
ddn <- as.Date("2010-01-01")
dc <- NA
ddn!=dc


#observation des dates pour les patients incohérents:
ALLDATE <- visite[,colnames(visite)[grep("DATE",colnames(visite))]]
ALLDATE$PATIENT <- as.character(visite$PATIENT)
ALLDATE[ALLDATE$PATIENT=="ZOLNIEROWICZ_HENRIETTE",]


# #Je transforme les facteurs en date
# for (i in colnames(ALLDATE)){
#   ALLDATE[,i] <- manage_date_ND(ALLDATE[,i])
# }


# #ALLDATE[,colnames(ALLDATE)] <- apply(ALLDATE,2,manage_date_ND)
# ALLDATENA <- apply(ALLDATE,2,function(.x)!is.na(.x)) #si true = non NA
# COLDATEnonNA <- colnames(ALLDATENA)[apply(ALLDATENA,2,sum)>0]
# 
# #Afficher les colonnes avec dates nonNA
# head(ALLDATE[ ,COLDATEnonNA])
# 
# #Afficher l'histoire d'un patient
# t(ALLDATE[1:5 ,COLDATEnonNA])


#---------------------------------------------------------------------
#---------------------------------------------------------------------

#VERSION AVEC TABLES Diag VNI et DCD


#source("C:/yann/travaux/sla/pgm/sarah.r")
# setwd("C:/yann/travaux/sla")
# l<-load("donnÃ©es/Diag.RData");l
# lv<-load("donnÃ©es/VNI.RData");lv

#rm(list=ls())

.dir <- "C:/Users/4051268/Documents/sauvegarde data/sla/data/20150827/"
Diag <-read.csv2(paste0(.dir,"Diag.csv")) ;head(Diag)
VNI<-read.csv2(paste0(.dir,"VNI.csv")) ;head(VNI)

Diag$PATIENT <- as.character(Diag$PATIENT)
VNI$PATIENT <- as.character(VNI$PATIENT)

#----------------------------------------------------------
who_is_date_ND (dcd$PATIENT,dcd$DATE_DCD)
# Je remplace ND jour et mois par 01/07 (pas de cas ici) et ND jour uniquement par 15 :
dcd$date_dcd <- manage_date_ND(dcd$DATE_DCD)
table(!is.na(dcd$date_dcd[dcd$FORM != "ID"]))


#-------------------------------------------------------------------
#SELECTION DES PATIENTS SLA

#nombre de patients au départ
length (unique(id$PATIENT))

#Je ne garde que les patients de la pitié :
id<-Diag[Diag$FORM=="ID",]
id <- id[id$CENTRE=="SLA01", ]
length (unique(id$PATIENT))
namesPSL <- unique(id[id$CENTRE=="SLA01", "PATIENT"])

#CREATION TABLEAU PV(DIAG et conclusion confondu) PV1(DIAG) et PV2(conclusion) : Diagnostic et date de diagnostic

#Je sélectionnes les lignes premiÃ¨re visite (et non visis_supl)
pv<-Diag[Diag$MODULE=="PV",] 
dim(pv)

#Je selectionne les lignes correspondant au diag : FORM=DIAG et FORM=conclusion
pv<-pv[pv$FORM!="ID",]
dim(pv)

#Je sélectionnes patients de la pitié:
pv<-pv[pv$PATIENT %in% namesPSL, ]
length(unique(pv$PATIENT))

#Je supprime les lignes sans Diagnostic 
pv <- pv[!is.na(pv$DIAGNOS),] 
dim(pv)
length(unique(pv$PATIENT))

#MERGE DIAGNOSTIC DE LIGNE DIAG ET DE LIGNE CONCLUSION
#MERGE pv1 et pv2 => pw
#Merge des lignes DIAG et des lignes conclusions de l'exam (les seuls differences potentielles sont date et diagnostic)
pv1<-pv[pv$FORM=="DIAG", c("PATIENT", "DATE_DIAG", "DIAGNOS")]
pv2<-pv[pv$FORM!="DIAG", c("PATIENT", "DATE_DIAG", "DIAGNOS")]
pw<-merge(pv1, pv2, by="PATIENT", all=T, suff=c(".diag", ".ccl")) #.d pour ligne diag, .e pour ligne conclusion
head(pw)
dim(pw)
length(unique(pw$PATIENT))

table(pw$DIAGNOS.diag, pw$DIAGNOS.ccl, exclude=NULL) #Je regarde si les diagnostics concordent

#Selection des lignes avec DIAGNOSTIC DE SLA 
pw<-pw[(pw$DIAGNOS.diag=="1" & !is.na(pw$DIAGNOS.diag)) & (pw$DIAGNOS.ccl=="1" | is.na(pw$DIAGNOS.ccl)),]
#!is.na(diagnos.d) utile? pw<-pw[(pw$DIAGNOS.d=="1") & (pw$DIAGNOS.e=="1" | is.na(pw$DIAGNOS.e)),]
dim(pw)
#nombre 
length(unique(pw$PATIENT))
table(pw$DIAGNOS.diag, pw$DIAGNOS.ccl, exclude=NULL)

#Je remplace les date ND (pour jour et/ou mois)
date.diagSLA_ND <- who_is_date_ND (vec_name = pw$PATIENT, vec_date = pw$DATE_DIAG.diag)
date.cclSLA_ND <- who_is_date_ND (vec_name = pw$PATIENT, vec_date = pw$DATE_DIAG.ccl) #ND uniquement pour diag, pas pour ccl
pw$date.diag <- manage_date_ND(vec = pw$DATE_DIAG.diag)
pw$date.ccl <- manage_date_ND(vec = pw$DATE_DIAG.ccl)

#SUPPRESION DES SLA SANS DATE DE DIAGNOSTIC
SLA_sans_date <- unique(pw[is.na(pw$date.diag) & is.na(pw$date.ccl), "PATIENT" ])
pw <- pw[ !is.na(pw$date.diag) | !is.na(pw$date.ccl),  ]
dim(pw)
length(unique(pw$PATIENT))

#Les Patients SLA avec une date de diagnostic n'ont bien qu'une seule ligne
table(tab2<-table(pw$PATIENT))
names_double_diag <- names(tab2[tab2>1])
pw <- pw [!pw$PATIENT %in% names_double_diag, ]

#head(pv[, c("PATIENT", "CENTRE", "MODULE", "FORM", "DATE_DIAG", "DIAGNOS")]) : ne plus utiliser DATE_DIAG, transformee en date!
pw <- pw[ , !colnames(pw)%in%c("DATE_DIAG.diag","DATE_DIAG.ccl")] #je garde date.diag et date.ccl
length(unique(pw$PATIENT))

names_sla_date <- unique(pw$PATIENT)
#voir plus tard pour garder la date la plus recente

#--------------------------------------------------------------------------
#id : ligne ID uniquement avec patient de la pitié

#Les 2037 patients SLA avec une date de diagnostique ont tous une seule identité, et viennent bien de la pitié
SLA <- Diag[Diag$PATIENT %in% names_sla_date, ] #%ce tableau ne sert qu'à vérifier cohérence
table(tabDIAG <- table(SLA$PATIENT[SLA$FORM=="DIAG" & SLA$DIAGNOS==1 & SLA$DATE_DIAG!= ""]))
namestabDIAG <- names(tabDIAG[tabDIAG>1])

table(tabID <- table(SLA$PATIENT[SLA$FORM=="ID"]))
namestabID <- names(tabID[tabID>1])
#Patients avec plusieurs identifiants:
Diag[Diag$PATIENT %in% namestabID & Diag$FORM=="ID",]


# CREATION DU TABLEAU ID : identifiant du patient : nom, centre, module
ID <- Diag[(Diag$PATIENT %in% names_sla_date) & Diag$FORM=="ID" & !(Diag$PATIENT%in% namestabID), c("PATIENT","CENTRE")]
table(table(ID$PATIENT))

#------------------------------------------------------------------
# MERGE ID ET pw -> idpw
idpw<-merge(ID, pw, by="PATIENT", all.x=F, all.y=F)
dim(idpw)
names_idpw <- unique(idpw$PATIENT)

#---------------------------------
# CREATION idpwv : patient SLA de la pitié avec vni


vni<-VNI
dim(vni)
head(vni)
dim(vni)

vni <- vni[vni$PATIENT %in% names_idpw, ]
table(table(unique(vni[vni$FORM== "ID", "PATIENT" ]))) #J'ai bien une ligne ID par patient SLA

vni_vent <- vni[vni$PATIENT %in% names_idpw & vni$FORM!= "ID", ]
names_vni <- unique(vni_vent$PATIENT)

table(table(vni_vent$PATIENT)) # 10 patients ont 2 lignes avec date VNI
table(tab<-table(vni_vent$PATIENT))
x<-names(tab[tab>1])

names_vni_duplicated <- sort(vni_vent$PATIENT[duplicated(vni_vent$PATIENT)])
vni_vent[vni_vent$PATIENT %in%names_vni_duplicated,  ] #10 patients avec 2 dates de VNI

who_is_date_ND(vec_name = vni_vent$PATIENT,vec_date = vni_vent$DATE_VNI)
vni_vent$date_vni <- manage_date_ND(vni_vent$DATE_VNI) #enlève les ND s'il y en a (on a vu plus haut qu'il n'y en a pas) et transforme en date

table(is.na(vni_vent$date_vni))

no_prevent_line <- names_idpw[! names_idpw %in% names_vni]

vni_vent <- vni_vent[ !vni_vent$PATIENT %in% names_vni_duplicated, colnames(vni_vent) %in% c("PATIENT","MODULE","FORM","date_vni") ] 

#MERGE idpw et vni_vent
idpw_vni <-merge(idpw, vni_vent, by="PATIENT", all.x=F, all.y=F)
dim(idpw_vni)
length(unique(idpw_vni$PATIENT))
saveRDS(idpw_vni, "data/idpw_vni.rds")

#nom des patients sélectionnés:
selected_names <- unique(idpw_vni$PATIENT)
saveRDS(selected_names, "data/selected_names.rds")
#copié dans : C:\Users\4051268\Documents\sauvegarde data\patients_analysis


#--------------------------------RAPPORT DES NA-------------------------------------

sink ("C:/Users/4051268/Documents/writing/listes_NA.txt") #enregistre dans un fichier texte


cat("\t\tRESUME DES DONNEES MANQUANTES\n\n\n\n")#imprime titre dans le fichier texte, avant de faire boucle

cat("patients avec ND pour le jour de diagnostic : remplacé par 15\n\n")
print(date.diagSLA_ND)

cat("\n\nNombre de patient SLA sans aucune date de diagnostic dans le fichier Diag.csv (patients supprimés) :  ")
print(length(SLA_sans_date))

cat("\n\nNombre de patient SLA avec plusieurs dates de diagnostic dans le fichier Diag.csv (patients supprimés) :\n\n ")
print(Diag[Diag$PATIENT %in% names_double_diag & Diag$FORM!="ID" & !(is.na(Diag$DIAGNOS)), ! colnames(Diag) %in% c("MODULE_STATUS","REF_NAME","REF_ID770S4V3638","REF_SURNAME","X")])

cat("\n\nNombre de patient SLA avec une date de diagnostic et plusieurs ID dans le fichier Diag.csv (patients supprimés) :\n\n ")
dupid<-Diag[Diag$PATIENT %in% namestabID & Diag$FORM=="ID", ! colnames(Diag) %in% c("DIAGNOS","DATE_DIAG","MODULE_STATUS","REF_NAME","REF_ID770S4V3638","REF_SURNAME","X")]
print(dupid[order(dupid$PATIENT),])

cat("\n\nNombre de patients sans ligne de préventilation mais présent dans fichier VNI :")
print(length(no_prevent_line))
cat("\nCe sont des patients ventilés mais sans date ou des patients sans VNI?\nY a-t-il un vecteur VNI 0/1 dans une des tables?\n\n")

cat("\n\n10 patients avec 2 dates de VNI : patients supprimés.\n\n")
dup <-VNI[VNI$PATIENT %in% names_vni_duplicated & VNI$FORM!="ID", ! colnames(VNI) %in% c("MODULE_STATUS","REF_NAME","REF_ID770S4V3638","REF_SURNAME","X","CENTRE") ]
print( dup[order(dup$PATIENT),] )

cat("\n\n\n\tEn résumé\n\n")
cat("nombre de patients SLA avec une date de diagnostic :")
print(length(names_sla_date))
cat("nombre de patients SLA avec une date de diagnostic et une seule identité :")
print(length(unique(ID$PATIENT)))
cat("\n\nnombre de patients SLA avec une date de diagnostic et date de VNI (après des patients avec 2 dates de VNI) :")
print(length(unique(idpw_vni$PATIENT)))
sink()

#--------------------------------------------------------------------------------------
#RAJOUT DES DATES DE DECES


DCD <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/20150827/DCD.csv")
DCD$PATIENT <- as.character(DCD$PATIENT)
#la fonction manage_date_ND prend déjà en charge les dates en facteur

#selected_names : patients de la pitié avec diagnostic de SLA(1) daté(fichier DIAG) et avec une date de début de vni(fichier VNI)
dcd <- DCD %>% filter(PATIENT %in% selected_names)
dcd <- droplevels(dcd)

#Description de dcd pour les 212 patients selectionnes:
table(dcd$FORM)
#150 lignes ccl examen avec les dates de DC
#212 lignes identités avec le centre

#-------------------------------------------------------------
#Gestion des dates ND

#Je sors les ND:
ND_dcd <- who_is_date_ND (dcd$PATIENT,dcd$DATE_DCD)
# Je remplace ND jour et mois par 01/07 (pas de cas ici) et ND jour uniquement par 15 :
dcd$date_dcd <- manage_date_ND(dcd$DATE_DCD)
table(!is.na(dcd$date_dcd[dcd$FORM != "ID"]))

#---------------------------------------------------------------
#Données manquantes et doublons
table(table(dcd[dcd$FORM=="ID","PATIENT"]))#1 ligne ID par patient
table(tab_2dc <- table(dcd[dcd$FORM!="ID","date_dcd"]))#2 dates de décès similaire (voir si pose pb pour une des méthodes)
table(tab_2dc <- table(dcd[dcd$FORM!="ID","Patient"]))#les 150 patients dcd n'ont qu'une seule date de décès. Je déduis que les 212-150 autres patients sont vivants?

#-------------------------------------------------------------
#Merge date de décès et idpw_vni
dcd_date <- dcd[dcd$FORM!="ID",c("PATIENT","date_dcd")]
idpwvnidc <- merge(idpw_vni, dcd_date, by="PATIENT", all.x=T, all.y=F) #all.x=T pour garder les lignes sans date de DC

#-------------------------------------------------
#supprimer colonnes inutiles : diag .ccl
#quand diagnos.ccl renseigner, la date et le diagnostique sont identiques entre .diag et .ccl
idpwvnidc[!is.na(idpwvnidc$date.ccl),]
table(idpwvnidc$date.diag==idpwvnidc$date.diag)
idpwvnidc[idpwvnidc$date.diag==idpwvnidc$date.diag & !is.na(idpwvnidc$date.ccl),]

idpwvnidc <- idpwvnidc[, ! colnames(idpwvnidc) %in% c("DIAGNOS.ccl","date.ccl")]
saveRDS(idpwvnidc,file="data/idpwvnidc.rds")

#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

#Autres




# x<-as.list(vni)
# x<-sapply(x, as.character)
# dim(x)
# head(x)
# y<-ifelse(is.na(x) | x=="",0, 1)
# sy<-rowSums(y)
# table(sy)
# 
# x<-as.character(vni$DATE)
# unique(x)
# 
# x<-as.character(vni$PAT)
# length(unique(x))
# 
# y<-vni[!is.na(vni$DATE_VNI) & vni$DATE_VNI!="",]
# dim(y)
# x<-as.character(y$PAT)
# length(unique(x))
# 
# pat<-y$PAT;pat
# d<-Diag[Diag$PATIENT %in% x,]
# dim(d)
# head(d)
# d<-unique(d)
# dim(d)
# dd<-d[d$FORM=="DIAG",]
# dim(dd)
# table(dd$DIAGNOS)




#Je ne garde que les patients avec une date de vni
vni$datevni <- vni$DATE_VNI
#------code pour CSV uniquement
vni$datevni <- as.Date(as.character(vni$datevni), "%d/%m/%Y")
#------
summary(as.numeric(vni$datevni))
summary(vni$datevni)

vni<-vni[!is.na(vni$datevni),]
dim(vni) #Je tombe à 232 lignes


#je ne garde que les patients satisfaisant centre=SLA01 et diagnostic=SLA cad presents dans idpw
vni <- vni[vni$PATIENT %in% namesidpw, ]
dim(vni)
length(unique(vni$PATIENT)) #2034 patients d'avant sont bien dans le tableau vni


#Je supprime patients avec plusieurs lignes vni
table(tab<-table(vni$PAT))
x<-names(tab[tab>1])
vni<-vni[!(vni$PAT %in% x),]
dim(vni)
table(tab<-table(vni$PAT))




#------------------------------
#RECHERCHE AUTRES FICHIERS VNI
path <- "C:/Users/4051268/Documents/sauvegarde data/sla/data/"
.dir <- dir("C:/Users/4051268/Documents/sauvegarde data/sla/data/",full.names = T, recursive = T)
.dir <- .dir[str_sub(.dir, -3, -1)=="sas"]
v <- "vni"

for (i in .dir){
  fic<-i
  x<-scan(i, what=as.character(), sep="\n")
  i<-grep(v, tolower(x));x[i]
}
list_mining <- lapply(.dir, function(i){
  fic<-i
  x<-scan(i, what=as.character(), sep="\n")
  .i<-grep(v, tolower(x));
  return(c(i,x[.i]))
})

#dossier avec fichier vni trouve potentiellement interessant:
#.dir <- "C:/Users/4051268/Documents/sauvegarde data/sla/data/pat/"
#.dir <- "C:/Users/4051268/Documents/sauvegarde data/sla/data/trt/"
.dir <- "C:/Users/4051268/Documents/sauvegarde data/sla/data/visite/" #Svérifier si ce sont les même patients que les 202

#chargement du fichier CSV de ce dossier
vni2 <- read.csv2 (paste0(.dir,"PATIENT.csv")) 
names(vni2)[grep("VNI", names(vni2))]
table(table(vni2$DATEVNI))


#---------------------------------

# CREATION idpwv avec vni2

#Je ne garde que les patients avec une date de vni
vni2$datevni <- vni2$DATEVNI
#------code pour CSV uniquement
vni2$datevni <- as.Date(as.character(vni2$datevni), "%d/%m/%Y")
#------
summary(as.numeric(vni2$datevni))
summary(vni2$datevni)

vni2<-vni2[!is.na(vni2$datevni),]
dim(vni2) #Je tombe à 232 lignes


#je ne garde que les patients satisfaisant centre=SLA01 et diagnostic=SLA cad presents dans idpw
vni2 <- vni2[vni2$PATIENT %in% namesidpw, ]
dim(vni2)
length(unique(vni2$PATIENT)) #2034 patients d'avant sont bien dans le tableau vni2


#Je supprime patients avec plusieurs lignes vni2
table(tab<-table(vni2$PAT))
x<-names(tab[tab>1])
vni2<-vni2[!(vni2$PAT %in% x),]
dim(vni2)
table(tab<-table(vni2$PAT))

idpwv2<-merge(idpw, vni2, by="PATIENT", all.x=F, all.y=T)
dim(idpwv2)






#A faire :
#regarder si des sans diagnostic en PV deviennent des sla en visite supplementaire
sup <-Diag[Diag$MODULE!="PV",]
table(sup$DIAGNOS)
