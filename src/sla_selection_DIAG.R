source("src/libraries_SLA.R")
source("src/objects_SLA.R")
source("src/fonctions_SLA.R")


#------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#VERSION AVEC LES TABLES trt/PATIENT.csv et visite/PATIENT2.csv : OBTENTION TABLE BDDSLA


#vérifier si ce sont les même patients que les 202 de la version avec DIAG DCD VNI

pat <-  read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/pat/PATIENT.csv") #donnees demo
ttt <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/trt/PATIENT.csv") #donne VNI
visite <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/visite/PATIENT2.csv")

#1 Selection du center SLA01
visite <- visite[visite$CENTRE_M1=="SLA01",]
pat <- pat[pat$CENTRE=="SLA01",]

visite$PATIENT <- as.character(visite$PATIENT)
ttt$PATIENT <- as.character(ttt$PATIENT)
pat$PATIENT <- as.character(pat$PATIENT)

#ttt <- ttt[ttt$PATIENT %in% visite$PATIENT | ttt$PATIENT %in% pat$PATIENT, ]
#---------------------------------------------------------
#suppression des doublons
names2visite <- names(table(visite$PATIENT)[table(visite$PATIENT)>1])
names2ttt <- names(table(ttt$PATIENT)[table(ttt$PATIENT)>1]) #NB : parmi les 28 doublons ttt, 8 ne sont de toutes façons pas dans visite
names2pat <- names(table(pat$PATIENT)[table(pat$PATIENT)>1])

visite <- visite [! visite$PATIENT %in% names2visite,] 
ttt <- ttt [! ttt$PATIENT %in% names2ttt ,] #il était écrit names(names2ttt) => NULL donc il restait un doublon dans la base BDDSLADEM (donc 203 patients au lieu de 202)
pat <- pat [! pat$PATIENT %in% names2pat,] 
#------------------------------
#SELECT SLA
ALLDIAG <-visite[,colnames(visite)[str_sub(colnames(visite),1,8)=="DIAG_V_M" | str_sub(colnames(visite),1,12) == "NEW_DIAG_V_M"]]
pick_diag <- lapply(1: nrow(ALLDIAG),function(.x){
  .l <- ALLDIAG[.x,]
  .l <- .l[!is.na(.l)]
  if (length(.l)!=0) diag <- tail(.l,1)
  else diag <- NA
  return (diag)
})
ALLDIAG$diag <- as.vector(do.call(rbind,pick_diag))
ALLDIAG$PATIENT <- as.character(visite$PATIENT)

ALLDIAG <- ALLDIAG[, c("PATIENT","diag")] 

# ALLDIAG$diagSLA <- ifelse(as.numeric(ALLDIAG$identical_diag)==1,1,0) #1:SLA, 0: autre diagnostic ou non identical diagnostic, NA: pas de diagnostic
# ALLDIAG <- ALLDIAG[, c("PATIENT","diagSLA","identical_diag")] 


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

pick_date_diag <- lapply(1: nrow(ALLDATEDIAG),function(.x){
  .l <- ALLDATEDIAG[.x,]
  .l <- .l[!is.na(.l)]
  .l <- as.Date(.l) #Prend en charge les elements qui ne seraient pas des dates
  .l <- .l[!is.na(.l)]
  if (length(.l)!=0) {
    if (min(.l)!= head(.l,1)) date <- "head_not_min"
    else date <- head(.l,1)
  } else {
    date <- NA
  }
  return(date)
})
ALLDATEDIAG$identical_date_diag <- as.vector(do.call(rbind,pick_date_diag))
table(ALLDATEDIAG$identical_date_diag=="head_not_min")

ALLDATEDIAG$PATIENT <- as.character(visite$PATIENT)

no_identical_date_diag <- ALLDATEDIAG[ALLDATEDIAG$identical_date_diag =="head_not_min" & !is.na(ALLDATEDIAG$identical_date_diag),]

ALLDATEDIAG$date_diag <- as.Date(as.numeric(ALLDATEDIAG$identical_date_diag),origin="1970-01-01") #les no identical date sont transformées en NA par coercion
ALLDATEDIAG <- ALLDATEDIAG[ , c("PATIENT","date_diag","identical_date_diag")]
#ceux avec une première date qui n'est pas la date minimum ont la date supprimée (mais eux ne sont pas supprimés)

#------------
#Date des premiers symptomes

#dans la table pat :
#NB tous les patients dans visite ont une ligne pat, tous les patients dans ttt ont une ligne pat
first_symptND <- who_is_date_ND(vec_name =pat$PATIENT,vec_date = pat$FIRSTSYMPTOM)
pat$date_1sympt <- manage_date_ND(pat$FIRSTSYMPTOM)
DATE1SYMPT <- pat[ ,c("PATIENT","date_1sympt")] 

#------------
#Date VNI
ALLVNI <- ttt[,colnames(ttt)[grep("VNI",colnames(ttt))]]
vni_ND <- who_is_date_ND(vec_name =ttt$PATIENT,vec_date = ttt$DATEVNI)
ALLVNI$DATEVNI <- manage_date_ND(ALLVNI$DATEVNI)
table(ALLVNI[!is.na(ALLVNI$DATEVNI),"TTT_VNI"])
head(ALLVNI)
ALLVNI$PATIENT <- as.character(ttt$PATIENT)
ALLVNI <- ALLVNI[ ,c("PATIENT","TTT_VNI","DATEVNI","DATEVNI_STOP") ]

# #VNI via visite : 8 patients avec une date
# ALLVNIbis <- visite[,colnames(visite)[grep("DATEVNI",colnames(visite))]]
# for (i in colnames(ALLVNIbis)){
#   ALLVNIbis[,i] <- manage_date_ND(ALLVNIbis[,i])
# }
# pick_date_VNI <- lapply(1: nrow(ALLVNIbis),function(.x){
#   #browser()
#   .l <- ALLVNIbis[.x,]
#   .l <- .l[!is.na(.l)]
#   .l <- as.Date(.l)
#   .l <- .l[!is.na(.l)]
#   is.TRUE <- length(.l)!=0 #si date de décès discordant, prendre la plus récente
#   #if (is.TRUE) date <- as.Date(min(.l,na.rm=T))
#   if (is.TRUE) date <- as.Date(min(.l))
#   else date <- NA 
#   return(date)
# })
# ALLVNIbis$date_vni <- do.call(rbind,pick_date_VNI)
# ALLVNIbis$date_vni <- as.Date(as.numeric(ALLVNIbis$date_vni),origin="1970-01-01")

#--------------
#Date Deces
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
pick_date_DC <- lapply(1: nrow(ALLDC),function(.x){
  .l <- ALLDC[.x,]
  .l <- .l[!is.na(.l)]
  .l <- as.Date(.l)
  .l <- .l[!is.na(.l)]
  is.TRUE <- all(.l[1]==.l,na.rm=T) & length(.l)!=0
  if (is.TRUE) date <- as.Date(.l[1])
  else{
    if (length(.l)==0) date <- NA
    else date <- "no_identical_date"
  }
  return(date)
})
ALLDC$identical_date_dc <- as.vector(do.call(rbind,pick_date_DC))

pick_date_DC <- lapply(1: nrow(ALLDC),function(.x){
  #browser()
  .l <- ALLDC[.x,]
  .l <- .l[!is.na(.l)]
  .l <- as.Date(.l)
  .l <- .l[!is.na(.l)]
  is.TRUE <- length(.l)!=0 #si date de décès discordant, prendre la plus récente
  #if (is.TRUE) date <- as.Date(min(.l,na.rm=T))
  if (is.TRUE) date <- as.Date(tail(.l,1))
  else date <- NA 
  return(date)
})
ALLDC$date_dc <- do.call(rbind,pick_date_DC)
ALLDC$date_dc <- as.Date(as.numeric(ALLDC$identical_date_dc),origin="1970-01-01") #les no identical date sont transformées en NA par coercion

ALLDC$PATIENT <- as.character(visite$PATIENT)
#no_identical_DC <- ALLDC[ALLDC$identical_date_dc=="no_identical_date" & !is.na(ALLDC$identical_date_dc),]

ALLDC <- ALLDC[ , c("PATIENT","identical_date_dc","date_dc")]

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
      #if ( max(sort(.l))==max(.l) ) ddn <- max(.l) #0 inconsistency
      if ( tail(.l,1) == max(.l) ) ddn <- max(.l) #21 inconsistency
      else ddn <- "last_ddn_not_max"
    }
  } else {
    ddn <- .l$date_dc
  }
  return(ddn)
})
ALLCS$identical_ddn <- as.vector(do.call(rbind,pick_date_CS))

#ALLCS$PATIENT <- as.character(visite$PATIENT) #déjà dans ALLCS avec merge
no_identical_CS <- ALLCS[ALLCS$identical_ddn=="date inconsistency" & !is.na(ALLCS$identical_ddn),]
ALLCS$ddn <- as.Date(as.numeric(ALLCS$identical_ddn),origin="1970-01-01") #pas de date insconsistency donc pas de NA par coercion
#ALLCS <- ALLCS[ , c("PATIENT","ddn","identical_ddn")]
ALLCS <- ALLCS[ , c("PATIENT","identical_ddn","ddn","identical_date_dc","date_dc")]

#----------------------
#merge
BDD <- merge(x=ALLDIAG,y=ALLDATEDIAG, by="PATIENT", all.x = T, all.y=T)
BDD <- merge(BDD, DATE1SYMPT, by="PATIENT", all.x=T, all.y=T)
BDD <- merge(BDD, ALLVNI, by="PATIENT", all.x = T, all.y=T)
BDD <- merge(BDD, ALLCS, by="PATIENT", all.x = T, all.y=T) #ALLDC déjà mergé dans ALLCS

#BDD$TTT_VNI <- ifelse (!is.na(BDD$DATEVNI),1,BDD$TTT_VNI)
saveRDS(BDD,file="data/BDD.rds")

#selection
BDDSLA <- BDD[BDD$diag==1 & !is.na(BDD$diag) & !is.na(BDD$DATEVNI),]

saveRDS(BDDSLA,file="data/BDDSLA.rds")

selected_names_meth1 <- BDDSLA$PATIENT
saveRDS(selected_names_meth1,"data/selected_names_meth1.rds")

# #--------------------------------------------
# #Données démographiques
# pat$DOB <- manage_date_ND(pat$DOB)
# pat$SEX <- factor(pat$SEX)
# DEMO <- pat[,c("PATIENT","SEX","DOB")]
# 
# DEMO <- DEMO[DEMO$PATIENT %in% BDDSLA$PATIENT,]
# #DEMO$PATIENT[duplicated(DEMO$PATIENT)] #1 seul doublon
# # DEMO[DEMO$PATIENT=="G*********E",]
# # # PATIENT SEX  DOB
# # # 4533 G*******E   2 <NA>
# # # 8391 G*******E   2 <NA>
# 
# #suppression d'un des exemplaires du doublon
# DEMO <- unique(DEMO)
# BDDSLADEM <- merge(BDDSLA, DEMO, by="PATIENT", all.x=T,all.y=F)
# saveRDS(BDDSLADEM,file="data/BDDSLADEM.rds")


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
DCD <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/20150827/DCD.csv")

Diag$PATIENT <- as.character(Diag$PATIENT)
VNI$PATIENT <- as.character(VNI$PATIENT)
DCD$PATIENT <- as.character(DCD$PATIENT)


#-------------------------------------------------------------------
#SELECTION DES PATIENTS SLA

#Je ne garde que les patients de la pitié : vecteur namesPSL
id<-Diag[Diag$FORM=="ID",]
id <- id[id$CENTRE=="SLA01", ]
namesPSL <- unique(id[id$CENTRE=="SLA01", "PATIENT"])

#CREATION TABLEAU PV(DIAG et conclusion confondu) PV1(DIAG) et PV2(conclusion) : Diagnostic et date de diagnostic

#Je sélectionnes les lignes premiÃ¨re visite (et non visis_supl) correspondant au diag : FORM=DIAG et FORM=conclusion
pv<-Diag[Diag$MODULE=="PV" & Diag$FORM!="ID", ] 

#Je selectionne les lignes 
#pv<-pv[pv$FORM!="ID",]

#Je sélectionnes patients de la pitié:
pv<-pv[pv$PATIENT %in% namesPSL, ]

#Je supprime les lignes sans Diagnostic 
pv <- pv[!is.na(pv$DIAGNOS),] 


#MERGE DIAGNOSTIC DE LIGNE DIAG ET DE LIGNE CONCLUSION
pv1<-pv[pv$FORM=="DIAG", c("PATIENT", "DATE_DIAG", "DIAGNOS")]
pv2<-pv[pv$FORM!="DIAG", c("PATIENT", "DATE_DIAG", "DIAGNOS")]
pw<-merge(pv1, pv2, by="PATIENT", all=T, suff=c(".diag", ".ccl")) #.diag pour ligne diag, .ccl pour ligne conclusion

table(pw$DIAGNOS.diag, pw$DIAGNOS.ccl, exclude=NULL, deparse.level = 2) #Je regarde si les diagnostics concordent

#Selection des lignes avec DIAGNOSTIC DE SLA 
pw <-pw [(pw$DIAGNOS.diag=="1" | is.na(pw$DIAGNOS.diag)) & (pw$DIAGNOS.ccl=="1" | is.na(pw$DIAGNOS.ccl)) & (!is.na(pw$DIAGNOS.ccl)| !is.na(pw$DIAGNOS.diag)),]
pw$diag <- pmin(pw$DIAGNOS.diag,pw$DIAGNOS.ccl,na.rm=T)


#DATE DIAGNOSTIC
who_is_date_ND(pw$PATIENT,pw$DATE_DIAG.diag)
pw$DATE_DIAG.diag <- manage_date_ND(vec = pw$DATE_DIAG.diag)
pw$DATE_DIAG.ccl <- manage_date_ND(vec = pw$DATE_DIAG.ccl)
#je garde date la plus ancienne
#pw$date_diag <- ifelse(is.na(pw$DATE_DIAG.diag) & is.na(pw$DATE_DIAG.ccl), NA, apply(pw[ ,c("DATE_DIAG.diag","DATE_DIAG.ccl")], 1, FUN=min, na.rm=T))
pw$date_diag <- pmin(pw$DATE_DIAG.diag,pw$DATE_DIAG.ccl,na.rm=T)

#8 Patients SLA ont 2 lignes de date => je les garde mais je met la date NA
table(tab2<-table(pw$PATIENT))
names_double_diag <- names(tab2[tab2>1])
#pw <- pw [!pw$PATIENT %in% names_double_diag, ]
pw [pw$PATIENT %in% names_double_diag, "date_diag"] <- NA
pw <- pw[ , colnames(pw)%in%c("PATIENT","diag","date_diag")]
pw <- unique(pw)

names_sla_psl <- unique(pw$PATIENT) #Patients sla de la pitié

#--------------------------------------------------------------------------
#id : ligne ID uniquement avec patient sla de la pitié

table(tabID <- table(SLA$PATIENT[SLA$FORM=="ID"]))
namestabID <- names(tabID[tabID>1])

#Patients avec plusieurs identifiants: ce sont des doublons identiques, je garde un des 2
Diag[Diag$PATIENT %in% namestabID & Diag$FORM=="ID",]
ID <- unique(Diag[(Diag$PATIENT %in% names_sla_psl) & Diag$FORM=="ID", c("PATIENT","CENTRE")])

#------------------------------------------------------------------
# MERGE ID ET pw -> idpw
idpw<-merge(ID, pw, by="PATIENT", all.x=T, all.y=T)
dim(idpw)
names_idpw <- unique(idpw$PATIENT)
#---------------------------------
# CREATION idpwv : patient SLA de la pitié avec vni
vni<-VNI

vni <- vni[vni$PATIENT %in% names_idpw, ] #Je prends les patients SLA qui viennent de la
table(table(unique(vni[vni$FORM== "ID", "PATIENT" ]))) #J'ai bien une ligne ID par patient SLA (seulement 6039 patients ont données VNI)

vni_vent <- vni[vni$PATIENT %in% names_idpw & vni$FORM!= "ID", ]
names_vni <- unique(vni_vent$PATIENT)

# 16 patients ont 2 lignes avec date VNI (une date première préventilation et une date préventilation) => Je suprrime
table(table(vni_vent$PATIENT)) 
table(tab<-table(vni_vent$PATIENT))
names_vni_duplicated <- names(tab[tab>1])

vni_vent$DATEVNI <- manage_date_ND(vni_vent$DATE_VNI) #enlève les ND s'il y en a (on a vu plus haut qu'il n'y en a pas) et transforme en date

vni_vent <- vni_vent[ !vni_vent$PATIENT %in% names_vni_duplicated, colnames(vni_vent) %in% c("PATIENT","MODULE","FORM","DATEVNI") ] 

#MERGE idpw et vni_vent
idpw_vni <-merge(idpw, vni_vent, by="PATIENT", all.x=F, all.y=F)
saveRDS(idpw_vni, "data/idpw_vni.rds")

#nom des patients sélectionnés:
selected_names <- unique(idpw_vni$PATIENT)
saveRDS(selected_names, "data/selected_names.rds")
#copié dans : C:\Users\4051268\Documents\sauvegarde data\patients_analysis

# #--------------------------------------------------------------------------------------
#RAJOUT DES DATES DE DECES

dcd <- DCD[DCD$FORM!="ID",]
ND_dcd <- who_is_date_ND (dcd$PATIENT,dcd$DATE_DCD)
dcd$date_dc <- manage_date_ND(dcd$DATE_DCD)

#selected_names : patients de la pitié avec diagnostic de SLA(1) et avec une date de début de vni(fichier VNI)
dcd <- dcd[ dcd$PATIENT %in% selected_names, ]

#doublons :identique => je garde 1 exemplaire
name2dcd <- names(table(dcd$PATIENT)[table(dcd$PATIENT)>1]) #les 150 patients dcd n'ont qu'une seule date de décès. Je déduis que les 212-150 autres patients sont vivants?
dcd[dcd$PATIENT %in% name2dcd,]
dcd <- unique(dcd)

dcd_date <- dcd[,c("PATIENT","date_dc")]
#-------------------------------------------------------------
#Merge date de décès et idpw_vni
idpwvnidc <- merge(idpw_vni, dcd_date, by="PATIENT", all.x=T, all.y=F) #all.x=T pour garder les lignes sans date de DC

#-------------------------------------------------
saveRDS(idpwvnidc,file="data/idpwvnidc.rds")

selected_names_meth2 <- unique(idpwvnidc$PATIENT)
saveRDS(selected_names_meth2,"data/selected_names_meth2.rds")


#----------------------------------------------------------------------
#----------------------------------------------------------------------
#FUSION DES BASES

#Merge de BDDSLA et idpwvnidc

BDIAG <- merge (BDDSLA,idpwvnidc, by= "PATIENT",all=T,suffixes = c(1,2))

BDIAG <- BDIAG[ ,c("PATIENT","diag1","diag2","date_diag1", "date_diag2","date_1sympt","TTT_VNI", "MODULE","DATEVNI1","FORM","DATEVNI2","date_dc1", "date_dc2","ddn")]
                                                                                                                          

view_coldiff <- function (data,col1,col2) {
  col1bis <- data[ , col1]
  col2bis <- data[ , col2]
  res <- data [(col1bis != col2bis) & !is.na(col1bis) & !is.na(col2bis),]
  return(res[, c("PATIENT",col1,col2)])
} 

view_coldiff(BDIAG,"diag1","diag2")#ok
view_coldiff(BDIAG,"date_diag1","date_diag2")
diffVNI <- view_coldiff(BDIAG,"DATEVNI1","DATEVNI2")
dim(diffVNI)
table(diffVNI$DATEVNI1>diffVNI$DATEVNI2)
median(abs(diffVNI$DATEVNI1-diffVNI$DATEVNI2)) 

view_coldiff(BDIAG,"date_dc1","date_dc2")#ok

#certains patients ont une date de VNI antérieure aux premiers symptomes
VNIbsympt <- BDIAG[ ((BDIAG$date_1sympt > BDIAG$DATEVNI1) & !is.na(BDIAG$DATEVNI1) ) | ((BDIAG$date_1sympt > BDIAG$DATEVNI2) & !is.na(BDIAG$DATEVNI2)) & !is.na(BDIAG$date_1sympt), ]
write.table(print(VNIbsympt),file="clipboard",sep="\t")

#1 patient a 2 date VNI et une des 2 est antérieure à VNI (la 1 justement...)
diffVNI$PATIENT[diffVNI$PATIENT %in% VNIbsympt$PATIENT]

BDIAG$diag <- ifelse(!is.na(BDIAG$diag1),BDIAG$diag1, BDIAG$diag2)
BDIAG$date_diag <- ifelse(!is.na(BDIAG$date_diag1),BDIAG$date_diag1, BDIAG$date_diag2)
BDIAG$date_diag <- as.Date(as.numeric(BDIAG$date_diag),origin="1970-01-01")
BDIAG$date_dc <- ifelse(!is.na(BDIAG$date_dc1),BDIAG$date_dc1, BDIAG$date_dc2)
BDIAG$date_dc <- as.Date(as.numeric(BDIAG$date_dc),origin="1970-01-01")
BDIAG$DATEVNI <- ifelse(!is.na(BDIAG$DATEVNI1),BDIAG$DATEVNI1, BDIAG$DATEVNI2)
BDIAG$DATEVNI <- as.Date(as.numeric(BDIAG$DATEVNI),origin="1970-01-01")


bd <- BDIAG[ ,c("PATIENT","diag","date_diag","date_1sympt","TTT_VNI","DATEVNI","date_dc","ddn")]

saveRDS(bd,"data/BDIAG.rds")

dim(BDIAG)
#-----------------------------
#-----------------------------
#-----------------------------
#-----------------------------


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
for (i in .dir) {
  num <- which(.dir_csv==i)
  a <- readRDS(paste0("data/ddn/ddn",num,".rds"))
  assign(paste0("ddn",num),a[[1]])
}

#Pour merger les ddn
#nom des dataframe ddn 
dfddn <- str_sub(dir("data/ddn"),1,-5)[sapply(dir("data/ddn"),function(x)is.data.frame(get(str_sub(x,1,-5))))]
#merge
for (i in dfddn){
  #num <- str_sub(i,4,-1)
  #num <- which(str_sub(dir("data/ddn"),1,-5)==i)
  num <- which(dfddn==i)
  .bd <- get(i)
  ddn_tot <- if(num==1) .bd else merge(ddn_tot, .bd, by="PATIENT", suffixes= c(num-1,num),all=TRUE)
  #if (num==length(dfddn)) 
}
#transformation des colonnes en date si pas déjà fait
for (i in colnames(ddn_tot)[-1]){
  ddn_tot[,i] <- as_date(ddn_tot[,i])
  ddn_tot[,i] <- ifelse (ddn_tot[,i]>Sys.Date(), NA, ddn_tot[,i])
}

ddn_tot$ddn <- apply(ddn_tot[,grep("max", colnames(ddn_tot))],1,max,na.rm=T) 
ddn_tot <- unique(ddn_tot[ ,c("PATIENT","ddn")])
ddn_tot$ddn <- as_date(ddn_tot$ddn)
ddn_tot <- na.omit(ddn_tot)

saveRDS(ddn_tot, "data/bdd_to_merge/ddn_tot.rds")

#-------------
#BASE DE DONNEES DECES
#listes_brut <- lapply(bdds, which_col,"DAT","DC",type="merge") #selectionne DATEDCOMPLAL : ???
lapply(bdds, which_col,"DAT","DCD",type="explo")
listes_brut <- lapply(bdds, which_col,"DAT","DCD",type="merge")

#tous les décès : res
#faire un merge dans listes non NA
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  #browser()
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}

bdd_dcd <- get_min_max(data = res, fun = "min")
bdd_dcd$date_dc <- manage_date_ND(bdd_dcd$min)
bdd_dcd <- unique(bdd_dcd[ , c("PATIENT","date_dc")])
bdd_dcd <- na.omit(bdd_dcd)

saveRDS(bdd_dcd, "data/bdd_to_merge/bdd_dcd.rds")

#--------------
#BASE DE DONNEES FIRST SYMPT
lapply(bdds, which_col,"FIRSTSYMPTOM",type="explo")
listes_brut <- lapply(bdds, which_col,"FIRSTSYMPTOM",type="merge")
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  #browser()
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}
bdd_1symp <- res
bdd_1symp$FIRSTSYMPTOM <- manage_date_ND(bdd_1symp$FIRSTSYMPTOM)
bdd_1symp <- unique(bdd_1symp)
bdd_1symp <- na.omit(bdd_1symp)

saveRDS(bdd_1symp, "data/bdd_to_merge/bdd_1symp.rds")

#---------------
#BASE DE DONNEES VNI

#DAT1VNI_MEO_V_M et DAT2VNI_MEO_V_M sont NA
apply(apply(visite[ ,grep("VNI_MEO_", colnames(visite))],2,is.na),2,sum)
# > dim(visite)
# [1] 4243 9700

#Je garde :
#lapply(bdds, which_col,"VNI","DATE",type="explo")
#"DATEVNI_V_M", "DATEVNI" et "DATE_VNI"
listes_brut <- lapply(bdds, which_col,string1="DATE", string2="VNI",string3="STOP",type="merge")
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  #browser()
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}
bdd_debVNI <- get_min_max(data = res, fun = "min")
bdd_debVNI$DATEVNI <- manage_date_ND(bdd_debVNI$min)
bdd_debVNI <- unique(bdd_debVNI[,c("PATIENT","DATEVNI")])
bdd_debVNI <- na.omit(bdd_debVNI)

saveRDS(bdd_debVNI, "data/bdd_to_merge/bdd_debVNI.rds")


#-------------
#BASE DE DONNEE DATE DIAG
listes_brut <- lapply(bdds, which_col,string1="DAT", string2="DIAG",type="merge")
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  #browser()
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}
bdd_DATEDIAG <- get_min_max(data = res, fun = "min")
bdd_DATEDIAG$date_diag <- manage_date_ND(bdd_DATEDIAG$min)
bdd_DATEDIAG <- unique(bdd_DATEDIAG[,c("PATIENT","date_diag")])
bdd_DATEDIAG <- na.omit(bdd_DATEDIAG)

saveRDS(bdd_DATEDIAG, "data/bdd_to_merge/bdd_DATEDIAG.rds")

#-------------
#BASE DE DONNEE DIAG : je ne prend que les diag de visite car je veux être sûre que c'est bien le dernier diag dispo qui est pris
listes_brut <- lapply(bdds, which_col,string1="DIAG_V_M",string3="DAT",type="explo")
#je veux bdd9

ALLDIAG <-bdd9[,colnames(bdd9)[str_sub(colnames(bdd9),1,8)=="DIAG_V_M" | str_sub(colnames(bdd9),1,12) == "NEW_DIAG_V_M"]]
pick_diag <- lapply(1: nrow(ALLDIAG),function(.x){
  .l <- ALLDIAG[.x,]
  .l <- .l[!is.na(.l)]
  if (length(.l)!=0) diag <- tail(.l,1)
  else diag <- NA
  return (diag)
})
ALLDIAG$diag <- as.vector(do.call(rbind,pick_diag))
ALLDIAG$PATIENT <- as.character(bdd9$PATIENT)
bdd_diag <- unique(ALLDIAG[, c("PATIENT","diag")] )

#pb : certains patients ont une ligne de diag NA => je supprime tous les NA
table(tab<-table(bdd_diag$PATIENT))
pat2diag <- names(tab)[tab>1]
bdd_diag[bdd_diag$PATIENT %in% pat2diag,]

bdd_diag <- na.omit(bdd_diag)
saveRDS(bdd_diag, "data/bdd_to_merge/bdd_diag.rds")

#-------------
#MERGE
newdir <- dir("F:/to push/sla_git/data/bdd_to_merge/")
newdir <- str_sub(newdir,1,-5)


for (i in 1:length(newdir)) {
  #browser()
  data <- get(newdir[i])
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}

head(res)

BASE_TOT <- res

saveRDS(BASE_TOT, "data/BASE_TOT.rds")


#------------------------------------
#------------------------------------
#SELECT SLA from BASE_TOT

BASE_SLA <- BASE_TOT[BASE_TOT$diag==1 & !is.na(BASE_TOT$diag) & !is.na(BASE_TOT$DATEVNI),]

#doublons
table(tab <- table(BASE_SLA$PATIENT))
namesdoublons <- names(tab)[tab>1]
names2 <- names(tab)[tab==2]
BASE_SLA[BASE_SLA$PATIENT%in% names2,] #les doublons sont foireux, je les supprime

BASE_SLA <- BASE_SLA[!BASE_SLA$PATIENT %in% namesdoublons, ]

saveRDS(BASE_SLA, "data/BASE_SLA.rds")


#----------------------------------
#----------------------------------
#Ajout des baseline
#F:\to push\sla_git\data\baseline_to_merge

BASE_SLA <- readRDS("data/BASE_SLA.rds")

#-------------
#BASE DE DONNEE DOB
listes_brut <- lapply(bdds, which_col,string1="DOB",type="merge")
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  #browser()
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}

#certains patients n'ont pas la même date de naissance...
res2 <- na.omit(res)
res2[as.character(res2$DOB.x)!=as.character(res2$DOB.y),]
pat2dob <- unique(res2[as.character(res2$DOB.x)!=as.character(res2$DOB.y), "PATIENT"])

bdd_DOB <- get_min_max(data = res, fun = "min") #je ne prend pas vmt la min, ça me permet de prendre l'une qd l'autre est NA
bdd_DOB <- bdd_DOB[!bdd_DOB$PATIENT %in% pat2dob, ] #je suprrime les date incohérentes
bdd_DOB$DOB <- manage_date_ND(bdd_DOB$min)
bdd_DOB <- unique(bdd_DOB[ ,c("PATIENT","DOB")])
bdd_DOB <- na.omit(bdd_DOB)

saveRDS(bdd_DOB, "data/baseline_to_merge/DOB.rds")

#--------------
#sexe
listes_brut <- lapply(bdds, which_col,string1="SEX", type="merge")
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  #browser()
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}
ALLSEX <-res[ ,!colnames(res)%in%"PATIENT"]
pick_sex <- lapply(1: nrow(ALLSEX),function(.x){
  .l <- ALLSEX[.x,]
  .l <- .l[!is.na(.l)]
  #browser()
  if (length(.l)!=0 & all(.l[1]==.l)) sex <- as.integer(tail(.l,1))
  # else {
  #   if(length(.l)!=0) sex <- "not sure"
  #   else sex <- NA
  # }
  else sex <- NA
  return (sex)
})

ALLSEX$sex_def <- as.vector(do.call(rbind,pick_sex))
ALLSEX$sex_def <- as.integer(ALLSEX$sex_def)
ALLSEX$PATIENT <- as.character(res$PATIENT)

ALLSEX[ALLSEX$sex_def=="not sure",] #6 not sure, it's the same personne, first name=marie
table(ALLSEX$sex_def, useNA = "a") #no NA except the 6 not sure
ALLSEX <- unique(ALLSEX[ ,c("PATIENT","sex_def")])
saveRDS(ALLSEX, "data/baseline_to_merge/sexe.rds")

#--------------
#Forme
v <- "fam"

for (i in .dir_sas){
  fic<-i
  x<-scan(i, what=as.character(), sep="\n")
  j<-grep(v, tolower(x));print(c(i,x[j]))
}


#bdd6 : SOD1 = "Mutation SOD1" #pb : la mutation peut être de novo, et autre mutation peut causer la maladie 
as.numeric(as.character(bdd6[ ,str_sub(colnames(bdd6), 1, 4)=="SOD1"]))

#bdd6 : ATCD_FAMIL_L1 à 6 : 1= SLA
listes_brut <- lapply(bdds, which_col,string1="ATCD_FAMIL_L", type="merge")
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  #browser()
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}

ALLATCD <-res[ ,!colnames(res)%in%"PATIENT"]
pick_atcd <- lapply(1: nrow(ALLATCD),function(.x){
  .l <- ALLATCD[.x,]
  .l <- .l[!is.na(.l)]
  if (length(.l)!=0){
    if (length(grep("SLA",.l,ignore.case = TRUE))!=0) atcd <- 1
    else atcd <- NA
  } else {
    atcd <- NA
  }
  return (atcd)
})

ALLATCD$familial <- as.vector(do.call(rbind,pick_atcd))
ALLATCD$PATIENT <- as.character(res$PATIENT)
ALLATCD <- unique(ALLATCD[ , c("PATIENT","familial")])
saveRDS(ALLATCD, "data/baseline_to_merge/forme.rds")

#------------
#site of onset
v <- "bulbaire"

for (i in .dir_sas){
  fic<-i
  x<-scan(i, what=as.character(), sep="\n")
  j<-grep(v, tolower(x));print(c(i,x[j]))
}

table(bdd6$LIEUDEB,useNA = "a")

site_onset <- bdd6[ ,c("PATIENT","LIEUDEB")] 

site_onset$LIEUDEB_recode <- Recode(as.factor(site_onset$LIEUDEB), "1 = 'bulbar';2 = 'cervical'; 10:15 = 'lower limb onset'; 3 = 'respiratory'; 4:9 = 'upper limb onset'")
#site_onset$LIEUDEB_recode <- recode(as.factor(site_onset$LIEUDEB), "1" = "bulbar", 10:15 = "lower limb onset", "2" = "cervical", "3" = "respiratory", as.character(c(4,5,6,7,8,9)) = "upper limb onset")

table(site_onset$LIEUDEB_recode, useNA = "a")
table(site_onset$LIEUDEB, useNA = "a")

site_onset <- unique(site_onset[ , c("PATIENT","LIEUDEB_recode")])
saveRDS(site_onset, "data/baseline_to_merge/site_onset.rds")


#--------------
#patients taking riluzole
listes_brut <- lapply(bdds, which_col,string1="rilu", type="explo")

#bdd6 
[3] "RILUZ"                                                               
[4] "POSORILU"                                                            
[5] "DATDRILU"                                                            
[6] "DATFRILU" 
#bdd8
[3] "RILUZ"                                                               
[4] "TTT_RILU_LNUM_L1"                                                    
[5] "POSORILU_L1"                                                         
[6] "DATDRILU_L1"                                                         
[7] "DATFRILU_L1" 
#bdd9
[3] "RILUZ_V_M1"                                                             
[4] "POSORILU_V_M1"                                                          
[5] "DATDRILU_V_M1"                                                          
[6] "DATFRILU_V_M1"   

table(bdd6$RILUZ,useNA = "a") #52
table(bdd8$POSORILU_L1,useNA = "a") #1955
table(bdd9$RILUZ_V_M1,useNA = "a") #

# #personnaliser
# listes_brut <- lapply(bdds, which_col,string1="RILUZ_V_M", type="merge")
# #
# 
# listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
# for (i in 1:length(listes_net)) {
#   #browser()
#   data <- listes_net[[i]]
#   res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
# }
# 
# ALLpick <-res[ ,!colnames(res)%in%"PATIENT"]
# pick_data <- lapply(1: nrow(ALLpick),function(.x){
#   .l <- ALLpick[.x,]
#   .l <- .l[!is.na(.l)]
#   if (length(.l)!=0){
#     #personnaliser
#     if (any(.l==1)) data <- 1
#     else data <- 0
#     #
#   } else { #si laligne est vide
#     data <- NA
#   }
#   return (data)
# })
# ALLpick$data <- as.vector(do.call(rbind,pick_data))
# ALLpick$PATIENT <- as.character(res$PATIENT)
# ALLpick <- unique(ALLpick[ , c("PATIENT","data")])
# 
# #personnaliser
# colnames(ALLpick)[-1] <- "rilu"
# names_dataset <- "RILU"
# #
# 
# assign(names_dataset,ALLpick)




#Les patients sont rilu si une date est renseignée

#personnaliser
listes_brut <- lapply(bdds, which_col,string1="DAT",string2="RILU", type="merge")
#

listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}

ALLpick <-res[ ,!colnames(res)%in%"PATIENT"]
pick_data <- lapply(1: nrow(ALLpick),function(.x){
  .l <- ALLpick[.x,]
  .l <- .l[!is.na(.l)]
  if (length(.l)!=0){
    #personnaliser
    data <- 1
  } else { #si laligne est vide
    data <- NA
  }
  return (data)
})
ALLpick$data <- as.vector(do.call(rbind,pick_data))
ALLpick$PATIENT <- as.character(res$PATIENT)
ALLpick <- unique(ALLpick[ , c("PATIENT","data")])

#personnaliser
colnames(ALLpick)[-1] <- "rilu"
names_dataset <- "RILU"
#

assign(names_dataset,ALLpick)

saveRDS(ALLpick, paste0("data/baseline_to_merge/",names_dataset,".rds"))

#----------------
#ALSFRS-R score
#personnaliser
listes_brut <- lapply(bdds, which_col,string1="FRSDT", type="explo")
#

listes_net <- list(bdd6[,c("PATIENT","ALS")],bdd9[,c("PATIENT","ALS_V_M1")])
for (i in 1:length(listes_net)) {
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}

ALLpick <-res[ ,!colnames(res)%in%"PATIENT"]
pick_data <- lapply(1: nrow(ALLpick),function(.x){
  .l <- ALLpick[.x,]
  .l <- .l[!is.na(.l)]
  if (length(.l)!=0){
    data <- .l[1] #si 2 score, alors c'est celui du fichier pat qui est pris. Si un seul renseigné, alors on prend le seul qui existe. 
  } else { #si laligne est vide
    data <- NA
  }
  return (data)
})
ALLpick$data <- as.vector(do.call(rbind,pick_data))
ALLpick$PATIENT <- as.character(res$PATIENT)
ALLpick <- unique(ALLpick[ , c("PATIENT","data")])

#personnaliser
colnames(ALLpick)[-1] <- "ALS_FRS_score"
names_dataset <- "ALS_FRS"
#

assign(names_dataset,ALLpick)
saveRDS(ALLpick, paste0("data/baseline_to_merge/",names_dataset,".rds"))

#----------------
#norris bulbar score
v <- "bulbaire"

for (i in .dir_sas){
  fic<-i
  x<-scan(i, what=as.character(), sep="\n")
  j<-grep(v, tolower(x));print(c(i,x[j]))
}

table(bdd6$E_BULBAIRE,useNA="a")
ALLpick <- bdd6[ ,c("PATIENT","E_BULBAIRE")]
names_dataset <- "E_BULBAIRE"
assign(names_dataset,ALLpick)
saveRDS(ALLpick, paste0("data/baseline_to_merge/",names_dataset,".rds"))

#---------------
#dyspnoea at rest and orthopnoea

v <- "dyspn"
v <- "orthopn"

for (i in .dir_sas){
  fic<-i
  x<-scan(i, what=as.character(), sep="\n")
  j<-grep(v, tolower(x));print(c(i,x[j]))
}

#pat : ALS_ALS_dyspne 1=dyspnee de repos
# 0 = 'Difficulté amenant à envisager ventilation' 
# 1 = 'Dyspnée de repos' 
# 2 = 'Dyspnée lors de l alimentation, habillage, bain' 
# 3 = 'Dyspnée d effort à la marche' 
# 4 = 'Normale' 
#dyspnee de repos : je garde 1

#pat : ALS_ALS_orthopne :
# 0 = 'Ne peut pas dormir' 
# 1 = 'Ne peut dormir qu assis' 
# 2 = 'Besoin de plus de deux oreillers' 
# 3 = 'Quelques difficultés nocturnes' 
# 4 = 'Aucune' 
#orthopnee : je garde 0 1 2 3 (3 à discuter)

find_table ("pat")
dysp_orth <- bdd6 [ , c("PATIENT","ALS_ALS_dyspne","ALS_ALS_orthopne")]

dysp_orth$dyspnee_repos <- Recode(dysp_orth$ALS_ALS_dyspne, "1=1;NA=NA;else=0")
dysp_orth$orthopnee <- Recode(dysp_orth$ALS_ALS_orthopne, "0:3=1; NA=NA ;else=0")
dysp_orth <- dysp_orth[ , c("PATIENT","dyspnee_repos","orthopnee")]
saveRDS(dysp_orth, paste0("data/baseline_to_merge/dysp_orth.rds"))

#---------
#Sitting FVC (CRF)
scan_notebook("cvf_assis",.dir_sas)

find_table("pre")

#lequel des trois donne sitting FVC(%predicted)?
quantile(as.numeric(as.character(bdd7$CVF_ASSIS_ESR_PP)),na.rm=T)
quantile(as.numeric(as.character(bdd7$CVF_ASSIS_OBSV_PP)),na.rm=T)
quantile(as.numeric(as.character(bdd7$CVF_ASSIS_THEO_PP)),na.rm=T)
#theo est bien le %predicted (comme dans lancet):
quantile(as.numeric(as.character(bdd7$CVF_ASSIS_OBSV_PP))/as.numeric(as.character(bdd7$CVF_ASSIS_ESR_PP))*100,na.rm=T)

quantile(as.numeric(as.character(bdd7$CVF_COUCHE_PP)),na.rm=T)
quantile(as.numeric(as.character(bdd7$CVF_ERS_PP)),na.rm=T) #espéré est la même que assis mais c'et plausible
quantile(as.numeric(as.character(bdd7$CVF_THEO_PP)),na.rm=T) #est-ce bien la CVF couché (%predicted)?
quantile(as.numeric(as.character(bdd7$CVF_COUCHE_PP))/as.numeric(as.character(bdd7$CVF_ERS_PP))*100,na.rm=T) #oui c'est quasi pareil

CVF <- bdd7[ ,c("PATIENT", "DATE_RESP_PP", "CVF_ASSIS_THEO_PP","CVF_THEO_PP","SNIP_PP", "SNIP_THEO_PP")]
colnames_bdd <- c("PATIENT","DATE_CVF","CVF_ASSIS_perc_pred","CVF_COUCHE_perc_pred","SNIP_cmH2O","SNIP_perc_pred")
colnames(CVF) <- colnames_bdd

for (i in colnames(CVF)) {
  CVF[,i] <- as.character(CVF[,i])
  CVF[,i] <- ifelse(CVF[,i]=="",NA,CVF[,i])
}
CVF$keep <- ifelse(is.na(CVF$CVF_ASSIS_perc_pred) & is.na(CVF$CVF_COUCHE_perc_pred) & is.na(CVF$SNIP_cmH2O) & is.na(CVF$SNIP_perc_pred),0,1)
CVF <- CVF[CVF$keep==1, colnames_bdd]

saveRDS(CVF, paste0("data/baseline_to_merge/CVF_SNIP.rds"))


#recherche des autres sniff nasal inspiratory pressure
listes_brut <- lapply(bdds, which_col,string1="SNIP", string2="PP", type="merge")
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}
#quand SNIPP_PP n'est pas renseigné, les SNIP_FAIBLE non plus donc SNIPP_PP suffit
res%>% filter(is.na(SNIP_PP) & is.na(SNIP_FAIBL_3_PP))
for (i in 1:3) {
  print(res[ is.na(res$SNIP_PP) & !is.na(res[ ,paste0("SNIP_FAIBL_",i,"_PP")]),])
}
#ok tout est déjà dans CVF

#-----------
#maximal inspiratory pressure (PIMAX probablement)
data <- bdd7[ ,c("PATIENT", "PIMAX_PP", "PIMAX_THEO_PP")]
colnames_data <- c("PATIENT","PIMAX_cmH2O","PIMAX_perc_pred")
colnames(data) <- colnames_data

for (i in colnames(data)) {
  data[,i] <- as.character(data[,i])
  data[,i] <- ifelse(data[,i]=="",NA,data[,i])
}
data$keep <- ifelse(is.na(data[,2]) & is.na(data[,3]),0,1)
data <- unique(data[data$keep==1, colnames_data])
names_dataset <- "PIMAX"
assign(names_dataset,data)
saveRDS(data, paste0("data/baseline_to_merge/",names_dataset,".rds"))

#------------
table(bdd7$DYSP_EFFORT_PP)




#RECHERCHE DE LA SIGNIFICATION DE PP dans pre/
#24 patients qui ont une datexam (du fichier pat/) ultérieure à datexa_v_m1 de visite/
table_datexam <- merge(bdd6[ ,c("PATIENT","DATEXAM")],bdd9[ ,c("PATIENT","DATEXAM_V_M1")],by="PATIENT", all=FALSE) #pb la date d'exam du fichier pat est parfois 
#table_datexam[,2:3] <- sapply(2:3,function(x)manage_date_ND(table_datexam[,x])) #marche pas on obtient des dates R
for (i in 2:3){
  table_datexam[,i] <-manage_date_ND(table_datexam[,i])
}
table(table_datexam$DATEXAM<table_datexam$DATEXAM_V_M1,useNA = "a")
table(is.na(table_datexam$DATEXAM))#307 NA
table(is.na(table_datexam$DATEXAM_V_M1))

datexam2 <- merge(bdd7[ ,c("PATIENT","DATE_RESP_PP")],bdd9[ ,c("PATIENT","DATEXAM_V_M1")],by="PATIENT", all=FALSE)
for (i in 2:3){
  datexam2[,i] <-manage_date_ND(datexam2[,i])
}
table(is.na(datexam2$DATE_RESP_PP))#2627 NA
table(datexam2$DATE_RESP_PP<datexam2$DATEXAM_V_M1,useNA = "a")
table((datexam2$DATE_RESP_PP-datexam2$DATEXAM_V_M1)/365,useNA = "a")
table((datexam2$DATE_RESP_PP-datexam2$DATEXAM_V_M1)/365>1)#177 eval respi 1 an après visite du premier mois
which((datexam2$DATE_RESP_PP-datexam2$DATEXAM_V_M1)/365>9)
datexam2[1239,] #DATE_RESP_PP : 2012-11-29 ; DATEXAM_V_M1 : 2003-09-17
datexam2[3020,] #DATEXAM_V_M1 : 1201-11-02


# v$PATIENT <- as.character(v$PATIENT)
# #ALLDATE[ALLDATE$PATIENT=="Z********E",] #EXAM M1 en 2009 et d?c?s en 1998



#Afficher les colonnes avec dates nonNA
head(v[ ,COLDATEnonNA])

#Afficher l'histoire d'un patient
t(v[1:5 ,COLDATEnonNA])

#------------------------------
#------------------------------

#observation des dates pour les patients incohérents:
ALLDATE <- visite[,colnames(visite)[grep("DAT",colnames(visite))]]
ALLDATE$PATIENT <- as.character(visite$PATIENT)
#ALLDATE[ALLDATE$PATIENT=="Z********E",] #EXAM M1 en 2009 et d?c?s en 1998

#Je transforme les facteurs en date
for (i in colnames(ALLDATE)){
  ALLDATE[,i] <- manage_date_ND(ALLDATE[,i])
}

#ALLDATE[,colnames(ALLDATE)] <- apply(ALLDATE,2,manage_date_ND)
ALLDATENA <- apply(ALLDATE,2,function(.x)!is.na(.x)) #si true = non NA
COLDATEnonNA <- colnames(ALLDATENA)[apply(ALLDATENA,2,sum)>0]

#Afficher les colonnes avec dates nonNA
head(ALLDATE[ ,COLDATEnonNA])

#Afficher l'histoire d'un patient
t(ALLDATE[1:5 ,COLDATEnonNA])


#--------------------------------------------
#flow chart :
#relancer tout sans suprrimer les doublons de visite et trt au départ

table(ttt[ttt$DATEVNI!="", "PATIENT"] %in% visite$PATIENT) #35 patients avec une date de VNI ne sont pas dans visite (et n'ont donc pas de diagnostic)

#Patients suivis dans SLA01
length(unique(BDD$PATIENT)) #9755

#Patients qui ne sont pas dans la base visite
length(unique(ttt$PATIENT[!ttt$PATIENT %in% visite$PATIENT])) #5500
names_notin_visite <- unique(ttt$PATIENT[!ttt$PATIENT %in% visite$PATIENT])
length(names_notin_visite)

#doublons visite et ttt (qui ne sont pas déjà comptés dans not in visite)
length (names2visite) #12
table(names2visite %in% names2ttt)
length(names2ttt[names2ttt %in% names2visite])#12 doublons visite et ttt
length(names2ttt[!names2ttt %in% names2visite & !names2ttt%in% names_notin_visite]) #8 doublons ttt non visite
table(sort(names2ttt)==sort(names2pat))#les doublons pat sont les doublons ttt

#NB : un seul doublon pat complètement identique : j'ai gardé une des copies


BDDbis <- BDD [! BDD$PATIENT %in% c(names_notin_visite,names2ttt,names2visite), ]

BDDbis[duplicated(BDDbis$PATIENT),] #plus de duplicats

#Patients avec plusieurs diagnostics, NA ou autre diagnostic
table(BDDbis$identical_diag,useNA = "a")
sum(table(BDDbis$identical_diag,useNA = "a")[c(-1,-11,-12)])

#Patients  with ALS
BDDbis <- BDDbis[BDDbis$diagSLA==1 & !is.na(BDDbis$diagSLA), ]
dim(BDDbis)

#Patients with a date diagnosis
table(is.na(BDDbis$identical_date_diag))#653 without date
table(BDDbis$identical_date_diag=="no identical date") #43 avec plusieurs date de diagnostique

BDDbis <- BDDbis[!is.na(BDDbis$identical_date_diag) & BDDbis$identical_date_diag!="no identical date", ]
BDDbis[is.na(BDDbis$date_diag),] #aucun patient sans date diag

#Patients with a date of vni
dim(BDDbis[!is.na(BDDbis$DATEVNI), ])
dim(BDD[!is.na(BDD$DATEVNI) & !is.na(BDD$date_diag) & BDD$diagSLA==1 & !is.na(BDD$diagSLA) & (!BDD$PATIENT %in% c(names_notin_visite,names2ttt,names2visite)) , ])
dim(BDD[!is.na(BDD$DATEVNI) & !is.na(BDD$identical_date_diag) & BDD$identical_date_diag!="no identical date" & BDD$diagSLA==1 & !is.na(BDD$diagSLA) & (!BDD$PATIENT %in% c(names_notin_visite,names2ttt,names2visite)) , ])

dim(BDD[BDD$diagSLA==1 & !is.na(BDD$diagSLA) & !is.na(BDD$date_diag) & BDD$TTT_VNI==1 & !is.na(BDD$TTT_VNI) & !is.na(BDD$DATEVNI) &(!BDD$PATIENT %in% c(names_notin_visite,names2ttt,names2visite)),])
dim(BDD[BDD$diagSLA==1 & !is.na(BDD$diagSLA) & !is.na(BDD$date_diag) & BDD$TTT_VNI==1 & !is.na(BDD$TTT_VNI) & !is.na(BDD$DATEVNI) &(!BDD$PATIENT %in% c(names2ttt,names2visite)),])


#251 avec différents diagnostics consecutifs
#1900 1
#7666 NA dont 5500 qui ne sont pas dans la base
table(unique(BDD[is.na(as.vector(BDD$identical_diag)),"PATIENT"]) %in% names_notin_visite) 
unique(BDD[as.vector(BDD$identical_diag)==1,"PATIENT"])

#Patients sans diagnostics
> dim(BDDSLADEM[! BDDSLADEM$PATIENT %in%  BDDSLADEM[duplicated(BDDSLADEM$PATIENT),"PATIENT"], ])
[1] 203  10




# #--------------------------------RAPPORT DES NA-------------------------------------
# 
# sink ("C:/Users/4051268/Documents/writing/listes_NA.txt") #enregistre dans un fichier texte
# 
# 
# cat("\t\tRESUME DES DONNEES MANQUANTES\n\n\n\n")#imprime titre dans le fichier texte, avant de faire boucle
# 
# cat("patients avec ND pour le jour de diagnostic : remplacé par 15\n\n")
# print(date.diagSLA_ND)
# 
# cat("\n\nNombre de patient SLA sans aucune date de diagnostic dans le fichier Diag.csv (patients supprimés) :  ")
# print(length(SLA_sans_date))
# 
# cat("\n\nNombre de patient SLA avec plusieurs dates de diagnostic dans le fichier Diag.csv (patients supprimés) :\n\n ")
# print(Diag[Diag$PATIENT %in% names_double_diag & Diag$FORM!="ID" & !(is.na(Diag$DIAGNOS)), ! colnames(Diag) %in% c("MODULE_STATUS","REF_NAME","REF_ID770S4V3638","REF_SURNAME","X")])
# 
# cat("\n\nNombre de patient SLA avec une date de diagnostic et plusieurs ID dans le fichier Diag.csv (patients supprimés) :\n\n ")
# dupid<-Diag[Diag$PATIENT %in% namestabID & Diag$FORM=="ID", ! colnames(Diag) %in% c("DIAGNOS","DATE_DIAG","MODULE_STATUS","REF_NAME","REF_ID770S4V3638","REF_SURNAME","X")]
# print(dupid[order(dupid$PATIENT),])
# 
# cat("\n\nNombre de patients sans ligne de préventilation mais présent dans fichier VNI :")
# print(length(no_prevent_line))
# cat("\nCe sont des patients ventilés mais sans date ou des patients sans VNI?\nY a-t-il un vecteur VNI 0/1 dans une des tables?\n\n")
# 
# cat("\n\n10 patients avec 2 dates de VNI : patients supprimés.\n\n")
# dup <-VNI[VNI$PATIENT %in% names_vni_duplicated & VNI$FORM!="ID", ! colnames(VNI) %in% c("MODULE_STATUS","REF_NAME","REF_ID770S4V3638","REF_SURNAME","X","CENTRE") ]
# print( dup[order(dup$PATIENT),] )
# 
# cat("\n\n\n\tEn résumé\n\n")
# cat("nombre de patients SLA avec une date de diagnostic :")
# print(length(names_sla_date))
# cat("nombre de patients SLA avec une date de diagnostic et une seule identité :")
# print(length(unique(ID$PATIENT)))
# cat("\n\nnombre de patients SLA avec une date de diagnostic et date de VNI (après des patients avec 2 dates de VNI) :")
# print(length(unique(idpw_vni$PATIENT)))
# sink()
# 

#------------------------------
#RECHERCHE AUTRES FICHIERS VNI
path <- "C:/Users/4051268/Documents/sauvegarde data/sla/data/"
.dir <- dir("C:/Users/4051268/Documents/sauvegarde data/sla/data/",full.names = T, recursive = T)
.dir <- .dir[str_sub(.dir, -3, -1)=="sas"]
v <- "vni"

for (i in .dir){
  fic<-i
  x<-scan(i, what=as.character(), sep="\n")
  j<-grep(v, tolower(x));print(x[j])
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
