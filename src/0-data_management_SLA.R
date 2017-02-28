source("src/fonctions_SLA.R")
source("src/objects_SLA.R")

#NB : pour lancer le script, il faut remettre sur la clé les bases de données à partir de :
#C:\Users\4051268\Documents\SLA\sauvegarde data\back up from usb key

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
#for (i in .dir_csv) {
  num <- which(.dir_csv==i)
  a <- readRDS(paste0("data/ddn/ddn",num,".rds")) #NB : supprimer ddn2.Rds du dossier data/ddn car le fichier est vide
  #browser()
  a <- a[[1]]
  #a <- ifelse
  a <- aggregate(a,by=list(a$PATIENT),max,na.rm=T) #qd des noms sont rassemblé et que toutes leurs dates valent NA, alors le tableau vaut NA
  assign(paste0("ddn",num),a)
}


# colincoherente <- colnames(bdd9)[as.vector(apply(bdd9[bdd9$PATIENT=="COULANGE_NORBERT",],2,function(x)!is.na(x)))]
# t(bdd9[bdd9$PATIENT=="COULANGE_NORBERT", colincoherente])
# #c'est datexam_v_m15 qui est renseignée (datexamvm14= juillet 2015) c'est incohérent car base extraite en aout 2015 mais aucun moyen de le repérer car dautre variable sont renseignées à m15
# #peut être dire date get_ddn que si date est sup à aout 2015 alors on prend la date n-1? A voir avec yann



#Pour merger les ddn
#nom des dataframe ddn
dfddn <- str_sub(dir("data/ddn"),1,-5)[sapply(dir("data/ddn"),function(x)is.data.frame(get(str_sub(x,1,-5))))]
#merge
for (i in dfddn){
  #num <- str_sub(i,4,-1)
  #num <- which(str_sub(dir("data/ddn"),1,-5)==i)
  num <- which(dfddn==i)
  .bd <- get(i)
  ddn_tot <- if(num==1) .bd[,c("PATIENT","max")] else merge(ddn_tot, .bd[,c("PATIENT","max")], by="PATIENT", suffixes= c(num-1,num),all=TRUE)
  #if (num==length(dfddn))
}
#transformation des colonnes en date si pas déjà fait


#reprendre ici

for (i in colnames(ddn_tot)[-1]){
  ddn_tot[,i] <- as_date(ddn_tot[,i])
  ddn_tot[,i] <- ifelse (ddn_tot[,i]>Sys.Date(), NA, ddn_tot[,i])
  ddn_tot[,i] <- as_date(ddn_tot[,i])
  #browser()
}

ddn_tot$ddn <- apply(ddn_tot[,grep("max", colnames(ddn_tot))],1,max,na.rm=T)

ddn_tot <- unique(ddn_tot[ ,c("PATIENT","ddn")]) #pas de duplicatat
ddn_tot$ddn <- as_date(ddn_tot$ddn)
#ddn_tot <- na.omit(ddn_tot) #PB : retire les patients qui ont une ddn NA

dim(ddn_tot)

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

bdd_dcd <- unique(bdd_dcd[ , c("PATIENT","date_dc")]) #seul les vrais doublons nom et date sont éliminés
#bdd_dcd <- na.omit(bdd_dcd)

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

bdd_1symp <- unique(bdd_1symp) #seul les vrais doublons nom et date sont éliminés
#bdd_1symp <- na.omit(bdd_1symp)

saveRDS(bdd_1symp, "data/bdd_to_merge/bdd_1symp.rds")

#---------------
#BASE DE DONNEES VNI

#DAT1VNI_MEO_V_M et DAT2VNI_MEO_V_M sont NA
#apply(apply(visite[ ,grep("VNI_MEO_", colnames(visite))],2,is.na),2,sum)
# > dim(visite)
# [1] 4243 9700

#Je garde :
#lapply(bdds, which_col,"VNI","DATE",type="explo")
#"DATEVNI_V_M", "DATEVNI" et "DATE_VNI"
#lapply(bdds, which_col,string1="DATE", string2="VNI",string3="STOP",type="explo")

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



# for (i in 1:nrow(bdd7)){
#   datvni <- bdd7[i, grep("DAT_MISOVR_VENT", colnames(bdd7))]
#   echecvni <- bdd7[i ,grep("ECHEC_MEO_VENT", colnames(bdd7))]
# }



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
bdd_DATEDIAG <- unique(bdd_DATEDIAG[,c("PATIENT","date_diag")])#seul les vrais doublons nom et date sont éliminés
#bdd_DATEDIAG <- na.omit(bdd_DATEDIAG)

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
bdd_diag <- unique(ALLDIAG[, c("PATIENT","diag")] )#seul les vrais doublons nom et diag sont éliminés

#pb : certains patients ont une ligne de diag NA => je supprime tous les NA
table(tab<-table(bdd_diag$PATIENT))
pat2diag <- names(tab)[tab>1]
bdd_diag[bdd_diag$PATIENT %in% pat2diag,]

#bdd_diag <- na.omit(bdd_diag)
saveRDS(bdd_diag, "data/bdd_to_merge/bdd_diag.rds")

#-------------
#MERGE
newdir <- dir("data/bdd_to_merge/")
newdir <- str_sub(newdir,1,-5)


for (i in 1:length(newdir)) {
  #browser()
  data <- get(newdir[i])
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}


head(res)

BASE_TOT <- res

#si ddn ultérieure à décès, alors ddn=décès
BASE_TOT$ddn <- ifelse (BASE_TOT$ddn > BASE_TOT$date_dc & !is.na(BASE_TOT$date_dc), BASE_TOT$date_dc, BASE_TOT$ddn)
saveRDS(BASE_TOT, "data/BASE_TOT.rds")

BASE_TOT <- readRDS("data/BASE_TOT.rds")



#------------------------------------
#------------------------------------
#SELECT SLA from BASE_TOT

BASE_SLA <- BASE_TOT[BASE_TOT$diag==1 & !is.na(BASE_TOT$diag) & !is.na(BASE_TOT$DATEVNI),]

#doublons
table(tab <- table(BASE_SLA$PATIENT))
namesdoublons <- names(tab)[tab>1]
#1 doublon dans bdd7 qui pose pb pour les mesures repetees => je retire (G******-N*****)
table(tab <- table(bdd7$PATIENT))
db2 <- names(tab)[tab>1] [ !names(tab)[tab>1] %in% namesdoublons & names(tab)[tab>1] %in% BASE_SLA$PATIENT]
namesdoublons <- c(namesdoublons,db2)
# data.frame(namesdoublons)
# BASE_SLA[BASE_SLA$PATIENT%in% namesdoublons,]
# names2 <- names(tab)[tab==2]
# BASE_SLA[BASE_SLA$PATIENT%in% names2,] #les doublons sont foireux, je les supprime

BASE_SLA <- BASE_SLA[!BASE_SLA$PATIENT %in% namesdoublons, ]
BASE_SLA$DOB <- NULL
BASE_SLA$ddn <- as_date(BASE_SLA$ddn)

saveRDS(BASE_SLA, "data/BASE_SLA.rds")

BASE_SLA <- readRDS("data/BASE_SLA.rds")
namesSLA <- BASE_SLA$PATIENT
table(table(namesSLA))

#flowchart : 
#patients totaux
table(table(unique(BASE_TOT$PATIENT)))
#patients sla
sum(table(table(BASE_TOT$PATIENT[BASE_TOT$diag==1])))
#table(BASE_TOT$diag==1, useNA = "a") #1925 : faux, ne tient pas compte des doublons
#patients date vni(parmi sla)
table(table(BASE_TOT[BASE_TOT$diag==1 & !is.na(BASE_TOT$diag) & !is.na(BASE_TOT$DATEVNI), "PATIENT"]))
sum(table(tab <- table(BASE_TOT[BASE_TOT$diag==1 & !is.na(BASE_TOT$diag) & !is.na(BASE_TOT$DATEVNI), "PATIENT"])))
#13 doublons
length(namesdoublons)
#434 patients
table(table(namesSLA))
#3 patients ont datevni > date décès
BASE_SLA[BASE_SLA$DATEVNI>BASE_SLA$ddn, "PATIENT"]

#NB : 8 patients ont toutes les colonnes NA (ils sont elimines si on fait na.omit, explique qu'on passe a 9749 patients au lieu de 9757)
BASE_TOT[is.na(BASE_TOT$diag) & is.na(BASE_TOT$DATEVNI) & is.na(BASE_TOT$FIRSTSYMPTOM) & is.na(BASE_TOT$ddn), ] 


#------------------------
#------------------------
#Nouvelle date vni à partir de DATEVNI et bdd7

#CREATION BASES

tmp <- merge(BASE_SLA[ , c("PATIENT","DATEVNI")], bdd7, by="PATIENT", all.x=T, all.y=T)
ALLpick <- tmp

namesallSLA <- BASE_TOT[BASE_TOT$diag==1 & !is.na(BASE_TOT$diag), "PATIENT"] 

.l <- lapply(unique(namesallSLA), function(i){
  #.l <- lapply(namesSLA, function(i){
  #.l <- lapply("AIME_CORINNE", function(i){
  print(paste(which(i==namesallSLA), "/", length(namesallSLA)))
  DATE_PP <- colnames(ALLpick)[grep("DATE_RESP_PP",colnames(ALLpick))]
  VNIon_PP <- colnames(ALLpick)[grep("VNI_ON_PP",colnames(ALLpick))] #decision de vni associée à la date prevent
  MEODAT_PP <- colnames(ALLpick)[grep("DAT_MISOVR_VENT_PP",colnames(ALLpick))] #date de VNI
  delaiMEO_PP <- colnames(ALLpick)[grep("DELAI_VENT_PP",colnames(ALLpick))] #delai de MEO prevu a partir de la date prevent
  echec_PP <-  colnames(ALLpick)[grep("ECHEC_MEO_VENT_PP",colnames(ALLpick))] #delai de MEO prevu a partir de la date prevent
  echec_RES_PP <-  colnames(ALLpick)[grep("MOTIFS_ECHEC_VENT_PP",colnames(ALLpick))]
  decision_PP <- colnames(ALLpick)[grep("DAT_DECIS_VENT_PP",colnames(ALLpick))]
  #browser()
  DATE_PV <- colnames(ALLpick)[grep("DATE_PREVENT_PP",colnames(ALLpick))] #PV n'existe pas, et associées aux var PV
  VNIon_PV <- colnames(ALLpick)[grep("VNI_ON_PV",colnames(ALLpick))] #decision de vni associée à la date prevent
  MEODAT_PV <- colnames(ALLpick)[grep("DAT_MISOVR_VENT_PV",colnames(ALLpick))] #date de VNI
  delaiMEO_PV <- colnames(ALLpick)[grep("DELAI_VENT_PV",colnames(ALLpick))] #delai de MEO prevu a partir de la date prevent
  echec_PV <-  colnames(ALLpick)[grep("ECHEC_MEO_VENT_PV",colnames(ALLpick))] #delai de MEO prevu a partir de la date prevent
  echec_RES_PV <-  colnames(ALLpick)[grep("MOTIFS_ECHEC_VENT_PV",colnames(ALLpick))]
  decision_PP <- colnames(ALLpick)[grep("DAT_DECIS_VENT_PV",colnames(ALLpick))]
  
  varDATE <- manage_date_ND(as.vector(t(ALLpick[ALLpick$PATIENT==i, c(DATE_PP, DATE_PV)])))
  varVNIon <- as.vector(t(ALLpick[ALLpick$PATIENT==i, c(VNIon_PP, VNIon_PV)]))
  MEODAT <- manage_date_ND(as.vector(t(ALLpick[ALLpick$PATIENT==i, c(MEODAT_PP, MEODAT_PV)])))
  delaiMEO <- as.vector(t(ALLpick[ALLpick$PATIENT==i, c(delaiMEO_PP,  delaiMEO_PV)]))
  echec <- as.vector(t(ALLpick[ALLpick$PATIENT==i, c(echec_PP,  echec_PV)]))
  raisechec <- as.vector(t(ALLpick[ALLpick$PATIENT==i, c(echec_RES_PP,  echec_RES_PV)]))
  raisechec <- as.vector(t(ALLpick[ALLpick$PATIENT==i, c(echec_RES_PP,  echec_RES_PV)]))
  
  #dateTOT <- data.frame(cbind(varDATE, varVNIon, MEODAT, delaiMEO, echec)); colnames(dateTOT) <- c("DATEXAM_RESPI", "VNIon", "MEODATE", "delaiMEO", "echecVNI")
  dateTOT <- data.frame(varDATE, varVNIon, MEODAT, delaiMEO, echec, raisechec); colnames(dateTOT) <- c("DATEXAM_RESPI", "VNIon", "MEODATE", "delaiMEO", "echecVNI", "raison")
  #browser()
  # if (nrow(dateTOT[!is.na(dateTOT$DATEXAM_RESPI),])==0){
  #   dateTOT2 <- data.frame(NA,NA,NA,NA,NA) 
  #   colnames(dateTOT2) <- c("DATEXAM_RESPI", "VNIon", "MEODATE", "delaiMEO", "echecVNI") 
  # } 
  #else dateTOT2 <- dateTOT[!is.na(dateTOT$DATEXAM_RESPI), ]
  dateTOT$PATIENT <- i
  #dateTOT2 <- dateTOT2[ ,c(6, 1:5)]
  dateTOT <- dateTOT[!is.na(dateTOT$DATEXAM_RESPI), ]
  return (dateTOT)
})
dec_vni <- do.call(rbind, .l)
saveRDS(dec_vni, "data/dec_vni.rds")
dec_vni <- readRDS("data/dec_vni.rds")

.l <- lapply(unique(namesallSLA), function(i){ #ne sert que pour voir les infos dispo afin de voir si algorythme pertinent
  #.l <- lapply(namesSLA, function(i){
  #.l <- lapply("AIME_CORINNE", function(i){
  print(paste(which(i==namesallSLA), "/", length(namesallSLA)))
  #browser()
  namesPP <- colnames(ALLpick)[grep("DATE_RESP_PP",colnames(ALLpick))]
  namesPV <- colnames(ALLpick)[grep("DATE_PREVENT_PP",colnames(ALLpick))]
  namesSV <- colnames(ALLpick)[grep("DATE_SOUSVENT",colnames(ALLpick))]
  var1 <- manage_date_ND(t(ALLpick[ALLpick$PATIENT==i, namesPP]))
  var2 <- manage_date_ND(t(ALLpick[ALLpick$PATIENT==i, namesPV]))
  var3 <- manage_date_ND(t(ALLpick[ALLpick$PATIENT==i, namesSV]))
  dateTOT <- data.frame(c(var1,var2,var3)) ; colnames(dateTOT) <- "DATEXAM_RESPI" #; rownames(dateTOT) <- c(namesPP, namesPV,namesSV)
  #dateTOT$DATEVNI <- ALLpick[ ALLpick$PATIENT==i, "DATEVNI"]
  dateTOT$PATIENT <- ALLpick[ ALLpick$PATIENT==i, "PATIENT"]
  dateTOT$visite <- c(namesPP, namesPV,namesSV)
  #browser()
  
  listv <- list(c("DYSP_DECUBI_PP","DYSP_DECUBI_PV","ORTHOPN_SV_F"), c("PACO2_PP","PACO2_PV","PACO2_SV_F"), c("PAO2_PP","PAO2_PV","PAO2_SV_F"), 
                c("HCO3_PP", "HCO3_PV_F", "HCO3_SV_F"), c("PH_PP", "PH_PV_F", "PH_SV_F"), c("CEPHAL_PP", "CEPHAL_PV_F", "CEPHAL_SV_F"))
  for (j in listv){
    #browser()
    namesPP <- colnames(ALLpick)[grep(j[1],colnames(ALLpick))]
    namesPV <- colnames(ALLpick)[grep(j[2],colnames(ALLpick))]
    namesSV <- colnames(ALLpick)[grep(j[3],colnames(ALLpick))][! grep(j[3],colnames(ALLpick)) %in% grep("CL1",colnames(ALLpick))]
    var1 <- t(ALLpick[ALLpick$PATIENT==i, namesPP])
    var2 <- t(ALLpick[ALLpick$PATIENT==i, namesPV])
    var3 <- t(ALLpick[ALLpick$PATIENT==i, namesSV])
    dateTOT2 <- data.frame(c(var1,var2,var3)) #; rownames(dateTOT2) <- c(namesPP, namesPV,namesSV)
    colnames(dateTOT2) <- str_sub(j[1],1,-4)
    resresp <- if (length(which(j[1]==listv[[1]]))!=0) cbind(dateTOT, dateTOT2) else cbind(resresp, dateTOT2)
  }
  resresp  <-  resresp [!is.na( resresp $DATEXAM_RESPI),]
})

bvr <- do.call(rbind, .l)
saveRDS(bvr, "data/bvr.rds")  

mm <- merge(dec_vni, bvr, by=c("PATIENT","DATEXAM_RESPI"), all = TRUE) #NB : si DATEXAM RESPI jms renseigné, datevni devient NA
mm <- merge(mm[,!names(mm) %in%"DATEVNI"], BASE_SLA[,c("PATIENT", "DATEVNI")], by="PATIENT", all=T)
mm <- mm %>% select(-raison)

#-------------------


#ALGORITHME
.l <- lapply(unique(mm$PATIENT), function(x){
  db <- mm %>% filter(PATIENT==x)
  if (any(!is.na(db$MEODATE))) {
    VNI <- min(db[!is.na(db$MEODATE), "MEODATE"], na.rm = T)
  } else {
    
    if(any(!is.na(db$DATEXAM_RESPI))) {
      db2 <- db[!is.na(db$DATEXAM_RESPI), ]
      
      if (length(grep("SOUSVENT",db2$visite))==nrow(db2)) { #que des visites SOUS VENT
        if (any(!is.na(db$DATEVNI))) VNI <- min(db$DATEVNI, na.rm=TRUE) #elimine les NA et ne garde qu'une date
        else VNI <- NA
      } else { #certaines visites sont PREVENT ou PP
        if (any(!is.na(db2$delaiMEO)) | any(db2$VNIon==1 & !is.na(db2$VNIon))){
          VNI <- min(db2[!is.na(db2$delaiMEO) | (db2$VNIon==1 & !is.na(db2$VNIon)), "DATEXAM_RESPI"], na.rm=T)
        } else {
          if (any(!is.na(db$DATEVNI))) VNI <- min(db$DATEVNI, na.rm=TRUE) #elimine les NA et ne garde qu'une date
          else VNI <- NA
        }
      }
      
    } else {
      if (any(!is.na(db$DATEVNI))) VNI <- min(db$DATEVNI, na.rm=TRUE) #elimine les NA et ne garde qu'une date
      else VNI <- NA
    }
  }
})

newVNI <- data.frame(unique(mm$PATIENT)) ; colnames(newVNI) <- "PATIENT"
newVNI$NEWVNI <- as.vector(do.call(rbind,.l))
newVNI$NEWVNI <- as_date(newVNI$NEWVNI)

saveRDS(newVNI, "data/newVNI.rds")


#recreation de BASE_SLA avec nouvelle dateVNI

newVNI <- readRDS("data/newVNI.rds")

BASE_TOT2 <- merge(BASE_TOT, newVNI, by=("PATIENT"), all=T)
BASE_TOT2$DATEVNIinit <- BASE_TOT2$DATEVNI
BASE_TOT2$DATEVNI <- BASE_TOT2$NEWVNI
BASE_TOT2$DATEVNI <- ifelse(is.na(BASE_TOT2$DATEVNI), BASE_TOT2$DATEVNIinit, BASE_TOT2$DATEVNI)
BASE_TOT2$DATEVNI <- as_date(BASE_TOT2$DATEVNI)
BASE_TOT2$ddn <- as_date(BASE_TOT2$ddn)
BASE_SLA2 <- BASE_TOT2[BASE_TOT2$diag==1 & !is.na(BASE_TOT2$diag) & !is.na(BASE_TOT2$DATEVNI),]
BASE_SLA3 <- BASE_SLA2[ ,!colnames(BASE_SLA2)%in%c("NEWVNI")] #juste pour voir lesquels ont une nouvelle date VNI différente, mais rajoute des doublons
BASE_SLA2 <- BASE_SLA2[ ,!colnames(BASE_SLA2)%in%c("NEWVNI","DATEVNIinit")]
BASE_SLA2 <- unique(BASE_SLA2)


#doublons
table(tab <- table(BASE_SLA2$PATIENT))
namesdoublons2 <- names(tab)[tab>1]
#1 doublon dans bdd7 qui pose pb pour les mesures repetees => je retire (G******-N*****)
table(tab <- table(bdd7$PATIENT))
db2 <- names(tab)[tab>1] [ !names(tab)[tab>1] %in% namesdoublons2 & names(tab)[tab>1] %in% BASE_SLA2$PATIENT]
namesdoublons2 <- c(namesdoublons2,db2)
BASE_SLA2[BASE_SLA2$PATIENT%in% namesdoublons2, ]#certains doublons sont foireux(pas même date de décès), je les supprime tous
BASE_SLA2 <- BASE_SLA2[!BASE_SLA2$PATIENT %in% namesdoublons2, ]



saveRDS(BASE_SLA2, "data/BASE_SLA_new.rds")
saveRDS(BASE_TOT2, "data/BASE_TOT_new.rds")

BASE_TOT <- readRDS("data/BASE_TOT_new.rds")
BASE_SLA <- readRDS("data/BASE_SLA_new.rds")
namesSLA <- BASE_SLA$PATIENT

#flowchart : 
#patients totaux
table(table(unique(BASE_TOT$PATIENT)))
#patients sla
sum(table(table(BASE_TOT$PATIENT[BASE_TOT$diag==1])))
#table(BASE_TOT$diag==1, useNA = "a") #1925 : faux, ne tient pas compte des doublons
#patients date vni(parmi sla)
table(table(BASE_TOT[BASE_TOT$diag==1 & !is.na(BASE_TOT$diag) & !is.na(BASE_TOT$DATEVNI), "PATIENT"]))
sum(table(tab <- table(BASE_TOT[BASE_TOT$diag==1 & !is.na(BASE_TOT$diag) & !is.na(BASE_TOT$DATEVNI), "PATIENT"])))
#6 doublons
length(namesdoublons2)
#497 patients
table(table(namesSLA))
#4 patients ont datevni > date décès
BASE_SLA[BASE_SLA$DATEVNI>BASE_SLA$ddn, "PATIENT"]

#NB : 8 patients ont toutes les colonnes NA (ils sont elimines si on fait na.omit, explique qu'on passe a 9749 patients au lieu de 9757)
BASE_TOT[is.na(BASE_TOT$diag) & is.na(BASE_TOT$DATEVNI) & is.na(BASE_TOT$FIRSTSYMPTOM) & is.na(BASE_TOT$ddn), ] 




#----------------------------------
#----------------------------------
#Ajout des baseline
#F:\to push\sla_git\data\baseline_to_merge


#-------------
#BASE DE DONNEE DOB
listes_brut <- lapply(bdds, which_col,string1="DOB",type="merge")
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}

#certains patients n'ont pas la même date de naissance...
res2 <- na.omit(res)
# res2[as.character(res2$DOB.x)!=as.character(res2$DOB.y),]
pat2dob <- unique(res2[as.character(res2$DOB.x)!=as.character(res2$DOB.y), "PATIENT"])

bdd_DOB <- get_min_max(data = res, fun = "min") #je ne prend pas vmt la min, ça me permet de prendre l'une qd l'autre est NA
bdd_DOB <- bdd_DOB[!bdd_DOB$PATIENT %in% pat2dob, ] #je suprrime les date incohérentes
bdd_DOB$DOB <- manage_date_ND(bdd_DOB$min)
bdd_DOB <- unique(bdd_DOB[ ,c("PATIENT","DOB")])
DOB <- na.omit(bdd_DOB)
saveRDS(DOB, "data/baseline_to_merge/indep_datevni/DOB.rds")

#--------------
#sexe
listes_brut <- lapply(bdds, which_col,string1="SEX", type="merge")
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}
ALLSEX <-res[ ,!colnames(res)%in%"PATIENT"]
pick_sex <- lapply(1: nrow(ALLSEX),function(.x){
  .l <- ALLSEX[.x,]
  .l <- .l[!is.na(.l)]
  if (length(.l)!=0 & all(.l[1]==.l)) sex <- as.integer(tail(.l,1))
  # else {
  #   if(length(.l)!=0) sex <- "not sure"
  #   else sex <- NA
  # }
  else sex <- NA #si plusieurs sexe possible, je supprime l'info completement : sexe = NA
  return (sex)
})

ALLSEX$sex_def <- as.vector(do.call(rbind,pick_sex))
ALLSEX$sex_def <- as.integer(ALLSEX$sex_def)
ALLSEX$PATIENT <- as.character(res$PATIENT)

#ALLSEX[ALLSEX$sex_def=="not sure",] #6 not sure, it's the same personne, first name=marie
#table(ALLSEX$sex_def, useNA = "a") #no NA except the 6 not sure
sexe <- unique(ALLSEX[ ,c("PATIENT","sex_def")])
saveRDS(sexe, "data/baseline_to_merge/indep_datevni/sexe.rds")

#--------------
#Forme familial oui/non 1/0

# #bdd6 : SOD1 = "Mutation SOD1" #pb : la mutation peut être de novo, et autre mutation peut causer la maladie donc je n'utilise pas

#bdd6 : ATCD_FAMIL_L1 à 6 : 1= SLA
listes_brut <- lapply(bdds, which_col,string1="ATCD_FAMIL_L", type="merge")
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
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
forme <- unique(ALLATCD[ , c("PATIENT","familial")])
saveRDS(forme, "data/baseline_to_merge/indep_datevni/forme.rds")

#------------
#site of onset

site_onset <- bdd6[ ,c("PATIENT","LIEUDEB")]

site_onset$LIEUDEB_recode <- Recode(as.factor(site_onset$LIEUDEB), "1 = 'bulbar';2 = 'cervical'; 10:15 = 'lower limb onset'; 3 = 'respiratory'; 4:9 = 'upper limb onset'")

site_onset <- unique(site_onset[ , c("PATIENT","LIEUDEB_recode")])
saveRDS(site_onset, "data/baseline_to_merge/indep_datevni/site_onset.rds")

#--------------
#patients taking riluzole
listes_brut <- lapply(bdds, which_col,string1="DATDRILU_V_M", type="explo")
listes_brut <- lapply(bdds, which_col,string1="RILU", string2="DATF", type="explo")

# #bdd6
# [3] "RILUZ"
# [4] "POSORILU"
# [5] "DATDRILU"
# [6] "DATFRILU"
# #bdd8
# [3] "RILUZ"
# [4] "TTT_RILU_LNUM_L1"
# [5] "POSORILU_L1"
# [6] "DATDRILU_L1"
# [7] "DATFRILU_L1"
# #bdd9
# [3] "RILUZ_V_M1"
# [4] "POSORILU_V_M1"
# [5] "DATDRILU_V_M1"
# [6] "DATFRILU_V_M1"
#
# table(bdd6$RILUZ,useNA = "a") #52
# table(bdd8$POSORILU_L1,useNA = "a") #1955
# table(bdd9$RILUZ_V_M1,useNA = "a") #

#DATE DEBUT RILU
#listes_brut <- lapply(bdds, which_col,string1="DATDRILU_V_M", type="merge")
listes_brut <- lapply(bdds, which_col,string1="RILU", string2="DATD", type="merge") #permet de récupérer 1500 dates!
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}

for (i in colnames(res)[grep("DAT", colnames(res))]) res[,i] <- manage_date_ND(res[,i])
RILU <- res[ ,!colnames(res)%in%"PATIENT"]
pick_atcd <- lapply(1: nrow(RILU),function(.x){
  .l <- RILU[.x,]
  .l <- .l[!is.na(.l)]
  if (length(.l)!=0){
    if(length(.l)==1) data <- .l
    else data <- min(.l)
    #else data <- "several" #2 personnes avec plusieurs dates, je prends la min
  } else {
    data <- NA
  }
  return (data)
})

RILU$DEBRILU <- as.vector(do.call(rbind,pick_atcd))
RILU$PATIENT <- as.character(res$PATIENT)
RILU1 <- unique(RILU[ , c("PATIENT","DEBRILU")])

#DATE FIN RILU
#listes_brut <- lapply(bdds, which_col,string1="DATFRILU_V_M", type="merge")
listes_brut <- lapply(bdds, which_col, string1="RILU", string2="DATF", type="merge")
listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
for (i in 1:length(listes_net)) {
  data <- listes_net[[i]]
  res <- if (i==1) data else merge(res,data,by="PATIENT",all=T)
}

for (i in colnames(res)[grep("DAT", colnames(res))]) res[,i] <- manage_date_ND(res[,i])
RILU <- res[ ,!colnames(res)%in%"PATIENT"]
pick_atcd <- lapply(1: nrow(RILU),function(.x){
  .l <- RILU[.x,]
  .l <- .l[!is.na(.l)]
  if (length(.l)!=0){
    if(length(.l)==1) data <- .l
    else data <- min(.l)
    #else data <- "several" #no sevearl date
  } else {
    data <- NA
  }
  return (data)
})

RILU$FINRILU <- as.vector(do.call(rbind,pick_atcd))
RILU$PATIENT <- as.character(res$PATIENT)
RILU2 <- unique(RILU[ , c("PATIENT","FINRILU")])
RILU <- merge(RILU1,RILU2,by="PATIENT",all=T)

names_dataset <- "RILU"
saveRDS(RILU, paste0("data/baseline_to_merge/dependant_datevni/",names_dataset,".rds"))


#----------------
#ALSFRS-R score
#listes_brut <- lapply(bdds, which_col,string1="ALS", type="explo") 

tmp <- merge(bdd9,months_before_vni,by="PATIENT",all.y=T) #attention : month before nvi n'est plus à jour
ALLpick <- tmp[ ,c("PATIENT","myrow","DATEVNI",colnames(tmp)[grep("ALS_V",colnames(tmp))]) ]
pick_data <- lapply(1: nrow(ALLpick),function(.x){
  #browser()
  myrow <- ALLpick[.x, "myrow"]
  if (is.na(myrow)){ # date de visite supplémentaire postérieure à la ise sous vni
    .pat <- ALLpick[.x, "PATIENT"]
    .vni <- ALLpick[.x, "DATEVNI"]
    .datALSPV <- manage_date_ND(bdd6[bdd6$PATIENT==.pat, "DATEXAM"] )
    if(!is.na(.vni) & !is.na(.datALSPV) & abs(.vni-.datALSPV)<90) data <- bdd6[bdd6$PATIENT==.pat, "ALS"] #Je regarde dans la base premiere visite si date vni proche de date exam
    else data <- NA
  } else {
    data <- ALLpick[.x, paste0("ALS_V_M",myrow)]
    # if (is.na(data)){
    #   data <- ALLpick[.x, paste0("ALS_V_M",(myrow-1))]#ok pour prendre l'échelle du mois précédent?
    # } else data <- data
  }
  #en prenant la dernière ALS dispo même si le patient est peut être plus grave à linstauration de la VNI: (je ne fais pas ça finalement)
  #.l <- ALLpick[.x, paste0("ALS_V_M",1:myrow)]
  #.l <- .l[!is.na(.l)] #transforme en vecteur
  # if (length(.l)==0)  data <- NA
  # else data <- tail(.l,1)
  return(data)
})
ALLpick$data <- as.vector(do.call(rbind,pick_data))
ALLpick$PATIENT <- as.character(tmp$PATIENT)
ALLpick <- unique(ALLpick[ , c("PATIENT","data")])
names_dataset <- "ALSFRS"
colnames(ALLpick)[-1] <- "ALS_score"
assign(names_dataset,ALLpick)
saveRDS(ALLpick, paste0("data/baseline_to_merge/dependant_datevni/",names_dataset,".rds"))

#----------------
#norris bulbar score
tmp <- merge(bdd9,months_before_vni,by="PATIENT",all.y=T)
ALLpick <- tmp[ ,c("myrow","DATEVNI",colnames(tmp)[grep("E_BULBAIRE_V_M",colnames(tmp))]) ]
pick_data <- lapply(1: nrow(ALLpick),function(.x){
  myrow <- ALLpick[.x, "myrow"]
  if (is.na(myrow)){
    data <- NA
  } else {
    data <- ALLpick[.x, paste0("E_BULBAIRE_V_M",myrow)]
  }
  return(data)
})
ALLpick$data <- as.vector(do.call(rbind,pick_data))
ALLpick$PATIENT <- as.character(tmp$PATIENT)
ALLpick <- unique(ALLpick[ , c("PATIENT","data")])
names_dataset <- "E_BULB"
colnames(ALLpick)[-1] <- "bulb_score"
assign(names_dataset,ALLpick)
saveRDS(ALLpick, paste0("data/baseline_to_merge/dependant_datevni/",names_dataset,".rds"))



#-----------------
#bicar from bdd9
# #TOUTE LA BASE EST NA (ce n'est pas une erreur), JE NE SAUVE PAS

# tmp <- merge(bdd9,months_before_vni,by="PATIENT",all.y=T)
# ALLpick <- tmp[ ,c("myrow","DATEVNI",colnames(tmp)[grep("BICARB_V_M",colnames(tmp))]) ]
# pick_data <- lapply(1: nrow(ALLpick),function(.x){
#   #browser()
#   myrow <- ALLpick[.x, "myrow"]
#   if (is.na(myrow)){
#     data <- NA
#   } else {
#     #browser()
#     data <- ALLpick[.x, paste0("BICARB_V_M",myrow)]
#   }
#   return(data)
# })
# ALLpick$data <- as.vector(do.call(rbind,pick_data))
# ALLpick$PATIENT <- as.character(tmp$PATIENT)
# ALLpick <- unique(ALLpick[ , c("PATIENT","data")])
# names_dataset <- "bicarbdd9"
# colnames(ALLpick)[-1] <- "bicar"
# assign(names_dataset,ALLpick)
# #saveRDS(ALLpick, paste0("data/baseline_to_merge/dependant_datevni/",names_dataset,".rds"))




#---------------
#all bl var from bdd7


tmp <- merge(BASE_SLA[ , c("PATIENT","DATEVNI")], bdd7, by="PATIENT", all.x=T, all.y=F)
ALLpick <- tmp
for (i in colnames(ALLpick)[grep("DAT",colnames(ALLpick))]) ALLpick[,i] <- manage_date_ND(ALLpick[,i])

#ALLpick <- ALLpick[ALLpick$PATIENT==namesSLA, ]

pick_data <- sapply(1: nrow(ALLpick),function(.x){
  #browser()
  .vni <- ALLpick[.x, "DATEVNI"]
  .l <- ALLpick[.x, grep("DATE_PREVENT_PP",colnames(ALLpick))]#je selectionne toutes les colonnes DATE_PREVENT_PP et je considere que sousvent est pendant la ventilation
  .g <- as.vector(t(.l))
  
  if(any(!is.na(.g))){
    row_last_date_before_vni <- tail(which((.g<=.vni)),1) #dernier mois dispo avant VNI (je fais l'hypothese que l'info si elle existe est bien renseignée)
    
    if (length(row_last_date_before_vni)==0) { #les dates de DATE_PREVENT_PP sont post à vni, alors jessaye DATE_RESP_PP
      #browser()
      month_bef_vni <- "after vni"
      .l <- ALLpick[.x, "DATE_RESP_PP"]
      if(!is.na(.l) & (.vni-.l)>0 & (.vni-.l)<90) { #ok si mesure faite 3 mois avant vni (je fais l'hypothese que cette colonne est moins bien renseignée que celle de suivi)
        last_date_bef_vni <- .l
        month_bef_vni <- "DATE_RESP_PP"
        dysp <- ALLpick[.x, "DYSP_REPOS_PP"]
        orthop <- ALLpick[.x, "DYSP_DECUBI_PP"]
        CVF_ASSIS_perc_pred <- as.numeric(as.character(ALLpick[.x, "CVF_ASSIS_THEO_PP"]))
        CVF_COUCHE_perc_pred <- as.numeric(as.character(ALLpick[.x, "CVF_THEO_PP"]))
        SNIP_cmH2O <- as.numeric(as.character(ALLpick[.x, "SNIP_PP"]))
        SNIP_perc_pred <- as.numeric(as.character(ALLpick[.x, "SNIP_THEO_PP"]))
        PIMAX_cmH2O <- as.numeric(as.character(ALLpick[.x, "PIMAX_PP"]))
        PIMAX_perc_pred <- as.numeric(as.character(ALLpick[.x, "PIMAX_THEO_PP"]))
        
        perc_time_under_spo2_90 <- as.numeric(as.character(ALLpick[.x, "SPO2_TPS_PP"]))
        
        DUREE_ENREG_H_PP <- as.numeric(as.character(ALLpick[.x, "DUREE_ENREG_H_PP"]))
        DUREE_ENREG_MIN_PP <- as.numeric(as.character(ALLpick[.x, "DUREE_ENREG_MIN_PP"]))
        #DUREE_ENREG_H_PP <- ifelse(!is.na(DUREE_ENREG_MIN_PP) & is.na(DUREE_ENREG_H_PP),0,DUREE_ENREG_H_PP)
        #DUREE_ENREG_MIN_PP <- ifelse(!is.na(DUREE_ENREG_H_PP) & is.na(DUREE_ENREG_MIN_PP),0,DUREE_ENREG_MIN_PP)
        time_under_spo2_90_h <- ifelse(is.na(DUREE_ENREG_H_PP) & is.na(DUREE_ENREG_MIN_PP), NA, sum(DUREE_ENREG_H_PP*60, DUREE_ENREG_MIN_PP, na.rm=T))
        time_under_spo2_90_h <- time_under_spo2_90_h/60
        
        # time_under_spo2_90_h <- apply(ALLpick[ ,c("DUREE_ENREG_H_PP","DUREE_ENREG_MIN_PP")], 1, function(x){
        #   mytime <- ifelse(is.na(x[1]) & is.na(x[2]), NA, sum(x[1]*60, x[2], na.rm=T))
        #   mytime <- mytime/60
        # }) 
        time_under_spo2_90_h <- time_under_spo2_90_h * perc_time_under_spo2_90/100
        
        # minutes <- sum((DUREE_ENREG_H_PP*60),DUREE_ENREG_MIN_PP,na.rm=T)
        # heures <- minutes/60
        # time_under_spo2_90_h <- (perc_time_under_spo2_90/100)*heures
        
        bicar <- as.numeric(as.character(ALLpick[.x, "HCO3_PP"] ))
        paco2 <- ALLpick[.x, "PACO2_PP"]
        
      } else { #date_prevent_PP post a vni et date_resp_pp post a vni
        last_date_bef_vni <- NA
        month_bef_vni <- "after vni"
        dysp <- NA
        orthop <- NA
        CVF_ASSIS_perc_pred <- NA
        CVF_COUCHE_perc_pred <- NA
        SNIP_cmH2O <- NA
        SNIP_perc_pred <- NA
        PIMAX_cmH2O <- NA
        PIMAX_perc_pred <- NA
        perc_time_under_spo2_90 <- NA
        time_under_spo2_90_h <- NA
        DUREE_ENREG_H_PP <- NA
        DUREE_ENREG_MIN_PP <- NA
        bicar <- NA
        paco2 <- NA
      }
      
    } else { #il y a une date prevent antérieure à vni (et je fais l'hypothese qu'elle est juste avant vni, mais en pratique elle peut être plus de 3 mois avant, cf S******_M*****)
      
      last_date_bef_vni <- .g[row_last_date_before_vni]
      last_date_bef_vni <- as_date(last_date_bef_vni)
      month_bef_vni <- colnames(.l[row_last_date_before_vni])
      month_bef_vni <- ifelse(str_sub(month_bef_vni,-3,-3)=="F", as.numeric(str_sub(month_bef_vni,-2,-1)),as.numeric(str_sub(month_bef_vni,-1,-1)))
      
      dysp <- ALLpick[.x, paste0("DYSP_REPOS_PV_F",month_bef_vni)]
      orthop <- ALLpick[.x, paste0("DYSP_DECUBI_PV_F",month_bef_vni)]
      CVF_ASSIS_perc_pred <- as.numeric(as.character(ALLpick[.x, paste0("CVF_ASSIS_THEO_PV_F",month_bef_vni)]))
      CVF_COUCHE_perc_pred <- as.numeric(as.character(ALLpick[.x, paste0("CVF_THEO_PV_F",month_bef_vni)]))
      SNIP_cmH2O <- as.numeric(as.character(ALLpick[.x, paste0("SNIP_PV_F",month_bef_vni)]))
      SNIP_perc_pred <- as.numeric(as.character(ALLpick[.x, paste0("SNIP_THEO_PV_F",month_bef_vni)]))
      PIMAX_cmH2O <- as.numeric(as.character(ALLpick[.x, paste0("PIMAX_PV_F",month_bef_vni)]))
      PIMAX_perc_pred <- as.numeric(as.character(ALLpick[.x, paste0("PIMAX_THEO_PV_F",month_bef_vni)]))
      
      perc_time_under_spo2_90 <- as.numeric(as.character(ALLpick[.x, paste0("SPO2_TPS_PV_F",month_bef_vni)]))
      
      DUREE_ENREG_H_PP <- as.numeric(as.character(ALLpick[.x, paste0("DUREE_ENREG_H_PV_F",month_bef_vni)]))
      DUREE_ENREG_MIN_PP <- as.numeric(as.character(ALLpick[.x, paste0("DUREE_ENREG_MIN_PV_F",month_bef_vni)]))
      #DUREE_ENREG_H_PP <- ifelse(!is.na(DUREE_ENREG_MIN_PP) & is.na(DUREE_ENREG_H_PP),0,DUREE_ENREG_H_PP)
      #DUREE_ENREG_MIN_PP <- ifelse(!is.na(DUREE_ENREG_H_PP) & is.na(DUREE_ENREG_MIN_PP),0,DUREE_ENREG_MIN_PP)
      time_under_spo2_90_h <- ifelse(is.na(DUREE_ENREG_H_PP) & is.na(DUREE_ENREG_MIN_PP), NA, sum(DUREE_ENREG_H_PP*60, DUREE_ENREG_MIN_PP, na.rm=T))
      time_under_spo2_90_h <- time_under_spo2_90_h/60
      
      # time_under_spo2_90_h <- apply(ALLpick[ ,c("DUREE_ENREG_H_PP","DUREE_ENREG_MIN_PP")], 1, function(x){
      #   mytime <- ifelse(is.na(x[1]) & is.na(x[2]), NA, sum(x[1]*60, x[2], na.rm=T))
      #   mytime <- mytime/60
      # }) 
      time_under_spo2_90_h <- time_under_spo2_90_h * perc_time_under_spo2_90/100
      # minutes <- sum((DUREE_ENREG_H_PP*60),DUREE_ENREG_MIN_PP,na.rm=T)
      # heures <- minutes/60
      # time_under_spo2_90_h <- (perc_time_under_spo2_90/100)*heures
      
      bicar <- as.numeric(as.character(ALLpick[.x, paste0("HCO3_PV_F",month_bef_vni)] ))
      paco2 <- as.numeric(as.character(ALLpick[.x, paste0("PACO2_PV_F",month_bef_vni)] ))
    }
    
    
  } else { # toutes les dates "DATE_PREVENT_PP" sont NA cad .g=NA => je regarde DATE_RESP_PP (=> idem que cas :"les DATE_PREVENT_PP sont posterieures à vni")
    .l <- ALLpick[.x, "DATE_RESP_PP"]
    
    if(!is.na(.l) & (.vni-.l)>0 & (.vni-.l)<90) { #ok si DATE_RESP_PP 3 mois avant vni (je fais l'hypothese que cette colonne est moins bien renseignée que celle de suivi(?))
      #browser()
      last_date_bef_vni <- .l
      month_bef_vni <- "DATE_RESP_PP"
      dysp <- ALLpick[.x, "DYSP_REPOS_PP"]
      orthop <- ALLpick[.x, "DYSP_DECUBI_PP"]
      CVF_ASSIS_perc_pred <- as.numeric(as.character(ALLpick[.x, "CVF_ASSIS_THEO_PP"]))
      CVF_COUCHE_perc_pred <- as.numeric(as.character(ALLpick[.x, "CVF_THEO_PP"]))
      SNIP_cmH2O <- as.numeric(as.character(ALLpick[.x, "SNIP_PP"]))
      SNIP_perc_pred <- as.numeric(as.character(ALLpick[.x, "SNIP_THEO_PP"]))
      PIMAX_cmH2O <- as.numeric(as.character(ALLpick[.x, "PIMAX_PP"]))
      PIMAX_perc_pred <- as.numeric(as.character(ALLpick[.x, "PIMAX_THEO_PP"]))
      
      perc_time_under_spo2_90 <- as.numeric(as.character(ALLpick[.x, "SPO2_TPS_PP"]))
      
      DUREE_ENREG_H_PP <- as.numeric(as.character(ALLpick[.x, "DUREE_ENREG_H_PP"]))
      DUREE_ENREG_MIN_PP <- as.numeric(as.character(ALLpick[.x, "DUREE_ENREG_MIN_PP"]))
      #DUREE_ENREG_H_PP <- ifelse(!is.na(DUREE_ENREG_MIN_PP) & is.na(DUREE_ENREG_H_PP),0,DUREE_ENREG_H_PP)
      #DUREE_ENREG_MIN_PP <- ifelse(!is.na(DUREE_ENREG_H_PP) & is.na(DUREE_ENREG_MIN_PP),0,DUREE_ENREG_MIN_PP)
      time_under_spo2_90_h <- ifelse(is.na(DUREE_ENREG_H_PP) & is.na(DUREE_ENREG_MIN_PP), NA, sum(DUREE_ENREG_H_PP*60, DUREE_ENREG_MIN_PP, na.rm=T))
      time_under_spo2_90_h <- time_under_spo2_90_h/60
      
      # time_under_spo2_90_h <- apply(ALLpick[ ,c("DUREE_ENREG_H_PP","DUREE_ENREG_MIN_PP")], 1, function(x){
      #   mytime <- ifelse(is.na(x[1]) & is.na(x[2]), NA, sum(x[1]*60, x[2], na.rm=T))
      #   mytime <- mytime/60
      # }) 
      time_under_spo2_90_h <- time_under_spo2_90_h * perc_time_under_spo2_90/100
      # minutes <- sum((DUREE_ENREG_H_PP*60),DUREE_ENREG_MIN_PP,na.rm=T)
      # heures <- minutes/60
      # time_under_spo2_90_h <- (perc_time_under_spo2_90/100)*heures
      
      bicar <- as.numeric(as.character(ALLpick[.x, "HCO3_PP"] ))
      paco2 <- ALLpick[.x, "PACO2_PP"]
    } else { #DATE_PREVENT_PP NA et DATE_RESP_PP NA ou post a vni
      last_date_bef_vni <- NA
      month_bef_vni <- NA
      dysp <- NA
      orthop <- NA
      CVF_ASSIS_perc_pred <- NA
      CVF_COUCHE_perc_pred <- NA
      SNIP_cmH2O <- NA
      SNIP_perc_pred <- NA
      PIMAX_cmH2O <- NA
      PIMAX_perc_pred <- NA
      perc_time_under_spo2_90 <- NA
      time_under_spo2_90_h <- NA
      DUREE_ENREG_H_PP <- NA
      DUREE_ENREG_MIN_PP <- NA
      bicar <- NA
      paco2 <- NA
    }
  }

  return (c(last_date_bef_vni, month_bef_vni, dysp, orthop, CVF_ASSIS_perc_pred, CVF_COUCHE_perc_pred, SNIP_cmH2O, SNIP_perc_pred, PIMAX_cmH2O, PIMAX_perc_pred,
            perc_time_under_spo2_90, time_under_spo2_90_h, bicar, paco2, DUREE_ENREG_H_PP, DUREE_ENREG_MIN_PP))
})
ALLpick <- data.frame(t(pick_data))
colnames(ALLpick) <- c("last_date_bef_vni","month_bef_vni", "dysp", "orthop", "CVF_ASSIS_perc_pred", "CVF_COUCHE_perc_pred", "SNIP_cmH2O", "SNIP_perc_pred", "PIMAX_cmH2O", "PIMAX_perc_pred",
                       "perc_time_under_spo2_90", "time_under_spo2_90_h", "bicar", "paco2", "DUREE_ENREG_H_PP", "DUREE_ENREG_MIN_PP")

ALLpick$PATIENT <- as.character(tmp$PATIENT)
ALLpick$last_date_bef_vni <- as_date(as.numeric(as.character(ALLpick$last_date_bef_vni)))
#ALLpick$DATEVNI <- tmp$DATEVNI
ALLpick <- unique(ALLpick)
names_dataset <- "ALLRESPI_bl"
assign(names_dataset,ALLpick)
saveRDS(ALLpick, paste0("data/baseline_to_merge/dependant_datevni/",names_dataset,".rds"))



#dans fchier pat (NB maintenant j'utilise bdd7(pre= surveillance respi) qui code tout simplement en 0,1. La concordance au niveau de la première donnée dispo n'était pas mal)
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
#orthopnee : je garde 0 1 2 (3 à discuter) #concordance meilleure en retirant le 3

#--------------
#Poids de forme
ALLpick <- bdd6[ ,c("PATIENT","WEIGHT_WB")]
names_dataset <- "NUTRI"
assign(names_dataset,ALLpick)
saveRDS(ALLpick, paste0("data/baseline_to_merge/dependant_datevni/",names_dataset,".rds")) #non dependant date vni mais pour éviter doublon ensuite

#--------------
#Poids, BMI, ALSFRS, echelle bulbaire

tmp <- merge(BASE_SLA[ , c("PATIENT","DATEVNI")], bdd9, by="PATIENT", all.x=T, all.y=F)
ALLpick <- tmp
for (i in colnames(ALLpick)[grep("DAT",colnames(ALLpick))]) ALLpick[,i] <- manage_date_ND(ALLpick[,i])

#ALLpick <- ALLpick[ALLpick$PATIENT==namesSLA, ]

pick_data <- sapply(1:nrow(ALLpick),function(.x){
  #browser()
  .vni <- ALLpick[.x, "DATEVNI"]
  .l <- ALLpick[.x, grep("DATEXAM_V",colnames(ALLpick))]#je selectionne toutes les colonnes DATE_PREVENT_PP et je considere que sousvent est pendant la ventilation
  .g <- as_date(as.vector(t(.l)))
  #if(length(.g)!=0 & !is.na(.vni) & any(!is.na(.g[(.vni-.g)<90]))) { 
  if(length(.g)!=0 & !is.na(.vni) &  any(!is.na(.g[.g<.vni & abs(.g-.vni)<90])) & any(.g<.vni & abs(.g-.vni)<90)) { 
    #myrow <- which(.g==min(.g[(.vni-.g)<90],na.rm=T))
    myrow <- which(.g==max(.g[.g<.vni & abs(.g-.vni)<90],na.rm=TRUE))
    namesdate <- names(.l[myrow])[1] #un patient a 1 meme date pour 2 visites differentes, et c'est justement la date juste avant vni...=> double le nombre de colonne et bug quand on rassemble
    nummonth <- ifelse(str_sub(namesdate,-3,-3)=="M", as.numeric(str_sub(namesdate,-2,-1)),as.numeric(str_sub(namesdate,-1,-1))) #ne marche que jusqu'à M99
    
    WEIGHT <- as.numeric(as.character(ALLpick[.x, paste0("WEIGHT_NUTRI_V_M",nummonth)]))
    BMI <- as.numeric(as.character(ALLpick[.x, paste0("BMI_V_M",nummonth)]))
    ALSFRS <- as.numeric(as.character(ALLpick[.x, paste0("ALS_V_M",nummonth)]))
    EBULB <- as.numeric(as.character(ALLpick[.x, paste0("E_BULBAIRE_V_M",nummonth)]))
  }else {
    WEIGHT <- NA
    BMI <- NA
    ALSFRS <- NA
    EBULB <- NA
  } 
  #browser()
  return(c(WEIGHT,BMI,ALSFRS,EBULB))
})  


#trop de données manquantes, j'essaye autre chose
#en fait non, inutile c'est des données répétées de toutes façon


ALLpick <- data.frame(t(pick_data))
colnames(ALLpick) <- c("WEIGHT","BMI","ALSFRS","EBULB")
ALLpick$PATIENT <- as.character(tmp$PATIENT)

ALLpick <- unique(ALLpick)
names_dataset <- "BASE_SUIVI_NEURO"
assign(names_dataset,ALLpick)
saveRDS(ALLpick, paste0("data/baseline_to_merge/dependant_datevni/",names_dataset,".rds"))


# #---------
# #Que sont exactement les varaiables CVF_ASSIS_ESR vs OBS vs THEO ?

# # #lequel des trois donne sitting FVC(%predicted)?
# # quantile(as.numeric(as.character(bdd7$CVF_ASSIS_ESR_PP)),na.rm=T)
# # quantile(as.numeric(as.character(bdd7$CVF_ASSIS_OBSV_PP)),na.rm=T)
# # quantile(as.numeric(as.character(bdd7$CVF_ASSIS_THEO_PP)),na.rm=T)
# # #theo est bien le %predicted (comme dans lancet):
# # quantile(as.numeric(as.character(bdd7$CVF_ASSIS_OBSV_PP))/as.numeric(as.character(bdd7$CVF_ASSIS_ESR_PP))*100,na.rm=T)
# #
# # quantile(as.numeric(as.character(bdd7$CVF_COUCHE_PP)),na.rm=T)
# # quantile(as.numeric(as.character(bdd7$CVF_ERS_PP)),na.rm=T) #espéré est la même que assis mais c'et plausible
# # quantile(as.numeric(as.character(bdd7$CVF_THEO_PP)),na.rm=T) #est-ce bien la CVF couché (%predicted)?
# # quantile(as.numeric(as.character(bdd7$CVF_COUCHE_PP))/as.numeric(as.character(bdd7$CVF_ERS_PP))*100,na.rm=T) #oui c'est quasi pareil

#------------
#twitch transdiaphragmatic pressure
#pression trans-diaphragmatique en réponse à une stimulation phrénique bilatérale (pdi,tw)

#RECHERCHES INFRUCTUEUSES

# scan_notebook("tw",.dir_sas)
# scan_notebook("pdi",.dir_sas)
# scan_notebook("stimulation",.dir_sas)
# scan_notebook("ptd",.dir_sas)
# listes_brut <- lapply(bdds, which_col,string1="pdi", type="explo")
# listes_brut <- lapply(bdds, which_col,string1="tw", type="explo")

#--------------
#MERGE des baselines dep et indep vni

name_df_baseline <- str_sub(dir("data/baseline_to_merge/indep_datevni"),1,-5)[sapply(dir("data/baseline_to_merge/indep_datevni"),function(x)is.data.frame(get(str_sub(x,1,-5))))]
for (i in name_df_baseline){
  num <- which(name_df_baseline==i)
  .bd <- get(i)
  bl_indep <- if(num==1) .bd else merge(bl_indep, .bd, by="PATIENT", suffixes= c(num-1,num),all=TRUE)
}

dfbldep <- str_sub(dir("data/baseline_to_merge/dependant_datevni"),1,-5)[sapply(dir("data/baseline_to_merge/dependant_datevni"),function(x)is.data.frame(get(str_sub(x,1,-5))))]
for (i in dfbldep){
  num <- which(dfbldep==i)
  .bd <- get(i)
  bl_dep <- if(num==1) .bd else merge(bl_dep, .bd, by="PATIENT", all=TRUE)
}
BASE_SLA_invar <- merge(BASE_SLA, bl_indep, by="PATIENT", all.x=T, all.y=F)
BASE_SLA_allbl <- merge(BASE_SLA_invar, bl_dep, by="PATIENT", all.x=T, all.y=F)


#Data management basique
BASE_SLA_allbl$DEBRILU <- as_date(BASE_SLA_allbl$DEBRILU)
BASE_SLA_allbl$FINRILU <- as_date(BASE_SLA_allbl$FINRILU)

for (i in c("dysp","orthop","CVF_ASSIS_perc_pred","CVF_COUCHE_perc_pred","SNIP_cmH2O","SNIP_perc_pred","PIMAX_cmH2O",
            "PIMAX_perc_pred","perc_time_under_spo2_90","time_under_spo2_90_h","bicar", "paco2")){
  BASE_SLA_allbl[,i] <- as.numeric(as.character(BASE_SLA_allbl[,i]))
}
BASE_SLA_allbl$LIEUDEB_recode <- as.character(BASE_SLA_allbl$LIEUDEB_recode)
BASE_SLA_allbl$month_bef_vni <- as.character(BASE_SLA_allbl$month_bef_vni)

BASE_SLA_allbl$PATIENT1 <- BASE_SLA_allbl$PATIENT

BASE_SLA_allbl$PATIENT <- anonymate_sla(BASE_SLA_allbl$PATIENT)
BASE_SLA_allbl <- BASE_SLA_allbl[,c(1,31,2:30)]


#Mise en commentaire pour ne pas modifier la clé par inadvertance
saveRDS(BASE_SLA_allbl[,-2], "data/BASE_SLA_allbl.rds")
saveRDS(BASE_SLA_allbl, "data/BASE_SLA_allbl_withnames.rds")


#-------------------------------------
#VARIABLES NEURO REPETEES:

#Poids, BMI, ALSFRS, echelle bulbaire

tmp <- merge(BASE_SLA[ , c("PATIENT","DATEVNI")], bdd9, by="PATIENT", all.x=T, all.y=F)
ALLpick <- tmp
for (i in colnames(ALLpick)[grep("DAT",colnames(ALLpick))]) ALLpick[,i] <- manage_date_ND(ALLpick[,i])

.l <- lapply(namesSLA, function(i){
  var1 <- manage_date_ND(t(ALLpick[ALLpick$PATIENT==i, grep("DATEXAM_V_M",colnames(ALLpick))]))
  .tmp <- data.frame(var1) ; colnames(.tmp) <- "DATEXAM_V_M" ; rownames(.tmp) <- paste0(i,1:nrow(.tmp))
  .tmp$PATIENT <- i
  .tmp$DATEVNI <- ALLpick[ ALLpick$PATIENT==i, "DATEVNI"]
  
  var_rep_neuro <- c("WEIGHT_NUTRI_V_M","BMI_V_M","ALS_V_M","E_BULBAIRE_V_M")  
  
  for (j in var_rep_neuro) {
    numvar <- which(j==var_rep_neuro)
    
    var <- j
    var2 <- t(ALLpick[ALLpick$PATIENT==i, grep(var,colnames(ALLpick))])
    if(j=="BMI_V_M") {
      var2 <- data.frame(var2)
      var2 <- var2[!rownames(var2)%in% rownames(var2)[grep("CL", rownames(var2))], ]
    }
    .res <- if(numvar==1) cbind(.tmp,var2) else cbind(.res,var2); colnames(.res)[ncol(.res)] <- j
  }
  .res <- .res[!is.na(.res$DATEXAM_V_M), ]
  #selectionne la plus récente date antérieure à VNI 
  max_date_bef_vni <- max(.res$DATEXAM_V_M[.res$DATEXAM_V_M<=.res$DATEVNI])
  #selectionne la date la plus proche de date vni
  #.res$DATEXAM_V_M[which(abs(.res$DATEXAM_V_M - .res$DATEVNI)==min(abs(.res$DATEXAM_V_M - .res$DATEVNI)))]
  .res <- .res[.res$DATEXAM_V_M>=max_date_bef_vni,]

  return(.res)
})

base_rep_neuro <- do.call(rbind, .l)
saveRDS(base_rep_neuro, "data/data_rep/base_rep_neuro.rds")



# #récupérer info à baseline (inutile finalement)
# nummonth <- ifelse(str_sub(namesdate,-3,-3)=="M", as.numeric(str_sub(namesdate,-2,-1)),as.numeric(str_sub(namesdate,-1,-1))) #ne marche que jusqu'à M99
# 
# pick_data <- sapply(1:nrow(ALLpick),function(.x){
#   #browser()
#   .vni <- ALLpick[.x, "DATEVNI"]
#   .l <- ALLpick[.x, grep("DATEXAM_V",colnames(ALLpick))]#je selectionne toutes les colonnes DATE_PREVENT_PP et je considere que sousvent est pendant la ventilation
#   .g <- as_date(as.vector(t(.l)))
#   if(length(.g)!=0 & !is.na(.vni) & any(!is.na(.g[(.vni-.g)<90]))) { 
#     myrow <- which(.g==min(.g[(.vni-.g)<90],na.rm=T))
#     namesdate <- names(.l[myrow])
#     nummonth <- ifelse(str_sub(namesdate,-3,-3)=="M", as.numeric(str_sub(namesdate,-2,-1)),as.numeric(str_sub(namesdate,-1,-1))) #ne marche que jusqu'à M99
#     
#     WEIGHT <- ALLpick[.x, paste0("WEIGHT_NUTRI_V_M",nummonth)]
#     BMI <- ALLpick[.x, paste0("BMI_V_M",nummonth)]
#     ALSFRS <- ALLpick[.x, paste0("ALS_V_M",nummonth)]
#     EBULB <- ALLpick[.x, paste0("E_BULBAIRE_V_M",nummonth)]
#   }else {
#     WEIGHT <- NA
#     BMI <- NA
#     ALSFRS <- NA
#     EBULB <- NA
#   } 
#   #browser()
#   return(c(WEIGHT,BMI,ALSFRS,EBULB))
# })  
# ALLpick <- data.frame(t(pick_data))
# colnames(ALLpick) <- c("WEIGHT","BMI","ALSFRS","EBULB")
# ALLpick$PATIENT <- as.character(tmp$PATIENT)
# 
# ALLpick <- unique(ALLpick)
# names_dataset <- "BASE_SUIVI_NEURO"
# assign(names_dataset,ALLpick)
# saveRDS(ALLpick, paste0("data/baseline_to_merge/dependant_datevni/",names_dataset,".rds"))

#----

#VARIABLES PNEUMO REPETEES : base de donnes bdd7 (pre/)
#tmp <- merge(BASE_SLA[ , c("PATIENT","DATEVNI")], bdd7, by="PATIENT", all.x=T, all.y=F)

tmp <- merge(BASE_SLA[ , c("PATIENT","DATEVNI")], bdd7, by="PATIENT", all.x=T, all.y=T)
ALLpick <- tmp
#for (i in colnames(ALLpick)[grep("DAT",colnames(ALLpick))]) ALLpick[,i] <- manage_date_ND(ALLpick[,i])
for (i in colnames(ALLpick)[grep("DATEVNI",colnames(ALLpick))]) ALLpick[,i] <- manage_date_ND(ALLpick[,i])


.l <- lapply(unique(namesallSLA), function(i){ #ne sert que pour voir les infos dispo afin de voir si algorythme pertinent
  #.l <- lapply(namesSLA, function(i){
  #.l <- lapply("AIME_CORINNE", function(i){
  print(paste(which(i==namesallSLA), "/", length(namesallSLA)))
  #browser()
  namesPP <- colnames(ALLpick)[grep("DATE_RESP_PP",colnames(ALLpick))]
  namesPV <- colnames(ALLpick)[grep("DATE_PREVENT_PP",colnames(ALLpick))]
  namesSV <- colnames(ALLpick)[grep("DATE_SOUSVENT",colnames(ALLpick))]
  var1 <- manage_date_ND(t(ALLpick[ALLpick$PATIENT==i, namesPP]))
  var2 <- manage_date_ND(t(ALLpick[ALLpick$PATIENT==i, namesPV]))
  var3 <- manage_date_ND(t(ALLpick[ALLpick$PATIENT==i, namesSV]))
  dateTOT <- data.frame(c(var1,var2,var3)) ; colnames(dateTOT) <- "DATEXAM_RESPI" #; rownames(dateTOT) <- c(namesPP, namesPV,namesSV)
  #dateTOT$DATEVNI <- ALLpick[ ALLpick$PATIENT==i, "DATEVNI"]
  dateTOT$PATIENT <- ALLpick[ ALLpick$PATIENT==i, "PATIENT"]
  dateTOT$visite <- c(namesPP, namesPV,namesSV)
  #browser()
  
  listv <- list(c("DYSP_DECUBI_PP","DYSP_DECUBI_PV","ORTHOPN_SV_F"), c("PACO2_PP","PACO2_PV","PACO2_SV_F"), c("PAO2_PP","PAO2_PV","PAO2_SV_F"), 
                c("HCO3_PP", "HCO3_PV_F", "HCO3_SV_F"), c("PH_PP", "PH_PV_F", "PH_SV_F"), c("CEPHAL_PP", "CEPHAL_PV_F", "CEPHAL_SV_F"))
  for (j in listv){
    #browser()
    namesPP <- colnames(ALLpick)[grep(j[1],colnames(ALLpick))]
    namesPV <- colnames(ALLpick)[grep(j[2],colnames(ALLpick))]
    namesSV <- colnames(ALLpick)[grep(j[3],colnames(ALLpick))][! grep(j[3],colnames(ALLpick)) %in% grep("CL1",colnames(ALLpick))]
    var1 <- t(ALLpick[ALLpick$PATIENT==i, namesPP])
    var2 <- t(ALLpick[ALLpick$PATIENT==i, namesPV])
    var3 <- t(ALLpick[ALLpick$PATIENT==i, namesSV])
    dateTOT2 <- data.frame(c(var1,var2,var3)) #; rownames(dateTOT2) <- c(namesPP, namesPV,namesSV)
    colnames(dateTOT2) <- str_sub(j[1],1,-4)
    resresp <- if (length(which(j[1]==listv[[1]]))!=0) cbind(dateTOT, dateTOT2) else cbind(resresp, dateTOT2)
  }
  resresp  <-  resresp [!is.na( resresp $DATEXAM_RESPI),]
})

bvr <- do.call(rbind, .l)


# namesPP <- colnames(ALLpick)[grep("DYSP_DECUBI_PP",colnames(ALLpick))]
# namesPV <- colnames(ALLpick)[grep("DYSP_DECUBI_PV",colnames(ALLpick))]
# namesSV <- colnames(ALLpick)[grep("ORTHOPN_SV_F",colnames(ALLpick))]
# var1 <- t(ALLpick[ALLpick$PATIENT==i, namesPP])
# var2 <- t(ALLpick[ALLpick$PATIENT==i, namesPV])
# var3 <- t(ALLpick[ALLpick$PATIENT==i, namesSV])
# dateTOT2 <- data.frame(c(var1,var2,var3))#rownames(dateTOT) <- c(namesPP, namesPV,namesSV)
# colnames(dateTOT2) <- "DYSP_DECUBI"
# dateTOT <- cbind(dateTOT, dateTOT2)
# 
# namesPP <- colnames(ALLpick)[grep("PACO2_PP",colnames(ALLpick))]
# namesPV <- colnames(ALLpick)[grep("PACO2_PV",colnames(ALLpick))]
# namesSV <- colnames(ALLpick)[grep("PACO2_SV_F",colnames(ALLpick))][! grep("PACO2_SV_F",colnames(ALLpick)) %in% grep("CL1",colnames(ALLpick))]
# var1 <- t(ALLpick[ALLpick$PATIENT==i, namesPP])
# var2 <- t(ALLpick[ALLpick$PATIENT==i, namesPV])
# var3 <- t(ALLpick[ALLpick$PATIENT==i, namesSV])
# dateTOT2 <- data.frame(c(var1,var2,var3))#rownames(dateTOT) <- c(namesPP, namesPV,namesSV)
# colnames(dateTOT2) <- "PACO2"
# dateTOT <- cbind(dateTOT, dateTOT2)
# 
# namesPP <- colnames(ALLpick)[grep("PAO2_PP",colnames(ALLpick))]
# namesPV <- colnames(ALLpick)[grep("PAO2_PV",colnames(ALLpick))]
# namesSV <- colnames(ALLpick)[grep("PAO2_SV_F",colnames(ALLpick))][! grep("PAO2_SV_F",colnames(ALLpick)) %in% grep("CL1",colnames(ALLpick))]
# var1 <- t(ALLpick[ALLpick$PATIENT==i, namesPP])
# var2 <- t(ALLpick[ALLpick$PATIENT==i, namesPV])
# var3 <- t(ALLpick[ALLpick$PATIENT==i, namesSV])
# dateTOT2 <- data.frame(c(var1,var2,var3))#rownames(dateTOT) <- c(namesPP, namesPV,namesSV)
# colnames(dateTOT2) <- "PAO2"
# dateTOT <- cbind(dateTOT, dateTOT2)


max_date_bef_vni <- max(.res$DATEXAM_V_M[.res$DATEXAM_V_M<.res$DATEVNI])
.res <- .res[.res$DATEXAM_V_M>=max_date_bef_vni,] 
})


#ORTHOPNEE: pb = la derniere est sous ventilation

#pb : certaines variables sont plus compliquées comme PAO2 et PACO2 
# listv <- list(c("DYSP_DECUBI_PP","DYSP_DECUBI_PV","ORTHOPN_SV_F"), c("PACO2_PP","PACO2_PV","PACO2_SV_F"))
# for (j in listv[2]){
#   #browser()
#   namesPP <- colnames(ALLpick)[grep(j[1],colnames(ALLpick))]
#   namesPV <- colnames(ALLpick)[grep(j[2],colnames(ALLpick))]
#   namesSV <- colnames(ALLpick)[grep(j[3],colnames(ALLpick))]
#   var1 <- t(ALLpick[ALLpick$PATIENT==i, namesPP])
#   var2 <- t(ALLpick[ALLpick$PATIENT==i, namesPV])
#   var3 <- t(ALLpick[ALLpick$PATIENT==i, namesSV])
#   dateTOT2 <- data.frame(c(var1,var2,var3)) ; rownames(dateTOT2) <- c(namesPP, namesPV,namesSV)
#   .res <- if (length(which(j[1]==listv[[1]]))!=0) cbind(dateTOT, dateTOT2) else cbind(.res, dateTOT2)
# } 

  
.l <- lapply(namesSLA, function(i){
  var1 <- manage_date_ND(t(ALLpick[ALLpick$PATIENT==i, grep("DATEXAM_V_M",colnames(ALLpick))]))
  .tmp <- data.frame(var1) ; colnames(.tmp) <- "DATEXAM_V_M" ; rownames(.tmp) <- paste0(i,1:nrow(.tmp))
  .tmp$PATIENT <- i
  .tmp$DATEVNI <- ALLpick[ ALLpick$PATIENT==i, "DATEVNI"]
  
  var_rep_neuro <- c("WEIGHT_NUTRI_V_M","BMI_V_M","ALS_V_M","E_BULBAIRE_V_M")  
  
  for (j in var_rep_neuro) {
    numvar <- which(j==var_rep_neuro)
    
    var <- j
    var2 <- t(ALLpick[ALLpick$PATIENT==i, grep(var,colnames(ALLpick))])
    if(j=="BMI_V_M") {
      var2 <- data.frame(var2)
      var2 <- var2[!rownames(var2)%in% rownames(var2)[grep("CL", rownames(var2))], ]
    }
    .res <- if(numvar==1) cbind(.tmp,var2) else cbind(.res,var2); colnames(.res)[ncol(.res)] <- j
  }
  .res <- .res[!is.na(.res$DATEXAM_V_M), ]
  #selectionne la plus récente date antérieure à VNI 
  max_date_bef_vni <- max(.res$DATEXAM_V_M[.res$DATEXAM_V_M<.res$DATEVNI])
  #selectionne la date la plus proche de date vni
  #.res$DATEXAM_V_M[which(abs(.res$DATEXAM_V_M - .res$DATEVNI)==min(abs(.res$DATEXAM_V_M - .res$DATEVNI)))]
  .res <- .res[.res$DATEXAM_V_M>=max_date_bef_vni,]
  
  return(.res)
})

base_rep_neuro <- do.call(rbind, .l)
saveRDS(base_rep_neuro, "data/data_rep/base_rep_neuro.rds")


#---
#ancienne version


#pour avoir date de la baseline 
bef_vni <- readRDS("data/baseline_to_merge/dependant_datevni/ALLRESPI_bl.rds")
bef_vni <- bef_vni[bef_vni$PATIENT %in% BASE_SLA_allbl$PATIENT1, ]
bef_vni <- bef_vni[ ,c("PATIENT","last_date_bef_vni","month_bef_vni")]
#SATISF_VENTIL

#variables répétées
lapply(bdds, which_col,string1="EPWORTH_VENT_SV", type="explo")
var_rep <- c("DATE_SOUSVENT_SV_F","SATISF_VENTIL_SV_","UTIL_VENTIL_DIURN","UTIL_VENTIL_NOCT","CAUSE_V_SATISF_SV_CHOICE_1_","CAUSE_V_SATISF_SV_CHOICE_2_","CAUSE_V_SATISF_SV_CHOICE_3_","CAUSE_V_SATISF_SV_CHOICE_4_",
             "DUREE_SOMM_VENT_SV_","QUALIT_SOMM_VENT_SV_","EVOL_SOMM_VNI_SV_","REVEIL_VENT_SV",
             "ACAUSE_R_VENT_SV_CHOICE_1_","ACAUSE_R_VENT_SV_CHOICE_2_","ACAUSE_R_VENT_SV_CHOICE_3_","ACAUSE_R_VENT_SV_CHOICE_4_",
             "NYCTUR_SV","ORTHOPN_SV_","DYSPN_SVENT_SV_","DYSPN_SOUSVENT_SV_","CEPHAL_SV_","SOMNOL_SV","EPWORTH_VENT_SV")

for (i in var_rep){
  num <- which(var_rep==i)
  listes_brut <- lapply(bdds, which_col,string1=i, type="merge", keep_col_NA=TRUE)
  listes_net <- listes_brut[sapply(listes_brut,function(x)!is.null(x))] #supprimer les élements de la liste sans information
  listes_net <- data.frame(listes_net)
  listes_net <- listes_net[listes_net$PATIENT %in% BASE_SLA_allbl$PATIENT1, ]
  listes_net <- reshape (listes_net, direction="long",idvar = "PATIENT",
                 varying=grep(i,colnames(listes_net)), sep="")
  #Création de la valeur time=0
  
  
  if (i=="DATE_SOUSVENT_SV_F") {
    #browser()
  listes_net$DATE_SOUSVENT_SV_F <- manage_date_ND(listes_net$DATE_SOUSVENT_SV_F)
  #bdd_0 <- bef_vni[ ,c("PATIENT","last_date_bef_vni")]
  bdd_0 <- sla[ ,c("PATIENT1","DATEVNI")]
  colnames(bdd_0)[1] <- "PATIENT"
  colnames(bdd_0)[2] <- names(listes_net)[3]
  } else {
    #browser()
    bdd_0 <- listes_net[listes_net$time==1 ,c("PATIENT", "time")]
    bdd_0[ ,names(listes_net)[3]] <- NA
  }
  bdd_0$time <- 0
  
  #browser()
  listes_net <- rbind(bdd_0,listes_net)
  bdd_rep <- if(num==1) listes_net else merge(bdd_rep, listes_net, by=c("PATIENT","time"), all=TRUE)
}

bdd_rep <- bdd_rep[order(bdd_rep$PATIENT,bdd_rep$time),]

# t0 est bien = dateVNI? => délai =0?
bdd_rep <- merge(bdd_rep, sla[, c("PATIENT1","DATEVNI")], by.x="PATIENT", by.y="PATIENT1",all=T)
#calcul du délai (vrai time)
bdd_rep$time <- as.numeric(bdd_rep$DATE_SOUSVENT_SV_F - bdd_rep$DATEVNI)

saveRDS(bdd_rep, "data/essairep.rds")

#--------
#--------
#BASE POUR ANALYSE DE TOUS LES SLA
all_SLA <- BASE_TOT[BASE_TOT$diag==1 & !is.na(BASE_TOT$diag), ] 
.tmp <- all_SLA
table(tab <- table(.tmp$PATIENT))
namesdoublons <- names(tab)[tab>1]
.tmp <- .tmp[!.tmp$PATIENT %in% namesdoublons, ]
.tmp$DOB <- NULL
.tmp$ddn <- as_date(.tmp$ddn)
all_SLA <- .tmp


#-----------
#variables

#var respi à baseline #je prend PP
tmp <- merge(BASE_SLA[ , c("PATIENT","DATEVNI")], bdd7, by="PATIENT", all.x=T, all.y=F)
ALLpick <- tmp
for (i in colnames(ALLpick)[grep("DAT",colnames(ALLpick))]) ALLpick[,i] <- manage_date_ND(ALLpick[,i])
pick_data <- sapply(1: nrow(ALLpick),function(.x){
  
date_exam_resp <- ALLpick[.x, "DATE_RESP_PP"]
dysp <- ALLpick[.x, "DYSP_REPOS_PP"]
orthop <- ALLpick[.x, "DYSP_DECUBI_PP"]
CVF_ASSIS_perc_pred <- as.numeric(as.character(ALLpick[.x, "CVF_ASSIS_THEO_PP"]))
CVF_COUCHE_perc_pred <- as.numeric(as.character(ALLpick[.x, "CVF_THEO_PP"]))
SNIP_cmH2O <- as.numeric(as.character(ALLpick[.x, "SNIP_PP"]))
SNIP_perc_pred <- as.numeric(as.character(ALLpick[.x, "SNIP_THEO_PP"]))
PIMAX_cmH2O <- as.numeric(as.character(ALLpick[.x, "PIMAX_PP"]))
PIMAX_perc_pred <- as.numeric(as.character(ALLpick[.x, "PIMAX_THEO_PP"]))

perc_time_under_spo2_90 <- as.numeric(as.character(ALLpick[.x, "SPO2_TPS_PP"]))

DUREE_ENREG_H_PP <- as.numeric(as.character(ALLpick[.x, "DUREE_ENREG_H_PP"]))
DUREE_ENREG_MIN_PP <- as.numeric(as.character(ALLpick[.x, "DUREE_ENREG_MIN_PP"]))
time_under_spo2_90_h <- ifelse(is.na(DUREE_ENREG_H_PP) & is.na(DUREE_ENREG_MIN_PP), NA, sum(DUREE_ENREG_H_PP*60, DUREE_ENREG_MIN_PP, na.rm=T))
time_under_spo2_90_h <- time_under_spo2_90_h/60
time_under_spo2_90_h <- time_under_spo2_90_h * perc_time_under_spo2_90/100

bicar <- as.numeric(as.character(ALLpick[.x, "HCO3_PP"] ))
paco2 <- ALLpick[.x, "PACO2_PP"]

return (c(date_exam_resp, dysp, orthop, CVF_ASSIS_perc_pred, CVF_COUCHE_perc_pred, SNIP_cmH2O, SNIP_perc_pred, PIMAX_cmH2O, PIMAX_perc_pred,
          perc_time_under_spo2_90, time_under_spo2_90_h, bicar, paco2, DUREE_ENREG_H_PP, DUREE_ENREG_MIN_PP))
})
ALLpick <- data.frame(t(pick_data))
colnames(ALLpick) <- c("date_exam_resp", "dysp", "orthop", "CVF_ASSIS_perc_pred", "CVF_COUCHE_perc_pred", "SNIP_cmH2O", "SNIP_perc_pred", "PIMAX_cmH2O", "PIMAX_perc_pred",
                       "perc_time_under_spo2_90", "time_under_spo2_90_h", "bicar", "paco2", "DUREE_ENREG_H_PP", "DUREE_ENREG_MIN_PP")

ALLpick$PATIENT <- as.character(tmp$PATIENT)
#ALLpick$DATEVNI <- tmp$DATEVNI
ALLpick <- unique(ALLpick)
names_dataset <- "ALLRESPI_bl_all"
assign(names_dataset,ALLpick)

#var neuro et nutri à baseline :bdd6

#BMI à l'inclusion
#c("BMI","BMI_CL1")] #même info mais BMI est plus précise
#c("WEIGHT_REF","WEIGHT_WB")] #même info mais WEIGHT_WB renseigné plus souvent : poids de forme
#c("WEIGHT_NUTRI","WEIGHT")] #WEIGHT_NUTRI plus souvent renseigné #même info 
#Donc je prend BMI, WEIGHT WB, WEIGHT_NUTRI et DATE_NUTRI (et DATE_EXAM car DATE_NUTRI mal renseignée mais probablement meme date)

NEURO_NUTRI <- bdd6[,c("PATIENT","DATE_NUTRI", "DATEXAM","WEIGHT_NUTRI","BMI","WEIGHT_WB","ALS","E_BULBAIRE")]
for (i in c("DATEXAM","DATE_NUTRI")) NUTRI[,i] <- manage_date_ND(NUTRI[,i])
for (i in c("WEIGHT_NUTRI","BMI","WEIGHT_WB")) NUTRI[,i] <- as.numeric(as.character(NUTRI[,i]))


#indep : "DOB", "familial", "sex_def", "LIEUDEB_recode"
all_SLA_bl <- merge(all_SLA, bl_indep, by="PATIENT", all.x=T, all.y=F)
all_SLA_bl <- merge(all_SLA_bl, RILU, by="PATIENT", all.x=T, all.y=F) #donne les date de rilu donc ne depend pas de VNI
all_SLA_bl <- merge(all_SLA_bl, NEURO_NUTRI, by="PATIENT", all.x=T, all.y=F)
all_SLA_bl <- merge(all_SLA_bl, ALLRESPI_bl_all, by="PATIENT", all.x=T, all.y=F)



#Data management basique
all_SLA_bl$DEBRILU <- as_date(all_SLA_bl$DEBRILU)
all_SLA_bl$FINRILU <- as_date(all_SLA_bl$FINRILU)

for (i in c("dysp","orthop","CVF_ASSIS_perc_pred","CVF_COUCHE_perc_pred","SNIP_cmH2O","SNIP_perc_pred","PIMAX_cmH2O",
            "PIMAX_perc_pred","perc_time_under_spo2_90","time_under_spo2_90_h","bicar","paco2")){
  all_SLA_bl[,i] <- as.numeric(as.character(all_SLA_bl[,i]))
}
all_SLA_bl$LIEUDEB_recode <- as.character(all_SLA_bl$LIEUDEB_recode)
all_SLA_bl$PATIENT1 <- all_SLA_bl$PATIENT

#all_SLA_bl$PATIENT <- anonymate_sla(all_SLA_bl$PATIENT)
#all_SLA_bl <- all_SLA_bl[,c(1,31,2:30)]
#saveRDS(all_SLA_bl[,-2], "data/all_SLA_bl.rds")
saveRDS(all_SLA_bl, "data/all_SLA_bl.rds")

#-----------------------------
#-----------------------------
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
#vnisla<-z[z$sla==1 & z$vni==1,]
vnisla<-z[z$vni==1,]
head(vnisla)
vnisla$n<-NULL
vnisla$nechec<-NULL
vnisla$nvni<-NULL

#d<-tapply(vnisla$datevni, vnisla$PATIENT, min) #date min de vni pour chaque patient: ATTENTION si la min est un echec, ça prend cette date quand meme!
d<-tapply(vnisla$datevni[vnisla$echec==0 | is.na(vnisla$echec)], vnisla$PATIENT[vnisla$echec==0 | is.na(vnisla$echec)], min) #ne prend pas en compte les echecs
d<-data.frame(PATIENT=names(d), datevni1=as.numeric(d))
vnisla<-merge(vnisla, d, by="PATIENT", all=T)
class(vnisla$datevni1)<-"Date"
head(vnisla)

x<-tapply(vnisla$vni, vnisla$PATIENT, sum) #nb de ligne permettant de definir une date de vni par patient
x<-data.frame(PATIENT=names(x), nvni=as.numeric(x))
vnisla<-merge(vnisla, x, by="PATIENT", all=T)

vnisla$echec[is.na(vnisla$echec)]<-0 #nb d'échec de vni
x<-tapply(vnisla$echec, vnisla$PATIENT, sum)
x<-data.frame(PATIENT=names(x), nechec=as.numeric(x))
vnisla<-merge(vnisla, x, by="PATIENT", all=T)

n<-table(vnisla$PATIENT) #nb de lignes avec une info sur la vni 
n<-data.frame(PATIENT=names(n), n=as.numeric(n))
vnisla<-merge(vnisla, n, by="PATIENT", all=T)
addmargins(table(vnisla$n))



#délai entre VNI et suivi?


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
#792 patients avec une datevni (qui sont donc SLA quoi qu'en dise la base)

saveRDS(vnisla, "data/vnisla.rds")

names_vnisla <- names(table(unique(vnisla$PATIENT)))
#---------------
#--------------
#essai reshape
#doublons de bdd9

tmp <- bdd9[bdd9$PATIENT%in%names_vnisla, ]
tmp <- merge(tmp, vnisla[,c("PATIENT", "datevni")])
for (i in colnames(tmp)[grep("DAT", colnames(tmp))]) tmp[,i] <- manage_date_ND(tmp[,i])
tab <- table(tmp$PATIENT)
db <- names(tab) [tab>1]
bdd9[bdd9$PATIENT%in%db, 1:100] #doublons foireux dans la base bdd9 
tmp <- tmp[!tmp$PATIENT %in% db, ]

n1 <- names(tmp)[ grep("WEIGHT_V_M", names(tmp))][1:10]
n2 <- names(tmp)[ grep("DATEXAM_V_M", names(tmp))][1:10]
#variables qui varient :
names_nomonth <- unique(ifelse(str_sub(names(tmp)[ grep("_M", names(tmp))],-3,-3)=="M", str_sub(names(tmp)[grep("_M", names(tmp))],1,-3), str_sub(names(tmp)[grep("_M", names(tmp))],1,-2)))
tmp2 <- tmp[1:10, c("PATIENT",n1,n2)]

tmp3 <- reshape (data=tmp2, direction="long", varying = c(n1,n2), sep="")
tmp3 <- reshape (data=tmp2, direction="long", varying = split(names(tmp)[ grep("_M", names(tmp))]), sep="")
tmp3[order(tmp3$PATIENT, tmp3$DATEXAM_V_M),]
c("WEIGHT_WB", "WEIGHT_NUTRI_V")
#------------------
#------------------
#pour recuperer toutes les valeurs pneumo

#----
#selectionner tous les couples de variables pneumo existant
#attention certaines variables ne sont pas selectionnees a cause des CL1, voir var neuro p-e (rajoutees à la main dans tableau csv en attendant)
# [1] "PIMAX_PV_F1_CL1"      "PEMAX_PV_F1_CL1"      "SPO2_EVEIL_PV_F1_CL1" "PAO2_SV_F1_CL1"      
# [5] "SAO2_SV_F1_CL1"       "PACO2_SV_F1_CL1"      "HCO3_SV_F1_CL1"       "PH_SV_F1_CL1" 
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
#je ne garde donc que les SV_Fx_CL1

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

# df_rep_pneumo <- rbind(df_rep_pneumo, get_var_bl_suivi_pneumo(c("NYCTURIE_PP", "NYCTURIE_PV",	"NYCTUR_SV")))
# df_rep_pneumo <- rbind(df_rep_pneumo, get_var_bl_suivi_pneumo(c("SOMNOLENCE_PP", "SOMNOLENCE_PV","SOMNOL_SV")))
# df_rep_pneumo <- rbind(df_rep_pneumo, get_var_bl_suivi_pneumo(c("EPWORTH_PP",	"EPWORTH_PV",	"EPWORTH_VENT_SV")))
# df_rep_pneumo <- rbind(df_rep_pneumo, get_var_bl_suivi_pneumo(c("MORPHO_PP_CHOICE_1",	"MORPHO_PV_CHOICE_1",	"MORPHO_SUR_SV_CHOICE_1")))
# df_rep_pneumo <- rbind(df_rep_pneumo, get_var_bl_suivi_pneumo(c("MORPHO_PP_CHOICE_2",	"MORPHO_PV_CHOICE_2",	"MORPHO_SUR_SV_CHOICE_2")))


saveRDS(df_rep_pneumo, "data/df_rep_pneumo.rds")

# # vs<-c("DATE_RESP", "DATE_PREVENT", "DATE_SOUSVENT")
# # extvs<-c("PP", "PP", "SV")
# # Ws<-c("PAO2", "PAO2", "PAO2") #a changer 
# # Ws<-c("PACO2", "PACO2", "PACO2") #a changer 
# # extws<-c("PP", "PV", "SV")
# vnisla <- readRDS("data/vnisla.rds")
# 
# 
# get_var_blf0_suivif1 <- function(Ws){
#   print(Ws)
#   vs<-c("DATE_RESP", "DATE_PREVENT", "DATE_SOUSVENT")
#   extvs<-c("PP", "PP", "SV")
#   extws<-c("PP", "PV", "SV")
#   
#   for (ie in 1:length(extvs)) {
#     #browser()
#     # ie<-ie+1  
#     v<-c(vs[ie], Ws[ie])
#     v<-paste(v, c(extvs[ie], extws[ie]), sep="_")
#     r<-c("", paste("_F", 1:100, sep=""))
#     vr<-expand.grid(list(v=v, r=r))
#     vr$f<-as.numeric(gsub("_F", "", vr$r, fix=T))
#     vr$f[is.na(vr$f)]<-0
#     vr$vr<-paste(vr$v, vr$r, sep="")
#     vr<-vr[vr$vr%in%names(bdd7),]
#     dim(vr)
#     w<-c("date", "x")
#     vrs<-split(vr, vr$f)
#     names(vrs)
#     vrs$"0"
#     vrs$"2"
#     for (iv in 1:length(vrs)) { 
#       #browser()
#       y<-bdd7[, c("PATIENT", vrs[[iv]]$vr)]
#       names(y)[-1]<-w
#       #y$ext<-ext #erreur
#       y$ext<-extws[ie]
#       #y$qui<-W #erreur
#       y$qui<-Ws[1]
#       y$f<-vrs[[iv]]$f[1]
#       if (iv==1) {
#         Yv<-y
#       } else {
#         Yv<-rbind(Yv, y)
#       }
#     }
#     Yv<-Yv[order(Yv$PATIENT, Yv$f),]
#     Yv$date<-as.Date(as.character(Yv$date), "%d/%m/%Y")
#     Yv$x<-as.numeric(as.character(Yv$x))
#     Yv<-Yv[!is.na(Yv$x) & !is.na(Yv$date),]
#     head(Yv)
#     if (ie==1) {
#       YYv<-Yv
#     } else {
#       YYv<-rbind(YYv, Yv)
#     }
#   }
#   #y<-merge(YYv, vnisla[, c("PATIENT", "date")], by="PATIENT", all=F, suff=c("", ".vni"))
#   y<-merge(YYv, vnisla[, c("PATIENT", "datevni")], by="PATIENT", all=F)
#   
#   #colonne delai(del) entre la date d'exam et la date de vni
#   #y$del<-as.numeric(y$date-y$date.vni)
#   y$del<-as.numeric(y$date-y$datevni)
#   y<-y[!is.na(y$del),]
#   
#   y<-y[order(y$PATIENT, y$date),]
#   head(y)
#   
#   #selection des variables de suivi : toutes
#   yp<-y[y$del>0,] 
#   head(yp)
#   summary(yp)
#   yp$f<-1 #1 pour suivi (en opposition à baseline)
#   
#   #selection valeur de baseline
#   yn<-y[y$del<=0,]
#   head(yn)
#   yn<-yn[order(yn$PATIENT, -yn$del),] #delai le plus petit en première ligne 
#   head(yn)
#   id<-unique(yn$PATIENT);id
#   dim(yn)
#   yn<-yn[match(id, yn$PATIENT),] #prend la première ligne de yn qui matche avec id (donc prend ligne correspondant au délai le plus petit pour chaque patient)
#   dim(yn)
#   head(yn)
#   summary(yn)
#   yn$f<-0 #0 pour baseline
#   
#   #je rassemble les tableaux baseline(yn) et suivi(yp)
#   y<-rbind(yp, yn)
#   y<-y[order(y$PATIENT, y$del),]
#   head(y)
#   return(y)
# }
# 
# 
# 
# 
# PaO2 <- get_var_blf0_suivif1(c("PAO2", "PAO2", "PAO2"))
# PaCO2 <- get_var_blf0_suivif1(c("PACO2", "PACO2", "PACO2"))
# listv <- list(c("DYSP_DECUBI","DYSP_DECUBI","ORTHOPN"), c("PACO2","PACO2","PACO2"), c("PAO2","PAO2","PAO2"), 
#               c("HCO3", "HCO3", "HCO3"), c("PH", "PH", "PH"), c("CEPHAL", "CEPHAL", "CEPHAL"))
# 
# .l <- lapply(listv,get_var_blf0_suivif1)
# tab_rep_pneumo <- do.call(rbind, .l)
# saveRDS(tab_rep_pneumo, "data/tab_rep_pneumo.rds")
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
#NB : 3 var pourlesquels il faudra une autre fonction pour les récupérer, à cause du codage:
#PAO2_SV_F1_CL1, SAO2_SV_F1_CL1, PACO2_SV_F1_CL1, HCO3_SV_F1_CL1, PH_SV_F1_CL1

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



# #selectionner tous les couples de variables existant :tableau qui peut être passé en argument de get_var_blf0_suivif1_neuro
# v1 <- names(bdd6)          
# v2 <- paste0(v1, "_V")
# r<-c(paste("_M", 1, sep=""))
# vr<-expand.grid(list(v=v2, r=r))
# vr$vr<-paste(vr$v, vr$r, sep="")
# vr<-vr[vr$vr%in%names(bdd9),]
# vr$base <- as.character(str_sub(vr$v, 1,-3))
# vr$suivi <- as.character(vr$v)
# list(vr$base, vr$suivi)
# vr <- vr[! vr$suivi %in% vr$suivi[grep("DAT", vr$suivi)], ]
# table(table(vr$base[vr$base %in% names(bdd6)]))
# saveRDS (vr, "data/variables_neuro.rds")
# 
# #appliquer la fonction à tous les couples de variables
# .l <- mapply(get_var_blf0_suivif1_neuro, Ws1 = vr$base, Ws2 = vr$suivi)
# tab_rep_neuro <- do.call(rbind, .l)


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

#ancienne version : version get_var_blf0_suivif1_neuro <- function(Ws){ (n'existe plus mais facile à refaire en suprrimant la ligne Ws <- c(Ws1, Ws2))
#listvn <- list(c("WEIGHT_NUTRI", "WEIGHT_NUTRI_V"), c("BMI","BMI_V"), c("ALS", "ALS_V"))
#.l <- lapply(listvn,get_var_blf0_suivif1_neuro) 
#tab_rep_neuro <- do.call(rbind, .l)


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


#---------------
#---------------
#---------------
#---------------

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
