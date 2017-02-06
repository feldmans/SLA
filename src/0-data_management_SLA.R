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

tmp <- merge(bdd9,months_before_vni,by="PATIENT",all.y=T)
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
  if(length(.g)!=0 & !is.na(.vni) & any(!is.na(.g[(.vni-.g)<90]))) { 
    myrow <- which(.g==min(.g[(.vni-.g)<90],na.rm=T))
    namesdate <- names(.l[myrow])
    nummonth <- ifelse(str_sub(namesdate,-3,-3)=="M", as.numeric(str_sub(namesdate,-2,-1)),as.numeric(str_sub(namesdate,-1,-1))) #ne marche que jusqu'à M99
    
    WEIGHT <- ALLpick[.x, paste0("WEIGHT_NUTRI_V_M",nummonth)]
    BMI <- ALLpick[.x, paste0("BMI_V_M",nummonth)]
    ALSFRS <- ALLpick[.x, paste0("ALS_V_M",nummonth)]
    EBULB <- ALLpick[.x, paste0("E_BULBAIRE_V_M",nummonth)]
  }else {
    WEIGHT <- NA
    BMI <- NA
    ALSFRS <- NA
    EBULB <- NA
  } 
  #browser()
  return(c(WEIGHT,BMI,ALSFRS,EBULB))
})  
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
#VARIABLES REPETEES : base de donnes bdd7 (pre/)

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
#c("WEIGHT_REF","WEIGHT_WB")] #même info mais WEIGHT_WB renseigné plus souvent
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