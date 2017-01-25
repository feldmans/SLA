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
ddn_tot <- na.omit(ddn_tot)

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

bdd_1symp <- unique(bdd_1symp) #seul les vrais doublons nom et date sont éliminés
bdd_1symp <- na.omit(bdd_1symp)

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
bdd_DATEDIAG <- unique(bdd_DATEDIAG[,c("PATIENT","date_diag")])#seul les vrais doublons nom et date sont éliminés
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
bdd_diag <- unique(ALLDIAG[, c("PATIENT","diag")] )#seul les vrais doublons nom et diag sont éliminés

#pb : certains patients ont une ligne de diag NA => je supprime tous les NA
table(tab<-table(bdd_diag$PATIENT))
pat2diag <- names(tab)[tab>1]
bdd_diag[bdd_diag$PATIENT %in% pat2diag,]

bdd_diag <- na.omit(bdd_diag)
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
table(BASE_TOT$diag==1,useNA = "a")
#patients date vni(parmi sla)
table(table(BASE_TOT[BASE_TOT$diag==1 & !is.na(BASE_TOT$diag) & !is.na(BASE_TOT$DATEVNI), "PATIENT"]))
sum(table(tab <- table(BASE_TOT[BASE_TOT$diag==1 & !is.na(BASE_TOT$diag) & !is.na(BASE_TOT$DATEVNI), "PATIENT"])))
#13 doublons#12? 
length(namesdoublons)
#434 patients#435?
table(table(namesSLA))
#3 patients ont datevni > date décès
BASE_SLA[BASE_SLA$DATEVNI>BASE_SLA$ddn, "PATIENT"]

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
#listes_brut <- lapply(bdds, which_col,string1="DATDRILU_V_M", type="explo")

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
listes_brut <- lapply(bdds, which_col,string1="DATDRILU_V_M", type="merge")
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
listes_brut <- lapply(bdds, which_col,string1="DATFRILU_V_M", type="merge")
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
tmp <- merge(bdd9,months_before_vni,by="PATIENT",all.y=T)
ALLpick <- tmp[ ,c("myrow","DATEVNI",colnames(tmp)[grep("ALS_V",colnames(tmp))]) ]
pick_data <- lapply(1: nrow(ALLpick),function(.x){
  #browser()
  myrow <- ALLpick[.x, "myrow"]
  if (is.na(myrow)){
    data <- NA
  } else {
    data <- ALLpick[.x, paste0("ALS_V_M",myrow)]
    # if (is.na(data)){
    #   data <- ALLpick[.x, paste0("ALS_V_M",(myrow-1))]#ok pour prendre l'échelle du mois précédent?
    # }
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


pick_data <- sapply(1: nrow(ALLpick),function(.x){
  .vni <- ALLpick[.x, "DATEVNI"]
  .l <- ALLpick[.x, grep("DATE_PREVENT_PP",colnames(ALLpick))]#je selectionne toutes les colonnes DATE_PREVENT_PP et je considere que sousvent est pendant la ventilation
  .g <- as.vector(t(.l))
  
  if(any(!is.na(.g))){
    row_last_date_before_vni <- tail(which((.g<=.vni)),1) #dernier mois dispo avant VNI (je fais l'hypothese que l'info si elle existe est bien renseignée)
    
    if (length(row_last_date_before_vni)==0) { #les dates sont post à vni, alors jessaye PP
      month_bef_vni <- "after vni"
      .l <- ALLpick[.x, "DATE_RESP_PP"]
      if(!is.na(.l) & (.vni-.l)>0 & (.vni-.l)<90) { #ok si mesure faite 3 mois avant vni (je fais l'hypothese que cette colonne est moins bien renseignée que celle de suivi)
        
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
        DUREE_ENREG_H_PP <- ifelse(!is.na(DUREE_ENREG_MIN_PP) & is.na(DUREE_ENREG_H_PP),0,DUREE_ENREG_H_PP)
        DUREE_ENREG_MIN_PP <- ifelse(!is.na(DUREE_ENREG_H_PP) & is.na(DUREE_ENREG_MIN_PP),0,DUREE_ENREG_MIN_PP)
        minutes <- sum((DUREE_ENREG_H_PP*60),DUREE_ENREG_MIN_PP,na.rm=T)
        heures <- minutes/60
        time_under_spo2_90_h <- (perc_time_under_spo2_90/100)*heures
        
        bicar <- as.numeric(as.character(ALLpick[.x, "HCO3_PP"] ))
        
      } else { #date_prevent_PP NA ou post a vni et date_resp_pp post a vni
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
        bicar <- NA
      }
      
    } else { #il y a une date prevent antérieure à vni (et je fais l'hypothese qu'elle est juste avant vni)
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
      
      DUREE_ENREG_H_PP <- as.integer(as.character(ALLpick[.x, paste0("DUREE_ENREG_H_PV_F",month_bef_vni)]))
      DUREE_ENREG_MIN_PP <- as.integer(as.character(ALLpick[.x, paste0("DUREE_ENREG_MIN_PV_F",month_bef_vni)]))
      DUREE_ENREG_H_PP <- ifelse(!is.na(DUREE_ENREG_MIN_PP) & is.na(DUREE_ENREG_H_PP),0,DUREE_ENREG_H_PP)
      DUREE_ENREG_MIN_PP <- ifelse(!is.na(DUREE_ENREG_H_PP) & is.na(DUREE_ENREG_MIN_PP),0,DUREE_ENREG_MIN_PP)
      minutes <- sum((DUREE_ENREG_H_PP*60),DUREE_ENREG_MIN_PP,na.rm=T)
      heures <- minutes/60
      time_under_spo2_90_h <- (perc_time_under_spo2_90/100)*heures
      
      bicar <- as.numeric(as.character(ALLpick[.x, paste0("HCO3_PV_F",month_bef_vni)] ))
      
    }
    
    
  } else { # toutes les dates prevent sont NA cad .g=NA
    .l <- ALLpick[.x, "DATE_RESP_PP"]
    
    if(!is.na(.l) & (.vni-.l)>0 & (.vni-.l)<90) { #ok si mesure faite 3 mois avant vni (je fais l'hypothese que cette colonne est moins bien renseignée que celle de suivi)
      #browser()
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
      DUREE_ENREG_H_PP <- ifelse(!is.na(DUREE_ENREG_MIN_PP) & is.na(DUREE_ENREG_H_PP),0,DUREE_ENREG_H_PP)
      DUREE_ENREG_MIN_PP <- ifelse(!is.na(DUREE_ENREG_H_PP) & is.na(DUREE_ENREG_MIN_PP),0,DUREE_ENREG_MIN_PP)
      minutes <- sum((DUREE_ENREG_H_PP*60),DUREE_ENREG_MIN_PP,na.rm=T)
      heures <- minutes/60
      time_under_spo2_90_h <- (perc_time_under_spo2_90/100)*heures
      
      bicar <- as.numeric(as.character(ALLpick[.x, "HCO3_PP"] ))
      
    } else { #date_prevent_PP NA ou post a vni et date_resp_pp NA ou post a vni
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
      bicar <- NA
    }
  }
  
  return (c(month_bef_vni, dysp, orthop, CVF_ASSIS_perc_pred, CVF_COUCHE_perc_pred, SNIP_cmH2O, SNIP_perc_pred, PIMAX_cmH2O, PIMAX_perc_pred,
            perc_time_under_spo2_90, time_under_spo2_90_h, bicar))
})
ALLpick <- data.frame(t(pick_data))
colnames(ALLpick) <- c("month_bef_vni", "dysp", "orthop", "CVF_ASSIS_perc_pred", "CVF_COUCHE_perc_pred", "SNIP_cmH2O", "SNIP_perc_pred", "PIMAX_cmH2O", "PIMAX_perc_pred",
                       "perc_time_under_spo2_90", "time_under_spo2_90_h", "bicar")

ALLpick$PATIENT <- as.character(tmp$PATIENT)
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
#merge
for (i in name_df_baseline){
  num <- which(name_df_baseline==i)
  .bd <- get(i)
  baseline_tot <- if(num==1) .bd else merge(baseline_tot, .bd, by="PATIENT", suffixes= c(num-1,num),all=TRUE)
}

BASE_SLA_invar <- merge(BASE_SLA, baseline_tot, by="PATIENT", all.x=T, all.y=F)
saveRDS(BASE_SLA_invar, "data/BASE_SLA_invar.rds")


dfbldep <- str_sub(dir("data/baseline_to_merge/dependant_datevni"),1,-5)[sapply(dir("data/baseline_to_merge/dependant_datevni"),function(x)is.data.frame(get(str_sub(x,1,-5))))]
#merge
for (i in dfbldep){
  num <- which(dfbldep==i)
  .bd <- get(i)
  bl_dep <- if(num==1) .bd else merge(bl_dep, .bd, by="PATIENT", all=TRUE)
}

BASE_SLA_allbl <- merge(BASE_SLA_invar, bl_dep, by="PATIENT", all.x=T, all.y=F)

bck_BASE_SLA

#Data management basique
BASE_SLA_allbl$DEBRILU <- as_date(BASE_SLA_allbl$DEBRILU)
BASE_SLA_allbl$FINRILU <- as_date(BASE_SLA_allbl$FINRILU)

for (i in c("dysp","orthop","CVF_ASSIS_perc_pred","CVF_COUCHE_perc_pred","SNIP_cmH2O","SNIP_perc_pred","PIMAX_cmH2O",
            "PIMAX_perc_pred","perc_time_under_spo2_90","time_under_spo2_90_h","bicar")){
  BASE_SLA_allbl[,i] <- as.numeric(as.character(BASE_SLA_allbl[,i]))
}
BASE_SLA_allbl$LIEUDEB_recode <- as.character(BASE_SLA_allbl$LIEUDEB_recode)
BASE_SLA_allbl$month_bef_vni <- as.character(BASE_SLA_allbl$month_bef_vni)

BASE_SLA_allbl$PATIENT1 <- BASE_SLA_allbl$PATIENT

BASE_SLA_allbl$PATIENT <- anonymate_sla(BASE_SLA_allbl$PATIENT)
BASE_SLA_allbl <- BASE_SLA_allbl[,c(1,28,2:27)]


#Mise en commentaire pour ne pas modifier la clé par inadvertance
saveRDS(BASE_SLA_allbl[,-2], "data/BASE_SLA_allbl.rds")
saveRDS(BASE_SLA_allbl, "data/BASE_SLA_allbl_withnames.rds")



#-------------------------------------
#VARIABLES REPETEES : base de donnes bdd7 (pre/)

lapply(bdds, which_col,string1="EPWORTH_VENT_SV", type="explo")
var_rep <- c("SATISF_VENTIL_SV_","UTIL_VENTIL_DIURN","UTIL_VENTIL_NOCT","CAUSE_V_SATISF_SV_CHOICE_1_","CAUSE_V_SATISF_SV_CHOICE_2_","CAUSE_V_SATISF_SV_CHOICE_3_","CAUSE_V_SATISF_SV_CHOICE_4_",
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
  bdd_rep <- if(num==1) listes_net else merge(bdd_rep, listes_net, by=c("PATIENT","time"), all=TRUE)
}
bdd_rep <- bdd_rep[order(bdd_rep$PATIENT,bdd_rep$time),]

saveRDS(bdd_rep, "data/essairep.rds")
