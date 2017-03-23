

.dir <- dir("C:/Users/4051268/Documents/SLA/sauvegarde data/sla/data/",full.names = T, recursive = T)

.dir_csv <- .dir[str_sub(.dir, -3, -1)=="csv"]
.dir_sas <- .dir[str_sub(.dir, -3, -1)=="sas"]

#=====================
#anonymat
#A creer une fois puis ne plus toucher
# names_pat <- unique(c(bdd1$PATIENT, bdd2$PATIENT, bdd3$PATIENT, bdd4$PATIENT, bdd5$PATIENT, bdd6$PATIENT, bdd7$PATIENT, bdd8$PATIENT, bdd9$PATIENT))
# anonymes <- data.frame(PATIENT = names_pat, numpat = paste0("ID", 1:length(names_pat)))
# write.csv2(anonymes, file="C:/Users/4051268/Documents/SLA/sauvegarde data/sla/data/tableau_anonymat.csv")

anonymes <- read.csv2("C:/Users/4051268/Documents/SLA/sauvegarde data/sla/anonymisation sarah/tableau_anonymat.csv")
anonymes$PATIENT <- as.character(anonymes$PATIENT)
anonymes$numpat <- as.character(anonymes$numpat)


#==================================
#Pour charger toutes les bases de données disponibles (se nommeront bdd 1 à 9)
for (i in .dir_csv) {
  print(i)
  num <- which(.dir_csv==i)
  a <- read.csv2(i)
  assign(paste0("bdd",num),a)
}
bdds <- paste0("bdd",1:9)

#Pour transformer les noms de patients en charactere
for (i in bdds){
  #browser()
  data <- get(i)
  data$PATIENT <- as.character(data$PATIENT)
  assign(i,data)
}

#Pour anonymiser bdd
for (i in bdds){
  #browser()
  data <- get(i)
  data <- merge(data, anonymes, by="PATIENT", all=F)
  data$PATIENT <- data$numpat
  data$numpat <- NULL
  data$PATIENT <- as.character(data$PATIENT)
  assign(i,data)
}

#============================
#BASE  DE DONNEES DDN

# #1/Boucle en commentaire car longue à faire tourner et résultat de la boucle déjà enregistrée dans data/ddn
# #Pour chercher la ddn de chaque base de donnée:
# for (i in .dir_csv) {
#   num <- which(.dir==i)
#   print(i)
#   a <- get_ddn(i,"DAT")
#   saveRDS(a,paste0("data/ddn/ddn",num,".rds"))
#   assign(paste0("ddn",num),a)
# }

#2/Pour charger les ddn obtenues ()
for (i in .dir_csv[-2]) {
  num <- which(.dir_csv==i)
  print(num)
  a <- readRDS(paste0("data/ddn/ddn",num,".rds")) #NB : supprimer ddn2.Rds du dossier data/ddn car le fichier est vide
  a <- a[[1]]
  a <- aggregate(a,by=list(a$PATIENT),max,na.rm=T) #qd des noms sont rassemblé et que toutes leurs dates valent NA, alors le tableau vaut NA
  assign(paste0("ddn",num),a)
}
ddns <- paste0("ddn", c(1, 3:9))

# #3/anonymiser ddn_tot (à faire une fois puis ddn est déjà sauvegardée anonymisée, on n'a plus à faire cette étape)
# for (i in ddns){
#   data <- get(i)
#   data <- merge(data, anonymes, by="PATIENT", all=F)
#   data$PATIENT <- data$numpat
#   data$PATIENT <- as.character(data$PATIENT)
#   data <- data[ , c("PATIENT", "max")]
#   saveRDS(data,paste0("data/ddn/",i,".rds"))
#   assign(i,data)
# }

#NB: En absence de base de données crées (avec juste les 9 bases de départ et aucun objet), on lance 1 puis 2  puis 3 (l'étape 3 écrase les ddn non anonymisé et les remplace par des ddn anonymisé)
#puis on ne lance plus que l'étape 2
#===================================





#A commenter qd datamanagement refait depuis le début
# BASE_TOT <- readRDS("data/BASE_TOT.rds")
# BASE_TOT$DOB <- NULL
# 
# BASE_SLA <- readRDS("data/BASE_SLA.rds")
# BASE_SLA$DOB <- NULL
# 
# BASE_SLA_invar <- readRDS("data/BASE_SLA_invar.rds")
# 
# BASE_SLA_allbl <- readRDS ("data/BASE_SLA_allbl.rds")





# BDD9 : pick Month just before vni

# tmp <- merge(BASE_SLA[ , c("PATIENT","DATEVNI")], bdd9[, c("PATIENT",colnames(bdd9)[grep("DATEXAM_V_M",colnames(bdd9))])], by="PATIENT", all.x=T, all.y=F)
# ALLpick <- tmp[ ,grep("DAT",colnames(tmp))]
# for (i in colnames(ALLpick)) ALLpick[,i] <- manage_date_ND(ALLpick[,i])
# pick_data <- lapply(1: nrow(ALLpick),function(.x){
#   .vni <- ALLpick[.x, "DATEVNI"]
#   .l <- ALLpick[.x, !colnames(ALLpick)%in% "DATEVNI"]#je selectionne toutes les colonnes DATEXAM_V_M
#   .g <- as.vector(t(.l))
#   if(any(!is.na(.g))){
#     row_last_date_before_vni <- tail(which((.g<=.vni)),1) #dernier mois dispo avant VNI (je fais l'hypothese que datexam est bien renseignée et que la dernière date dispo sera proche de vni)
#     if (length(row_last_date_before_vni)==0) month_bef_vni <- NA
#     else {
#       month_bef_vni <- colnames(.l[row_last_date_before_vni])
#       month_bef_vni <- ifelse(str_sub(month_bef_vni,-3,-3)=="M", as.numeric(str_sub(month_bef_vni,-2,-1)),as.numeric(str_sub(month_bef_vni,-1,-1))) #ne marche que jusqu'à M99
#     }
#   } else {
#     month_bef_vni <- NA #j'ai NA si la premiere visite est postérieure à la VNI
#   }
# 
#   return (month_bef_vni)
# })
# ALLpick$data <- as.vector(do.call(rbind,pick_data))
# ALLpick$PATIENT <- as.character(tmp$PATIENT)
# ALLpick$DATEVNI <- tmp$DATEVNI
# ALLpick <- unique(ALLpick[ , c("PATIENT","data","DATEVNI")])
# colnames(ALLpick)[2] <- "myrow"
# names_dataset <- "months_before_vni"
# assign(names_dataset,ALLpick)
# saveRDS(ALLpick, paste0("data/",names_dataset,".rds")) #ce n'est que les patients de BASE_SLA (434 patients)
months_before_vni <- readRDS("data/months_before_vni.rds")








#NON UTILISE! VOIR POUR SUPPRIMER
# #BDD7: pick month just before vni
# tmp <- merge(BASE_SLA[ , c("PATIENT","DATEVNI")], bdd7[, c("PATIENT",colnames(bdd7)[grep("DATE_PREVENT_PP_F",colnames(bdd7))])], by="PATIENT", all.x=T, all.y=F)
# ALLpick <- tmp[ ,grep("DAT",colnames(tmp))]
# for (i in colnames(ALLpick)) ALLpick[,i] <- manage_date_ND(ALLpick[,i])
# pick_data <- lapply(1: nrow(ALLpick),function(.x){
#   #•browser()
#   .vni <- ALLpick[.x, "DATEVNI"]
#   .l <- ALLpick[.x, !colnames(ALLpick)%in% "DATEVNI"]#je selectionne toutes les colonnes DATEXAM_V_M
#   .g <- as.vector(t(.l))
#   if(any(!is.na(.g))){
#     row_last_date_before_vni <- tail(which((.g<.vni)),1) #dernier mois dispo avant VNI
#     if (length(row_last_date_before_vni)==0) month_bef_vni <- "after vni"
#     else {
#       month_bef_vni <- colnames(.l[row_last_date_before_vni])
#       month_bef_vni <- ifelse(str_sub(month_bef_vni,-3,-3)=="M", as.numeric(str_sub(month_bef_vni,-2,-1)),as.numeric(str_sub(month_bef_vni,-1,-1))) #ne marche que jusqu'à M99
#     }
#   } else {
#     month_bef_vni <- NA
#   }
# 
#   return (month_bef_vni)
# })
# ALLpick$data <- as.vector(do.call(rbind,pick_data))
# ALLpick$PATIENT <- as.character(tmp$PATIENT)
# ALLpick$DATEVNI <- tmp$DATEVNI
# #ALLpick <- unique(ALLpick[ , c("PATIENT","data","DATEVNI")])
# 
# 
# # #explo : quelles colonnes ont des infos avant vni
# # tmp <- merge(BASE_SLA[ , c("PATIENT","DATEVNI")], bdd7[, c("PATIENT",colnames(bdd7)[grep("DAT",colnames(bdd7))])], by="PATIENT", all.x=T, all.y=F)
# # ALLpick <- tmp[ ,grep("DAT",colnames(tmp))]
# # for (i in colnames(ALLpick)) ALLpick[,i] <- manage_date_ND(ALLpick[,i])
# # pick_data <- lapply(1: nrow(ALLpick),function(.x){
# #   #browser()
# #   .vni <- ALLpick[.x, "DATEVNI"]
# #   .l <- ALLpick[.x, !colnames(ALLpick)%in% "DATEVNI"]#je selectionne toutes les colonnes DATEXAM_V_M
# #   .g <- as.vector(t(.l))
# #   if(any(!is.na(.g))){
# #     row_last_date_before_vni <- tail(which((.g<.vni)),1) #dernier mois dispo avant VNI
# #     if (length(row_last_date_before_vni)==0) month_bef_vni <- "after vni"
# #     else {
# #       month_bef_vni <- colnames(.l[row_last_date_before_vni])
# #       #month_bef_vni <- ifelse(str_sub(month_bef_vni,-3,-3)=="M", as.numeric(str_sub(month_bef_vni,-2,-1)),as.numeric(str_sub(month_bef_vni,-1,-1))) #ne marche que jusqu'à M99
# #     }
# #   } else {
# #     month_bef_vni <- NA
# #   }
# #   
# #   return (month_bef_vni)
# # })
# # ALLpick$data <- as.vector(do.call(rbind,pick_data))
# # table(ALLpick$data)
# # 
# # 
# # ALLpick$PATIENT <- as.character(tmp$PATIENT)
# # ALLpick$DATEVNI <- tmp$DATEVNI
# # ALLpick <- unique(ALLpick[ , c("PATIENT","data","DATEVNI")])
# 
# 
# 
# ALLpick <- unique(ALLpick[ , c("PATIENT","data","DATEVNI")])
# colnames(ALLpick)[2] <- "myrow"
# names_dataset <- "months_before_vni"
# assign(names_dataset,ALLpick)
# saveRDS(ALLpick, paste0("data/",names_dataset,".rds")) #ce n'est que les patients de BASE_SLA (434 patients)
