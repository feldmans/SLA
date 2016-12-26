

.dir <- dir("C:/Users/4051268/Documents/sauvegarde data/sla/data/",full.names = T, recursive = T)

.dir_csv <- .dir[str_sub(.dir, -3, -1)=="csv"]
.dir_sas <- .dir[str_sub(.dir, -3, -1)=="sas"]

#Pour charger toutes les bases de données disponibles (se nommeront bdd 1 à 9)
for (i in .dir_csv) {
  print(i)
  num <- which(.dir_csv==i)
  a <- read.csv2(i)
  assign(paste0("bdd",num),a)
}
bdds <- paste0("bdd",1:9)

for (i in bdds){
  #browser()
  data <- get(i)
  data$PATIENT <- as.character(data$PATIENT)
  assign(i,data)
}
str(bdd1$PATIENT)



BASE_TOT <- readRDS("data/BASE_TOT.rds")
BASE_TOT$DOB <- NULL

BASE_SLA <- readRDS("data/BASE_SLA.rds")
BASE_SLA$DOB <- NULL

BASE_SLA_invar <- readRDS("data/BASE_SLA_invar.rds")



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
#     month_bef_vni <- NA
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
