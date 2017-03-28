######################
#     FLOW CHART     #
######################

#===========================
#chargement des bases


source("src/libraries_SLA.R")
source("src/fonctions_SLA.R")
bdds <- paste0("bdd",1:9)
for (i in bdds){
  tmp <- readRDS(paste0("data/bdds/", i, ".rds"))
  assign(i, tmp)
}

#bases avec doublons +- incohérences vni
#vnisla
vnisla_dbl_pb <- readRDS( "data/vnisla_sans_modif.rds") #contient les doublons de bl et les incohérence de vni de bdd_dates
vnisla_dbl <- readRDS("data/vnisla_sansvniinco.rds") #contient les doublons mais pas les incohérences de vni
#toutes les dates (vni, dc etc..)
bdd_dates_pb <- readRDS("data/bdd_dates_avecpbdates.rds")
bdd_dates_dbl <- readRDS("data/bdd_dates_avecdoublons.rds")
#toutes les baseline :
bl_dbl <- readRDS("data/bl_avecdoublons.rds")
#toutes les données répétées mergés en dr
dr_dbl <- readRDS("data/df_rep_avecdoublons.rds")


#bases sans doublon
vnisla <- readRDS("data/vnisla.rds")
dr <- readRDS("data/df_rep.rds")
bl <- readRDS("data/bl.rds")
vnisla <- readRDS("data/vnisla.rds")
bdd_dates <- readRDS("data/bdd_dates.rds")



#===========================
#flow chart


#--------------------

#Patients prospectifs SLA (est-ce que tous les patients de la base sont bien SLA, meme ceux sans vni??)
#base suivi neuro ou pneumo?? (4255 dans suivi neuro, 9755 dans la base pneumo(baseline et suivi))
length(unique(bdd9$PATIENT))


#--------------------

#Patients vni et sla : 796
length(unique(vnisla_dbl_pb$PATIENT)) #NB: pas de doublons dans vnisla, mais contient des patients qui sont doublons dans bl et avec pb de date vni


#--------------------

#PATIENT avec une date de fin de vni ou date de décès antérieure ou égale à la date de debut de vni :26 patients
pbvni <- bdd_dates_pb[bdd_dates_pb$datevni>bdd_dates_pb$fin_vni | bdd_dates_pb$time.vni<=0, ]
length(unique(pbvni$PATIENT))
#ou
length(unique(vnisla_dbl_pb$PATIENT)) - length(unique(vnisla_dbl$PATIENT))

#--------------------

#PATIENTS avec doublons
table(tab <- table(bl_dbl$PATIENT))
names(tab)[tab>1] #7 doublons
names_doublonsbl <- names(tab)[tab>1]
#rep
#(on ne peut pas savoir si doublons rep car base repetees, on e ne peut que voir les duplicatats)
#bdd_dates
table(tab <- table(bdd_dates_dbl$PATIENT))
names(tab)[tab>1] #1 doublons
names_doublonsdates <- names(tab)[tab>1]
#vnisla
table(tab <- table(vnisla_dbl$PATIENT))
names(tab)[tab>1] #0 doublons
names_doublonssla <- names(tab)[tab>1]
#total
names_doublons <- unique(c(names_doublonsbl, names_doublonsdates, names_doublonssla))
length(names_doublons)
#ou
length(unique(vnisla_dbl$PATIENT)) - length(unique(vnisla$PATIENT))

#--------------------

#PATIENTS ANALYSES
length(unique(vnisla$PATIENT)) 
#ou
length(unique(bl$PATIENT))


