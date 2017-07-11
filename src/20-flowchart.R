######################
#     FLOW CHART     #
######################

#===========================
#chargement des bases


source("src/libraries_SLA.R")
source("src/02-fonctions_SLA.R")
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

#date de dernieres nouvelles JG : ce sont des patients avec une date de vni et un follow up pneumo mais pas de follow up neuro (mais comme on réucpère leur date de décès on considere qu'ils ont un suivi neuro) 
DC_2.df <- read.csv2("data/date_evt_patientHorsSuiviNeuro.csv")
DC_2.df$PATIENT <- as.character(DC_2.df$PATIENT)
DC_2.df <- DC_2.df[DC_2.df$date_evt!="", ]

#bases sans doublon
vnisla <- readRDS("data/vnisla.rds")
dr <- readRDS("data/df_rep.rds")
bl <- readRDS("data/bl.rds")
vnisla <- readRDS("data/vnisla.rds")
bdd_dates <- readRDS("data/bdd_dates.rds")

dim(vnisla)
dim(bdd_dates)

#Liste des patients considérés comme ayant un suivi neuro (soit dans bdd9, soit repeché par JG, soit ayant date de décès dans bdd_dates(cad date de décès qqpart dans la base))
pat.suiv <- c(bdd9$PATIENT, DC_2.df$PATIENT, bdd_dates$PATIENT[!is.na(bdd_dates$date_dc)])

#NB2 : certains patients sont dans bdd_dates alors que pas de suivi neuro ni dans base de JG : c'est parec qu'on les a gardé s'ils avaient une date de décès
table(bdd_dates_pb$PATIENT %in% c(bdd9$PATIENT, DC_2.df$PATIENT))
#===========================
#flow chart


#--------------------

#Patients prospectifs SLA (est-ce que tous les patients de la base sont bien SLA, meme ceux sans vni??)
length(unique(pat.suiv))

#les patients rajoutés par JG ne sont pas dans bdd9
table(DC_2.df$PATIENT %in% bdd9$PATIENT) 

#--------------------

#Patients vni et sla : attention :vnisla contient des patients sans suivi neuro 
length (unique(vnisla_dbl_pb$PATIENT[vnisla_dbl_pb$PATIENT %in% pat.suiv])) #NB: pas de doublons dans vnisla, mais contient des patients qui sont doublons dans bl et avec pb de date vni
#=> 779 patients avec vni et suivi neuro (ou repechage par JG ou date de deces)


#NB les patients rajoutés par JG sont tous dans vnisla_dbl_pb (ainsi que les patients de bdd_dates)
table(DC_2.df$PATIENT %in% vnisla_dbl_pb$PATIENT) 
table(bdd_dates$PATIENT %in% vnisla_dbl_pb$PATIENT) 

#--------------------

#PATIENT avec une date de fin de vni ou date de décès antérieure ou égale à la date de debut de vni :26 patients
pbvni <- bdd_dates_pb[(bdd_dates_pb$datevni > bdd_dates_pb$fin_vni &!is.na(bdd_dates_pb$date_dc) & !is.na(bdd_dates_pb$fin_vni)) | bdd_dates_pb$time.vni<=0, ]
length(unique(pbvni$PATIENT[pbvni$PATIENT %in% pat.suiv]))


#NB les patients rajoutés par JG sont tous dans bdd_dates_pb
table(DC_2.df$PATIENT %in% bdd_dates_pb$PATIENT)
#mais 3 patients pas dans vnisla_dbl
table(DC_2.df$PATIENT %in% vnisla_dbl$PATIENT)
#parce que 3 patients de DC_2.df ont pb de date 
table(DC_2.df$PATIENT %in% pbvni$PATIENT)


#--------------------

#PATIENTS avec doublons
table(tab <- table(bl_dbl$PATIENT))
names(tab)[tab>1] #6 doublons
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
table(names_doublons %in% pat.suiv)


#--------------------

#PATIENTS ANALYSES
length(unique(vnisla$PATIENT[vnisla$PATIENT %in% pat.suiv])) 
#ou
length(unique(bl$PATIENT))
length(unique(bl$PATIENT[bl$PATIENT %in% pat.suiv]))#idem


