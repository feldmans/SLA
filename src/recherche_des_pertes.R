pat <-  read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/pat/PATIENT.csv") #donnees demo
ttt <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/trt/PATIENT.csv") #donne VNI
visite <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/visite/PATIENT2.csv")

met1 <- readRDS("data/selected_names_meth1.rds")
met2 <- readRDS("data/selected_names_meth2.rds")

met2_no_met1 <- met2[!met2 %in% met1]


names2visite
names2ttt

#----------------------------
#met2 doublés dans visite et ttt
table(met2 %in% names2visite)
table(met2 %in% names2ttt)
#=> pas de doublons

#-------------------------------------
#ALLDIAG ET ALLDATEDIAG : perte de 37 patients sans diag et sans date de diag (+12 qui ne sont pas dans visite)

#met2 sans DIAG
table(met2 %in% ALLDIAG$PATIENT) #(12 ne sont pas dedans car pas dans visite)
table(ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1, "diag" ],useNA = "a") #37 sans diagnostic (+12 qui ne sont pas dans la base), 70 SLA, 2 autres

#Nom des patients sans diagnostics avec met 1 (mais dont diag est 1 avec méthode 2)
#ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDIAG$diagSLA), "PATIENT" ]
ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDIAG$diag), "PATIENT" ]

#met2 sans date de diag
table(met2 %in% ALLDATEDIAG$PATIENT)#(12 ne sont pas dedans car pas dans visite)
table(ALLDATEDIAG[ALLDATEDIAG$PATIENT %in% met2_no_met1, "date_diag" ],useNA="a") #37 sans date de diagnostic (+12 qui ne sont pas dans la base)

#Au final : 37 n'ont ni diag ni date et 12 ne sont pas dans la base visite
table(ALLDATEDIAG[ALLDATEDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDATEDIAG$date_diag), "PATIENT" ] %in%
      ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1  & is.na(ALLDIAG$diag), "PATIENT"])
table(ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1  & is.na(ALLDIAG$diag), "PATIENT"] %in%  
      ALLDATEDIAG[ALLDATEDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDATEDIAG$date_diag), "PATIENT" ])


#---------------------------------
#ALLDC et ALLCS : 2 pertes (sauf les 12 de visite)

#toujours ces mêmes 12 qui ne sont pas dans visite
table(met2 %in% ALLDC$PATIENT)
table(met2 %in% ALLCS$PATIENT)

#table(ALLDC[ALLDC$PATIENT %in% met2_no_met1, "date_dc" ], useNA = "a") #31 sans date de DC mais on s'en fiche
ALLCS[ALLCS$PATIENT %in% met2_no_met1, ]
table(is.na(ALLCS[ALLCS$PATIENT %in% met2_no_met1, "ddn"])) #2 (+ 12 non présents dans visite) met2 non met1 n'ont pas de ddn

#nom des 2 pertes


#-----------------------------------
#ALLVNI : 109 PATIENTS pertes SANS DATE DE VNI (dont 5 notés TTT_VNI=1, 44 notés TTT_VNI=0 et 60 sans statut pour la VNI) : seuls 79 n'étaient pas déjà NA diag et/ou datediag

table(met2 %in% ALLVNI$PATIENT) #tous les patients met2 sont dans la base ttt et donc dans la base ALLVNI
ALLVNI[ALLVNI$PATIENT %in% met2_no_met1, ]
table(is.na(ALLVNI[ALLVNI$PATIENT %in% met2_no_met1, "DATEVNI"])) #109 patients sans date de VNI
table(ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & is.na(ALLVNI$DATEVNI), "TTT_VNI"], useNA = "a") #44 patients sans date de VNI sont TTT_VNI=0 

table(ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & is.na(ALLVNI$DATEVNI) & ALLVNI$TTT_VNI==1, "PATIENT"]) #nom des 5 patients qui sont notés comme ayant VNI mais sans date
table(ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & !is.na(ALLVNI$DATEVNI) & ALLVNI$TTT_VNI==0, "PATIENT"]) #pas de patients avec une date et TTT_VNI=0
table(ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & !is.na(ALLVNI$DATEVNI) & is.na(ALLVNI$TTT_VNI), "PATIENT"]) #pas de patients avec une date et TTT_VNI=NA

#81 patients sans dat de VNI mais qui ont diag
table(!ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & is.na(ALLVNI$DATEVNI), "PATIENT"] %in%
        c(ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1  & is.na(ALLDIAG$diag), "PATIENT"],ALLDATEDIAG[ALLDATEDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDATEDIAG$date_diag), "PATIENT" ]))

#nom des 81 patients qui ont une dateVNI=NA mais qui n'ont pas de diag NA ni de date diagNA (soit parce que pas dans la base visite, soit parceque date renseignée)
VNINA_DIAGnonNA <- ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & is.na(ALLVNI$DATEVNI), "PATIENT"][!ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & is.na(ALLVNI$DATEVNI), "PATIENT"] %in%
        c(ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1  & is.na(ALLDIAG$diag), "PATIENT"],ALLDATEDIAG[ALLDATEDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDATEDIAG$date_diag), "PATIENT" ])]
#nom des 12 patients qui ne sont pas dans la base visite
met2_no_met1[!met2_no_met1 %in% visite$PATIENT]
#ces 12 patients sont-ils parmi les 81 VNI=NA mais diag non NA?
table(met2_no_met1[!met2_no_met1 %in% visite$PATIENT] %in% VNINA_DIAGnonNA) #oui 11 déjà comptés dans les VNINA_DIAGnonNA

#---------------------------------------
#AU TOTAL : 
# 9 PATIENTS DIAG=NA et DATEDIAG=NA mais VNI renseignée
# 28 PATIENTS DIAG=NA et DATEDIAG=NA et DATEVNI=NA
# 81 PATIENTS DATEVNI=NA (dont 11 qui ne sont pas présent dans la base visite donc non présent dans base ALLDIAG et ALLDATEDIAG)
# 1 PATIENT qui a une DATEVNI non NA mais qui n'est pas dans visite)
#2 patients sans DDN
9+28+81+1+2 = 121

#J'ai retrouvé les 121 met2 qui ne sont pas dans met1

#=> Donc potentiellement je peux repêcher ces infos dans dans la base faite avec la méthode 1, 
#sauf pour les 12 patients qui ne sont pas dans visite et les 2 patients sans ddn


#nom des patients qui sont dans visite dont les infos sont manquantes:

#PATIENTS NON RATTRAPABLES
#12 Patients qu'il est inutile de garder car pas dans visite (sauf si on trouve autre moyen d'avoir ddn) :
namesdrop <- met2_no_met1[!met2_no_met1 %in% visite$PATIENT]
#pas de ddn : non rattrapable
namesdrop2 <- ALLCS[ALLCS$PATIENT %in% met2_no_met1 & is.na(ALLCS$ddn), "PATIENT"]

#PATIENTS RATTRAPABLES 35+96
#Pas de diagnostic ni de date diagnostic (ce sont les memes): 35
names_diag_datediag <- ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDIAG$diag) & (!ALLDIAG$PATIENT%in%namesdrop) & (!ALLDIAG$PATIENT%in%namesdrop2), "PATIENT" ]

#all(diag==datediag)
# all(ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDIAG$diag), "PATIENT" ] 
#     == ALLDATEDIAG[ALLDATEDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDATEDIAG$date_diag), "PATIENT" ])

#Patients sans date de VNI (rattrapables) : 96
names_VNI <- ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & is.na(ALLVNI$DATEVNI)& (!ALLVNI$PATIENT%in%namesdrop) & (!ALLVNI$PATIENT%in%namesdrop2), "PATIENT"]


saveRDS(names_diag_datediag, "data/names_diag_datediag.rds")
saveRDS(names_diag_datediag, "data/names_VNI.rds")
