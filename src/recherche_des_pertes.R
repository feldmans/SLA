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
#ALLDIAG ET ALLDATEDIAG : perte de 38+10+2 = 50 patients sans diag et/ou sans date de diag (+12 qui ne sont pas dans visite)

#met2 sans DIAG
table(met2 %in% ALLDIAG$PATIENT) #(12 ne sont pas dedans car pas dans visite)
# FALSE  TRUE 
# 12   200 
table(ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1, "diagSLA" ],useNA = "a") #40 sans diagnostic (+12 qui ne sont pas dans la base)

#Nom des patients sans diagnostics avec met 1 (mais dont diag est 1 avec méthode 2)
ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDIAG$diagSLA), "PATIENT" ]



#met2 sans date de diag
table(met2 %in% ALLDATEDIAG$PATIENT)#(12 ne sont pas dedans car pas dans visite)
table(ALLDATEDIAG[ALLDATEDIAG$PATIENT %in% met2_no_met1, "date_diag" ],useNA="a") #48 sans date de diagnostic (+12 qui ne sont pas dans la base)

#Au final : 38 n'ont ni diag ni date, 10 n'ont seulement pas de date, 2 n'ont seulement pas de diag, 12 ne sont pas dans la base visite
table(ALLDATEDIAG[ALLDATEDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDATEDIAG$date_diag), "PATIENT" ] %in%
      ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1  & is.na(ALLDIAG$diagSLA), "PATIENT"])

table(ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1  & is.na(ALLDIAG$diagSLA), "PATIENT"] %in%  
      ALLDATEDIAG[ALLDATEDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDATEDIAG$date_diag), "PATIENT" ])


#---------------------------------
#ALLDC et ALLCS : pas de perte (sauf les 12 de visite)

#toujours ces mêmes 12 qui ne sont pas dans visite
table(met2 %in% ALLDC$PATIENT)
table(met2 %in% ALLCS$PATIENT)

#table(ALLDC[ALLDC$PATIENT %in% met2_no_met1, "date_dc" ], useNA = "a") #35 sans date de DC mais on s'en fiche
ALLCS[ALLCS$PATIENT %in% met2_no_met1, ]
table(is.na(ALLCS[ALLCS$PATIENT %in% met2_no_met1, "ddn"])) #les 130-12= 118 met2 non met1 ont une ddn

#-----------------------------------
#ALLVNI : 109 PATIENTS pertes SANS DATE DE VNI (dont 5 notés TTT_VNI=1, 44 notés TTT_VNI=0 et 60 sans statut pour la VNI) : seuls 79 n'étaient pas déjà NA diag et/ou datediag

table(met2 %in% ALLVNI$PATIENT) #tous les patients met2 sont dans la base ttt et donc dans la base ALLVNI
ALLVNI[ALLVNI$PATIENT %in% met2_no_met1, ]
table(is.na(ALLVNI[ALLVNI$PATIENT %in% met2_no_met1, "DATEVNI"])) #109 patients sans date de VNI
table(ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & is.na(ALLVNI$DATEVNI), "TTT_VNI"], useNA = "a") #109 patients sans date de VNI

table(ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & is.na(ALLVNI$DATEVNI) & ALLVNI$TTT_VNI==1, "PATIENT"]) #nom des 5 patients qui sont notés comme ayant VNI mais sans date
table(ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & !is.na(ALLVNI$DATEVNI) & ALLVNI$TTT_VNI==0, "PATIENT"]) #pas de patients avec une date et TTT_VNI=0
table(ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & !is.na(ALLVNI$DATEVNI) & is.na(ALLVNI$TTT_VNI), "PATIENT"]) #pas de patients avec une date et TTT_VNI=NA


table(!ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & is.na(ALLVNI$DATEVNI), "PATIENT"] %in%
        c(ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1  & is.na(ALLDIAG$diagSLA), "PATIENT"],ALLDATEDIAG[ALLDATEDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDATEDIAG$date_diag), "PATIENT" ]))

#nom des 79 patients qui ont une dateVNI=NA mais qui n'ont pas de diag NA ni de date diagNA (soit parce que pas dans la base visite, soit parceque date renseignée)
VNINA_DIAGnonNA <- ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & is.na(ALLVNI$DATEVNI), "PATIENT"][!ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & is.na(ALLVNI$DATEVNI), "PATIENT"] %in%
        c(ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1  & is.na(ALLDIAG$diagSLA), "PATIENT"],ALLDATEDIAG[ALLDATEDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDATEDIAG$date_diag), "PATIENT" ])]
#nom des 12 patients qui ne sont pas dans la base visite
met2_no_met1[!met2_no_met1 %in% visite$PATIENT]
#ces 12 patients sont-ils parmi les 79 VNI=NA mais diag non NA?
table(met2_no_met1[!met2_no_met1 %in% visite$PATIENT] %in% VNINA_DIAGnonNA) #oui 11 déjà comptés dans les VNINA_DIAGnonNA

#---------------------------------------
#AU TOTAL : 
# 20 PATIENTS DIAG=NA et/ou DATEDIAG=NA
# 30 PATIENTS DIAG=NA et/ou DATEDIAG=NA ET DATEVNI=NA
# 79 PATIENTS DATEVNI=NA (dont 11 qui ne sont pas présent dans la base visite donc non présent dans base ALLDIAG et ALLDATEDIAG)
# 1 PATIENT qui a une DATEVNI non NA mais qui n'est pas dans visite)
20+30+79+1 = 130

#J'ai retrouvé les 130 met2 qui ne sont pas dans met1

#=> Donc potentiellement je peux repêcher ces infos dans dans la base faite avec la méthode 1, sauf pour les 12 patients qui ne sont pas dans visite


#nom des patients qui sont dans visite dont les infos sont manquantes:

#Pas de diagnostic : (NB : peut être que ce sera rattrapés par l'ajout des NEW_DIAG=1)
ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDIAG$diagSLA), "PATIENT" ]
#Pas de date de diagnostic :
ALLDATEDIAG[ALLDATEDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDATEDIAG$date_diag), "PATIENT" ]
#Pas de diag ni de date de diagnostic :
ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDIAG$diagSLA), "PATIENT" ][ALLDIAG[ALLDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDIAG$diagSLA), "PATIENT" ] %in% ALLDATEDIAG[ALLDATEDIAG$PATIENT %in% met2_no_met1 & is.na(ALLDATEDIAG$date_diag), "PATIENT" ]]
#tous ont une ddn
#Patients sans date de VNI:
ALLVNI[ALLVNI$PATIENT %in% met2_no_met1 & is.na(ALLVNI$DATEVNI), "PATIENT"]

#12 Patients qu'il est inutile de garder car pas dans visite (sauf si on trouve autre moyen d'avoir ddn) :
met2_no_met1[!met2_no_met1 %in% visite$PATIENT]


#--------------------------------------------------------------
#NB : Pas de perte à cause des dernières nouvelles, donc inutile de chercher une autre base
dim(BDD[BDD$diagSLA==1 & !is.na(BDD$date_diag) & !is.na(BDD$DATEVNI), ])

