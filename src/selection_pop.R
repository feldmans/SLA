#source("C:/yann/travaux/sla/pgm/sarah.r")
# setwd("C:/yann/travaux/sla")
# l<-load("données/Diag.RData");l
# lv<-load("données/VNI.RData");lv

.dir <- "C:/Users/4051268/Documents/sauvegarde data/sla/data/20150827/"
Diag <-read.csv2(paste0(.dir,"Diag.csv")) ;head(Diag)
lv<-read.csv2(paste0(.dir,"VNI.csv")) ;head(lv)


#Table Diag:
Diag[Diag$PATIENT=="GUILLET_MICHELINE",] #3 lignes par patient : id (avec le centre), diag et conclusion avec le diagnos(différence diag et conclusion?)
table(Diag$FORM,Diag$CENTRE)#centre jamais donné à la ligne DIAG
#la date de diagnostic est toujours donnée pour les lignes DIAG et conclusion, au moins l'année
table(is.na(Diag[Diag$FORM=="DIAG","DATE_DIAG"])) 
table(is.na(Diag[Diag$FORM=="Conclusion de l'examen","DATE_DIAG"]))

table(table(Diag[Diag$FORM=="DIAG","PATIENT"])) #2343 patients n'ont pas de ligne diag, 7203 en ont une seule, 13 en ont 2



# CREATION DU TABLEAU ID : identifiant du patient : nom, centre, module
#Suppression des patients avec plusieurs lignes ID

#NB : y a-t-il des patients avec une conclusion et un diag sans ID? Si oui supprimé car on ne connaitra pas son centre
id<-Diag[Diag$FORM=="ID",]
dim(id)
summary(id)

table(tab<-table(id$PATIENT)) # certains patients ont-ils plusieurs lignes ID
tab[tab>1] #Quels patients?
x1<-names(tab[tab>1]) #Je fais un vecteur avec leurs noms
essai <- id[id$PAT %in% x1, c("PATIENT", "CENTRE", "DATE_DIAG", "DIAGNOS")]
table(table(essai$PATIENT)) #27 patients avec 2 identités


id<-id[!(id$PAT %in% x1),] #Je supprime ces patients qui ont plusieurs ID
#Je garde uniquement colonnes utiles pour id:
id<-id[ ,c("PATIENT","REF_NAME","REF_SURNAME","CENTRE")] 

#nom des patients avec une seule identite :
namesid <- as.vector(id$PATIENT)

#NB: les lignes ID n'ont jamais de diagnostic
table(Diag[Diag$FORM=="ID","DIAGNOS"],useNA = "a" )


#CREATION TABLEAU PV(DIAG et conclusion confondu) PV1(DIAG) et PV2(conclusion) : Diagnostic et date de diagnostic

#Je s?lectionnes les lignes première visite (et non visis_supl)
pv<-Diag[Diag$MODULE=="PV",] 
dim(pv)

#Je selectionne les lignes correspondant au diag : FORM=DIAG et FORM=conclusion
pv<-pv[pv$FORM!="ID",]
dim(pv)

#Je s?lectionne les patients qui ont exactement une identite :
# pv <- pv[pv$PATIENT %in% namesid , ]
# dim(pv)

#Je supprime les lignes sans Diagnostic 
pv<-pv[!is.na(pv$DIAGNOS),] 
dim(pv)
#Je supprime les lignes sans date de diagnostic
pv$date<-as.Date(as.character(pv$DATE_DIAG), "%d/%m/%Y")
summary(as.numeric(pv$date))
pv<-pv[!is.na(pv$date),]

#=> à ce stade seuls les patients avec au moins un diagnostic daté sont conservés

#je sélectionne le patient avec plus de 2 diagnostics (possible d'avoir un diag pour DIAG, un pour conclusion, si plus c'est un doublon)
table(tab<-table(pv$PAT))#Il est possible d'avoir 1 ou 2 lignes/diagnostic par patient (DIAG et/ou conclusion) mais pas 3. 0= ceux qui n'ont ni ligne DIAg ni ligne conclusion
x2<-names(tab[tab>2]) 
pv[pv$PAT %in% x2, c("PATIENT", "CENTRE", "MODULE", "FORM", "date", "DIAGNOS")]
pv<-pv[!(pv$PAT %in% x2),] 

#head(pv[, c("PATIENT", "CENTRE", "MODULE", "FORM", "DATE_DIAG", "DIAGNOS")]) : ne plus utiliser DATE_DIAG, transformé en date!
head(pv[, c("PATIENT", "CENTRE", "MODULE", "FORM", "date", "DIAGNOS")])


#Merge des lignes DIAG et des lignes conclusions de l'exam (les seuls différences potentielles sont date et diagnostic)
pv1<-pv[pv$FORM=="DIAG", c("PATIENT", "date", "DIAGNOS")]
pv2<-pv[pv$FORM!="DIAG", c("PATIENT", "date", "DIAGNOS")]
pw<-merge(pv1, pv2, by="PATIENT", all=T, suff=c(".diag", ".ccl")) #.d pour ligne diag, .e pour ligne conclusion
head(pw)

table(pw$DIAGNOS.diag, pw$DIAGNOS.ccl, exclude=NULL) #Je regarde si les diagnostics concordent

#Sélection des lignes avec DIAGNOSTIC DE SLA 
pw<-pw[(pw$DIAGNOS.diag=="1" & !is.na(pw$DIAGNOS.diag)) & (pw$DIAGNOS.ccl=="1" | is.na(pw$DIAGNOS.ccl)),]
#!is.na(diagnos.d) utile? pw<-pw[(pw$DIAGNOS.d=="1") & (pw$DIAGNOS.e=="1" | is.na(pw$DIAGNOS.e)),]
dim(pw)


#CREATION DE IDPW : PATIENTS AVEC DIAGNOSTIC SLA 
#Merge des lignes Diagnostic (dIAG et conclusion confondu) et des lignes ID
idpw<-merge(id, pw, by="PATIENT", all.x=F, all.y=T)
dim(idpw)

#vérif des merges:
#J'ai bien le même nombre de lignes
dim(pw)
dim(idpw)
#J'ai bien le même nombre de patients doublés
table(tabpw<-table(pw$PATIENT))#4 noms sont présents 2 fois
table(tabidpw<-table(idpw$PATIENT))

#qui sont les patients doublés:
pw[tabpw>1,]
names_dbl <- names(tabidpw[tabidpw>1])
idpw[idpw$PATIENT %in% names_dbl,] 

#Si on ne s?lectionne pas les patients avec une seule identi?, pw prend des patients sans identit?s donc sans centre:
table(idpw$CENTRE,useNA = "a") #ne marche que si on ne s?lecitonne pas les patients dans namesid

#SELECTION DES PATIENTS VNI
vni<-VNI
vni$datevni<-vni$DATE_VNI
summary(as.numeric(vni$datevni))
vni<-vni[!is.na(vni$datevni),]
dim(vni)
table(tab<-table(vni$PAT))

x<-names(tab[tab>1])
vni<-vni[!(vni$PATIENT %in% x),]
dim(vni)
table(tab<-table(vni$PAT))


idpwv<-merge(idpw, vni, by="PATIENT", all.x=F, all.y=T)
dim(idpwv)


#A faire :
#regarder si des sans diagnostic en PV deviennent des sla en visite supplémentaire
sup <-Diag[Diag$MODULE!="PV",]
table(sup$DIAGNOS)
