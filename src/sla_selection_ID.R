library(stringr)

manage_date_ND <- function(vec){ #vec doit être un vecteur avec éléments de la forme 04/04/1989 ou "04/04/1989"
  vec <- as.character(vec)
  exist_year <-!is.na(str_sub(vec, 7, 10))
  vec[exist_year] <- gsub("ND/ND", "01/07",vec,fixed=T)
  vec[exist_year] <- gsub("ND", "15",vec,fixed=T)
  vec_d <- as.Date(vec,"%d/%m/%Y")
  return(vec_d)
}

who_is_date_ND <- function(vec_name,vec_date) {
  vec_date <- as.character(vec_date)
  exist_year <-!is.na(str_sub(vec_date, 7, 10))
  exist_month <- !is.na(str_sub(vec_date,4,5))
  name_ND <- vec_name[grep("ND",vec_date[exist_year])]
  date_ND <- vec_date[grep("ND",vec_date[exist_year])]
  df_ND <- data.frame(name = name_ND, date = date_ND,
                      N_ND = ifelse ( date_ND %in% vec_date[exist_month], "day", "day_month"))
  return(df_ND)
}





#source("C:/yann/travaux/sla/pgm/sarah.r")
# setwd("C:/yann/travaux/sla")
# l<-load("donnÃ©es/Diag.RData");l
# lv<-load("donnÃ©es/VNI.RData");lv

#rm(list=ls())

.dir <- "C:/Users/4051268/Documents/sauvegarde data/sla/data/20150827/"
Diag <-read.csv2(paste0(.dir,"Diag.csv")) ;head(Diag)
VNI<-read.csv2(paste0(.dir,"VNI.csv")) ;head(VNI)

Diag$PATIENT <- as.character(Diag$PATIENT)
VNI$PATIENT <- as.character(VNI$PATIENT)

#----------------------------------------------------------
who_is_date_ND (dcd$PATIENT,dcd$DATE_DCD)
# Je remplace ND jour et mois par 01/07 (pas de cas ici) et ND jour uniquement par 15 :
dcd$date_dcd <- manage_date_ND(dcd$DATE_DCD)
table(!is.na(dcd$date_dcd[dcd$FORM != "ID"]))


#-------------------------------------------------------------------

#CREATION TABLEAU PV(DIAG et conclusion confondu) PV1(DIAG) et PV2(conclusion) : Diagnostic et date de diagnostic


#Je sélectionnes les lignes premiÃ¨re visite (et non visis_supl)
pv<-Diag[Diag$MODULE=="PV",] 
dim(pv)

#Je selectionne les lignes correspondant au diag : FORM=DIAG et FORM=conclusion
pv<-pv[pv$FORM!="ID",]
dim(pv)

#Je sélectionne les patients qui ont exactement une identite :
#pv <- pv[pv$PATIENT %in% namesid , ]
dim(pv)
length(unique(pv$PATIENT))

#Je supprime les lignes sans Diagnostic 
pv <- pv[!is.na(pv$DIAGNOS),] 
dim(pv)
length(unique(pv$PATIENT))

#Je supprime les lignes sans date de diagnostic
table(table(pv$DATE_DIAG,useNA = "a"),useNA = "a")
pv$date<-as.Date(as.character(pv$DATE_DIAG), "%d/%m/%Y")
table(!is.na(pv$date))
pv$date <- manage_date_ND(pv$DATE_DIAG)
summary(as.numeric(pv$date))
pv<-pv[!is.na(pv$date),]
length(unique(pv$PATIENT))
#=> A ce stade seuls les patients avec au moins un diagnostic datÃ© sont conservÃ©s

#je selectionne le patient avec plus de 2 diagnostics (possible d'avoir un diag pour DIAG, un pour conclusion, si plus c'est un doublon)
table(tab<-table(pv$PATIENT))#Il est possible d'avoir 1 ou 2 lignes/diagnostic par patient (DIAG et/ou conclusion) mais pas 3. 0= ceux qui n'ont ni ligne DIAg ni ligne conclusion
#plus de patient avec 3 diag quand on retire les patients qui n'ont pas d'ID
x2<-names(tab[tab>2]) 
pv[pv$PATIENT %in% x2, c("PATIENT", "CENTRE", "MODULE", "FORM", "date", "DIAGNOS")]
pv<-pv[!(pv$PAT %in% x2),] 

#head(pv[, c("PATIENT", "CENTRE", "MODULE", "FORM", "DATE_DIAG", "DIAGNOS")]) : ne plus utiliser DATE_DIAG, transformÃ© en date!
head(pv[, c("PATIENT", "CENTRE", "MODULE", "FORM", "date", "DIAGNOS")])


#--------------------------------------------------------------------------

# CREATION DU TABLEAU ID : identifiant du patient : nom, centre, module


#Je ne garde que les patients avec une identité car seules les lignes ID ont le centre
table(Diag[Diag$FORM!="ID","CENTRE"],useNA = "a" )

id<-Diag[Diag$FORM=="ID",]
dim(id)
summary(id)
#Nombre de patients avec une identité:
length(unique(id$PATIENT))

#Je ne garde que les patients de la pitié :
id <- id[id$CENTRE=="SLA01",]
dim(id)
#nb de patients avec id unique et de la pitié:
length(unique(id$PATIENT))


#Suppression des patients avec plusieurs lignes ID
table(tab<-table(id$PATIENT)) # certains patients ont-ils plusieurs lignes ID
tab[tab>1] #Quels patients?
x1 <- names(tab[tab>1]) #Je fais un vecteur avec leurs noms
essai <- id[id$PAT %in% x1, c("PATIENT", "CENTRE", "DATE_DIAG", "DIAGNOS")]
table(table(essai$PATIENT)) #27 patients avec 2 identites

id<-id[!(id$PAT %in% x1),] #Je supprime ces patients qui ont plusieurs ID
#Je garde uniquement colonnes utiles pour id:
id<-id[ ,c("PATIENT","REF_NAME","REF_SURNAME","CENTRE")] 
dim(id)

#nom des patients avec une seule identite :
namesid <- as.vector(id$PATIENT)

#NB: les lignes ID n'ont jamais de diagnostic
table(Diag[Diag$FORM=="ID","DIAGNOS"],useNA = "a" )







#---------------------------------------

#CREATION IDPW : PATIENTS DE LA PITIE AVEC SLA

#MERGE pv1 et pv2 => pw
#Merge des lignes DIAG et des lignes conclusions de l'exam (les seuls differences potentielles sont date et diagnostic)
pv1<-pv[pv$FORM=="DIAG", c("PATIENT", "date", "DIAGNOS")]
pv2<-pv[pv$FORM!="DIAG", c("PATIENT", "date", "DIAGNOS")]
pw<-merge(pv1, pv2, by="PATIENT", all=T, suff=c(".diag", ".ccl")) #.d pour ligne diag, .e pour ligne conclusion
head(pw)
dim(pw)
length(unique(pw$PATIENT))

table(pw$DIAGNOS.diag, pw$DIAGNOS.ccl, exclude=NULL) #Je regarde si les diagnostics concordent

#Selection des lignes avec DIAGNOSTIC DE SLA 
pw<-pw[(pw$DIAGNOS.diag=="1" & !is.na(pw$DIAGNOS.diag)) & (pw$DIAGNOS.ccl=="1" | is.na(pw$DIAGNOS.ccl)),]
#!is.na(diagnos.d) utile? pw<-pw[(pw$DIAGNOS.d=="1") & (pw$DIAGNOS.e=="1" | is.na(pw$DIAGNOS.e)),]
dim(pw)
length(unique(pw$PATIENT))

#MERGE des lignes Diagnostic (dIAG et conclusion confondu) et des lignes ID
idpw<-merge(id, pw, by="PATIENT", all.x=F, all.y=T)
dim(idpw)

#verif des merges:
#J'ai bien le meme nombre de lignes
dim(pw)
dim(idpw)
#Je n'ai plus de patients doublés (quand je retire les noms non présents dans ID, sinon 4 noms présents 2 fois)
table(tabpw<-table(pw$PATIENT))#4 noms sont presents 2 fois
table(tabidpw<-table(idpw$PATIENT))

#nom des patients jusque là
namesidpw <- as.vector(idpw$PATIENT)

# #qui sont les patients doubles (dans le cas où on ne retire pas patients qui ne sont pas dans id):
# pw[tabpw>1,]
# names_dbl <- names(tabidpw[tabidpw>1])
# idpw[idpw$PATIENT %in% names_dbl,] 
# #Si on ne sélectionne pas les patients avec une seule identié, pw prend des patients sans identités donc sans centre:
# table(idpw$CENTRE,useNA = "a") #ne marche que si on ne sélecitonne pas les patients dans namesid

#data$DIAG.f<-factor(data$DIAG , c(0:9, "10"), c()) #pgm sas

#---------------------------------

# CREATION idpwv : patient SLA de la pitié avec vni


vni<-VNI
dim(vni)
head(vni)
dim(vni)

x<-as.list(vni)
x<-sapply(x, as.character)
dim(x)
head(x)
y<-ifelse(is.na(x) | x=="",0, 1)
sy<-rowSums(y)
table(sy)

x<-as.character(vni$DATE)
unique(x)

x<-as.character(vni$PAT)
length(unique(x))

y<-vni[!is.na(vni$DATE_VNI) & vni$DATE_VNI!="",]
dim(y)
x<-as.character(y$PAT)
length(unique(x))

pat<-y$PAT;pat
d<-Diag[Diag$PATIENT %in% x,]
dim(d)
head(d)
d<-unique(d)
dim(d)
dd<-d[d$FORM=="DIAG",]
dim(dd)
table(dd$DIAGNOS)

#Je ne garde que les patients avec une date de vni
vni$datevni <- vni$DATE_VNI
#------code pour CSV uniquement
vni$datevni <- as.Date(as.character(vni$datevni), "%d/%m/%Y")
#------
summary(as.numeric(vni$datevni))
summary(vni$datevni)

vni<-vni[!is.na(vni$datevni),]
dim(vni) #Je tombe à 232 lignes


#je ne garde que les patients satisfaisant centre=SLA01 et diagnostic=SLA cad presents dans idpw
vni <- vni[vni$PATIENT %in% namesidpw, ]
dim(vni)
length(unique(vni$PATIENT)) #2034 patients d'avant sont bien dans le tableau vni


#Je supprime patients avec plusieurs lignes vni
table(tab<-table(vni$PAT))
x<-names(tab[tab>1])
vni<-vni[!(vni$PAT %in% x),]
dim(vni)
table(tab<-table(vni$PAT))

idpwv<-merge(idpw, vni, by="PATIENT", all.x=F, all.y=T)
dim(idpwv)
selected_names <- unique(idpwv$PATIENT)

saveRDS(selected_names, "data/selected_names.rds")



#------------------------------
#RECHERCHE AUTRES FICHIERS VNI
path <- "C:/Users/4051268/Documents/sauvegarde data/sla/data/"
.dir <- dir("C:/Users/4051268/Documents/sauvegarde data/sla/data/",full.names = T, recursive = T)
.dir <- .dir[str_sub(.dir, -3, -1)=="sas"]
v <- "vni"

for (i in .dir){
  fic<-i
  x<-scan(i, what=as.character(), sep="\n")
  i<-grep(v, tolower(x));x[i]
}
list_mining <- lapply(.dir, function(i){
  fic<-i
  x<-scan(i, what=as.character(), sep="\n")
  .i<-grep(v, tolower(x));
  return(c(i,x[.i]))
})

#dossier avec fichier vni trouve potentiellement interessant:
#.dir <- "C:/Users/4051268/Documents/sauvegarde data/sla/data/pat/"
#.dir <- "C:/Users/4051268/Documents/sauvegarde data/sla/data/trt/"
.dir <- "C:/Users/4051268/Documents/sauvegarde data/sla/data/visite/" #Svérifier si ce sont les même patients que les 202

#chargement du fichier CSV de ce dossier
vni2 <- read.csv2 (paste0(.dir,"PATIENT.csv")) 
names(vni2)[grep("VNI", names(vni2))]
table(table(vni2$DATEVNI))


#---------------------------------

# CREATION idpwv avec vni2

#Je ne garde que les patients avec une date de vni
vni2$datevni <- vni2$DATEVNI
#------code pour CSV uniquement
vni2$datevni <- as.Date(as.character(vni2$datevni), "%d/%m/%Y")
#------
summary(as.numeric(vni2$datevni))
summary(vni2$datevni)

vni2<-vni2[!is.na(vni2$datevni),]
dim(vni2) #Je tombe à 232 lignes


#je ne garde que les patients satisfaisant centre=SLA01 et diagnostic=SLA cad presents dans idpw
vni2 <- vni2[vni2$PATIENT %in% namesidpw, ]
dim(vni2)
length(unique(vni2$PATIENT)) #2034 patients d'avant sont bien dans le tableau vni2


#Je supprime patients avec plusieurs lignes vni2
table(tab<-table(vni2$PAT))
x<-names(tab[tab>1])
vni2<-vni2[!(vni2$PAT %in% x),]
dim(vni2)
table(tab<-table(vni2$PAT))

idpwv2<-merge(idpw, vni2, by="PATIENT", all.x=F, all.y=T)
dim(idpwv2)






#A faire :
#regarder si des sans diagnostic en PV deviennent des sla en visite supplementaire
sup <-Diag[Diag$MODULE!="PV",]
table(sup$DIAGNOS)
