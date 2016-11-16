library(dplyr)
library(stringr)
library(survival)

sla <- readRDS("data/BDDSLA.rds")
sla$time <- as.numeric(sla$ddn - sla$date_diag)
sla$censor <- ifelse (!is.na(sla$date_dc),1, 0)

table(is.na(sla$ddn))

attach (sla)
idc.surv <- survfit(Surv(sla$time,sla$censor)~1)
plot(idc.surv)


idc.suiv <- survfit(Surv(sla$time,1-sla$censor)~1)










#------------------------------------------------------------------------
selected_names<- readRDS("data/selected_names.rds")
idpwvnidc <- readRDS("data/idpwvnidc.rds")
head(idpwvnidc)
dim(idpwvnidc)

idc <- idpwvnidc
table(is.na(idc$date_dcd))
idc$censor <- ifelse(!is.na(idc$date_dcd),1,0) #0 si pas de date de DC, 1 si date de DC

#time
idc$time <- ifelse(idc$censor==1, idc$date_dcd-idc$date.diag, idc$ddv-idc$date.diag)


pat <-  read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/pat/PATIENT.csv")
ttt <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/trt/PATIENT.csv")
visite <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/visite/PATIENT2.csv") #Svérifier si ce sont les même patients que les 202
# VNI <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/20150827/VNI.csv")
# NM <-  read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/20150827/neuro_mobilite.csv")
# DG <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/20150827/Diag.csv")
# DM <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/20150827/demographie.csv")
# DCD <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/20150827/DCD.csv")

dim(pat[!is.na(pat$DATEXAM) & pat$PATIENT %in% idc$PATIENT ,c("DATEXAM","PATIENT")]) #aussi DOB
#ttt : DATEVNI
#visite : VNI
visite <- visite[visite$PATIENT %in% selected_names, ]

#dates VNI MEO de visite : que NA
VNI_MEO <- visite[,colnames(visite)[grep("VNI_MEO",colnames(visite))]]
for (i in colnames(VNI_MEO)){
  #browser()
  VNI_MEO[,i] <- manage_date_ND(VNI_MEO[,i])
}
apply(apply(VNI_MEO,2,function(.x)!is.na(.x)),2,sum)#toutes les dates VNI_MEO sont NA

#JE fais un tableau à partir de visite qui contient toutes les dates
ALLDATE <- visite[,colnames(visite)[grep("DATE",colnames(visite))]]

#Je transforme les facteurs en date
for (i in colnames(ALLDATE)){
  #browser()
  ALLDATE[,i] <- manage_date_ND(ALLDATE[,i])
}
#ALLDATE[,colnames(ALLDATE)] <- apply(ALLDATE,2,manage_date_ND)
ALLDATENA <- apply(ALLDATE,2,function(.x)!is.na(.x)) #si true = non NA
COLDATEnonNA <- colnames(ALLDATENA)[apply(ALLDATENA,2,sum)>0]

#Afficher les colonnes avec dates nonNA
head(ALLDATE[ ,COLDATEnonNA])

#Afficher l'histoire d'un patient
t(ALLDATE[1:5 ,COLDATEnonNA])


dim(ALLDATE)
attach(idc)
idc.surv <- survfit(Surv(date_dcd))