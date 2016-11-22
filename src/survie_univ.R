library(dplyr)
library(stringr)
library(survival)

#sla <- readRDS("data/BDDSLA.rds")
sla <- readRDS("data/BDDSLADEM.rds")

table(is.na(sla$ddn))
table(!is.na(sla$date_dc))

sla$time.vni <- as.numeric(sla$ddn - sla$DATEVNI)
sla$time.diag <- as.numeric(sla$ddn - sla$date_diag)
sla$censor <- ifelse (!is.na(sla$date_dc),1, 0)
sla$SEX <- factor(sla$SEX, levels=c(1,2), labels=c("h","f") ) #1 = 'Masculin' 2 = 'Féminin' 

#----------------------------------------------
#VERSION : DATE DE DEBUT = DATE DE DIAGNOSTIC 

#range date de début
range(sla$date_diag)

#survie
idc.surv <- survfit(Surv(sla$time,sla$censor)~1, conf.int=.95)
plot(idc.surv)
min(idc.surv$time[idc.surv$surv<=0.5])#idc.surv
#monthly estimates print:
summary(idc.surv, seq(30,max(idc.surv$time),by=30))

#suivi
idc.suiv <- survfit(Surv(sla$time.diag,1-sla$censor)~1)

#survie selon sexe :
table(sla$SEX)
sex.surv <- survfit(Surv(sla$time.diag,sla$censor)~sla$SEX, conf.int=.95)
plot(sex.surv,col=c(2,4))
legend (3000,1,c(levels(sla$SEX)),lty=c(1,1),col=c(2,4))
#log rank:
survdiff(Surv(sla$time.diag,sla$censor)~sla$SEX)


#-------------------------------------------
#VERSION DDEBUT = DATEVNI

#range date de début
range(sla$DATEVNI)

#survie
debvni <- survfit(Surv(sla$time.vni,sla$censor)~1, conf.int=.95)
plot(debvni)
min(debvni$time[debvni$surv<=0.5]) #debvni

#suivi
debvni.suiv <- survfit(Surv(sla$time.vni,1-sla$censor)~1)

#survie selon sexe :
sex.surv.vni <- survfit(Surv(sla$time.vni,sla$censor)~sla$SEX, conf.int=.95)
plot(sex.surv.vni,col=c(2,4))
legend (3000,1,c(levels(sla$SEX)),lty=c(1,1),col=c(2,4))
#log rank:
survdiff(Surv(sla$time.vni,sla$censor)~sla$SEX)




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