#source("C:/yann/travaux/sla/pgm/sarah.r")
setwd("C:/yann/travaux/sla")

l<-load("données/Diag.RData");l
lv<-load("données/VNI.RData");lv

rm(list=ls())
#Version CSV
.dir <- "C:/Users/4051268/Documents/sauvegarde data/sla/data/20150827/"
Diag <-read.csv2(paste0(.dir,"Diag.csv")) ;head(Diag)
VNI <-read.csv2(paste0(.dir,"VNI.csv")) ;head(VNI)

# Diagb <- Diag
# VNIb <- VNI

#version RData
.dir <- "C:/Users/4051268/Documents/sauvegarde data/sla/data/"
l<-load(paste0(.dir,"Diag.RData"))
lv<-load(paste0(.dir,"VNI.RData"))


# #Difference CSV et RDATA : la colomne patient est codée en charactère pour RData, facteur pour CSV => je passe en charactère
# identical(Diag[1 ,colnames(Diagb)],Diagb[1,])
# str(Diag)
# str(Diagb)
# Diagb$PATIENT <- as.vector(Diagb$PATIENT)
# identical(Diag[1 ,colnames(Diagb)],Diagb[1,])
# identical(Diag[ ,colnames(Diagb)],Diagb)




id<-Diag[Diag$FORM=="ID",]
dim(id)

summary(id)

table(tab<-table(id$PAT))
tab[tab>1]
x<-names(tab[tab>1])
id[id$PAT %in% x, c("PATIENT", "CENTRE", "DATE_DIAG", "DIAGNOS")]

id<-id[!(id$PAT %in% x),]






pv<-Diag[Diag$MODULE=="PV",]
dim(pv)
pv<-pv[pv$FORM!="ID",]
dim(pv)

pv<-pv[!is.na(pv$DIAGNOS),]
dim(pv)

pv$date<-as.Date(as.character(pv$DATE_DIAG), "%d/%m/%Y")
summary(as.numeric(pv$date))

pv<-pv[!is.na(pv$date),]

table(tab<-table(pv$PAT))
x<-names(tab[tab>2])
pv[pv$PAT %in% x, c("PATIENT", "CENTRE", "MODULE", "FORM", "date", "DIAGNOS")]

pv<-pv[!(pv$PAT %in% x),]

head(pv[, c("PATIENT", "CENTRE", "MODULE", "FORM", "DATE_DIAG", "DIAGNOS")])


pv1<-pv[pv$FORM=="DIAG", c("PATIENT", "MODULE", "FORM", "date", "DIAGNOS")]
pv2<-pv[pv$FORM!="DIAG", c("PATIENT", "date", "DIAGNOS")]
pw<-merge(pv1, pv2, by="PATIENT", all=T, suff=c(".d", ".e"))
head(pw)

table(pw$DIAGNOS.d, pw$DIAGNOS.e, exclude=NULL)

pw<-pw[(pw$DIAGNOS.d=="1" & !is.na(pw$DIAGNOS.d)) & (pw$DIAGNOS.e=="1" | is.na(pw$DIAGNOS.e)),]
dim(pw)


idpw<-merge(id, pw, by="PATIENT", all.x=F, all.y=T)
dim(idpw)

table(table(idpw$PATIEN))







vni<-VNI
vni$datevni<-vni$DATE_VNI #dans Rdata : DATE_VNI est une date, dans CSV : c'est une facteur

#pour CSV uniquement
vni$PATIENT <- as.character(vni$PATIENT)
vni$datevni <- as.Date(as.character(vni$datevni), "%d/%m/%Y")

summary(as.numeric(vni$datevni))
vni<-vni[!is.na(vni$datevni),]
dim(vni) #RData : [1] 531  12  #CSV : [1] 10117    11 CSV après modif patient et datevni : [1] 531  11
table(tab<-table(vni$PATIENT))
#Rdata:
# 1     2 
# 495   18 

#CSV
# 1     2     3 
# 9024  512   23
#CSV apres modif :
# 1     2 
# 495  18 

x<-names(tab[tab>1])
vni<-vni[!(vni$PAT %in% x),]
dim(vni) #Rdata : [1] 495  12
table(tab<-table(vni$PAT))

#----------------
namesidpw <- as.vector(unique(id$PATIENT))
namesidpw <- as.vector(idpw$PATIENT)

vni<-vni[vni$PATIENT %in% namesidpw,]
dim(vni)
#----------------
idpwv<-merge(idpw, vni, by="PATIENT", all.x=F, all.y=T)
# all.x=T : extra rows will be added to the output, one for each row in x that has no matching row in y
# all.y=T : extra rows will be added to the output, one for each row in y that has no matching row in x
# Il y a des rows de y vni (par exemple vni pour d'autres diag qui ne sont pas dans x)
dim(idpwv)


