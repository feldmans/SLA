# install.packages("devtools")
# devtools::install_github("kassambara/r2excel")
library(r2excel)



#selection doublons
visite <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/visite/PATIENT2.csv")
visite <- visite[visite$CENTRE_M1=="SLA01",]
visite$PATIENT <- as.character(visite$PATIENT)
names2visite <- names(table(visite$PATIENT)[table(visite$PATIENT)>1])

ttt <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/trt/PATIENT.csv") #donne VNI
ttt$PATIENT <- as.character(ttt$PATIENT)
names2ttt <- names(table(ttt$PATIENT)[table(ttt$PATIENT)>1])

#bdd filtrée
sla <- readRDS("data/BDDSLADEM.rds")
sla$time.vni <- as.numeric(sla$ddn - sla$DATEVNI)
sla$time.diag <- as.numeric(sla$ddn - sla$date_diag)
sla$censor <- ifelse (!is.na(sla$date_dc),1, 0)
sla$SEX <- factor(sla$SEX, levels=c(1,2), labels=c("h","f") ) #1 = 'Masculin' 2 = 'Féminin' 

# patients qui sont sélectionnés ave cune méthode mais pas avec une autre:
selected_names_meth1 <- readRDS("data/selected_names_meth1.rds")
selected_names_meth2 <- readRDS("data/selected_names_meth2.rds")
name_inc2 <- sort(selected_names_meth2[!selected_names_meth2 %in% selected_names_meth1])
name_inc1 <- sort(selected_names_meth1[!selected_names_meth1 %in% selected_names_meth2])
df_inc <- data.frame(meth1_nometh2=c(name_inc1,rep("",9)),meth2_nometh1=name_inc2)

#Patients avec diagnostic différent:
selno_identical_diag <- readRDS("data/no_identical_diag.rds")
for (i in 1:nrow(selno_identical_diag)){
  selno_identical_diag[i,"res"] <- ifelse (tail(as.numeric(selno_identical_diag[i,])[!is.na(as.numeric(selno_identical_diag[i,]))],1)==1,"KEEP","DROP")
}
selno_identical_diag <- selno_identical_diag[selno_identical_diag$res=="KEEP", ]
selno_identical_diag <- selno_identical_diag[ ,colnames(selno_identical_diag)[apply(apply(selno_identical_diag,2,function(x)!is.na(x)),2,sum)>0]]
vect_no_identical_diag <- apply(selno_identical_diag,1,function(.x)as.numeric(.x)[!is.na(as.numeric(.x))])

#Rapport

wb <- createWorkbook(type="xlsx")
sheet1 <- createSheet(wb, sheetName = "doublons base trt_PATIENT")
sheet2 <- createSheet(wb, sheetName = "doublons base visite_PATIENT2")
sheet3 <- createSheet(wb, sheetName = "base de donnée filtrée")
sheet4 <- createSheet(wb, sheetName = "différences de sélection")
sheet5 <- createSheet(wb, sheetName = "diagnostic différents")

xlsx.addHeader(wb,sheet1, value="Doublons trt : seuls les colonnes avec une différence sont affichées")
xlsx.addLineBreak(sheet1, 2)
for (i in names2ttt) print.report(i,ttt,file=wb, sheet=sheet1)

xlsx.addHeader(wb,sheet2, value="Doublons visite : seuls les colonnes avec une différence sont affichées")
xlsx.addLineBreak(sheet2, 2)
for (i in names2visite) print.report(i,visite,file=wb, sheet=sheet2)

xlsx.addTable(wb,sheet3,sla)

xlsx.addTable(wb,sheet4,df_inc)

for (i in 1:length(vect_no_identical_diag)) xlsx.addTable(wb,sheet5,data.frame(t(vect_no_identical_diag[i])))

saveWorkbook(wb, "rapport_SLA.xlsx")
xlsx.openFile("rapport_SLA.xlsx")


