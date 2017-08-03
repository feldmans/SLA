#=====================
#=====================
# modele joint SLA  ||
#=====================
#=====================


source("src/libraries_SLA.R")
source("src/02-fonctions_SLA.R")
library(lattice)
library(nlme)
#install.packages("WWGbook")
library(WWGbook)
#====================================================
#====================================================


#=======================
#chargement bases
#=======================

#-----------------------
#variables baselines
bl <- readRDS("data/bl.rds")
bdd_dates <- readRDS("data/bdd_dates.rds")

#-----------------------
#variables dont la valeur dépendant du temps
d <- readRDS("data/df_rep.rds")
d <- d[d$PATIENT %in% bl$PATIENT, ]

#-----------------------
#baseline recuperees

#BMI
bl_bmi <- readRDS("data/bl_bmi.rds")
table(bl_bmi$PATIENT %in% bl$PATIENT)
#NB: on avait nettoyé dr : si consult apres date de deces ce n'est pas un deces et time.vni==date de derniere consult - datevni
#il faudra la nettoyer ici#=> ok fait dans 00-datamanaegement

#=========================
#merge bl et bl_bmi

#Je supprime les BMI superieurs a 45
d <- d %>% filter(!(qui == "BMI" & x >= 45))

bl_bmi %>% filter(PATIENT =="ID140") %>% select(time.vni) # 597 au lieu de 598 : mais deja comme ca dans bdd_dates => c'est parce qu dr a ete change qd consult ulterieur a date deces, mais pas bdd_dates => je change aussi bdd_dates #ok MAJ dans 00-datamanegement

#je fait une nouvelle baseline en fusionnant les baseline de dr et bl_bmi et je prend la date la plus proche de datevni
bl_bmi2 <- bind_rows(d, bl_bmi) %>% select(one_of(colnames(d))) %>% filter(qui == "BMI" & f==0) %>% 
  group_by(PATIENT) %>% arrange(desc(date)) %>% 
  slice(1)

#verif:
bind_rows(d, bl_bmi) %>% select(one_of(colnames(d))) %>% filter(qui == "BMI" & f==0) %>% 
  group_by(PATIENT) %>% arrange(desc(date)) %>% filter(PATIENT == "ID101") 
bind_rows(d, bl_bmi) %>% select(one_of(colnames(d))) %>% filter(qui == "BMI" & f==0) %>% 
  group_by(PATIENT) %>% arrange(desc(date)) %>% filter(PATIENT == "ID101") %>% 
  slice(1)
hist(bl_bmi2$x)

#je merge les nouvelles baseline de BMI avec d
d <- bind_rows(d %>% filter(!(qui == "BMI" & f==0)), bl_bmi2) %>% filter(x < 80) %>% mutate(del = ifelse(del<0, 0, del))



#donnees manquantes
npos0 <- d %>% filter(qui=="BMI") %>% group_by(PATIENT) %>% 
  summarise(n0 = sum(del<=0), npos = sum(del>0), n = n0+npos) %>% arrange(PATIENT) 

tab <- table(bl=npos0$n0, nbval=pmin(npos0$n,2))
res <- sum(tab[1,]) #nb de patients sans baseline (mais avec un suivi)
res2 <- sum(tab) #bl et/ou suivi
res3 <- round(res/res2 *100,2) #% de patients sans baseline (denominateur = patients avec au moins une valeur)
data.frame(pat_noNA = res2, F1_noF0 = res, perc = paste0(res3, "%"))

#essqi dplyr (pas fini)
d %>% filter(qui=="BMI") %>% group_by(PATIENT) %>% 
  summarise(n0 = sum(del<=0), npos = pmin(1, sum(del>0)), n = n0+npos) %>% arrange(PATIENT) 


#=======================
#selection des variables
#=======================

my_col <- read.csv("data/names_var_sel.csv",stringsAsFactors = FALSE)
bl <- bl %>% select(PATIENT, one_of(my_col$variables)) 
d <- d %>% filter(qui %in% my_col$variables) 

varbl.vec <- bl %>% select(one_of(my_col$variables)) %>% colnames()
vardr.vec <- d %>% count(qui) %>% .$qui
#======================
#1 seul tableau longitudinal
#======================
head(bl)
da_init <- left_join(d %>% filter(qui=="BMI"), bl) %>% select(PATIENT, time.vni, del, evt, qui, x, one_of(varbl.vec)) %>% 
  mutate(del = del/30.4375, time.vni = time.vni/30.4375)
da <- da_init %>% na.omit 
# da$del<-da$del/30.4375
# da$time.vni <- da$time.vni/30.4375
#======================
#tableau pour cox
#======================
#plus besoin finalement, on part de da_init
da2_init <- d %>% mutate(del = del/30.4375, time.vni = time.vni/30.4375)
da2_init <- da2_init %>% group_by(PATIENT, qui, date) %>% mutate(NB = n(),nr = row_number()) %>% filter(nr==1) #je supprime les doublons
da2 <- d %>% filter(PATIENT %in% da$PATIENT) %>% mutate(del = del/30.4375, time.vni = time.vni/30.4375)

#-------------------
# ti
#-------------------

#ces ti ne marchent pas pour les mois, ok pour les jours uniquement!
# ti<-0:max(d$time.vni)
# ti<-0:max(da$time.vni)
# ti<-0:max(da2$time.vni)
# ti <- sort(unique(da2$time.vni))
# ti <- sort(unique(c(da2$time.vni, da2$del)))
ti <- sort(unique(c(da2_init$time.vni, da2_init$del)))
length(ti)

#====================================================
#====================================================

#=======================
#Modele de survie
#=======================


#-----------------
#Cox multivarié 
#-----------------

#avec toutes les variables selectionnees (longitudinales et baselines) et tous les patients
#--------------------------------------
#version Sarah

# #version dplyr
# y3 <- da2 %>% filter(qui==var) %>% group_by(PATIENT) %>% arrange(PATIENT, del) %>% 
#   mutate(delapres = lead(del), 
#          delapres = ifelse(is.na(delapres), time.vni, delapres), 
#          rn = row_number(), n = n(), evt2 = ifelse(rn == n, evt, 0)) %>% 
#   mutate(start = del, stop = delapres, etat = evt2) %>% 
#   survSplit(., end="stop", start="start", cut=ti, event="etat") %>% 
#   arrange(PATIENT, start) %>% 
#   mutate(etat = ifelse(evt==1 & etat==0 & stop==time.vni, 1, etat)) %>%
#   select(PATIENT, start, stop, etat, x)
# colnames(y3)[colnames(y3)=="x"] <- var
# 
# 
# y3[y3$delapres!=y$delapres,]  #tout est ok
# y3[y3$evt2!=y$evt2,]  #tout est ok
# sapply(colnames(Yt), function(x)all(y3[,x] == Yt[,x]))

# y %>% filter(PATIENT =="ID140") %>% select(-(E_PHAR_LAR: APPAREILLE_PP))
# #le probleme vient des lignes ou time.vni bl n'est pas identique : pb au moment de la creation de time.vni a del = 0
# #ok pb regle : on navait change date incoherente dans dr mais pas dans bdd_date


get_split <- function(data, var){
  #browser()
  y3.2 <- data %>% filter(qui==var) %>% group_by(PATIENT) %>% arrange(PATIENT, del) %>% 
    mutate(delapres = lead(del), 
           delapres = ifelse(is.na(delapres), time.vni, delapres), 
           rn = row_number(), n = n(), evt2 = ifelse(rn == n, evt, 0)) %>% 
    mutate(start = del, stop = delapres, etat = evt2) %>% 
    survSplit(., end="stop", start="start", cut=ti, event="etat") %>% 
    arrange(PATIENT, start) %>% 
    mutate(etat = ifelse(evt==1 & etat==0 & stop==time.vni, 1, etat)) %>%
    #mutate(etat = ifelse(evt==1 & etat==0 & stop==time.vni, 1, etat)) %>%
    select(PATIENT, start, stop, etat, evt, del, time.vni, x)
  colnames(y3.2)[colnames(y3.2)=="x"] <- var
  return(y3.2)
}

#-----------------------------------
#initié le tableau par tous les del sans covariables

# #2 cs pour une meme date : je garde la 1e ligne arbitrairement
# da2_init <- da2_init %>% group_by(PATIENT, qui, date) %>% mutate(NB = n(),nr = row_number()) %>% 
#   filter(nr==1) 
# 
# y <- da2_init %>% group_by(PATIENT) %>% arrange(PATIENT, del) %>%
#   mutate(delapres = lead(del),
#          delapres = ifelse(is.na(delapres), time.vni, delapres),
#          rn = row_number(), n = n(), evt2 = ifelse(rn == n, evt, 0)) %>%
#   mutate(start = del, stop = delapres, etat = evt2) %>%
#   survSplit(., end="stop", start="start", cut=ti, event="etat") %>%
#   arrange(PATIENT, start) %>% select(PATIENT, start, stop, del, etat, evt, time.vni) %>% unique %>% filter(!is.na(start))
# 
# for (.var in vardr.vec[1]){
#   #for (.var in vardr.vec[2]){
#   y2 <- get_split(da2_init, .var)
#   print(head(y2))
#   y <- full_join(y, y2 %>% select(PATIENT, start, stop, etat, evt, time.vni, one_of(.var)), by = c("PATIENT", "start", "stop", "etat", "evt", "time.vni"))
#   print(head(y))
# }
# 
# y %>% filter(PATIENT =="ID3369") %>% arrange(start) %>% tail
# y %>% filter(PATIENT =="ID181") %>% View
# 
# 
# all.cox2 <- left_join(y, bl) %>%
#   select(PATIENT, start, stop, etat, del, time.vni, everything())
# 
# full_join(y, get_split(da2_init, "CEPHAL_PP")) %>% filter(PATIENT == "ID3369") %>% arrange(start) %>% View()
# 
# #avant de supprimer les lignes start NA, 
# #je vérifie evt = 1 a bien l'avant denriere ligne 1  
# all.cox2  %>% group_by(PATIENT) %>% 
#   mutate(startna = ifelse(is.na(start), 1, 0), NAstart = max(startna)) %>%
#   filter(NAstart == 1 & (row_number()==(n()-1) | row_number()==n())) %>% 
#   select(PATIENT,start, stop,del, etat, evt) %>% arrange(PATIENT) %>% 
#   filter(evt == 1)
# #je vérifie que les start NA sont bien les dernières lignes
# all.cox2  %>% group_by(PATIENT) %>% 
#   mutate(rn = row_number(), maxrn = max(rn)) %>%
#   #filter(is.na(start) & rn != max(rn)) %>% 
#   filter(is.na(start) & rn == max(rn)) %>% 
#   select(PATIENT,start, stop,del, etat, evt, rn , maxrn) 
# #ok je peux supprimer les start NA
# all.cox2 <- all.cox2 %>% filter(!is.na(start))
# 
# #pb de del NA
# all.cox2 %>% filter(is.na(del))
# # j'abandaonne cette façon de faire 

#----------
#J'initie avec ALS

y <- get_split(da2_init, vardr.vec[1])
#y <- get_split(da2, vardr.vec[1])
for (.var in vardr.vec[-1]){
  #for (.var in vardr.vec[2]){
  y2 <- get_split(da2_init, .var)
  #y2 <- get_split(da2, .var)
  print(head(y2))
  y <- full_join(y, y2 %>% select(PATIENT, start, stop, del, etat, evt, time.vni, one_of(.var)), by = c("PATIENT", "start", "stop", "etat", "evt", "time.vni"))
  print(head(y))
  rm(y2)
}
#merge dr splitte et bl
all.cox <- left_join(y, bl) %>% arrange(PATIENT, start)
#Je rassemble les del
all.cox <- all.cox %>% mutate(all_del = all.cox %>% select(starts_with("del", ignore.case=FALSE)) %>% mutate(all_del = apply(.,1, min, na.rm=T)) %>% .$all_del) %>% 
  select(-starts_with("del", ignore.case=FALSE)) %>% mutate(del = all_del, all_del = NULL) %>% 
  select(PATIENT, start, stop, etat, del, time.vni, everything())



#Suppression des lignes de start NA, 
#je vérifie evt = 1 a bien l'avant denriere ligne 1  
all.cox  %>% group_by(PATIENT) %>% 
  mutate(startna = ifelse(is.na(start), 1, 0), NAstart = max(startna)) %>%
  #filter(NAstart == 1 & (row_number()==(n()-1) | row_number()==n() | row_number()==(n()-2))) %>% 
  filter(NAstart == 1 & (row_number()==(n()-1) | row_number()==n())) %>% 
  select(PATIENT,start, stop,del, etat, evt) %>% arrange(PATIENT) %>% 
  filter(evt == 1) 
#je vérifie que les start NA sont bien les dernières lignes
all.cox  %>% group_by(PATIENT) %>% arrange(PATIENT, start) %>% 
  mutate(rn = row_number(), maxrn = max(rn)) %>%
  #filter(PATIENT =="ID5697") %>% arrange(start) %>% select(start, stop,  rn , maxrn) %>% .$rn
  filter(is.na(start) & rn != max(rn)) %>% 
  #filter(is.na(start) & rn == max(rn)) %>% 
  select(PATIENT,start, stop,del, etat, evt, rn , maxrn) 
#non : 5697 5797 5899 # maj ok : c'est parce que je n'avais pas trié par start

#ok je peux supprimer les start NA
all.cox <- all.cox %>% filter(!is.na(start))

#Je supprime les patients qui n'ont pas de consultation baseline (c'est a dire delai minimal supereiru a 0) # 747 - 706 = on perd 41 patients
all.cox <- all.cox %>% group_by(PATIENT) %>% mutate(mindel = min(del)) %>% filter(mindel==0)




y %>% filter(PATIENT == "ID101" ) %>% head #pb lignes 3 et 4 : vient du start stop qui est different a partir de MORPHO_PPCHOICE 1 #MAJ : ti en mois en time.vni en mois : plus de pb 
y %>% filter(PATIENT == "ID101" ) #%>% View #pb lignes 3 et 4 : vient du start stop qui est different a partir de MORPHO_PPCHOICE 1 #MAJ : ti en mois en time.vni en mois : plus de pb 
y %>% filter(PATIENT == "ID1052" ) %>% head #pb lignes 3 et 4 : vient du start stop qui est different a partir de MORPHO_PPCHOICE 1 #MAJ : ti en mois en time.vni en mois : plus de pb 
#verif que etat 1 au meme endroit
get_split(da2, "MORPHO_PP_CHOICE_1") %>% filter(PATIENT == "ID101" ) %>% head
get_split(da2, "BMI") %>% filter(PATIENT == "ID101" ) %>% head
da2 %>% filter(PATIENT == "ID101" & qui == "MORPHO_PP_CHOICE_1") 
da2 %>% filter(PATIENT == "ID101" & qui == "ALS") 

# saveRDS(all.cox, "data/all.cox_NOtimeTransf20170726.rds") # version 468 patients
saveRDS(all.cox, "data/all.cox_NOtimeTransf20170727.rds")

#------------------------------------
#Version yann

vardr.vec
var <- "BMI"
#y<-da[da$qui==var,] %>% filter(PATIENT %in% c("ID101", "ID949")) %>% select(PATIENT:TOUX_EFFICACE)

y<-da2[da2$qui==var,]

#y<-d[d$qui==var,] %>% mutate(del = ifelse(del<0, 0, del)) #faut il mettre les delai negatifs à 0? =>oui, deja fait
y<-y[order(y$PATIENT, y$del),] 
z<-tapply(y$del, y$PATIENT, c)
zf<-tapply(y$time.vni, y$PATIENT, c)
zm<-mapply(function(x,y) c(x[-1], y[1]), z, zf)
names(z)[1:5] # il manque le 107 a cause du na.omit => c'est normal
zm[[2]]


z<-tapply(y$evt, y$PATIENT, c)
fct<-function (x) {
  x[-length(x)]<-0
  return(x)
}
ze<-sapply(z, fct)

y$delapres<-unlist(zm)
y$evt2<-unlist(ze)
yt<-y
yt$start<-yt$del
yt$stop<-yt$delapres
yt$etat<-yt$evt2

yt<-survSplit(data = yt, end="stop", start="start", cut=ti, event="etat")
yt<-yt[order(yt$PATIENT, yt$start),]

#des sujets meurt le jour de la visite (?)
yt[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni,]
yt$etat[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni]<-1

yt[ ,var] <- yt$x
Yt<-yt[, c("PATIENT", "start", "stop", "etat", var)]

#ajouter une variable répétée
var <- vr1[2]
var <- "ALS"
y<-d[d$qui==var,]

y<-y[order(y$PATIENT, y$del),]
z<-tapply(y$del, y$PATIENT, c)
zf<-tapply(y$time.vni, y$PATIENT, c)
zm<-mapply(function(x,y) c(x[-1], y[1]), z, zf)
names(z)[1:3]
zm[[1]]
z<-tapply(y$evt, y$PATIENT, c)
fct<-function (x) {
  x[-length(x)]<-0
  return(x)
}
ze<-sapply(z, fct)
y$delapres<-unlist(zm)
y$evt2<-unlist(ze)

yt<-y
yt$start<-yt$del
yt$stop<-yt$delapres
yt$etat<-yt$evt2
yt<-survSplit(yt, end="stop", start="start", cut=ti, event="etat")
yt<-yt[order(yt$PATIENT, yt$start),]
#des sujets meurt le jour de la visite
yt[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni,]
yt$etat[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni]<-1

yt[,var] <-yt$x

dim(Yt)
Yt<-merge(Yt, yt[, c("PATIENT", "start", var)], by=c("PATIENT", "start"), all=T)
Yt<-merge(Yt, yt, by=c("PATIENT", "start"), all=T)
dim(Yt)

head(Yt)

#mettre les variables initiales
y0<-bl[,  c("PATIENT", "SEX")]
y0 <- bl[,  c("PATIENT", vb1, vb2, vb3)]

dim(Yt)
Yt<-merge(Yt, y0, by="PATIENT", all.x=T, all.y=F)
dim(Yt)

#==========================================================
#selection des variables et des patients de all.cox (tableau pour l'analyse multivariee)

all.cox <- readRDS("data/all.cox_NOtimeTransf20170727.rds")


#-------------------------------------------------
#PB

#il y a des del evt etime.vni et ALS NA 3=> pb venait de ALS vide au depart pour ces patients. pb resolu en mergeant aussi evt del et time.vni
all.cox %>% filter(is.na(del) & is.na(evt) & is.na(time.vni)) %>% dim
all.cox %>% filter(is.na(del) & is.na(evt) & is.na(time.vni) & is.na(ALS)) %>% dim #=> pb vient de ALS
all.cox %>% filter(is.na(del) & is.na(evt) & is.na(time.vni) & is.na(ALS)) %>% count(PATIENT) #88 patient : c'est bien le nombre de aptient avec NA pour ces variables
#pb : il ya des start na : c'est parce que nettoyage au niveau de get_split n'est pas complet
all.cox %>% filter(is.na(start))
all.cox %>% filter(stop == del) %>% count(start) #quand stop = del alors tjr NA
all.cox %>% filter(is.na(start) & stop != del)  #start est na alors stop tjr == del
all.cox %>% filter(is.na(start) & evt==1)  #ID181 uniquement : on lui a bien mis etat == 1 a l'avant derniere ligne mais on a pas supprime derniere ligne
all.cox %>% filter(is.na(start) & evt==0)  #iD1081, 1208, 126 etc...

#NB MAJ de ti => pb des lignes avec infos supplementaires reglees
# Etude de ID181: 2 pb : le dernier start est NA mais aussi deux lignes avec des infos complémentaiers pour start = 1.54
all.cox %>% filter(PATIENT == "ID181" ) %>% View 
all.cox %>% filter(PATIENT == "ID181" & is.na(start))
all.cox %>% filter(PATIENT == "ID181" & stop>3)
#pb desdeux lignes supplémentaires vient du fait que ti prend toutes les valeurs de time.vni(delai entr epose de vni et deces) mais pas de del(qui est le délai entre la pose de vni et la consult)

# Etude de ID1081: 2 pb : le dernier start est NA mais aussi deux lignes avec des infos complémentaiers pour start = 11.92 et 37.22
all.cox %>% filter(PATIENT == "ID1081" ) %>% View 
all.cox %>% filter(PATIENT == "ID1081" & is.na(start)) #qd la derniere consult est la date de derniere nouvelle alors la derniere ligne est NA pour start => j'en fais quoi??
all.cox %>% filter(PATIENT == "ID1081" & stop>40)

# Etude de ID1208: 2 pb : le dernier start est NA mais aussi deux lignes avec des infos complémentaiers pour start = 11.92 et 37.22
all.cox %>% filter(PATIENT == "ID1208" ) %>% View 
all.cox %>% filter(PATIENT == "ID1208" & is.na(start)) #qd la derniere consult est la date de derniere nouvelle alors la derniere ligne est NA pour start => j'en fais quoi??
all.cox %>% filter(PATIENT == "ID1208" & stop>40)

#pb patient sans baseline=> c'est pourquoi pour BMI on a que 395 patients sans NA (alors qu'on a fait un na omit sur da qui est la base repetee BMI, avant split sur tous les evt)
all.cox %>% filter(is.na(BMI))
all.cox %>% filter(is.na(BMI) & !is.na(start))
all.cox %>% filter(PATIENT == "ID4930") #pas de baseline
all.cox %>% filter(PATIENT == "ID9415" & start>4) #pas de baseline
all.cox %>% filter(PATIENT == "ID9415" & start<4.66) %>% count(BMI) 
all.cox %>% filter(PATIENT == "ID1081" & stop >40) #le NA BMI vient de la ligne del = stop (cette consult n'a ete replie que pour certaines variables et les autres sont NA)
all.cox %>% filter(PATIENT == "ID7877" & stop >15) #le NA BMI vient de la ligne del = stop (cette consult n'a ete replie que pour certaines variables et les autres sont NA)
#essai pour comparer avec Yann notamment les bug
y <- get_split(da2, "ALS")
y2 <- get_split(da2, "BMI")
full_join(y, y2, by = c("PATIENT", "start", "stop", "etat")) %>% filter(PATIENT =="ID1067") %>% View
full_join(y, y2, by = c("PATIENT", "start", "stop", "etat")) %>% filter(PATIENT =="ID101") %>% View


#-------------------------------------------------
#Analyse des donnees manquantes

#combien de patient si on fait un NA omit?
all.cox %>% na.omit %>% count(PATIENT)#3

#combien de NA par variable
isnotNA <- function(x) !is.na(x)
count_na <- function(x) sum(is.na(x))
any_NA <- function(x) any(is.na(x))
no_NA_atall <- function(x) all(!is.na(x))
onlyNA <- function(x) all(is.na(x))

# PATIENT sélectionnable par variable : cad patient avec une baseline pour la variable dinteret

#si bl présente, est-ce qu'une autre variable peut valloir 0?
# isBl1AndSuiv0 <- function(data, var){
# 
#   var <- enquo(var)
#   print(var)
#   df1 <- data %>% group_by(PATIENT) %>% mutate(bl = ifelse(is.na(!!var) & start == 0, 0, 1), bl = min(bl), #ya t-il une bl <- 
#                                            atleast1 = ifelse(!is.na(!!var) & del !=0 , 1, 0), atleast1 = max(atleast1), #y a til une variable autre que bl qui est non na
#                                            #nabl = ifelse(is.na(ALS) & del != 0, 1, 0), napat = max(nabl)
#                                            maxdel = max(del)) %>% #filter(PATIENT =="ID3486") %>% select(ALS, start, bl)
#     filter(bl==1 & atleast1 == 0 & max(del!=0)) %>% 
#     select(!!var, del, bl, atleast1,maxdel, PATIENT)
#   #nrow(df1)
#   print(df1)
#   
# }

isBl1AndSuiv0 <- function(data2, x){
  data2[ ,"var"] <- data2[ ,x]
  df1 <- data2 %>% group_by(PATIENT) %>% mutate(bl = ifelse(is.na(var) & start == 0, 0, 1), bl = min(bl), #ya t-il une bl <- 
                                               atleast1 = ifelse(!is.na(var) & del !=0 , 1, 0), atleast1 = max(atleast1), #y a til une variable autre que bl qui est non na
                                               #nabl = ifelse(is.na(ALS) & del != 0, 1, 0), napat = max(nabl)
                                               maxdel = max(del)) %>% #filter(PATIENT =="ID3486") %>% select(ALS, start, bl)
    filter(bl==1 & atleast1 == 0 & max(del!=0)) %>% 
    select(var, del, bl, atleast1,maxdel, PATIENT)
  colnames(df1)[colnames(df1)=="var"] <- x
  print(df1)
  nrow(df1)
}
e1 <- sapply(colnames(all.cox), function(x)isBl1AndSuiv0(all.cox, x))
sapply("BMI", function(x)isBl1AndSuiv0(all.cox, x))
e1; sum(e1)
#Non si j'ai une baseline, alors j'ai toujours une valeur après 

#Donc il suffit de garder del==0 pour chaque patient, et compter combien j'ai de NA par colonne
#1 seul start = 0 par patient, et j'ai bien 706 start = 0 (NB on a retire les PATIENTS sans consultation bl)
all.cox %>% filter(start == 0) %>% count(PATIENT) %>% filter(n>1)
all.cox %>% filter(start == 0) %>% group_by(PATIENT) %>% mutate(n = n()) %>% filter(n>1) %>%  select(PATIENT, start, stop, del, evt) #patient a la fois evt = 1 et evt = 0...
#pour chaque variable, nombre de NA
all.cox %>% filter(start == 0) %>% summarise_all(is.na) %>% select(-PATIENT) %>% summarise_all(sum) %>% t
#nombre de patient avec une baseline (et donc pas de NA), pour chaque variable
all.cox %>% filter(start == 0) %>% summarise_all(isnotNA ) %>% select(-PATIENT) %>% summarise_all(sum) %>% t


#=> il faut maintenant pour chaque variable, la liste des patients sans baseline, qui sont à supprimer 
#(au lieu de faire un na omit qui supprimera les baseline NA, mais pas les valeurs renseignées suivantes)

#Si je garde la variable ALS, ce sont les patients a retirer



PatToDelete <- function(data2, var){
  data2[ ,"x"] <- data2[ ,var]
  data2 %>% filter(start == 0) %>%  
    filter(is.na(x)) %>% pull(PATIENT)
  # var <- enquo(var)
  # data2 %>% filter(start == 0) %>%  
  #   filter(is.na(!!var)) %>% pull(PATIENT)
}
.l <-lapply(vardr.vec, function(x) PatToDelete(all.cox, x))
for (i in seq_along(vardr.vec)) assign(vardr.vec[i], .l[[i]])

Npat <- data.frame(variables = vardr.vec, N2del = sapply(.l, length), stringsAsFactors = F)

#Je ne garde que les variables avec moins de 30% de donnees manquantes a baseline
my_var <- Npat %>% filter(N2del<706*0.3) %>% pull(variables)

#Nombre de patients a supprimer si on garde toutes les variables avec moins de 30% de donnees manquantes
vec <- vector()
for (var in my_var) vec <- unique(c(vec, get(var)))
length(vec)

#Choose the smallest number to delete (NB : the number in the Combin var is the rank in the vector my_var)
var_5 <- expand.grid(v1 = seq_along(my_var), v2 = seq_along(my_var), v3 = seq_along(my_var), v4 = seq_along(my_var), v5 = seq_along(my_var))
x <- var_5[1,]
var_5$combin <- apply(var_5, 1, function(x)paste(sort(unique(as.numeric(x))), collapse = ";"))
duplicated(c(1,1))
var_5 <- var_5[!duplicated(var_5$combin), ]
to_try <- lapply(seq_len(nrow(var_5)), function(i) unique(as.numeric(var_5[i, 1:5])))
var_5$Ndel <- sapply(seq_along(to_try),function(j){
  a <- to_try[[j]]
  vec <- vector()
  for (i in a){
    namesdel <- get(my_var[i])
    #print(length(namesdel))
    vec <- unique(c(vec, namesdel))
    todel <- length(vec)
  }
  return(todel)
})

var_5 %>% arrange(Ndel)

#502 Patients en gardant les 5 variables avec moins de 30% de manquants baseline
all.cox %>% select(one_of(my_var)) %>% na.omit %>% count(PATIENT)

#nombre de patients si on rajoute les variables baseline
noselect <- vardr.vec[!vardr.vec %in% my_var] 
saveRDS(noselect, "data/analyses/noselect.rds")
all.cox %>% select(-one_of(noselect)) %>% na.omit %>% count(PATIENT)

all.cox %>% select(-one_of(noselect), -E_PHAR_LAR, -APPAREILLE_PP) %>% na.omit %>% count(PATIENT) #en retirant cvf_ERS on gagne 1 patient, en retirant toux efficace on gagne 50 patients
all.cox_noNA <- all.cox %>% select(-one_of(noselect), -E_PHAR_LAR, -APPAREILLE_PP) %>% na.omit 

saveRDS(all.cox_noNA, "data/all.cox_noNA.rds")

# #nb de NA par patient par variable
# all.cox %>% group_by(PATIENT) %>% summarise_all(count_na) 
# #pour chaque pqtient et pour chqaue variable, dit s'il y a au moins un NA
# all.cox %>% group_by(PATIENT) %>% summarise_all(any_NA)
# #pour chaque variable, combien de patient avec au moins 1 NA
# all.cox %>% group_by(PATIENT) %>% 
#   #select(-start) %>% 
#   summarise_all(any_NA) %>% #et pas mutate all qui donne TF pr chaque ligne du patient
#   #summarise_all(onlyNA) %>% #pas pareil parce que pour certain patient il n'y a pas de baseline donc NA puis on a une valeur
#   ungroup %>% select(-PATIENT) %>% summarise_all(sum) %>% t
# 
# all.cox %>% filter(PATIENT =="ID2395" & !is.na(ALS))
# 
# all.cox %>% group_by(PATIENT) %>% mutate(nabl = ifelse(is.na(ALS) & del == 0, 1, 0), napat = max(nabl))  %>% select(ALS, del, nabl)
#   mutate(.var = all.cox %>% select(one_of(var)) %>% pull) %>% 
#   mutate(bl = ifelse((is.na(.var) & start == 0) | (is.na(.var) & row_number() == n()), 0, 1)) %>%
#   group_by(PATIENT) %>%  slice(1) %>% ungroup %>% count(bl)
# 
# #pour chaque variable, combien de patient sans NA
# noNA_var <- all.cox %>% group_by(PATIENT) %>% #select(-start) %>%
#   summarise_all(no_NA_atall) %>% #et pas mutate all qui donne TF pr chaque ligne du patient
#   ungroup %>% select(-PATIENT) %>% summarise_all(sum) %>% t
# #essai de na.omit em retirant des variables (le pb des start ne modifie pas le compte des patienst car na.omit retire des lignes)
# select_var <- noNA_var %>% data.frame(NB=., var=row.names(.), stringsAsFactors = FALSE) %>% filter(NB>300) %>% .$var
# all.cox %>% select(PATIENT, one_of(select_var)) %>% na.omit %>% count(PATIENT)#363 PATTIENT avec un seuil de 300
# # #Avec la selection combien de patient 
# # all.cox %>% select(PATIENT, one_of(select_var)) %>% group_by(PATIENT) %>% #select(-start) %>%
# #   summarise_all(any_NA) %>% #et pas mutate all qui donne TF pr chaque ligne du patient
# #   ungroup %>% select(-PATIENT) %>% mutate(nNA = apply(.,1,sum), isNA = ifelse(nNA>0, 1, 0)) %>% count(isNA) 
# 
# noNA_rows <- all.cox %>% select(PATIENT, one_of(select_var)) %>% na.omit #363 PATTIENT avec un seuil de 300
# noNA_rows %>% count(PATIENT)
# #Pb ces patients n'ont peut etre pas de baseline
# noNA_rows %>% group_by(PATIENT) %>% summarise(mindel = min(del), minstart = min(start)) %>% filter(minstart!=0) #94 patients sans baseline


#==========================================================
# Tranformation des variables
all.cox <- readRDS("data/all.cox_NOtimeTransf20170727.rds")
noselect <- readRDS("data/analyses/noselect.rds")
ac <- readRDS("data/all.cox_noNA.rds")
ac <- ac %>% ungroup
transf <- read.csv2("data/variables_and_transformations.csv", stringsAsFactors = FALSE)
variables.vec <- all.cox %>% select(-one_of(noselect), -E_PHAR_LAR, -APPAREILLE_PP) %>% na.omit %>% colnames 
transf <- transf %>% filter(variable %in% variables.vec | grepl("LIEUDEB", variable))

#LOG LINEARITE
med_CVF_ERS <- ac %>% filter(!is.na(CVF_ERS)) %>% group_by(PATIENT) %>% filter(row_number()==1) %>% ungroup %>% summarise(median(CVF_ERS)) %>% as.numeric
med_SLAtillvni <- ac %>% filter(!is.na(SLAtillvni)) %>% group_by(PATIENT) %>% filter(row_number()==1) %>% ungroup %>% summarise(median(SLAtillvni)) %>% as.numeric
med_agevni <- ac %>% filter(!is.na(agevni)) %>% group_by(PATIENT) %>% filter(row_number()==1) %>% ungroup %>% summarise(median(agevni)) %>% as.numeric
med_ALS <- ac %>% filter(!is.na(ALS)) %>% group_by(PATIENT) %>% filter(row_number()==1) %>% ungroup %>% summarise(median(ALS)) %>% as.numeric
med_BMI <- ac %>% filter(!is.na(BMI)) %>% group_by(PATIENT) %>% filter(row_number()==1) %>% ungroup %>% summarise(median(BMI)) %>% as.numeric
 transf %>% filter(recode == TRUE)
ac <- ac %>% mutate(CVF_ERS_LL = ifelse(CVF_ERS<med_CVF_ERS, 0, 1), 
                   SLAtillvni_LL = ifelse(SLAtillvni<med_SLAtillvni, 0, 1),
                   agevni_LL = ifelse(agevni<med_agevni, 0, 1),
                   ALS_LL = ifelse(ALS < med_ALS, 0, 1),
                   BMI_LL = ifelse(BMI< med_BMI, 0, 1))

#RISQUES PROPORTIONNELS
transf %>% filter(RP == FALSE & recode == TRUE) #aucune variable recodee et a transformer 
transf %>% filter(RP == FALSE)

#LIEUDEB : separation en binaire et tranformation du temps
ac <- ac %>% mutate(cerv = ifelse(LIEUDEB_recode=="cervical",1,0),
              llimb = ifelse(LIEUDEB_recode=="lower limb", 1, 0),
              resp = ifelse(LIEUDEB_recode=="respiratory", 1, 0),
              ulimb = ifelse(LIEUDEB_recode=="upper limb", 1, 0)) %>% 
  mutate(resp_t = resp / stop)

#transformation du temps
ac <- ac %>% mutate(R_MUSCL_ACCES_t = R_MUSCL_ACCES * stop^3, #R_MUSCL_ACCES
              ENC_BRONCHIQ_t = ENC_BRONCHIQ * log(stop),#ENC_BRONCHIQ
              FERM_BOUCHE_t = FERM_BOUCHE * log(stop))#FERM_BOUCHE
           
#coupure du temps
#FAUS_ROUTE
ac <- ac %>% mutate(FAUS_ROUTE_t1 = FAUS_ROUTE * ifelse(stop <= 6, 1, 0),
              FAUS_ROUTE_t2 = FAUS_ROUTE * ifelse(stop > 6 & stop <= 18, 1, 0),
              FAUS_ROUTE_t3 = FAUS_ROUTE * ifelse(stop > 18 & stop <= 36, 1, 0),
              FAUS_ROUTE_t4 = FAUS_ROUTE * ifelse(stop >36, 1, 0)) %>% 
  #REVEIL_ETOUF
  mutate(REVEIL_ETOUF_t1 = REVEIL_ETOUF * ifelse(stop <= 6, 1, 0),
         REVEIL_ETOUF_t2 = REVEIL_ETOUF * ifelse(stop > 6 & stop <= 18, 1, 0),
         REVEIL_ETOUF_t3 = REVEIL_ETOUF * ifelse(stop > 18 & stop <= 36, 1, 0),
         REVEIL_ETOUF_t4 = REVEIL_ETOUF * ifelse(stop >36, 1, 0))

#==========================================================
# Construction du coxmultivarie

my_var_transf <- list(c("SEX"), c("SLAtillvni_LL"), c("TOUX_EFFICACE"),  c("cerv", "llimb", "resp", "ulimb", "resp_t"), 
     c("R_MUSCL_ACCES","R_MUSCL_ACCES_t"), c("ENC_BRONCHIQ", "ENC_BRONCHIQ_t"),
     c("FAUS_ROUTE_t1", "FAUS_ROUTE_t2", "FAUS_ROUTE_t3", "FAUS_ROUTE_t4"),
     c("RESP_PARADOX"), c("CVF_ERS_LL"),  c("DYSP_PAROLE"),
     c("REVEIL_MULTI"), c("sla_familiale"), c("agevni_LL"), c("DYSP_REPOS"),
     c("DYSP_DECUBI"), c("FERM_BOUCHE", "FERM_BOUCHE_t"), c("DYSP_PAROX"), 
     c("REVEIL_ETOUF_t1", "REVEIL_ETOUF_t2", "REVEIL_ETOUF_t3", "REVEIL_ETOUF_t4"),
     c("ALS_LL"), c("BMI_LL"), c("PACO2_PP_CL1"), c("PACO2_PP"), c("MORPHO_PP_CHOICE_1"))

# #stepwise backward
# tmp.var <- my_var_transf
# all.var <- paste(unlist(tmp.var), collapse = " + ")
# cox23.clus <- coxph(as.formula(paste0("Surv(start, stop, etat)~",all.var, "+ cluster(PATIENT)")), data = ac) 
# cox23 <- coxph(as.formula(paste0("Surv(start, stop, etat)~",all.var)), data = ac)
# identical(cox23.clus$coefficients,cox23$coefficients)
# 
# summary(cox23)
# tmp.var.new <- tmp.var[-11]
# all.var.new <- paste(unlist(tmp.var.new), collapse = " + ")
# delvar <- unlist(tmp.var[11])
# ddl <- length(delvar)
# #faut-il faire un init ou pas de sens? init pour avoir la pvalue de la variable, pas du modele
# cox22 <- coxph(as.formula(paste0("Surv(start, stop, etat)~",all.var.new, "+ cluster(PATIENT)")), data = ac)
# init <- c(rep(0,ddl), coef(cox22))
# cox23 <- coxph(as.formula(paste0("Surv(start, stop, etat)~", paste(delvar, all.var.new, sep = " + "), "+ cluster(PATIENT)")), data = ac, init = init) 
# rscore <- cox23$rscore
# pval <- 1-pchisq(rscore, ddl)
# 
# p23 <- sapply(1:23, function(i){
#   print(i)
#   tmp.var.new <- tmp.var[-i]
#   all.var.new <- paste(unlist(tmp.var.new), collapse = " + ")
#   delvar <- unlist(tmp.var[i])
#   ddl <- length(delvar)
#   #faut-il faire un init ou pas de sens? init pour avoir la pvalue de la variable, pas du modele
#   cox22 <- coxph(as.formula(paste0("Surv(start, stop, etat)~",all.var.new, "+ cluster(PATIENT)")), data = ac)
#   init <- c(rep(0,ddl), coef(cox22))
#   cox23 <- coxph(as.formula(paste0("Surv(start, stop, etat)~", 
#                                    paste(paste(delvar, collapse = " + "), all.var.new, sep = " + "),
#                                    "+ cluster(PATIENT)")
#                             ), data = ac, init = init) 
#   rscore <- cox23$rscore
#   pval <- 1-pchisq(rscore, ddl)
#   print(pval)
#   return(pval)
# })
# p23 <- data.frame(index = 1:23, pvalrscore = p23)
# numtodel <- p23 %>% arrange(pvalrscore) %>% filter(row_number() == n()) %>% .$index
# tmp.var <- tmp.var[-numtodel]
# p22 <- sapply(1:22, function(i){
#   print(i)
#   tmp.var.new <- tmp.var[-i]
#   all.var.new <- paste(unlist(tmp.var.new), collapse = " + ")
#   delvar <- unlist(tmp.var[i])
#   ddl <- length(delvar)
#   #faut-il faire un init ou pas de sens? init pour avoir la pvalue de la variable, pas du modele
#   cox_parc <- coxph(as.formula(paste0("Surv(start, stop, etat)~",all.var.new, "+ cluster(PATIENT)")), data = ac)
#   init <- c(rep(0,ddl), coef(cox_parc))
#   cox_full <- coxph(as.formula(paste0("Surv(start, stop, etat)~", 
#                                    paste(paste(delvar, collapse = " + "), all.var.new, sep = " + "),
#                                    "+ cluster(PATIENT)")
#   ), data = ac, init = init) 
#   rscore <- cox_full$rscore
#   pval <- 1-pchisq(rscore, ddl)
#   print(pval)
#   return(pval)
# })
# 
# tmp <- data.frame(index = seq(tmp.var), name_first = unlist(lapply(tmp.var, '[[',1)), pvalrscore = p22)
# tmp <- tmp %>% arrange(pvalrscore)
# print(tmp)
# numtodel <- tmp %>% filter(row_number() == n()) %>% .$index
# tmp.var <- tmp.var[-numtodel]

#--------------------------------------------------------
#loop for backward selection
res <- data.frame()
tmp.var.init <- my_var_transf
tmp.var <- my_var_transf
all.var <- paste(unlist(tmp.var.init), collapse = " + ")

for (j in seq(tmp.var.init)){
  print(j)
  p.tmp <- sapply(seq(tmp.var), function(i){
    print(i)
    tmp.var.new <- tmp.var[-i]
    all.var.new <- paste(unlist(tmp.var.new), collapse = " + ")
    delvar <- unlist(tmp.var[i])
    ddl <- length(delvar)
    #faut-il faire un init ou pas de sens? init pour avoir la pvalue de la variable, pas du modele
    cox_parc <- coxph(as.formula(paste0("Surv(start, stop, etat)~",all.var.new, "+ cluster(PATIENT)")), data = ac)
    init <- c(rep(0,ddl), coef(cox_parc))
    cox_full <- coxph(as.formula(paste0("Surv(start, stop, etat)~", 
                                        paste(paste(delvar, collapse = " + "), all.var.new, sep = " + "),
                                        "+ cluster(PATIENT)")
    ), data = ac, init = init) 
    rscore <- cox_full$rscore
    pval <- 1-pchisq(rscore, ddl)
    print(pval)
    return(pval)
  })
  tmp <- data.frame(index = seq(tmp.var), name_first = unlist(lapply(tmp.var, '[[',1)), pvalrscore = p.tmp)
  tmp <- tmp %>% arrange(pvalrscore) %>% mutate(numrow = row_number())
  print(tmp)
  res <- bind_rows(res, data.frame(NA), tmp)
  numtodel <- tmp %>% filter(row_number() == n()) %>% .$index
  tmp.var <- tmp.var[-numtodel]
  if(max(tmp$pvalrscore)<0.05) {
    print("no more to delete, all variable < 0,05")
    print(res)
    stop()
  }
}
res

# #test du rapport de vraisemblance tres significatif avec le modele plein=> seleciton se justifie (Harrell p523)
# #backward meilleur que forward Harrell p70
# 
# 
# #stepwise forward
# tmp.var <- my_var_transf[1:2]
# all.var <- paste(unlist(tmp.var), collapse = " + ")
# cox2 <- coxph(as.formula(paste0("Surv(start, stop, etat)~",all.var, "+ cluster(PATIENT)")), data = ac) 
# #est-ce qu'on rajoute variable 3?
# i <- 3
# for (i in 3:length(my_var_transf)){
#   old.cox <- get(paste0("cox", i-1))
#   print(i)
#   print(my_var_transf[i])
#   nbpar <- length(unlist(my_var_transf[i]))
#   init <- c(coef(old.cox), rep(0, nbpar))
#   tmp.var_new <- c(tmp.var, my_var_transf[i])
#   all.var <- paste(unlist(tmp.var_new), collapse = " + ")
#   assign(paste0("cox", i), coxph(as.formula(paste0("Surv(start, stop, etat)~",all.var, "+ cluster(PATIENT)")), data = ac, init = init)) 
#   cox.tmp <- get(paste0("cox", i))
#   ddl <- nbpar
#   pval <- 1-pchisq(cox.tmp$rscore, ddl)
#   print(pval)
#   if (pval<0.5) tmp.var <- tmp.var_new #0.5 pour eviter surselection comme indique par harrell
#   else assign(paste0("cox", i), old.cox)
#   print(tmp.var)
# }
# length(tmp.var)
# cox.forward <- cox23
# unlist(my_var_transf)[!unlist(my_var_transf) %in% unlist(tmp.var)]
# #stepwise backward
# cox23 <- coxph(as.formula(paste0("Surv(start, stop, etat)~",all.var, "+ cluster(PATIENT)")), data = ac) 
# 
# i <- 1
# tmp.var <- my_var_transf[as.numeric(paste0("-", i))]
# nbpar <- length(unlist(my_var_transf[i]))
# init <- c(coef(cox23, rep(0, nbpar)))
# 
# cox22 <- coxph(as.formula(paste0("Surv(start, stop, etat)~",tmp.var, "+ cluster(PATIENT)")), data = ac, init = init) 


#-----------------------------------
# Model multivarie a partir de la selection backward

tmp.var <- my_var_transf[c(2, 3, 4, 6, 10, 19, 20, 22)]
all.var <- paste(unlist(tmp.var), collapse = " + ")
cox_select <- coxph(as.formula(paste0("Surv(start, stop, etat)~",all.var, "+ cluster(PATIENT)")), data = ac) 

p.tmp <- sapply(seq(tmp.var), function(i){
  print(i)
  tmp.var.new <- tmp.var[-i]
  all.var.new <- paste(unlist(tmp.var.new), collapse = " + ")
  delvar <- unlist(tmp.var[i])
  ddl <- length(delvar)
  #faut-il faire un init ou pas de sens? init pour avoir la pvalue de la variable, pas du modele
  cox_parc <- coxph(as.formula(paste0("Surv(start, stop, etat)~",all.var.new, "+ cluster(PATIENT)")), data = ac)
  init <- c(rep(0,ddl), coef(cox_parc))
  cox_full <- coxph(as.formula(paste0("Surv(start, stop, etat)~", 
                                      paste(paste(delvar, collapse = " + "), all.var.new, sep = " + "),
                                      "+ cluster(PATIENT)")
  ), data = ac, init = init) 
  rscore <- cox_full$rscore
  pval <- 1-pchisq(rscore, ddl)
  print(pval)
  return(pval)
})
tmp <- data.frame(index = seq(tmp.var), name_first = unlist(lapply(tmp.var, '[[',1)), pvalrscore = p.tmp)
tmp <- tmp %>% arrange(pvalrscore) %>% mutate(numrow = row_number())
print(tmp) #NB : ce n'est pas la pval de lieudeb cervical mais de toutes les variables lieudeb  

#----------------------------------------
# Cox with only baseline

ac <- readRDS("data/all.cox_NOtimeTransf20170727.rds")
ac <- ac %>% ungroup %>% select(PATIENT, start, stop, etat, one_of(varbl.vec)) %>% na.omit 

varbl.vec

#pb avec sla till vni


#ac <- ac %>% select(PATIENT, start, stop, etat, one_of(unlist(my_var_transf_bl))) %>% na.omit
 ac %>% select(PATIENT, start, stop, etat, one_of(varbl.vec)) %>% na.omit %>% count(PATIENT) #482 PATIENT avant transformation
# verifier les NA
# pour chaque variable, combien de patient sans NA
isnotNA <- function(x) !is.na(x)
ac %>% group_by(PATIENT) %>% filter(start==0) %>% #select(-start) %>%
  summarise_all(isnotNA) %>% #et pas mutate all qui donne TF pr chaque ligne du patient
  #summarise_all(no_NA_atall) %>% #et pas mutate all qui donne TF pr chaque ligne du patient
  ungroup %>% select(-PATIENT) %>% summarise_all(sum) %>% t


#LOG LINEARITE
med_CVF_ERS <- ac %>% filter(!is.na(CVF_ERS)) %>% group_by(PATIENT) %>% filter(row_number()==1) %>% ungroup %>% summarise(median(CVF_ERS)) %>% as.numeric
med_SLAtillvni <- ac %>% filter(!is.na(SLAtillvni)) %>% group_by(PATIENT) %>% filter(row_number()==1) %>% ungroup %>% summarise(median(SLAtillvni)) %>% as.numeric
med_agevni <- ac %>% filter(!is.na(agevni)) %>% group_by(PATIENT) %>% filter(row_number()==1) %>% ungroup %>% summarise(median(agevni)) %>% as.numeric
transf %>% filter(recode == TRUE)
ac <- ac %>% mutate(CVF_ERS_LL = ifelse(CVF_ERS<med_CVF_ERS, 0, 1), 
                    SLAtillvni_LL = ifelse(SLAtillvni<med_SLAtillvni, 0, 1),
                    agevni_LL = ifelse(agevni<med_agevni, 0, 1))

#RISQUES PROPORTIONNELS
transf %>% filter(RP == FALSE & recode == TRUE) #aucune variable recodee et a transformer 
transf %>% filter(RP == FALSE)

#LIEUDEB : separation en binaire et tranformation du temps
ac <- ac %>% mutate(cerv = ifelse(LIEUDEB_recode=="cervical",1,0),
                    llimb = ifelse(LIEUDEB_recode=="lower limb", 1, 0),
                    resp = ifelse(LIEUDEB_recode=="respiratory", 1, 0),
                    ulimb = ifelse(LIEUDEB_recode=="upper limb", 1, 0)) %>% 
  mutate(resp_t = resp / stop)

#transformation du temps
ac <- ac %>% mutate(R_MUSCL_ACCES_t = R_MUSCL_ACCES * stop^3, #R_MUSCL_ACCES
                    ENC_BRONCHIQ_t = ENC_BRONCHIQ * log(stop),#ENC_BRONCHIQ
                    FERM_BOUCHE_t = FERM_BOUCHE * log(stop))#FERM_BOUCHE

#coupure du temps
#FAUS_ROUTE
ac <- ac %>% mutate(FAUS_ROUTE_t1 = FAUS_ROUTE * ifelse(stop <= 6, 1, 0),
                    FAUS_ROUTE_t2 = FAUS_ROUTE * ifelse(stop > 6 & stop <= 18, 1, 0),
                    FAUS_ROUTE_t3 = FAUS_ROUTE * ifelse(stop > 18 & stop <= 36, 1, 0),
                    FAUS_ROUTE_t4 = FAUS_ROUTE * ifelse(stop >36, 1, 0)) %>% 
  #REVEIL_ETOUF
  mutate(REVEIL_ETOUF_t1 = REVEIL_ETOUF * ifelse(stop <= 6, 1, 0),
         REVEIL_ETOUF_t2 = REVEIL_ETOUF * ifelse(stop > 6 & stop <= 18, 1, 0),
         REVEIL_ETOUF_t3 = REVEIL_ETOUF * ifelse(stop > 18 & stop <= 36, 1, 0),
         REVEIL_ETOUF_t4 = REVEIL_ETOUF * ifelse(stop >36, 1, 0))


my_var_transf_bl <- list(c("SEX"), c("SLAtillvni_LL"), c("TOUX_EFFICACE"),  c("cerv", "llimb", "resp", "ulimb", "resp_t"), 
                      c("R_MUSCL_ACCES","R_MUSCL_ACCES_t"), c("ENC_BRONCHIQ", "ENC_BRONCHIQ_t"),
                      c("FAUS_ROUTE_t1", "FAUS_ROUTE_t2", "FAUS_ROUTE_t3", "FAUS_ROUTE_t4"),
                      c("RESP_PARADOX"), c("CVF_ERS_LL"),  c("DYSP_PAROLE"),
                      c("REVEIL_MULTI"), c("sla_familiale"), c("agevni_LL"), c("DYSP_REPOS"),
                      c("DYSP_DECUBI"), c("FERM_BOUCHE", "FERM_BOUCHE_t"), c("DYSP_PAROX"), 
                      c("REVEIL_ETOUF_t1", "REVEIL_ETOUF_t2", "REVEIL_ETOUF_t3", "REVEIL_ETOUF_t4"))


ac <- ac %>% select(PATIENT, start, stop, etat, one_of(unlist(my_var_transf_bl))) %>% na.omit
res <- data.frame()
tmp.var.init <- my_var_transf_bl
tmp.var <- my_var_transf_bl
all.var <- paste(unlist(tmp.var.init), collapse = " + ")

for (j in seq(tmp.var.init)){
  print(j)
  p.tmp <- sapply(seq(tmp.var), function(i){
    print(i)
    tmp.var.new <- tmp.var[-i]
    all.var.new <- paste(unlist(tmp.var.new), collapse = " + ")
    delvar <- unlist(tmp.var[i])
    ddl <- length(delvar)
    #faut-il faire un init ou pas de sens? init pour avoir la pvalue de la variable, pas du modele
    cox_parc <- coxph(as.formula(paste0("Surv(start, stop, etat)~",all.var.new, "+ cluster(PATIENT)")), data = ac)
    init <- c(rep(0,ddl), coef(cox_parc))
    cox_full <- coxph(as.formula(paste0("Surv(start, stop, etat)~", 
                                        paste(paste(delvar, collapse = " + "), all.var.new, sep = " + "),
                                        "+ cluster(PATIENT)")
    ), data = ac, init = init) 
    rscore <- cox_full$rscore
    pval <- 1-pchisq(rscore, ddl)
    print(pval)
    return(pval)
  })
  tmp <- data.frame(index = seq(tmp.var), name_first = unlist(lapply(tmp.var, '[[',1)), pvalrscore = p.tmp)
  tmp <- tmp %>% arrange(pvalrscore) %>% mutate(numrow = row_number())
  print(tmp)
  res <- bind_rows(res, data.frame(NA), tmp)
  numtodel <- tmp %>% filter(row_number() == n()) %>% .$index
  tmp.var <- tmp.var[-numtodel]
  if(max(tmp$pvalrscore)<0.05) {
    print("no more to delete, all variable < 0,05")
    print(res)
    stop()
  }
}
res


# set.seed(1010)
# n=1000;p=100
# nzc=trunc(p/10)
# x=matrix(rnorm(n*p),n,p)
# 
# beta=rnorm(nzc)
# fx=x[,seq(nzc)]%*%beta/3
# hx=exp(fx)
# ty=rexp(n,hx)
# tcens=rbinom(n=n,prob=.3,size=1)# censoring indicator
# y=cbind(time=ty,status=1-tcens) # y=Surv(ty,1-tcens) with library(survival)
# foldid=sample(rep(seq(10),length=n))
# 
# fit1_cv=cv.glmnet(ac[ ,unlist(my_var_transf)], Surv(ac$start, ac$stop, ac$etat), family="cox",foldid=ac$PATIENT)
# plot(fit1_cv)
# title("Cox Family",line=2.5)










tie<-sort(unique(da2$time.vni[da2$evt==1]));tie
dim(all.cox)
a<-all.cox[all.cox$start%in%tie | all.cox$stop%in%tie,]
dim(a)
summary(a$start)
summary(a$stop)


a2<-a[(a$start==a$stop | is.na(a$start)) & a$stop==a$time.vni,]
dim(a2)
a2[, c("PATIENT", "start", "stop")]
id<-"ID1348"
id<-c("ID8412")
all.cox[all.cox$PATIENT%in%id,c("PATIENT", "start", "stop", "ALS")]


# all.cox %>% group_by(PATIENT) %>% summarise_all(any_NA) %>% select(-PATIENT) %>%  apply(.,1,sum)
# all.cox %>% group_by(PATIENT) %>% summarise_all(any_NA) %>% select(-PATIENT) %>%  summarise_all(sum)
# all.cox %>% group_by(PATIENT) %>% summarise_all(any_NA) %>% count






#-----------------------------------------------------------------

yt<-na.omit(Yt)
coxt<-coxph(Surv(start, stop, etat)~SEX+dyspnee+cephal+cluster(PATIENT), data=yt)
summary(coxt)


coxt0<-coxph(Surv(start, stop, etat)~SEX+dyspnee+cluster(PATIENT), data=yt)
#cephal
nbpar<-1
init<-c(coef(coxt0), rep(0, nbpar))
coxt1<-coxph(Surv(start, stop, etat)~SEX+dyspnee+cephal+cluster(PATIENT), data=yt, init=init)
summary(coxt1)

#=================================================
#=================================================

#=======================
#Modele longitudinal
#=======================
attach(autism)
sicdegp.f <- factor(sicdegp)
age.f <- factor(age)
autism.updated <- data.frame(autism, sicdegp.f, age.f)
age.2 <- age -2
age.2sq <- age.2*age.2
sicdegp2 <- cut(sicdegp, breaks = 0:3, labels = FALSE)
sicdegp2.f <- factor(sicdegp2)
autism.updated <- subset(data.frame(autism, sicdegp2.f, age.2), !is.na(vsae))
autism.grouped <- groupedData(vsae~age.2|childid, data = autism.updated, order.groups = FALSE)

model6.1.fit <- lme(vsae ~ age.2 +I(age.2^2) + sicdegp2.f + age.2:sicdegp2.f +I(age.2^2):sicdegp2.f,
                    random = ~ age.2 + I(age.2^2), method = "REML",
                    data = autism.grouped)
model6.2.fit <- lme(vsae ~ age.2 +I(age.2^2) + sicdegp2.f + age.2:sicdegp2.f +I(age.2^2):sicdegp2.f,
                    random = ~ age.2 + I(age.2^2) -1, method = "REML", #-1 retire ici l'intercept par enfant
                    data = autism.grouped)
summary(model6.2.fit)
model6.2a.fit <- update(model6.2.fit, random = ~ age.2 -1) #-1 ici retire l'effet de l'age au carre
summary(model6.2a.fit)
#Y a t il une difference significative entre ces deux modeles
dif <- logLik(model6.2.fit)*2 - logLik(model6.2a.fit)*2
h6.1.pvalue <- 0.5*(1-pchisq(dif,1) + 0.5*(1-pchisq(dif,2)))
#modeles significativement different, on prend l'AIC le plus petit cad 
AIC(model6.2.fit)
AIC(model6.2a.fit)

#-----------------------------
#plot de BMI en fonction du temps
d$evt <- as.factor(d$evt)
 d %>% filter(qui == "BMI" & PATIENT == "ID2215") 
 d %>% filter(qui == "BMI" & x>60) 
 d2 <- d %>% filter(qui=="BMI")
xyplot(x ~ del | evt, data = d2) #le | sépare la population en  vivant et transplante/mort
xyplot(x ~ del | evt, group = PATIENT, data = d2) #group permet de grouper par identifiant
xyplot(x ~ del | evt, group = PATIENT, data = da, panel = function(x, y,...){ #...permet de conserver group (on ne peut pas le mettre dans panel)
  panel.xyplot(x, y, type = "b", col= 1, ...) #trace les trajectoires
  panel.loess(x, y, col = 2, lwd = 2) #trace un moyenne des trajectoires?
}, xlab = "delai (jours)", ylab = "BMI(kg/m2)")  

#-----------------------------
#definition de la structure hierarchique
da.grp <- groupedData(x ~ del | PATIENT, data = da)


#=====================================
#modele mixte sans covariables

#-----------------------------
#selection des effets aleatoires

lmeBMI.111 <- lme(fixed = x ~ del + I(del^2), data = da.grp, random = ~ del + I(del^2), na.action = na.omit, method = "REML")
lmeBMI.011 <- update(lmeBMI.111, random = ~ del + I(del^2) - 1)
lmeBMI.010 <- update(lmeBMI.111, random = ~ del - 1)
lmeBMI.100 <- update(lmeBMI.111, random = ~ 1)
lmeBMI.110 <- update(lmeBMI.111, random = ~ 1 + del)
#lmeBMI.110 <- update(lmeBMI.111, random = ~ del)
lmeBMI.101 <- update(lmeBMI.111, random = ~ 1 + I(del^2))
lmeBMI.001 <- update(lmeBMI.111, random = ~ I(del^2) - 1)

lmeBMI.11 <- lme(fixed = x ~ del, data = da.grp, random = ~ del, na.action = na.omit, method = "REML")
lmeBMI.10 <- update(lmeBMI.11, random = ~ 1)
lmeBMI.01 <- update(lmeBMI.11, random = ~ del - 1)

lmeBMI.1 <- lme(fixed = x ~ 1, data = da.grp, random = ~ 1, na.action = na.omit, method = "REML")

lmeBMI.1111 <- lme(fixed = x ~ del + I(del^2) + I(del^3), data = da.grp, random = ~ del + I(del^2) +I(del^3), na.action = na.omit, method = "REML") #ne tourne pas
lmeBMI.1110 <- lme(fixed = x ~ del + I(del^2) + I(del^3), data = da.grp, random = ~ del + I(del^2), na.action = na.omit, method = "REML")
lmeBMI.1100 <- update(lmeBMI.1110, random = ~ del)
lmeBMI.1000 <- update(lmeBMI.1110, random = ~ 1)
lmeBMI.0110 <- update(lmeBMI.1110, random = ~  del + I(del^2) - 1)
lmeBMI.0100 <- update(lmeBMI.1110, random = ~  del - 1)
lmeBMI.0001 <- update(lmeBMI.1110, random = ~  I(del^3) - 1)
lmeBMI.1001 <- update(lmeBMI.1110, random = ~  I(del^3))
lmes<-list(lmeBMI.111=lmeBMI.111, lmeBMI.011=lmeBMI.011, lmeBMI.010=lmeBMI.010, lmeBMI.100=lmeBMI.100, lmeBMI.110=lmeBMI.110,
           lmeBMI.101 = lmeBMI.101, lmeBMI.001 = lmeBMI.001,
           lmeBMI.11 = lmeBMI.11, lmeBMI.10 = lmeBMI.10, lmeBMI.01 = lmeBMI.01, 
           lmeBMI.1 = lmeBMI.1,
           lmeBMI.1110 = lmeBMI.1110, lmeBMI.1100 = lmeBMI.1100, lmeBMI.1000 = lmeBMI.1000, lmeBMI.0110 = lmeBMI.0110, 
           lmeBMI.0100 = lmeBMI.0100, lmeBMI.0001 = lmeBMI.0001, lmeBMI.1001 = lmeBMI.1001)
           
AIC.lme <- sapply(lmes, AIC)
AIC.lme <- data.frame(AIC.lme)
AIC.lme$variable <-  rownames(AIC.lme)
AIC.lme[order(AIC.lme$AIC.lme),]

#representation graphique
br<-sapply(lmes, function(l) l$coef$ran)
bf<-sapply(lmes, function(l) l$coef$fix)
nr<-lapply(br, function(x) dim(x)[2]);nr
bf2<-mapply(function(x,y) x[1:y], bf, nr);bf2
lv<-sapply(lmes, logLik);lv

pat<-unique(as.character(da.grp$PATIENT))
ti<-da.grp$time.vni[match(pat, da$PATIENT)]/30.4375
head(ti)
head(da.grp)

x<-seq(0, 100, l=201)
im<-1;bf2i<-bf2[[im]];bf2i;nri<-nr[[im]];nri;bri<-br[[im]];head(bri)
names(lmes)[im]

y<-array(0, dim=c(length(x), 1+dim(bri)[1]))
for (i in 1:dim(bri)[1]) {
  vide<-x>ti[i]
  if (i==1) {
    bhat<-bf2i
    for (j in 1:nri) y[,i]<-y[,i]+bhat[j]*(x^(j-1))
  }
  bhat<-bf2i+bri[i,]
  for (j in 1:nri) y[,i+1]<-y[,i+1]+bhat[j]*(x^(j-1))
  y[vide,i+1]<-NA
}

ylim<-range(y, na.rm=T);ylim
xlim<-range(x)
coul<-rep("pink", dim(y)[2]);coul[1]<-"red"
lty<-rep(2, dim(y)[2]);lty[1]<-1
lwd<-rep(1, dim(y)[2]);lwd[1]<-2
matplot(x, y, col=coul, lty=lty, lwd=lwd, type="l")
lines(x, y[, 1], col=coul[1], lwd=lwd[1])
#ok jusqu'à 40 mois puis plus assez de donnees pour avoir des predictions fiables


#selection des effets fixes

#pas besoihyhn car on fait un modele sans covariable.
lmeBMI.111 <- lme(fixed = x ~ del + I(del^2), data = da.grp, random = ~ del + I(del^2), na.action = na.omit, method = "REML")

py2<-poly(da.grp$del, df=2)
py3<-poly(da.grp$del, df=3)
py4<-poly(da.grp$del, df=4)
lmeBMIp2<- lme(fixed = x ~ py2, data = da.grp, random = ~ py2, na.action = na.omit, method = "REML")
lmeBMIp3<- lme(fixed = x ~ py3, data = da.grp, random = ~ py2, na.action = na.omit, method = "REML")
lmeBMIp4<- lme(fixed = x ~ py4, data = da.grp, random = ~ py2, na.action = na.omit, method = "REML")

library(splines)
s2<-ns(da.grp$del, df=2)
s3<-ns(da.grp$del, df=3)
s4<-ns(da.grp$del, df=4)
lmeBMIs2<- lme(fixed = x ~ s2, data = da.grp, random = ~ s2, na.action = na.omit, method = "REML")
#lmeBMIs3<- lme(fixed = x ~ s3, data = da.grp, random = ~ py2, na.action = na.omit, method = "REML")


fixef(lmeBMI.111)
fixef(lmeBMIp2)
fixef(lmeBMIp3)
AIC(lmeBMI.111)
AIC(lmeBMIs2)
AIC(lmeBMIp2)
AIC(lmeBMIp3)
AIC(lmeBMIp4)


resid(lmeBMI.111)
summary(lmeBMI.111)


#selection de lq mqtrice des residus ?

#diqgnostic
#hypothese de la constance de la variance poour les residus. presence d'outlyers
mod<-lmeBMIs2
y0<-resid(mod, type="p", asList=T)

y<-resid(mod, type="p", asList=FALSE)
x<-fitted(mod)
bmi<-mod$data$x

summary(lm(y~x))
lim<-qnorm(1-0.025)
plot(x, y, col="red")
abline(h=c(-1,1)*lim)

plot(bmi, x, col="red")
m2<-lm(bmi~offset(x)-1);m2
summary(m1<-lm(bmi~x))
anova(m2, m1, test="Chisq")

#mod<-lmeBMI.111
plot(mod,
     resid(., type="p") ~ fitted(.),
     id = 0.05,
     abline = 0)
#la variance des residus est-elle constante au cours du temps.presence d'outlyer
plot(mod,
     resid(.)~ del,
     abline = 0)

#normalite des residus
qqnorm(mod,
       ~resid(.),
       id = 0.05)

#diqgnostic des random effects
qqnorm(mod,
       ~ranef(.),
       id = 0.10)

#il faut regarder les outlier pour verifier s'il n'y a pas une valeur aberrante
da.grp %>% filter(PATIENT == "ID8867")
pairs(lmeBMI.111, ~ ranef(.), id = ~ PATIENT == "ID8867")
#valeur observee vs valeur predite
plot(lmeBMI.111, x ~ fitted(.), id = 0.05)
#good agreement except some outlyers

#je compare les resultats sans ID8867
lmeBMI.111.out <- lme(fixed = x ~ del + I(del^2), data = da.grp[da.grp$PATIENT != "ID8867",], random = ~ del + I(del^2), na.action = na.omit, method = "REML")
summary(lmeBMI.111)
summary(lmeBMI.111.out)
#pas de grands changements

#=====================================
#modele avec covariables

#-----------------------------
#selection des effets aleatoires
na.omit(da %>% select(PATIENT, evt:APPAREILLE_PP)) %>% count(PATIENT)


varbl <- paste(varbl.vec, collapse = " + ")
lmeBMI <- lme(fixed = as.formula(paste0("x ~ del + I(del^2) +", varbl)) , data = da.grp, random = ~ del + I(del^2), na.action = na.omit, method = "REML")
lmeBMI0 <- lme(fixed = as.formula(paste0("x ~ del + I(del^2)")) , data = da.grp, random = ~ del + I(del^2), na.action = na.omit, method = "REML")
lmeBMI.2 <- update(lmeBMI, random = ~ del + I(del^2) - 1)#pas de random intercept
lmeBMI.2a <- update(lmeBMI, random = ~ del - 1)#pas de random del^2
lmeBMI.2b <- update(lmeBMI, random = ~ 1)#
lmeBMI.2c <- update(lmeBMI, random = ~ 1+del)#
lmeBMI.2d <- update(lmeBMI, random = ~ I(del^2))
lmeBMI.2e <- update(lmeBMI, random = ~ I(del^2)-1)


lmeBMIb <- lme(fixed = as.formula(paste0("x ~ del +", varbl)) , data = da.grp, random = ~ del, na.action = na.omit, method = "REML")
lmeBMIb.2 <- update(lmeBMIb, random = ~ del - 1)#pas de random intercept
lmeBMIb.2b <- update(lmeBMIb, random = ~ 1)

lmeBMIc <- lme(fixed = as.formula(paste0("x ~ +", varbl)) , data = da.grp, random = ~ 1, na.action = na.omit, method = "REML")

lmeBMI3 <- lme(fixed = as.formula(paste0("x ~ del + I(del^2) + I(del^3) +", varbl)) , data = da.grp, random = ~ del + I(del^2) + I(del^3), na.action = na.omit, method = "REML")
lmeBMI3.1000 <- update(lmeBMI3, random = ~ 1)
lmeBMI3.0100 <- update(lmeBMI3, random = ~ del-1)
lmeBMI3.0010 <- update(lmeBMI3, random = ~ I(del^2)-1)
lmeBMI3.0001 <- update(lmeBMI3, random = ~ I(del^3)-1)
lmeBMI3.1100 <- update(lmeBMI3, random = ~ del)
lmeBMI3.1010 <- update(lmeBMI3, random = ~ I(del^2))
lmeBMI3.1001 <- update(lmeBMI3, random = ~ I(del^3))

lmes<-list(d2alea.111=lmeBMI, d2alea.011=lmeBMI.2, d2alea.010=lmeBMI.2a, d2alea.100=lmeBMI.2b, d2alea.110=lmeBMI.2c, d2alea.101=lmeBMI.2d, d2alea.001=lmeBMI.2e,
           d1alea.11=lmeBMIb, d1alea.10=lmeBMIb.2b, d1alea.01=lmeBMIb.2, d0alea.1=lmeBMIc,
           d3alea.1111=lmeBMI3, d3alea.1000=lmeBMI3.1000, d3alea.0100=lmeBMI3.0100, d3alea.0010=lmeBMI3.0010, d3alea.0001=lmeBMI3.0001,
           d3alea.1100=lmeBMI3.1100, d3alea.1010=lmeBMI3.1010, d3alea.1001=lmeBMI3.1001)
sapply(lmes, AIC)


br<-sapply(lmes, function(l) l$coef$ran)
bf<-sapply(lmes, function(l) l$coef$fix)

nr<-lapply(br, function(x) dim(x)[2]);nr

bf2<-mapply(function(x,y) x[1:y], bf, nr);bf2

lv<-sapply(lmes, logLik);lv

pat<-unique(as.character(da.grp$PATIENT))
ti<-da.grp$time.vni[match(pat, da$PATIENT)]/30.4375
head(ti)
head(da.grp)

x<-seq(0, 100, l=201)
im<-1;bf2i<-bf2[[im]];bf2i;nri<-nr[[im]];nri;bri<-br[[im]];head(bri)
names(lmes)[im]

y<-array(0, dim=c(length(x), 1+dim(bri)[1]))
for (i in 1:dim(bri)[1]) {
  vide<-x>ti[i]
  if (i==1) {
    bhat<-bf2i
    for (j in 1:nri) y[,i]<-y[,i]+bhat[j]*(x^(j-1))
  }
  bhat<-bf2i+bri[i,]
  for (j in 1:nri) y[,i+1]<-y[,i+1]+bhat[j]*(x^(j-1))
  y[vide,i+1]<-NA
}

ylim<-range(y, na.rm=T);ylim
xlim<-range(x)
coul<-rep("pink", dim(y)[2]);coul[1]<-"red"
lty<-rep(2, dim(y)[2]);lty[1]<-1
lwd<-rep(1, dim(y)[2]);lwd[1]<-2
matplot(x, y, col=coul, lty=lty, lwd=lwd, type="l")
lines(x, y[, 1], col=coul[1], lwd=lwd[1])


#aléatoire
lv
unr<-unlist(nr);unr
M<-length(lv)
z<-expand.grid(list(im1=1:M, im2=1:M))
z<-z[z$im1<z$im2,]
z$m1<-names(lv)[z$im1]
z$m2<-names(lv)[z$im2]
z$lv1<-lv[z$im1]
z$lv2<-lv[z$im2]
z$n1<-unr[z$im1]
z$n2<-unr[z$im2]
z$rdv<-abs(2*(z$lv1-z$lv2))
z$ddl<-abs(z$n1-z$n2)
z$p<-(1-pchisq(z$rdv, z$ddl))/2+(1-pchisq(z$rdv, z$ddl+1))/2
z

#-----------------------------
#selection des effets fixes

lmes2<-lapply(lmes, update, method="ML")

library(MASS)
lmeBMI.ml <- update(lmeBMI, method = "ML")
mystep <- stepAIC(lmeBMI.ml, direction = "backward", scope=list(lower=~ del + I(del^2)))

lmeBMI.3 <- update(lmeBMI.ml, fixed = ~ del + I(del^2) + E_PHAR_LAR + TOUX_EFFICACE + CVF_ERS +
                     agevni + SEX + DYSP_DECUBI + FERM_BOUCHE)
resid(lmeBMI.3)
#par defaut pas de corrélation intra groupe des résidus. lire p.135. residual dans random effect donne l'es résidus qui restent.'erreur résiduel qui est normalement nulle. Si non nul les effets aléatoire n'ont pas capturé toute la corrélation entre les données. 
plot(predict(lmeBMI.3), resid(lmeBMI.3)) 
resid(lmeBMI.3)[resid(lmeBMI.3)>3] #faire les résidus et prédits en fonction du temps et ploter#marginal car n'inclut pas les effets aléatoires.
predict(lmeBMI.3)[resid(lmeBMI.3)>3]
tab=da.grp[da.grp$PATIENT=="ID8867", c("PATIENT", "x", "del")]

lmeBMI.4 <- update(lmeBMI.ml, fixed = ~ del + I(del^2) + E_PHAR_LAR + TOUX_EFFICACE + CVF_ERS +
                     agevni + SEX + DYSP_DECUBI + FERM_BOUCHE)
getVarCov(lmeBMI.3, type = "marginal")

lmeBMI.marg <- lme(fixed = x ~ del + I(del^2) , data = da.grp, random = ~ del + I(del^2), na.action = na.omit, method = "REML")
#Pas de variables de groupe de traitement donc on est sûr que la variance est la même pour tout l'échantillon.

