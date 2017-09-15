###########################
#    SURVIE MULTIVARIEE   #
###########################


source("src/libraries_SLA.R")
#source("src/01-fonctions_SLA.R")
source("src/02-fonctions_SLA.R")

#====================================================
#====================================================


#=======================
#chargement bases
#=======================

#-----------------------
#variables baselines
bl <- readRDS("data/bl.rds")
bl$TOUX_EFFICACE[is.na(bl$TOUX_EFFICACE)] <- 2
bdd_dates <- readRDS("data/bdd_dates.rds")

#-----------------------
#variables dont la valeur dépendant du temps
d <- readRDS("data/df_rep.rds")
d <- d[d$PATIENT %in% bl$PATIENT, ]

#-----------------------
#tableau pour le memoire
mem <- read.csv2("data/20170816 all univarie_tableau complet.csv",  stringsAsFactors = F)
mem$variable2 <- mem$variable
mem$variable <- str_extract(mem$variable, "[A-Za-z|_|0-9]*")

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

#je recree BMI_c3
d <- bind_rows(d %>% filter(qui=="BMI") %>% mutate(qui = "BMI_c3",
                                                   x = ifelse(x<18.5, 1,
                                                              ifelse(x>=18.5 & x<25, 2, 3))), 
               d %>% filter(qui != "BMI_c3"))



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

#my_col <- read.csv("data/names_var_sel20170808.csv",stringsAsFactors = FALSE)
#my_col <- read.csv2("data/names_var_sel20170809.csv",stringsAsFactors = FALSE)
my_col <- read.csv2("data/names_var_sel20170814.csv",stringsAsFactors = FALSE)
bl <- bl %>% select(PATIENT, one_of(my_col$variable)) 
d <- d %>% filter(qui %in% my_col$variable) 

varbl.vec <- bl %>% select(one_of(my_col$variable)) %>% colnames()
vardr.vec <- d %>% count(qui) %>% .$qui #j'ai deliberemment supprimer BMI au profit de BMI_c3 qui est plsu adapte au type de variable

varbl.vec <- varbl.vec[varbl.vec!="SLAtillvni_sup25mo"]
vardr.vec <- vardr.vec[vardr.vec!="BMI"]
#======================
#tableau pour cox
#======================
da2_init <- d %>% mutate(del = del/30.4375, time.vni = time.vni/30.4375)
da2_init <- da2_init %>% group_by(PATIENT, qui, date) %>% mutate(NB = n(),nr = row_number()) %>% filter(nr==1) #je supprime les doublons

#-------------------
# ti
#-------------------
#ce ti ne marche pas pour les mois, ok pour les jours uniquement!
# ti<-0:max(d$time.vni)
#ti qui marche pour les jours et les mois
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

#y <- get_split(da2_init, vardr.vec[1])
y <- get_split(da2_init, names(table(da2_init$qui))[1])

#for (.var in vardr.vec[-1]){
for (.var in names(table(da2_init$qui))[-1]){
  y2 <- get_split(da2_init, .var)
  print(head(y2))
  y <- full_join(y, y2 %>% select(PATIENT, start, stop, del, etat, evt, time.vni, one_of(.var)), by = c("PATIENT", "start", "stop", "etat", "evt", "time.vni"))
  print(head(y))
  rm(y2)
}
#merge dr splitte et bl
all.cox <- left_join(y, bl) %>% arrange(PATIENT, start)
all.cox <- y %>% arrange(PATIENT, start)

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
  filter(is.na(start) & rn != max(rn)) %>% #aucune ligne ayant start na qui ne soit pas la derniere ligne
  #filter(is.na(start) & rn == max(rn)) %>% 
  select(PATIENT,start, stop,del, etat, evt, rn , maxrn) 
#non : 5697 5797 5899 # maj ok : c'est parce que je n'avais pas trié par start

#ok je peux supprimer les start NA
all.cox <- all.cox %>% filter(!is.na(start))

# #OK fait a letape data management(et survie univariee refaite)
# #Je supprime les patients qui n'ont pas de consultation baseline pour ces variables selectionnees (c'est a dire delai minimal supereiru a 0) # 747 - 706 = on perd 41 patients
# all.cox <- all.cox %>% group_by(PATIENT) %>% mutate(mindel = min(del)) %>% filter(mindel==0)
# #"ID4770" "ID6311" "ID7144" "ID8658" n'avaient pas ete supprimes en faisant cette etape a 00-data #ok pb regle(venait des bl imputees)
# length(unique(y$PATIENT))
# #da2_init %>% filter(PATIENT == "ID4770") %>% View

# saveRDS(all.cox, "data/all.cox_NOtimeTransf20170726.rds") # version 468 patients
#saveRDS(all.cox, "data/all.cox_NOtimeTransf20170727.rds")
#saveRDS(all.cox, "data/all.cox_NOtimeTransf20170808.rds")
#saveRDS(all.cox, "data/all.cox_NOtimeTransf20170809.rds")
saveRDS(all.cox, "data/all.cox_NOtimeTransf20170814.rds")
#saveRDS(all.cox, "data/all.cox_allvarrep20170817.rds")

#==========================================================
#selection des variables et des patients de all.cox (tableau pour l'analyse multivariee)

#all.cox <- readRDS("data/all.cox_NOtimeTransf20170727.rds")
#all.cox <- readRDS("data/all.cox_NOtimeTransf20170808.rds")
#all.cox <- readRDS("data/all.cox_NOtimeTransf20170809.rds")
all.cox <- readRDS("data/all.cox_NOtimeTransf20170814.rds")

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

#Donc il suffit de garder del==0 pour chaque patient, et compter combien j'ai de NA par colonne
#1 seul start = 0 par patient, et j'ai bien 706 start = 0 (NB on a retire les PATIENTS sans consultation bl)
all.cox %>% filter(start == 0) %>% count(PATIENT) %>% filter(n>1) #aucun>1
all.cox %>% filter(start == 0) %>% group_by(PATIENT) %>% mutate(n = n()) %>% filter(n>1) %>%  select(PATIENT, start, stop, del, evt) #patient a la fois evt = 1 et evt = 0...
#pour chaque variable, nombre de NA
all.cox %>% filter(start == 0) %>% summarise_all(is.na) %>% select(-PATIENT) %>% summarise_all(sum) %>% t
all.cox %>% filter(start == 0) %>% select(TRONC) %>% summarise_all(is.na)  %>% summarise_all(sum) %>% t
#nombre de patient avec une baseline (et donc pas de NA), pour chaque variable
all.cox %>% filter(start == 0) %>% summarise_all(isnotNA) %>% select(-PATIENT) %>% summarise_all(sum) %>% t


#=> il faut maintenant pour chaque variable, la liste des patients sans baseline, qui sont à supprimer 
#(au lieu de faire un na omit qui supprimera les baseline NA, mais pas les valeurs renseignées suivantes)

#Si je garde la variable ALS, ce sont les patients a retirer


PatToDelete <- function(data2, var){
  print(var)
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

# #for all variables(not only the one selected)
# # .l <-lapply(names(table(da2_init$qui)), function(x) PatToDelete(all.cox, x))
# Npat <- data.frame(variable = names(table(da2_init$qui)), N2del = sapply(.l, length), stringsAsFactors = F)
# Npat$Nbl <- nrow(bl)-Npat$N2del
# Npat$percbl <- round(Npat$Nbl/nrow(bl),2)
# mybl <- left_join(mem[, c("variable", "variable2")], Npat)
# write.csv2(mybl, file = "variables_memoires_bl.csv")


#Je ne garde que les variables longitudinales avec moins de 30% de donnees manquantes a baseline
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

all.cox %>% select(-one_of(noselect)) %>% na.omit %>% count(PATIENT) 
all.cox %>% select(-one_of(noselect), -CVF_ERS) %>% na.omit %>% count(PATIENT) #en retirant cvf_ERS on gagnerait 22 patients mais information importante
all.cox_noNA <- all.cox %>% select(-one_of(noselect)) %>% na.omit 
#restent 439 patients
saveRDS(all.cox_noNA, "data/all.cox_noNA.rds")


#==========================================================
# Tranformation des variables
#all.cox <- readRDS("data/all.cox_NOtimeTransf20170727.rds")
#all.cox <- readRDS("data/all.cox_NOtimeTransf20170808.rds")
all.cox <- readRDS("data/all.cox_NOtimeTransf20170809.rds")
all.cox <- readRDS("data/all.cox_NOtimeTransf20170814.rds")
noselect <- readRDS("data/analyses/noselect.rds")
ac <- readRDS("data/all.cox_noNA.rds")
ac <- ac %>% ungroup
#transf <- read.csv2("data/variables_and_transformations20170808.csv", stringsAsFactors = FALSE)
transf <- read.csv2("data/names_var_sel20170809.csv", stringsAsFactors = FALSE)
transf <- read.csv2("data/names_var_sel20170814.csv", stringsAsFactors = FALSE)
variables.vec <- all.cox %>% select(-one_of(noselect)) %>% na.omit %>% colnames 
#transf <- transf %>% filter(variable %in% variables.vec | grepl("LIEUDEB", variable)) #?
transf <- transf %>% filter(variable %in% variables.vec)


#binarisation des variables qualitatives
transf %>% group_by(variable) %>% mutate(rn = row_number()) %>% filter(rn>1)
#LIEUDEB : separation en binaire et tranformation du temps#non plus de transformation du temps
# ac <- ac %>% mutate(cerv = ifelse(LIEUDEB_recode=="cervical",1,0),
#                     llimb = ifelse(LIEUDEB_recode=="lower limb", 1, 0),
#                     resp = ifelse(LIEUDEB_recode=="respiratory", 1, 0),
#                     ulimb = ifelse(LIEUDEB_recode=="upper limb", 1, 0)) #%>% 
ac <- ac %>% mutate(cerv_ulimb = ifelse(LIEUDEB_rec2=="cerv_ulimb",1,0),
                    llimb = ifelse(LIEUDEB_rec2=="llimb", 1, 0),
                    resp = ifelse(LIEUDEB_rec2=="resp", 1, 0)) #%>% 

#mutate(resp_t = resp / stop)

#TOUX EFFIACE : faire comprendre que c'est quali
ac <- ac %>% mutate(TOUX_EFFICACE_1 = ifelse(TOUX_EFFICACE == 1, 1, 0),
                    TOUX_EFFICACE_NA = ifelse(TOUX_EFFICACE == 2, 1, 0))  

#BMI_c3 : faire comprendre que c'est quali
ac <- ac %>% mutate(BMI_inf18.5 = ifelse(BMI_c3 == 1, 1, 0),
                    BMI_sup25 = ifelse(BMI_c3 == 3, 1, 0))  



#LOG LINEARITE
transf %>% filter(recode == TRUE)
# med_CVF_ERS <- ac %>% filter(!is.na(CVF_ERS)) %>% group_by(PATIENT) %>% filter(row_number()==1) %>% ungroup %>% summarise(median(CVF_ERS)) %>% as.numeric
# med_SLAtillvni <- ac %>% filter(!is.na(SLAtillvni)) %>% group_by(PATIENT) %>% filter(row_number()==1) %>% ungroup %>% summarise(median(SLAtillvni)) %>% as.numeric
# med_agevni <- ac %>% filter(!is.na(agevni)) %>% group_by(PATIENT) %>% filter(row_number()==1) %>% ungroup %>% summarise(median(agevni)) %>% as.numeric
med_ALS <- ac %>% filter(!is.na(ALS)) %>% group_by(PATIENT) %>% filter(start==0) %>% ungroup %>% summarise(median(ALS)) %>% as.numeric
#BMI finalement decoupe en classe
#med_BMI <- ac %>% filter(!is.na(BMI)) %>% group_by(PATIENT) %>% filter(row_number()==1) %>% ungroup %>% summarise(median(BMI)) %>% as.numeric

ac <- ac %>% mutate(
  # CVF_ERS_LL = ifelse(CVF_ERS<=med_CVF_ERS, 0, 1), 
  # SLAtillvni_LL = ifelse(SLAtillvni<=med_SLAtillvni, 0, 1),
  # agevni_LL = ifelse(agevni<=med_agevni, 0, 1),
  ALS_LL = ifelse(ALS <= med_ALS, 0, 1)#,
  #BMI_LL = ifelse(BMI<= med_BMI, 0, 1)
)


#RISQUES PROPORTIONNELS
transf %>% filter(RP == FALSE & recode == TRUE)



#transformation du temps
transf %>% filter(RP == FALSE) %>% filter(!grepl(";", transf))


ac <- ac %>% mutate(ENC_BRONCHIQ_t = ENC_BRONCHIQ * log(stop),
                    sla_familiale_t = sla_familiale * (stop^2),
                    BPCO_PP_t = BPCO_PP * (stop^3),
                    RESP_PARADOX_t = RESP_PARADOX * 1/(stop),
                    FERM_BOUCHE_t = FERM_BOUCHE * log(stop))

#coupure du temps
transf %>% filter(RP == FALSE) %>% filter(grepl(";", transf))
ac <- ac %>%
  mutate(SAS_PREEXIST_PP_t1 = SAS_PREEXIST_PP * ifelse(stop <= 18, 1, 0),
         SAS_PREEXIST_PP_t2 = SAS_PREEXIST_PP * ifelse(stop > 18, 1, 0)) %>%
  
  mutate(DYSP_PAROX_t1 = DYSP_PAROX * ifelse(stop <= 4, 1, 0),
         DYSP_PAROX_t2 = DYSP_PAROX * ifelse(stop > 4, 1, 0)) %>%
  
  mutate(cerv_ulimb_t1 = cerv_ulimb * ifelse(stop <= 4, 1, 0),
         cerv_ulimb_t2 = cerv_ulimb * ifelse(stop > 4 & stop <= 24, 1, 0),
         cerv_ulimb_t3 = cerv_ulimb * ifelse(stop > 24 & stop <= 36, 1, 0),
         cerv_ulimb_t4 = cerv_ulimb * ifelse(stop > 36, 1, 0),
         
         llimb_t1 = llimb * ifelse(stop <= 4, 1, 0),
         llimb_t2 = llimb * ifelse(stop > 4 & stop <= 24, 1, 0),
         llimb_t3 = llimb * ifelse(stop > 24 & stop <= 36, 1, 0),
         llimb_t4 = llimb * ifelse(stop > 36, 1, 0),
         
         resp_t1 = resp * ifelse(stop <= 6, 1, 0), #JE change 4 en 6 car ne converge pas dans modele final
         resp_t2 = resp * ifelse(stop > 6 & stop <= 18, 1, 0),#JE change 24 en 18 car moche dans modele final
         resp_t3 = resp * ifelse(stop > 18 & stop <= 36, 1, 0),
         resp_t4 = resp * ifelse(stop > 36, 1, 0)) %>% 
  
  mutate(REVEIL_ETOUF_t1 = REVEIL_ETOUF * ifelse(stop <= 4, 1, 0),
         REVEIL_ETOUF_t2 = REVEIL_ETOUF * ifelse(stop > 4 & stop <= 14, 1, 0),
         REVEIL_ETOUF_t3 = REVEIL_ETOUF * ifelse(stop > 14 & stop <= 45, 1, 0),
         REVEIL_ETOUF_t4 = REVEIL_ETOUF * ifelse(stop > 45, 1, 0)) %>%
  
  mutate(TOUX_EFFICACE_1_t1 = TOUX_EFFICACE_1 * ifelse(stop <= 2, 1, 0),
         TOUX_EFFICACE_1_t2 = TOUX_EFFICACE_1 * ifelse(stop > 2 & stop <= 18, 1, 0),
         TOUX_EFFICACE_1_t3 = TOUX_EFFICACE_1 * ifelse(stop > 18 & stop <= 36, 1, 0),
         TOUX_EFFICACE_1_t4 = TOUX_EFFICACE_1 * ifelse(stop > 36, 1, 0),
         
         TOUX_EFFICACE_NA_t1 = TOUX_EFFICACE_NA * ifelse(stop <= 2, 1, 0),
         TOUX_EFFICACE_NA_t2 = TOUX_EFFICACE_NA * ifelse(stop > 2 & stop <= 18, 1, 0),
         TOUX_EFFICACE_NA_t3 = TOUX_EFFICACE_NA * ifelse(stop > 18 & stop <= 36, 1, 0),
         TOUX_EFFICACE_NA_t4 = TOUX_EFFICACE_NA * ifelse(stop > 36, 1, 0)) %>%
  
  mutate(agevni_t1 = agevni * ifelse(stop <= 18, 1, 0),
         agevni_t2 = agevni * ifelse(stop > 18 & stop <= 36, 1, 0),
         agevni_t3 = agevni * ifelse(stop > 36, 1, 0)) %>%
  
  mutate(R_MUSCL_ACCES_t1 = R_MUSCL_ACCES * ifelse(stop <= 4, 1, 0),
         R_MUSCL_ACCES_t2 = R_MUSCL_ACCES * ifelse(stop > 4 & stop <= 16, 1, 0),
         R_MUSCL_ACCES_t3 = R_MUSCL_ACCES * ifelse(stop > 16 & stop <= 36, 1, 0),
         R_MUSCL_ACCES_t4 = R_MUSCL_ACCES * ifelse(stop > 36, 1, 0)) %>%
  
  mutate(SEX_t1 = SEX * ifelse(stop <= 3, 1, 0),
         SEX_t2 = SEX * ifelse(stop > 3 & stop <= 18, 1, 0),
         SEX_t3 = SEX * ifelse(stop > 18, 1, 0)) %>%
  
  mutate(DYSP_PAROLE_t1 = DYSP_PAROLE * ifelse(stop <= 6, 1, 0),
         DYSP_PAROLE_t2 = DYSP_PAROLE * ifelse(stop > 6 & stop <= 30, 1, 0),
         DYSP_PAROLE_t3 = DYSP_PAROLE * ifelse(stop > 30, 1, 0)) %>%
  
  mutate(REVEIL_MULTI_t1 = REVEIL_MULTI * ifelse(stop <= 3, 1, 0),
         REVEIL_MULTI_t2 = REVEIL_MULTI * ifelse(stop > 3 & stop <= 18, 1, 0),
         REVEIL_MULTI_t3 = REVEIL_MULTI * ifelse(stop > 18 & stop <= 32, 1, 0),
         REVEIL_MULTI_t4 = REVEIL_MULTI * ifelse(stop > 32, 1, 0)) %>%
  
  mutate(PACO2_PP_t1 = PACO2_PP * ifelse(stop <= 3, 1, 0),
         PACO2_PP_t2 = PACO2_PP * ifelse(stop > 3 & stop <= 22, 1, 0),
         PACO2_PP_t3 = PACO2_PP * ifelse(stop > 22, 1, 0)) %>%
  
  mutate(PACO2_PP_CL1_t1 = PACO2_PP_CL1 * ifelse(stop <= 6, 1, 0),
         PACO2_PP_CL1_t2 = PACO2_PP_CL1 * ifelse(stop > 6 & stop <= 22, 1, 0),
         PACO2_PP_CL1_t3 = PACO2_PP_CL1 * ifelse(stop > 22 & stop <= 36, 1, 0),
         PACO2_PP_CL1_t4 = PACO2_PP_CL1 * ifelse(stop > 36, 1, 0)) %>%
  
  mutate(BMI_inf18.5_t1 = BMI_inf18.5 * ifelse(stop <= 5, 1, 0),
         BMI_inf18.5_t2 = BMI_inf18.5 * ifelse(stop > 5 & stop <= 18, 1, 0),
         BMI_inf18.5_t3 = BMI_inf18.5 * ifelse(stop > 18 & stop <= 40, 1, 0),
         BMI_inf18.5_t4 = BMI_inf18.5 * ifelse(stop > 40, 1, 0),
         
         BMI_sup25_t1 = BMI_sup25 * ifelse(stop <= 5, 1, 0),
         BMI_sup25_t2 = BMI_sup25 * ifelse(stop > 5 & stop <= 18, 1, 0),
         BMI_sup25_t3 = BMI_sup25 * ifelse(stop > 18 & stop <= 40, 1, 0),
         BMI_sup25_t4 = BMI_sup25 * ifelse(stop > 40, 1, 0)) %>%
  
  mutate(ALS_LL_t1 = ALS_LL * ifelse(stop <= 4, 1, 0),
         ALS_LL_t2 = ALS_LL * ifelse(stop > 4 & stop <= 22, 1, 0),
         ALS_LL_t3 = ALS_LL * ifelse(stop > 22, 1, 0)) %>% 
  
  mutate(MORPHO_PP_CHOICE_1_t1 = MORPHO_PP_CHOICE_1 * ifelse(stop <= 3, 1, 0),
         MORPHO_PP_CHOICE_1_t2 = MORPHO_PP_CHOICE_1 * ifelse(stop > 3 & stop <= 22, 1, 0),
         MORPHO_PP_CHOICE_1_t3 = MORPHO_PP_CHOICE_1 * ifelse(stop > 22, 1, 0))


# #FAUS_ROUTE
# mutate(FAUS_ROUTE_t1 = FAUS_ROUTE * ifelse(stop <= 6, 1, 0),
#             FAUS_ROUTE_t2 = FAUS_ROUTE * ifelse(stop > 6 & stop <= 18, 1, 0),
#             FAUS_ROUTE_t3 = FAUS_ROUTE * ifelse(stop > 18 & stop <= 36, 1, 0),
#             FAUS_ROUTE_t4 = FAUS_ROUTE * ifelse(stop >36, 1, 0)) %>% 

saveRDS(ac, "data/ac_transf20170814.rds")
#==========================================================
# Construction du coxmultivarie
transf

a <- names(ac)[grep(unique(transf$variable)[12], names(ac))];a
a <- names(ac)[grep("BMI", names(ac))];a
cat(paste(shQuote(a, type="cmd"), collapse=", "))

my_var_transf <- list(c("SLAtillvni"), 
                      c("cerv_ulimb_t1", "cerv_ulimb_t2", "cerv_ulimb_t3", "cerv_ulimb_t4",
                        "llimb_t1", "llimb_t2", "llimb_t3", "llimb_t4",
                        "resp_t1", "resp_t2", "resp_t3", "resp_t4"),
                      c("FAUS_ROUTE"),
                      c("REVEIL_ETOUF_t1", "REVEIL_ETOUF_t2", "REVEIL_ETOUF_t3", "REVEIL_ETOUF_t4"),
                      c("TOUX_EFFICACE_1_t1", "TOUX_EFFICACE_1_t2", "TOUX_EFFICACE_1_t3", "TOUX_EFFICACE_1_t4",
                        "TOUX_EFFICACE_NA_t1", "TOUX_EFFICACE_NA_t2", "TOUX_EFFICACE_NA_t3", "TOUX_EFFICACE_NA_t4"),
                      c("ENC_BRONCHIQ", "ENC_BRONCHIQ_t"),
                      c("agevni_t1", "agevni_t2", "agevni_t3"),
                      c("CVF_ERS"), 
                      c("sla_familiale", "sla_familiale_t"),
                      c("BPCO_PP", "BPCO_PP_t"),
                      c("RESP_PARADOX", "RESP_PARADOX_t"),
                      c("R_MUSCL_ACCES_t1", "R_MUSCL_ACCES_t2", "R_MUSCL_ACCES_t3", "R_MUSCL_ACCES_t4"),
                      c("SEX_t1", "SEX_t2", "SEX_t3"),
                      c("DYSP_PAROLE_t1", "DYSP_PAROLE_t2", "DYSP_PAROLE_t3"),
                      c("SAS_PREEXIST_PP_t1", "SAS_PREEXIST_PP_t2"),
                      c("FERM_BOUCHE", "FERM_BOUCHE_t"),
                      c("DYSP_PAROX_t1", "DYSP_PAROX_t2"),
                      c("REVEIL_MULTI_t1", "REVEIL_MULTI_t2", "REVEIL_MULTI_t3", "REVEIL_MULTI_t4"),
                      c("PACO2_PP_t1", "PACO2_PP_t2", "PACO2_PP_t3"),
                      c("PACO2_PP_CL1_t1", "PACO2_PP_CL1_t2", "PACO2_PP_CL1_t3", "PACO2_PP_CL1_t4"),
                      c("BMI_inf18.5_t1", "BMI_inf18.5_t2", "BMI_inf18.5_t3", "BMI_inf18.5_t4",
                        "BMI_sup25_t1", "BMI_sup25_t2", "BMI_sup25_t3", "BMI_sup25_t4"),
                      c("ALS_LL_t1", "ALS_LL_t2", "ALS_LL_t3"),
                      c("MORPHO_PP_CHOICE_1_t1", "MORPHO_PP_CHOICE_1_t2", "MORPHO_PP_CHOICE_1_t3"))

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
  tmp <- data.frame(index = seq(tmp.var), name_first = unlist(lapply(tmp.var, '[[',1)), pvalrscore = p.tmp, stringsAsFactors = FALSE)
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
resbackward <- res
write.table(res, file = "clipboard", sep = "\t")

# #test du rapport de vraisemblance tres significatif avec le modele plein=> seleciton se justifie (Harrell p523)
# #backward meilleur que forward Harrell p70

#--------------------------------------------------------
#loop for forward selection

res <- data.frame()
res2 <- data.frame()
tmp.var.init <- my_var_transf
tmp.var <- my_var_transf
all.var <- paste(unlist(tmp.var.init), collapse = " + ")
all.var.old <- c(1)
cox_parc_new <- coxph(Surv(start, stop, etat)~ 1, data = ac)

for (j in seq(tmp.var.init)){
  #for (j in 1:2){
  print(j)
  p.tmp <- sapply(seq(tmp.var), function(i){
    print(i)
    tmp.var.new <- tmp.var[[i]]
    all.var.new <- paste(tmp.var.new, collapse = " + ")
    ddl <- length(tmp.var.new)
    #faut-il faire un init ou pas de sens? init pour avoir la pvalue de la variable, pas du modele
    #cox_parc <- coxph(as.formula(paste0("Surv(start, stop, etat)~",all.var.new, "+ cluster(PATIENT)")), data = ac)
    cox_parc <- cox_parc_new
    init <- c(rep(0,ddl), coef(cox_parc))
    cox_full <- coxph(as.formula(paste0("Surv(start, stop, etat)~", 
                                        paste(all.var.new, all.var.old, sep = " + "),
                                        "+ cluster(PATIENT)")
    ), data = ac, init = init) 
    rscore <- cox_full$rscore
    pval <- 1-pchisq(rscore, ddl)
    print(pval)
    return(pval)
  })
  #tmp <- data.frame(index = seq(tmp.var), name_first = unlist(lapply(tmp.var, '[[',1)), pvalrscore = p.tmp)
  tmp <- data.frame(index = seq(tmp.var), rank = j, name_var = unlist(lapply(tmp.var, paste, collapse = ";")), pvalrscore = p.tmp)
  tmp <- tmp %>% arrange(pvalrscore) 
  print(tmp)
  res <- bind_rows(res, tmp[1,])
  res2 <- bind_rows(res2, data.frame(NA), tmp)
  numtokeep <- tmp %>% filter(row_number() == 1) %>% .$index
  if(j==1) { 
    all.var.old <- paste(unlist(tmp.var[numtokeep]), collapse = "+")
  } else {
    all.var.old <- paste(all.var.old, paste(unlist(tmp.var[numtokeep]), collapse = " + "), sep= " + ")
  }
  tmp.var <- tmp.var[-numtokeep]
  cox_parc_new <- coxph(as.formula(paste0("Surv(start, stop, etat)~",all.var.old, "+ cluster(PATIENT)")), data = ac)
  
  if(min(tmp$pvalrscore)>0.05) {
    print("no more to add, all variable > 0,05")
    print(res)
    stop()
  }
}
res <- res[, c("rank", "name_var", "pvalrscore")]
resforward <- res


write.table(resforward, file = "clipboard", sep = "\t", row.names = FALSE)
write.table(res2, file = "clipboard", sep = "\t", row.names c= FALSE)

#============================
#Modele parcimonieux
#===========================

ac <- readRDS("data/ac_transf20170814.rds")

# all.var.sel <- resforward$name_var
# dput(unlist(strsplit(all.var.sel, ";")))
all.var.sel <- list("SLAtillvni", 
                 c("REVEIL_ETOUF_t1", "REVEIL_ETOUF_t2", "REVEIL_ETOUF_t3", "REVEIL_ETOUF_t4"), 
                 c("DYSP_PAROLE_t1", "DYSP_PAROLE_t2", "DYSP_PAROLE_t3"), 
                 c("ALS_LL_t1", "ALS_LL_t2", "ALS_LL_t3"), 
                 c("BMI_inf18.5_t1", "BMI_inf18.5_t2","BMI_inf18.5_t3", "BMI_inf18.5_t4", 
                 "BMI_sup25_t1", "BMI_sup25_t2", "BMI_sup25_t3", "BMI_sup25_t4"),
                 "FAUS_ROUTE", 
                 c("ENC_BRONCHIQ", "ENC_BRONCHIQ_t"), 
                 c("sla_familiale", "sla_familiale_t"),
                 c("cerv_ulimb_t1", "cerv_ulimb_t2", "cerv_ulimb_t3", "cerv_ulimb_t4", 
                 "llimb_t1", "llimb_t2", "llimb_t3", "llimb_t4", 
                 "resp_t1", "resp_t2", "resp_t3", "resp_t4"), 
                 c("PACO2_PP_t1","PACO2_PP_t2", "PACO2_PP_t3"), 
                 c("SEX_t1", "SEX_t2", "SEX_t3"),
                 c("agevni_t1", "agevni_t2", "agevni_t3"))
all.var.sel.vec <- paste(unlist(all.var.sel), collapse = " + ")
coxt <- coxph(as.formula(paste0("Surv(start, stop, etat)~", all.var.sel.vec," + cluster(PATIENT)")), data = ac)

#=============================
#verification
#residu de Schoenfeld pour les RP
zt <- cox.zph(coxt, transf="identity")
for (i in 1:(nrow(zt$table)-1)){
  iz<-i
  plot(zt[iz], resid = FALSE)
  abline(h=0, col="red")
  abline(h=coef(coxt)[iz], col="blue")
}
#residu de martingale pour loglinéarité


#et cox snell


# #test du rapport de vraisemblance tres significatif avec le modele plein=> seleciton se justifie (Harrell p523)
# #backward meilleur que forward Harrell p70


#=============================
#pvalue
p.tmp <- sapply(seq(all.var.sel), function(i){
  print(i)
  tmp.var.new <- all.var.sel[-i]
  all.var.new <- paste(unlist(tmp.var.new), collapse = " + ")
  delvar <- unlist(all.var.sel[i])
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
tmp <- data.frame(index = seq(all.var.sel), name_first = gsub("_t1", "", unlist(lapply(all.var.sel, '[[',1))), pvalrscore = round(p.tmp,4))
tmp <- tmp %>% arrange(pvalrscore) %>% mutate(numrow = row_number())
print(tmp) #NB : ce n'est pas la pval de lieudeb cervical mais de toutes les variables lieudeb  

#=============================
#HRIC, coef (et merge avec transf et pvalue)
all.var.sel
#---------------------
#---------------------
#HRIC des var inchangees
var <- unlist(all.var.sel[!grepl("_t", all.var.sel)])
test <- summary(coxt)
HRIC <- round(test$conf.int,3)
coef.all <- round(coef(coxt),3)
.l <- lapply(var, function(var){
  HRIC <- HRIC[grep(var, rownames(HRIC)),]
  HRIC <- paste0(HRIC[1], " [", HRIC[3], " - ", HRIC[4],"]")
})
HRIC  <-  do.call(rbind, .l)
HRIC <- data.frame(variable = var, HRIC = HRIC, stringsAsFactors = F)
HRIC$tps <- NA
HRIC$pvalue <- sapply(var, function(var)tmp[grep(var, tmp$name_first), "pvalrscore"])
HRIC$transf <- NA
HRIC$coef <- sapply(var, function(var)coef.all[grep(var, names(coef.all))])
HRIC2 <- HRIC

#---------------------
#---------------------
#HRIC des var dont l'effet depend du temps
unlist(all.var.sel[grepl("_t", all.var.sel)])
  
#---------------------
#transformation du temps
var <-all.var.sel[unlist(lapply(all.var.sel,function(x) any(grepl("_t$",x))))]
unlist(lapply(var, "[", 1))


all.var <- unlist(all.var.sel)
t <- 12
b<-coef(coxt)
vb<-coxt$var #idem que vcov(coxt)

#variable 
var <- "ENC_BRONCHIQ"
ind <- grep(var, all.var)
#version 1
ct<-c(1, log(t))#Attention : a modifier
mct<-(ct)%*%t(ct)
HRIC <- exp(qnorm(c(0.5, 0.025, 0.975), sum(b[ind]*ct), sqrt(sum(vb[ind,ind]*mct))))
#version 2 (idem que version 1) 
t_t <- log(t)
variance <- vb[ind[1],ind[1]]+vb[ind[2],ind[2]]*(t_t)^2+2*vb[ind[1],ind[2]]*(t_t)
m <- b[ind[1]]+b[ind[2]]*(t_t) #coef de l'HR
HRIC <- c(exp(m), exp(m + qnorm(0.975)*sqrt(variance) * c(-1,1)))
HRIC <- round(HRIC, 3)
HRIC <- paste0(HRIC[1], " [", HRIC[2], " - ", HRIC[3],"]")
HRIC <- data.frame(variable = var, HRIC = HRIC, stringsAsFactors = F)

HRIC$tps <- t
HRIC$pvalue <- tmp[grep(var, tmp$name_first), "pvalrscore"]
HRIC$transf <- transf[grep(var, transf$variable), "transf"]
HRIC$coef <- paste(coef.all[grep(var, names(coef.all))], collapse = ";")
HRIC2 <- rbind(HRIC2, HRIC)

#sla_fam
var <- "sla_familiale"
ind <- grep(var, all.var)
ct<-c(1, t^2)
mct<-(ct)%*%t(ct)
HRIC <- exp(qnorm(c(0.5, 0.025, 0.975), sum(b[ind]*ct), sqrt(sum(vb[ind,ind]*mct))))

HRIC <- round(HRIC, 3)
HRIC <- paste0(HRIC[1], " [", HRIC[2], " - ", HRIC[3],"]")
HRIC <- data.frame(variable = var, HRIC = HRIC, stringsAsFactors = F)

HRIC$tps <- t
HRIC$pvalue <- tmp[grep(var, tmp$name_first), "pvalrscore"]
HRIC$transf <- transf[grep(var, transf$variable), "transf"]
HRIC$coef <- paste(coef.all[grep(var, names(coef.all))], collapse = ";")
HRIC2 <- rbind(HRIC2, HRIC)

#---------------------
#decoupe du temps
var <- lapply(var, "[",1) %>% gsub("_t1", "",.) %>% gsub("_LL", "", .)


#variable Qti ou binaire
#all
#dput(var)
var <- c("REVEIL_ETOUF", "DYSP_PAROLE", "ALS", "PACO2_PP", "SEX", "agevni")
.l <- lapply(var, function(var){
  print(var)
  .var <- paste0(var, "$")#resolution pb : ds transf il ya aussi PP_CL1
  .transf <- transf[grep(.var, transf$variable), "transf"]
  coef.s <- coef(coxt)[grep(var, names(coef(coxt)))]
  vcov.s <- diag(vcov(coxt))[grep(var, names(coef(coxt)))]
  b <- as.numeric(unlist(strsplit(.transf, ";")))
  i <- findInterval(t, b) + 1 #findInterval commence à 0...
  HRIC <- round(exp(cbind(coef.s[i], qnorm(0.025, coef.s[i], sqrt(vcov.s[i])), qnorm(1-0.025, coef(coxt)[i], sqrt(vcov.s[i])))),3)
  HRIC <- paste0(HRIC[1], " [", HRIC[2], " - ", HRIC[3],"]")
  pvalue <- tmp[grep(var, tmp$name_first), "pvalrscore"]
  mycoef <- paste(round(coef.s, 3), collapse = ";")
  mydf <- data.frame(variable = var, HRIC = HRIC, tps = t, pvalue = pvalue, transf = .transf, coef = mycoef, stringsAsFactors = F)
  return(mydf)
})
HRIC <- do.call(rbind, .l)
HRIC2 <- rbind(HRIC2, HRIC) 


#variable quali

#BMI
mylev <- unlist(all.var.sel[grep("BMI", all.var.sel)]) %>% gsub("_t.*", "",.) %>% unique
var <- "BMI"
coef.s <- coef(coxt)[grep(var, names(coef(coxt)))]
vcov.s <- diag(vcov(coxt))[grep(var, names(coef(coxt)))]
.transf <- unique(transf[grep(var, transf$variable), "transf"])
b <- as.numeric(unlist(strsplit(.transf, ";")))
i <- findInterval(t, b) + 1 #findInterval commence à 0...
pvalue <- tmp[grep(var, tmp$name_first), "pvalrscore"]

mydf <- data.frame(variable = var, HRIC = HRIC, tps = t, pvalue = pvalue, transf = .transf, coef = mycoef, stringsAsFactors = F)


HRIC <- lapply(mylev, function(lev){
  coefbeta2 <- coef.s[grep(lev, names(coef.s))]
  vcov2 <- vcov.s[grep(lev, names(coef.s))]
  HRIC <- round(exp(cbind(coefbeta2[i], qnorm(0.025, coefbeta2[i],
                                              sqrt(vcov2[i])), qnorm(1-0.025, coefbeta2[i], sqrt(vcov2[i])))),3)
  HRIC <- paste0(HRIC[1], " [", HRIC[2], " - ", HRIC[3],"]")
  mycoef <- paste(round(coefbeta2, 3), collapse = ";")
  data.frame(variable = lev, HRIC = HRIC, tps = t, pvalue = pvalue, transf = .transf, coef = mycoef, stringsAsFactors = F)
})
HRIC <- do.call(rbind, HRIC)
HRIC2 <- rbind(HRIC2, HRIC) 

#LIEUDEB 
var <- "LIEUDEB"
pvalue <- tmp[grep("cerv_ulimb", tmp$name_first), "pvalrscore"]


.m <- mapply(function(mylev, .transf){
  coef.s <- coef(coxt)[unlist(lapply(mylev, grep, names(coef(coxt))))]
  vcov.s <- diag(vcov(coxt))[unlist(lapply(mylev, grep, names(coef(coxt))))]
  b <- as.numeric(unlist(strsplit(.transf, ";")))
  i <- findInterval(t, b) + 1 #findInterval commence à 0...
  HRIC <- round(exp(cbind(coef.s[i], qnorm(0.025, coef.s[i],
                                           sqrt(vcov.s[i])), qnorm(1-0.025, coef.s[i], sqrt(vcov.s[i])))),3)
  HRIC <- paste0(HRIC[1], " [", HRIC[2], " - ", HRIC[3],"]")
  mycoef <- paste(round(coef.s, 3), collapse = ";")
  HRIC <- data.frame(variable = mylev, HRIC = HRIC, tps = t, pvalue = pvalue, transf = .transf, coef = mycoef, stringsAsFactors = F)
}
  ,
  mylev = c("cerv_ulimb", "llimb", "resp"),
  .transf = c("4;24;36", "4;24;36", "6;18;36")
)
HRIC <- data.frame(t(matrix(unlist(.m), nrow = ncol(HRIC2))))
colnames(HRIC) <- colnames(HRIC2)
HRIC2 <- rbind(HRIC2, HRIC) 


HRIC2

write.table(HRIC2, file = "clipboard", sep = "\t", row.names = F)
#=======================
#=======================

#chargement bases
#=======================
#variables baselines
bl <- readRDS("data/bl.rds")
bdd_dates <- readRDS("data/bdd_dates.rds")

#=======================
#variables dont la valeur dépendant du temps
d <- readRDS("data/df_rep.rds")
d <- d[d$PATIENT %in% bl$PATIENT, ]

#=======================
#RESULTATS UNIVAR
df1 <- readRDS("data/analyses/HRIC_bin_bl.rds")
df2 <- readRDS("data/analyses/HRIC_quanti_bl.rds")
df3 <- readRDS("data/analyses/HRIC_var_rep.rds")
df4 <- readRDS("data/analyses/HRIC_rep_quali.rds")
df5 <- readRDS("data/analyses/HRIC_quali_bl.rds")
missbl.df <- readRDS("data/missingbl.rds")
missrep.df <- readRDS("data/missingrep.rds")


#=======================
#=======================
#sélection des variables
n <- 0.3*nrow(bl)

df1 <- merge(missbl.df, df1, by="variable")
vb1 <- as.character(unique(df1 %>% filter(pvalue<0.2 & missing <= n)  %>% select(variable))$variable)

df2 <- merge(missbl.df, df2, by="variable")
vb2 <- as.character(unique(df2 %>% filter(pvalue<0.2 & missing <= n)  %>% select(variable))$variable)

df5 <- merge(missbl.df, df5, by="variable")
vb3 <- as.character(unique(df5 %>% filter(pvalue<0.2 & missing <= n)  %>% select(variable))$variable)

df3 <- merge(missrep.df, df3, by="variable")
vr1 <- as.character(unique(df3 %>% filter(pvalue<0.2 & N_pat_missing <= n)  %>% select(variable))$variable)

df4 <- merge(missrep.df, df4, by="variable")
vr2 <- as.character(unique(df4 %>% filter(pvalue<0.2 & N_pat_missing <= n)  %>% select(variable))$variable)

c(vb1, vb2, vb3, vr1, vr2)

#------------------------
#nb de bl et suivi manquant pour les variables repetees


##################
#version Sarah
.l <- lapply(c(vr1, vr2), function(.var){
y<-d[d$qui==.var,]
ypos<-y[y$del>0, ]
y0<-y[y$del<=0, ]

npos0 <- full_join(
  y0 %>% group_by(PATIENT) %>% summarise(n0=n()),
  ypos %>% group_by(PATIENT) %>% summarise(npos=n())
) %>% 
  mutate(n0 = ifelse(is.na(n0), 0, n0),#n0 = pmin(n0,1), #inutile car n0 vaut deja 0 ou 1 car aucun patient ne peut aavoir plusieurs baseline 
         npos = ifelse(is.na(npos), 0, npos), 
         n = npos + n0)

tab <- table(bl=npos0$n0, nbval=pmin(npos0$n,2)) 
#bl: 0=pas de baseline, 1=il y a baseline. 
#nbval: 0= impossible(si ni baseline ni suivi, le patient n'est pas dans y) 1=1 seule cs (baseline ou suivi), 2= au moins 2 cs(baseline ou suivi)
#les patients qui nous intéressent sont ceux avec une baseline et une suivi au moins soit bl=1 et nbval=2
res <- sum(tab[1,]) #nb de patients sans baseline (mais avec un suivi)
res2 <- sum(tab)
res3 <- round(res/res2 *100,2) #% de patients sans baseline (denominateur = patients avec au moins une valeur)
res <- data.frame(res2, res, res3 = paste0(res3, "%"))
})
.l <- do.call(rbind,.l)
bl_suiv.df <- data.frame(variable = c(vr1, vr2), n_bl_suiv = .l$res2, no_bl = .l$res, perc_no_bl = .l$res3)
bl_suiv.df
write.table(print(bl_suiv.df), file = "clipboard", sep="\t", row.names = FALSE)

##################
#version Yann

.l <- lapply(c(vr1, vr2), function(.var){
  #var <- vr1[1]
  var <- .var
  y0 <- bl[,  c("PATIENT", vb1, vb2, vb3)]
  y<-d[d$qui==var,]
  y<-merge(y, y0, by="PATIENT", all.x=T, all.y=F)
  
  # y<-y[y$date<=y$dfin,] #c'est toujours le cas maintenant
  # summary(y$del)
  
  ypos<-y[y$del>0, ]
  y0<-y[y$del<=0, ]
  
  n0<-tapply(y0$PATIENT, y0$PATIENT, length)
  npos<-tapply(ypos$PATIENT, ypos$PATIENT, length)
  n0<-data.frame(PATIENT=names(n0), n0=as.numeric(n0), stringsAsFactors = F)
  npos<-data.frame(PATIENT=names(npos), npos=as.numeric(npos), stringsAsFactors = F)
  npos0<-merge(n0, npos, by="PATIENT", all=T)
  npos0$n0[is.na(npos0$n0)]<-0
  npos0$n0<-pmin(npos0$n0,1)
  npos0$npos[is.na(npos0$npos)]<-0
  npos0$n<-npos0$npos+npos0$n0
  
  summary(npos0)
  tab <- table(bl=npos0$n0, nbval=pmin(npos0$n,2)) 
  #bl: 0=pas de baseline, 1=il y a baseline. 
  #nbval: 0= impossible(si ni baseline ni suivi, le patient n'est pas dans y) 1=1 seule cs (baseline ou suivi), 2= au moins 2 cs(baseline ou suivi)
  #les patients qui nous intéressent sont ceux avec une baseline et une suivi au moins soit bl=1 et nbval=2
  res <- sum(tab[1,]) #nb de patients sans baseline (mais avec un suivi)
  res2 <- sum(tab)
  res3 <- round(res/res2 *100,2) #% de patients sans baseline (denominateur = patients avec au moins une valeur)
  res <- data.frame(res2, res, res3 = paste0(res3, "%"))
})
.l <- do.call(rbind,.l)
bl_suiv.df <- data.frame(variable = c(vr1, vr2), n_bl_suiv = .l$res2, no_bl = .l$res, perc_no_bl = .l$res3)
bl_suiv.df
write.table(print(bl_suiv.df), file = "clipboard", sep="\t", row.names = FALSE)

#---------------------
#recuperer les baseline

bdd7 %>% select(PATIENT, DATE_RESP_PP, starts_with("DATE_PREVENT")) %>% colnames
bdd7 %>% select(PATIENT,starts_with("ALS_TOT_RESP")) %>%colnames

#pour recuperer les dates preventil de pneumo 
my_date <- bdd7 %>% select(PATIENT, DATE_RESP_PP, starts_with("DATE_PREVENT")) %>%
  mutate_at(vars(DATE_RESP_PP, starts_with("DATE_PREVENT")), manage_date_ND) %>% 
  gather(key = var_date, value=date,DATE_RESP_PP, starts_with("DATE_PREVENT")) %>%
  extract(
    col = var_date,
    into = c("type_cs", "num_cs"),
    regex = ".{1,}_.{1,}_([A-Z]{2})_*F*([0-9]*)"
  ) %>%
  mutate(num_cs = ifelse (num_cs == "",0, num_cs))
#pour recuperer les dates de neuro
 date_bdd6 <- bdd6 %>% select(PATIENT, DATEXAM) %>% mutate(DATEXAM = manage_date_ND(DATEXAM))
 date_bdd9 <- bdd9 %>% select(PATIENT, starts_with("DATEXAM")) %>% # %>% mutate(DATEXAM = manage_date_ND(DATEXAM))
   mutate_at(vars(starts_with("DATEXAM")), manage_date_ND) %>% 
     gather(key = var_date, value=date, starts_with("DATEXAM")) %>% 
     mutate(num_cs=str_extract(var_date, "[0-9]*$"))
    
   

#ALS_TOT_RESP
full_join(
  my_date
  , 
  #pour les variables
  bdd7 %>% select(PATIENT,starts_with("ALS_TOT_RESP")) %>% 
    gather(key = qui, value = x, starts_with("ALS_TOT_RESP")) %>%
    extract(
      col = qui,
      into = c("qui", "type_cs", "num_cs"),
      regex = "(ALS_TOT_RESP)_([A-Z]{2})_*F*([0-9]*)"
    ) %>%
    mutate(num_cs = ifelse (num_cs=="",0, num_cs))
  ,
  by = c("PATIENT", "num_cs") #type_cs est problematique car pas meme nomenclature entre les dates et les variables
) %>% 
  #merge avec bdd_dates
  inner_join(bdd_dates) %>% 
  #ne garder que les baselines
  mutate(del = datevni - date,
         f = ifelse(del<=0, 0, 1)) %>% 
  filter(!is.na(x) & f==0)
#pas de bl a rajouter

#ALS_RESP
full_join(
  my_date
  , 
  #pour les variables
  bdd7 %>% select(PATIENT,starts_with("ALS_RESP")) %>% 
    gather(key = qui, value = x, starts_with("ALS_RESP")) %>% 
    extract(
      col = qui,
      into = c("qui", "type_cs", "num_cs"),
      regex = "(ALS_RESP)_([A-Z]{2})_*F*([0-9]*)"
    ) %>%
    mutate(num_cs = ifelse (num_cs=="",0, num_cs)) 
  ,
  by = c("PATIENT", "num_cs") #type_cs est problematique car pas meme nomenclature entre les dates et les variables
)%>%  
  #merge avec bdd_dates (doit etre present dans les deux)
  inner_join(bdd_dates) %>% 
  #ne garder que les baselines
  mutate(del = datevni - date,
         f = ifelse(del<=0, 0, 1)) %>% 
  filter(!is.na(x) & f==0)
  #pas de bl a rajouter


#BMI_PP et PV
#NB : il n'y a pas de BMI_SV(les BMI de suivi viennent du fichier neuro)

bm1 <- full_join(
  my_date
  , 
  #pour les variables
  bdd7 %>% select(PATIENT,starts_with("BMI")) %>% 
    mutate_at(vars(starts_with("BMI")), function(x) as.numeric(as.character(x))) %>% 
    gather(key = qui, value = x, starts_with("BMI")) %>% 
    extract(
      col = qui,
      into = c("qui", "type_cs", "num_cs"),
      regex = "(BMI)_([A-Z]{2})_*F*([0-9]*)"
    ) %>%
    mutate(num_cs = ifelse (num_cs=="", 0, num_cs)) #PP est num_cs = 0
  ,
  #type_cs est problematique pour merger car pas meme nomenclature entre les dates et les variables(l'un est PP l'qutre PV)
  #de toutes facon num_cs suffit car pas de SV
  by = c("PATIENT", "num_cs") 
)%>%  
  #merge avec bdd_dates (doit etre present dans les deux)
  inner_join(bdd_dates) %>% 
  #ne garder que les baselines
  mutate(del = date - datevni,
         f = ifelse(del<=0, 0, 1)) %>% 
  filter(!is.na(x) & f == 0 & x < 45)
#qq bl a rajouter


#pour les variables neuro bl
bm2 <- bdd6 %>% select(PATIENT, DATEXAM, BMI, BMI_CL1) %>% 
  mutate(DATEXAM = manage_date_ND(DATEXAM)) %>% 
    mutate_at(vars(starts_with("BMI")), function(x)as.numeric(as.character(x))) %>%
    gather(key = qui, value = x, starts_with("BMI")) %>% 
    mutate(date = DATEXAM, DATEXAM = NULL, CL1 = ifelse(qui == "BMI", 0, 1), qui = "BMI", num_cs = as.character(0)) %>% 
    inner_join(bdd_dates) %>% 
    #ne garder que les baselines
    mutate(del = date - datevni,
           f = ifelse(del<=0, 0, 1)) %>% 
    filter(!is.na(x) & f==0 & x < 45)


#pour les variables neuro repetees
bm3 <- full_join(
  date_bdd9
  , 
  bdd9 %>% select(PATIENT,starts_with("BMI")) %>%
  mutate_at(vars(starts_with("BMI")), function(x)as.numeric(as.character(x))) %>%
    gather(key = qui, value = x, starts_with("BMI")) %>% 
    extract(
      col = qui,
      into = c("qui", "num_cs", "CL1"),
      regex = "(BMI)_V_M([0-9]*)_*([CL1]*)"
    ) %>%  #%>% group_by(CL1) %>% summarise(n())
    mutate(CL1 = ifelse (CL1=="",0, 1))
  ,
  by = c("PATIENT","num_cs")
) %>% 
    inner_join(bdd_dates) %>% 
  #ne garder que les baselines
  mutate(del = date - datevni,
         f = ifelse(del<=0, 0, 1)) %>% 
  filter(!is.na(x) & f==0 & x < 45)
#qq bl a rajouter

#a la fin prendre la plus recente des dates bdd6 bdd9 et bdd7
bl_bmi <- bind_rows(bm1, bm2, bm3) %>% group_by(PATIENT) %>% arrange(desc(del)) %>% slice(1) %>% mutate(f=0)

#NB: on avait nettoyé dr : si consult apres date de deces ce n'est pas un deces et time.vni==date de derniere consult - datevni


saveRDS(bl_bmi, "data/bl_bmi.rds")


#----
#servira à la fin pour vérifier avec SEX qu'on a bien le bon nombre d'évènements
id<-unique(y$PATIENT)
yu<-y[match(id, y$PATIENT),]

c(length(unique(as.character(yu$PATIENT))),length(u<-unique(as.character(y$PATIENT))))
#----
y$delfin<-as.numeric(y$dfin-y$datevni)

y<-y[order(y$PATIENT, y$del),]
c(length(unique(as.character(yu$PATIENT))),length(u<-unique(as.character(y$PATIENT))))

z<-tapply(y$del, y$PATIENT, c)
zf<-tapply(y$time.vni, y$PATIENT, c)
zm<-mapply(function(x,y) c(x[-1], y[1]), z, zf)

ze<-tapply(y$evt, y$PATIENT, c)
fct<-function (x) {
  x[-length(x)]<-0
  return(x)
}
ze<-sapply(ze, fct)

id<-unique(y$PATIENT)
match("ID181",u)
i<-95
y[y$PATIENT==id[i], c("datevni", "del", "date", "time.vni", "dfin", "delfin", "evt")]
z[[i]];zf[[i]];zm[[i]];ze[[i]]

y$delapres<-unlist(zm)
y$evt2<-unlist(ze)
c(length(unique(as.character(yu$PATIENT))),length(u<-unique(as.character(y$PATIENT))))

y[y$PATIENT==id[i], c("datevni", "del", "date", "time.vni", "dfin", "delfin", "evt", "evt2","delapres")]

#y[y$PATIENT=="ID7275", c("del", "delapres", "time.vni")]

ti<-0:max(y$time.vni)

yt<-y
yt$start<-yt$del
yt$stop<-yt$delapres
yt$etat<-yt$evt2
yt<-survSplit(yt, end="stop", start="start", cut=ti, event="etat")
yt<-yt[order(yt$PATIENT, yt$start),]

c(length(unique(as.character(yu$PATIENT))),length(u<-unique(as.character(y$PATIENT))),
                                                  length(unique(as.character(yt$PATIENT))))
i<-match(u, yt$PATIENT, nomatch=0);summary(i)
y[y$PATIENT %in% u[i==0], c("PATIENT","date", "date_dc", "time.vni", "evt", "evt2", "del", "delapres")]
d[d$PATIENT %in% u[i==0] & d$qui=="ALS", c("PATIENT","datevni", "date", "date_dc", "time.vni", "evt", "del", "dfin")]

tab<-table(yt$PATIENT)
table(y$del==0 & y$del==y$delapres)

#des sujets meurent le jour de la visite
yt[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni, c("PATIENT","date", "date_dc", "time.vni", "evt", "evt2", "del", "delapres", "start", "stop", "etat")]
yt$etat[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni]<-1

d[d$PATIENT=="ID181" & d$qui=="ALS", c("PATIENT", "date", "qui", "del", "date_dc", "time.vni", "dfin")]

#on vérifie avec la variable non dépendante du temps qu'on a bien le bon nombre d'évènement
cox<-coxph(Surv(time.vni, evt)~SEX, data=yu)
coxt<-coxph(Surv(start, stop, etat)~SEX, data=yt)
cox
coxt
#meme nb d'evt mais pas meme coef 


#-----------------
#construire la base pour l'analyse multivariée et faire pas à pas 


ti<-0:max(d$time.vni)
length(ti)

var <- vr1[1]
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


head(y)
yt<-y
yt$start<-yt$del
yt$stop<-yt$delapres
yt$etat<-yt$evt2
yt<-survSplit(yt, end="stop", start="start", cut=ti, event="etat")
yt<-yt[order(yt$PATIENT, yt$start),]

#des sujets meurt le jour de la visite (?)
yt[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni,]
yt$etat[yt$evt==1 & yt$etat==0 & yt$stop==yt$time.vni]<-1

yt[ ,var] <- yt$x
Yt<-yt[, c("PATIENT", "start", "stop", "etat", var)]

#ajouter une variable répétée
var <- vr1[2]
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
Yt<-merge(Yt, yt[, c("PATIENT", "start", "cephal")], by=c("PATIENT", "start"), all=T)
dim(Yt)

head(Yt)

#mettre les variables initiales
y0<-bl[,  c("PATIENT", "SEX")]
y0 <- bl[,  c("PATIENT", vb1, vb2, vb3)]

dim(Yt)
Yt<-merge(Yt, y0, by="PATIENT", all.x=T, all.y=F)
dim(Yt)


yt<-na.omit(Yt)
coxt<-coxph(Surv(start, stop, etat)~SEX+dyspnee+cephal+cluster(PATIENT), data=yt)
summary(coxt)


coxt0<-coxph(Surv(start, stop, etat)~SEX+dyspnee+cluster(PATIENT), data=yt)
#cephal
nbpar<-1
init<-c(coef(coxt0), rep(0, nbpar))
coxt1<-coxph(Surv(start, stop, etat)~SEX+dyspnee+cephal+cluster(PATIENT), data=yt, init=init)
summary(coxt1)


coxt$rscore
sc<-coxt1$rscore
c(score=sc, p=1-pchisq(sc, nbpar))

dd<-d[d$qui %in% c(vr1, vr2), ]

yd<-dd
m<-tapply(yd$del, list(yd$PATIENT, as.character(yd$qui)), min, na.rm=T)
M<-tapply(yd$del, list(yd$PATIENT, as.character(yd$qui)), max, na.rm=T)
mp<-tapply(yd$del, list(yd$PATIENT, as.character(yd$qui)), function(x) min(x[x>0], na.rm=T))
head(mp)

#nb sujet avec au moins une valeur : par variable

n<-ifelse(is.na(m), 0, 1)
N<-dim(n)
nbv<-N[2];N<-N[1]

sn<-rowSums(n)
table(sn)
colSums(n)

#avoir une bl
nbl<-ifelse(!is.na(m) & m<=0, 1, 0)
snbl<-rowSums(nbl)
prop.table(table(c(0:nbv, snbl))-1)
colSums(nbl)/N

snbl<-rowSums(nbl[, -14])
prop.table(table(c(0:nbv, snbl))-1)

#avoir une rép
nr<-ifelse(!is.na(M) & M>0, 1, 0)
snr<-rowSums(nr)
table(snr)
colSums(nr)/N

#avoir une rép et bl
nblr<-ifelse(!is.na(m) & m<=0 & !is.na(M) & M>0, 1, 0)
snblr<-rowSums(nblr)
table(snblr)
colSums(nblr)/N

#sans bl avec r : del minimum
x<-ifelse(is.finite(mp), mp, 99999)
nsblr<-ifelse(!is.na(m) & m>0 & !is.na(M) & M>0, 1, 0)
x<-x*nsblr
x[x==0]<-99999
head(x)
snsblr<-rowSums(nsblr)
table(snsblr)
colSums(nblr)/N

