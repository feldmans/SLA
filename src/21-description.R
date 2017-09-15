###################################################
#     DESCRIPTION DE LA POPULATION VNI ET SLA     #             
###################################################

#chargement 
source("src/libraries_SLA.R")
source("src/02-fonctions_SLA.R")


#baseline
bl <- readRDS("data/bl_b.rds")
bdd_dates <- readRDS("data/bdd_dates.rds")
bl <- bl[ ,c(names(bl)[!names(bl) %in% names(bdd_dates)], "PATIENT")] 
names_nobl <- c(names(bl)[grep("ALS_", names(bl))], names(bl)[grep("E_BULBAIRE_", names(bl))], 
                names(bl)[grep("TEST_MUSCUL_", names(bl))])
bl <- bl[ ,!names(bl) %in% names_nobl]
#bl[ , c("DOB", "FIRSTSYMPTOM", "LIEUDEB", "DELAI", "ECHEC_MEO_VENT", "DELAI_VENT", "DUREE_MEP_VNI", "CAUCHEMAR")] <- NULL

#baseline a partir de dr
dr <- readRDS("data/df_rep.rds")

table(dr$qui)
#passer en wide
dr <- dr %>% filter(f==0) %>% 
  group_by(qui, PATIENT) %>%
  mutate(ind = row_number()) %>% select(PATIENT, ind, qui, x) %>% 
  spread(qui, x)# %>% 
  

myall <- full_join(bl, dr)


#analyses
myall$LIEUDEB_rec2
#il faudrait recalculer CHUTE CV
#A faire pour les 706 analyses et pour les 439 du modele multivarie vs ceux non selectionnes pour le multivarie
mydes <- myall[ ,c("PATIENT", "agevni", "SEX", "BMI", "BMI_c3", "TABAGISME_PP", "PAQUET_AN_PP", "SLAtillvni", "ALS", "LIEUDEB_rec2",
          "E_BULBAIRE", "CVF_OBS_THEO", "CVF_ERS", "CVF_ASSIS_ESR", "VEMS_CVF", "PIMAX_THEO", "SNIP_THEO",
          "DEBIT_POINTE", "PAO2_PP", "PACO2_PP", "PH_PP", "SPO2_TPS_PP", "CVF_THEO", "CVF_ASSIS_THEO")]


a <- lapply(names(mydes)[-1], des_all, mydes)#1 = PATIENT
a <- do.call(rbind, a)
a <- a[!rownames(a)%in% c("SEX_1", "TABAGISME_PP_0"), ]
#a <- a[a$`missing values`!="", ]
write.table(print(a), file="clipboard", sep="\t", row.names=T)

# describe_qualitative("DYSP_EFFORT" , bl)
# a <- lapply(names(bl)[-1], describe_all, bl)
# a <- do.call(rbind, a)
# #a <- a[a$`missing values`!="", ]
# a$range <- as.character(a$range)
# a$variable <- rownames(a)
# a <- a[ ,c("variable", "valeur", "missing values")]
# write.table(print(a), file="clipboard", sep="\t", row.names=F)

# a <- lapply(names(bl), des_all, bl)
# a <- do.call(rbind, a)
# a <- a[a$`missing values`!="", ]
# write.table(print(a), file="clipboard", sep="\t", row.names=T)

#range de date
n.ob <- bl$PATIENT
range(manage_date_ND(bdd9[bdd9$PATIENT %in% n.ob, "DATEXAM_V_M1"]), na.rm=T)
head(sort(manage_date_ND(bdd9$DATEXAM_V_M1)), 10)


#AGE_DEBUT = age de début des symptomes

#---------------------------------
#description individus dans modele multivarie versus ceux retires du modele multivarie

ac <- readRDS("data/ac_transf20170814.rds")

mm <- unique(ac$PATIENT)
mydes$mm <- ifelse(mydes$PATIENT %in% mm, 1, 0)

#patients inclus dans le modele multivarie
a <- lapply(names(mydes)[-1], des_all, mydes[mydes$mm == 1, ])#1 = PATIENT
a <- do.call(rbind, a)
a <- a[!rownames(a)%in% c("SEX_1", "TABAGISME_PP_0"), ]
#a <- a[a$`missing values`!="", ]
write.table(print(a), file="clipboard", sep="\t", row.names=T)

#patients exclus du modele multivarie
a <- lapply(names(mydes)[-1], des_all, mydes[mydes$mm == 0, ])#1 = PATIENT
a <- do.call(rbind, a)
a <- a[!rownames(a)%in% c("SEX_1", "TABAGISME_PP_0"), ]
#a <- a[a$`missing values`!="", ]
write.table(print(a), file="clipboard", sep="\t", row.names=T)

