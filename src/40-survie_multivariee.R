###########################
#    SURVIE MULTIVARIEE   #
###########################


source("src/libraries_SLA.R")
#source("src/01-fonctions_SLA.R")
source("src/02-fonctions_SLA.R")

#=======================
#=======================

#chargement bases
#=======================
#variables baselines
bl <- readRDS("data/bl.rds")
bdd_dates <- readRDS("data/bdd_dates.rds")

#data management
bl$LIEUDEB_recode <- Recode(as.factor(bl$LIEUDEB), "1 = 'bulbar';2 = 'cervical'; 10:15 = 'lower limb'; 3 = 'respiratory'; 4:9 = 'upper limb'")
d <- bl #base avec les colonnes qui ne sont pas dans bdd_dates
match(c("LIEUDEB", "DOB", "FIRSTSYMPTOM"), names(bl))
dim(bl)
dim(d)
d[ ,c("LIEUDEB")] <- NULL
d[ ,c("DOB")] <- NULL
d[ ,c("FIRSTSYMPTOM")] <- NULL

#raison de la mise en place de la vni :
for (i in (1:20)){
  d[ ,paste0("crit", i)] <- ifelse((d$CRIT_1_VNI==i & !is.na(d$CRIT_1_VNI==i)) | (d$CRIT_2_VNI==i & !is.na(d$CRIT_2_VNI==i)) | (d$CRIT_3_VNI==i & !is.na(d$CRIT_3_VNI==i)), 1, 0)
}
d$CRIT_1_VNI <- NULL
d$CRIT_2_VNI <- NULL
d$CRIT_3_VNI <- NULL
d$TYPESOD1 <- NULL
d$COOPERATION <- NULL
d$VEMS_OBSV <- NULL
d$P_TRANS_02 <- NULL


#J'impute les NA en 0 pour les variables d'interrogatoire et de clinique qui s'y prete
#apply(d, 2, table, useNA="a")
imp_0_vec <- c("BPCO_PP", "ASTHME_PP", "SAS_PREEXIST_PP", "APPAREILLE_PP", "DYSP_EFFORT","DYSP_REPOS", "DYSP_PAROLE", "DYSP_DECUBI", "DYSP_PAROX",
               "FAUS_ROUTE", "REVEIL_MULTI", "REVEIL_ETOUF", "CAUCHEMAR", "R_MUSCL_ACCES", "RESP_PARADOX", "ENC_BRONCHIQ", "E_PHAR_LAR", "OXY_THERAP")
d[,imp_0_vec] <- apply(d[,imp_0_vec], 2, function(x) {
  x[is.na(x)] <- 0 
  return(x)})

#variable pour lesquelles NA est a imputer par 1
d[is.na(d$FERM_BOUCHE) ,c("FERM_BOUCHE")] <- 1

#modalites 2 (= non evaluables) :JG dit de mettre a 0 mais je pense qu'il vaut mieux mettre NA
d[,c("DYSP_EFFORT", "DYSP_PAROX")] <- apply(d[,c("DYSP_EFFORT", "DYSP_PAROX")], 2, function(x) {
  x[x==2] <- NA 
  return(x)})

#d$VNI_ON <- NULL
#d$ECHEC_MEO_VENT <- NULL

d [d$SEX == 1, "SEX"] <- 0
d [d$SEX == 2, "SEX"] <- 1
bl <- d

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
  
  npos0 <- full_join(
    y0 %>% group_by(PATIENT) %>% summarise(n0=n()),
    ypos %>% group_by(PATIENT) %>% summarise(npos=n())
  ) %>% 
    mutate(n0 = ifelse(is.na(n0), 0, n0),
           n0 = pmin(n0), 
           npos = ifelse(is.na(npos), 0, npos), 
           n = npos + n0)
  
  # n0<-tapply(y0$PATIENT, y0$PATIENT, length)
  # npos<-tapply(ypos$PATIENT, ypos$PATIENT, length)
  # n0<-data.frame(PATIENT=names(n0), n0=as.numeric(n0), stringsAsFactors = F)
  # npos<-data.frame(PATIENT=names(npos), npos=as.numeric(npos), stringsAsFactors = F)
  # npos0<-merge(n0, npos, by="PATIENT", all=T)
  # npos0$n0[is.na(npos0$n0)]<-0
  # npos0$n0<-pmin(npos0$n0,1)
  # npos0$npos[is.na(npos0$npos)]<-0
  # npos0$n<-npos0$npos+npos0$n0
  
  summary(npos0)
  tab <- table(bl=npos0$n0, nbval=pmin(npos0$n,2)) #bl: 0=pas de baseline, 1=il y a baseline. nbval: 1=1 seule cs (baseline ou suivi), 2= au moins 2 cs(baseline ou suivi)
  #les patients qui nous intéressent sont ceux avec une baseline et une suivi au moins soit bl=1 et nbval=2
  res <- sum(tab[1,]) #pas de baseline 
})
.l <- unlist(.l)
bl_suiv.df <- data.frame(variable = c(vr1, vr2), n_bl_suiv = .l)
bl_suiv.df


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

#pour recuperer les dates preventil de neuro 
my_date <- bdd7 %>% select(PATIENT, DATE_RESP_PP, starts_with("DATE_PREVENT")) %>%
  mutate_at(vars(DATE_RESP_PP, starts_with("DATE_PREVENT")), manage_date_ND) %>% 
  gather(key = var_date, value=date,DATE_RESP_PP, starts_with("DATE_PREVENT")) %>%
  extract(
    col = var_date,
    into = c("type_cs", "num_cs"),
    regex = ".{1,}_.{1,}_([A-Z]{2})_*F*([0-9]*)"
  ) %>%
  mutate(num_cs = ifelse (num_cs == "",0, num_cs))

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


#BMI_PP
full_join(
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
#qq bl a rajouter


#BMI_CL1 : vient de bdd6

#a la fin prendre la plus recente des 2 dates bdd6 et bdd7
full_join(
  
  , 
  #pour les variables
  bdd7 %>% select(PATIENT,starts_with("BMI_CL1")) %>% 
    mutate_at(vars(starts_with("BMI")), function(x) as.numeric(as.character(x))) %>% 
    gather(key = qui, value = x, starts_with("BMI")) %>% 
    extract(
      col = qui,
      into = c("qui", "type_cs", "num_cs"),
      regex = "(BMI)_([A-Z]{2})_*F*([0-9]*)"
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
#qq bl a rajouter


#merger avec dr


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

