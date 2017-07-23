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

