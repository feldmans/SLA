library(dplyr)
library(stringr)
library(survival)
library(rms)

#sla <- readRDS("data/BDDSLA.rds")
#sla <- readRDS("data/BDDSLADEM.rds")
sla <- readRDS("data/BASE_SLA_allbl.rds")

table(is.na(sla$ddn))
table(!is.na(sla$date_dc))


sla$time.vni <- as.numeric(sla$ddn - sla$DATEVNI)
sla$time.sym <- as.numeric(sla$ddn - sla$FIRSTSYMPTOM)
sla$censor <- ifelse (!is.na(sla$date_dc),1, 0)
sla$sex <- factor(sla$sex_def, levels=c(1,2), labels=c("h","f") ) #1 = 'Masculin' 2 = 'Féminin' 
sla$sex <- sla$sex_def #1 = 'Masculin' 2 = 'Féminin' 
sla$agevni <- round(as.numeric(sla$DATEVNI-sla$DOB)/365.25,0)
sla$rilu <- ifelse(sla$DATEVNI>sla$DEBRILU & !is.na(sla$DEBRILU), 1,0)
sla$rilu <- ifelse(sla$DATEVNI<sla$FINRILU & !is.na(sla$FINRILU), 0,sla$rilu)
sla$familial <- ifelse (is.na(sla$familial),0,sla$familial)
#sla$ddn <- ifelse(sla$ddn>as_date("2015-08-27"), as_date("2015-08-27"), sla$ddn)

var_bl <- c("familial", "sex", "agevni", "LIEUDEB_recode", "rilu", "dysp","orthop","CVF_ASSIS_perc_pred","CVF_COUCHE_perc_pred","SNIP_cmH2O",
  "SNIP_perc_pred","PIMAX_cmH2O","PIMAX_perc_pred","perc_time_under_spo2_90","time_under_spo2_90_h","bicar","ALS_score","bulb_score")

str(sla[,var_bl])

var_quali <- c("familial", "sex", "LIEUDEB_recode", "rilu", "dysp", "orthop")
var_quanti <- var_bl [! var_bl %in% var_quali]

# sla1<- sla
# sla1[,var_quanti] <- apply(sla1[,var_quanti], 2, function(x) as.numeric(as.character(x)))#ok : idem que boucle
# sla2 <- sla
# sla2[,var_quanti] <- as.numeric(as.character(unlist(sla2[,var_quanti]))) #pas très précis : des différences de moyenne avec les 3 autres méthodes
# sla3 <- sla
# sla3[,var_quanti] <- apply(sla3[,var_quanti], 2, function(x) as.numeric(x)) # ok : idem que sla 1 et boucle : apply converti déjà automatiquement les facteurs en charactère 
# sla4 <- sla
# for (i in var_quanti){
#   sla4[,i] <- as.numeric(as.character(sla4[,i]))
# }
# choix : sla3
sla[,var_quanti] <- apply(sla[,var_quanti], 2, function(x) as.numeric(x))
sla[,var_quali[!var_quali%in% "LIEUDEB_recode"]] <- apply(sla[,var_quali[!var_quali%in% "LIEUDEB_recode"]], 2, function(x) as.numeric(x))


#----------------------------------------------
#VERSION : DATE DE DEBUT = DATE DE 1sym

#range date de début
range(sla$FIRSTSYMPTOM,na.rm=T)

#suivi
idc.suiv <- survfit(Surv(sla$time.sym,1-sla$censor)~1)
plot(idc.suiv,xscale=365.25, yscale= 100, xlab="time (years)")

#survie
idc.surv <- survfit(Surv(sla$time.sym,sla$censor)~1, conf.int=.95)
plot(idc.surv,xscale=365.25, yscale= 100, xlab="time (years)")
min(idc.surv$time[idc.surv$surv<=0.5])#mediane de survie

#survie selon baseline :

#verif des hypothèses:
  
  #risques proportionnels
#a <- apply(sla[,var_quanti],2,function(x)cox.zph(coxph(Surv(sla$time.sym,sla$censor)~x))$table[3])
a <- lapply(var_bl,function(x){
  a <- cox.zph(coxph(Surv(sla$time.sym,sla$censor)~sla[,x]))
  plot(a,main=x)
  a <- data.frame(a$table)
  a <- a[nrow(a),3]
  return(a)
})
risque.prop.all <- data.frame(var_bl,pval=unlist(a))

#1 var ne respecte pas risque prop : ALS_score : p=0.0474
a <- cox.zph(coxph(Surv(sla$time.sym,sla$censor)~sla$ALS_score))
plot(a)#pourtant beta(t) est horizontal
print(a)

  #Loglinearité (pour var quanti uniquement?) lieudeb quali non ordonné donc non binaire : forcément une droite
for (i in var_quanti){
  a <- coxph(Surv(sla$time.vni,sla$censor)~sla[,i])
  res <- residuals(a, type="martingale")
  X <- sla[,i]
  X <- X[!is.na(sla[,i])]
  par(mfrow=c(2,2))
  #plot(sla[!is.na(sla[,i]),i],res,xlab=i,ylab="residuals")
  plot(X,res,xlab=i,ylab="residuals")
  abline(h=0,lty=2)
  #lines(lowess(sla[!is.na(sla[,i]),i],res,iter=0))
  lines(lowess(X,res,iter=0))
  
  b <- coef(a)
  plot(X,b*X+res,xlab=i,ylab="component+residuals")
  abline(lm(b*X+res~X),lty=2)
  lines(lowess(X,b*X+res,iter=0))
}

#Test du score (log rank)

score <- lapply(var_bl,function(x){
  a <- coxph(Surv(sla$time.vni,sla$censor)~sla[,x])
  a<-summary(a)
  res <- round(as.numeric(a$sctest[3]),3)
})
#score <- unlist(score)
data.frame(var_bl,tes_du_score=unlist(score))


#courbes pour var quali

table(sla$SEX)
i <- "sex"
for (i in var_quali){
  var <- sla[,i]
  var <- as.factor(var)
  a <- survfit(Surv(sla$time.sym,sla$censor)~var, conf.int=.95)
  plot(a,col=c(2,4),xscale=365.25, yscale= 100, xlab="time (years)", main=i)
  legend (3000,1,legend=c(levels(var)),lty=c(1,1),col=c(2,4))
}


#-------------------------------------------
#date de debut = date de vni

#range date de début
range(sla$DATEVNI,na.rm=T)

#suivi
idc.suiv <- survfit(Surv(sla$time.vni,1-sla$censor)~1)
plot(idc.suiv,xscale=365.25, yscale= 100, xlab="time (years)")

#survie
idc.surv <- survfit(Surv(sla$time.vni,sla$censor)~1, conf.int=.95)
plot(idc.surv,xscale=365.25, yscale= 100, xlab="time (years)")
min(idc.surv$time[idc.surv$surv<=0.5])#mediane de survie

#survie selon baseline :

#verif des hypothèses:

#risques proportionnels
#a <- apply(sla[,var_quanti],2,function(x)cox.zph(coxph(Surv(sla$time.vni,sla$censor)~x))$table[3])
a <- lapply(var_bl,function(x){
  a <- cox.zph(coxph(Surv(sla$time.vni,sla$censor)~sla[,x]))
  plot(a,main=x)
  a <- data.frame(a$table)
  a <- a[nrow(a),3]
  return(a)
})
risque.prop.all <- data.frame(var_bl,pval=unlist(a))

#1 var ne respecte pas risque prop : ALS_score : p=0.0474
a <- cox.zph(coxph(Surv(sla$time.vni,sla$censor)~sla$ALS_score))
plot(a)#beta(t) augmente un peu avec le temps
print(a)

#Loglinearité (pour var quanti uniquement?) lieudeb quali non ordonné donc non binaire : forcément une droite
for (i in var_quanti){
  a <- coxph(Surv(sla$time.vni,sla$censor)~sla[,i])
  res <- residuals(a, type="martingale")
  X <- sla[,i]
  X <- X[!is.na(sla[,i])]
  par(mfrow=c(2,2))
  #plot(sla[!is.na(sla[,i]),i],res,xlab=i,ylab="residuals")
  plot(X,res,xlab=i,ylab="residuals")
  abline(h=0,lty=2)
  #lines(lowess(sla[!is.na(sla[,i]),i],res,iter=0))
  lines(lowess(X,res,iter=0))
  
  b <- coef(a)
  plot(X,b*X+res,xlab=i,ylab="component+residuals")
  abline(lm(b*X+res~X),lty=2)
  lines(lowess(X,b*X+res,iter=0))
}

#Test du score (log rank)

score <- lapply(var_bl,function(x){
  a <- coxph(Surv(sla$time.vni,sla$censor)~sla[,x])
  a<-summary(a)
  res <- round(as.numeric(a$sctest[3]),3)
})
#score <- unlist(score)
data.frame(var_bl,tes_du_score=unlist(score))


#courbes pour var quali

for (i in var_quali){
  var <- sla[,i]
  var <- as.factor(var)
  a <- survfit(Surv(sla$time.vni,sla$censor)~var, conf.int=.95)
  plot(a,col=c(2,4),xscale=365.25, yscale= 100, xlab="time (years)", main=i)
  legend (3000,1,legend=c(levels(var)),lty=c(1,1),col=c(2,4))
}





