################################
#    SUIVI et SURVIE GLOBALE   #
################################

#chargement 
source("src/libraries_SLA.R")
source("src/02-fonctions_SLA.R")
#bl <- readRDS("data/bl.rds")
bl <- readRDS("data/bl_b.rds")

#=====================
#from NIV beginning

s<-bl[!is.na(bl$time.vni),]

#median follow-up (year):
sym.suiv <- survfit(Surv(time.vni,1-evt)~1,data=s)
an <- paste0 (round(summary(sym.suiv)$table['median']/365.25,2)," [", round(summary(sym.suiv)$table['0.95LCL']/365.25,2),"-",round(summary(sym.suiv)$table['0.95UCL']/365.25,2),"]")
an

#median follow-up (months):

sym.suiv2 <- survfit(Surv((time.vni*12/365.25),1-evt)~1,data=s)
m <- paste0 (round(summary(sym.suiv2)$table['median'],2)," [", round(summary(sym.suiv2)$table['0.95LCL'],2),"-",round(summary(sym.suiv2)$table['0.95UCL'],2),"]")
m

# Global survival
#en annee
vni.surv <- survfit(Surv(time.vni,evt)~1, data=s, conf.int=.95)
#en mois
vni.surv2 <- survfit(Surv(time.vni*12/365.25, evt)~1, data=s, conf.int=.95)
# vni.surv2 <- survfit(Surv(time.vni*12/365.25,censor)~1, data=s, conf.int=.95)
# ggsurv(vni.surv2) +#https://www.r-bloggers.com/survival-plots-have-never-been-so-informative/
#   #scale_x_continuous(breaks=seq(0,max(s$time.vni),1), labels=0:(length(seq(0,max(s$time.vni),1))-1)) +
#   scale_y_continuous(labels=percent) +
#   labs(x="Time of follow-up, months") 

#en annees
ggsurv(vni.surv, cens.col = "black") +#https://www.r-bloggers.com/survival-plots-have-never-been-so-informative/
  #scale_x_continuous(breaks=seq(0,max(s$time.vni),365.25), labels=0:(length(seq(0,max(s$time.vni),365.25))-1)) +
  scale_x_continuous(breaks=seq(0,max(s$time.vni),12/365.25), labels=0:(length(seq(0,max(s$time.vni),12/365.25))-1)) +
  scale_y_continuous(labels=percent) +
  #labs(x="Time of follow-up, years") +
  labs(x = "Durée de suivi, an", y = "Survie, %") +
  theme(axis.title = element_text(size= 18), axis.text = element_text(size=14))#, 
#en mois
ggsurv(vni.surv2, cens.col = "black") +#https://www.r-bloggers.com/survival-plots-have-never-been-so-informative/
  #scale_x_continuous(breaks=seq(0,max(s$time.vni),365.25), labels=0:(length(seq(0,max(s$time.vni),365.25))-1)) +
  scale_x_continuous(breaks=seq(0,max(s$time.vni*12/365.25),12), labels=seq(0,max(s$time.vni*12/365.25),12)) +
  scale_y_continuous(labels=percent) +
  #labs(x="Time of follow-up, years") +
  labs(x = "Durée de suivi, mois", y = "Survie, %") +
  theme(axis.title = element_text(size= 16), axis.text = element_text(size=14))+
  theme(axis.title.x = element_text(margin = margin(t=0.5, unit = "cm"))) #+ #top, right, bottom, left
  #theme(panel.background = element_rect(fill = "aliceblue"))
# panel.grid.major = element_line(colour = "grey"), 
        # panel.background = element_rect(colour = "grey",fill = "white"))

#survie à 1 an
sv <- summary(vni.surv, time=(365))
df <- data.frame(time = sv$time/365 , survival = sv$surv*100, LCI = sv$lower*100, UCI = sv$upper*100)
df[,2:4] <- round(df[,2:4], 0)
cat(paste0(df$time, " year(s) after beginning of NIV, survival[IC95%] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))


# median survival (year):
an <- paste0 (round(summary(vni.surv)$table['median']/365.25,2)," [", round(summary(vni.surv)$table['0.95LCL']/365.25,2),"-",round(summary(vni.surv)$table['0.95UCL']/365.25,2),"]")
an


# median survival (month):
m <- paste0 (round(summary(vni.surv2)$table['median'],2)," [", round(summary(vni.surv2)$table['0.95LCL'],2),"-",round(summary(vni.surv2)$table['0.95UCL'],2),"]")
m




#=====================
#from first symptoms

# Global survival 
s <- bl
s$time.sym <- as.numeric(s$datevni - s$FIRSTSYMPTOM)
s <- s[!is.na(s$time.sym),]
sym.surv <- survfit(Surv(time.sym, evt)~1, data=s, conf.int=.95)

# sym.surv2 <- survfit(Surv(time.sym*12/365.25,censor)~1, data=s, conf.int=.95)
# ggsurv(sym.surv2) +#https://www.r-bloggers.com/survival-plots-have-never-been-so-informative/
#   #scale_x_continuous(breaks=seq(0,max(s$time.sym),1), labels=0:(length(seq(0,max(s$time.sym),1))-1)) +
#   scale_y_continuous(labels=percent) +
#   labs(x="Time of follow-up, months") 

ggsurv(sym.surv) +#https://www.r-bloggers.com/survival-plots-have-never-been-so-informative/
  scale_x_continuous(breaks=seq(0,max(s$time.sym),365.25), labels=0:(length(seq(0,max(s$time.sym),365.25))-1)) +
  scale_y_continuous(labels=percent) +
  labs(x="Time of follow-up, years") +
  theme(axis.title = element_text(size= 18))

#survie à 1 an
s$time.sym.an <- s$time.sym/365
sym.surv.an <- survfit(Surv(time.sym.an, evt)~1, data=s, conf.int=.95)
#sv <- summary(sym.surv, time=365*2)
sv <- summary(sym.surv.an, time=1)
#df <- data.frame(time = sv$time/365 , survival = sv$surv*100, LCI = sv$lower*100, UCI = sv$upper*100)
df <- data.frame(time = sv$time , survival = sv$surv*100, LCI = sv$lower*100, UCI = sv$upper*100)
df[,2:4] <- round(df[,2:4], 0)
cat(paste0(df$time, " years after first symptoms, survival[IC95%] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))


#median survival (year):
an <- paste0 (round(summary(sym.surv)$table['median']/365.25,2)," [", round(summary(sym.surv)$table['0.95LCL']/365.25,2),"-",round(summary(sym.surv)$table['0.95UCL']/365.25,2),"]")
an

#median survival (month):
sym.surv2 <- survfit(Surv(time.sym*12/365.25, evt)~1, data=s, conf.int=.95)
m <- paste0 (round(summary(sym.surv2)$table['median'],2)," [", round(summary(sym.surv2)$table['0.95LCL'],2),"-",round(summary(sym.surv2)$table['0.95UCL'],2),"]")
m
