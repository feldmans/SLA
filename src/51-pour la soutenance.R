source("src/libraries_SLA.R")
#source("src/01-fonctions_SLA.R")
source("src/02-fonctions_SLA.R")

#=======================
#=======================
#variables baselines
bl <- readRDS("data/bl.rds")
bdd_dates <- readRDS("data/bdd_dates.rds")
d <- bl



var <- "DYSP_PAROLE"
#var <- "R_MUSCL_ACCES"
s <- d
s$a <- s[ ,var]
s <- s[!is.na(s$a),]
recode =FALSE
#s$a_recode <- ifelse (s$a < median(s$a), 0, 1)
s$a_recode <- s$a

#courbes pour la survie

#en mois
.title <- "Dyspnée à la parole"
.title <- "Recrutement des muscles accessoires"
vec_time_IC <- 12
s$tps <- (s$time.vni/(365.25/12)) + 0.001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner

km <- survfit(Surv(tps,evt)~a_recode, data=s, conf.int=.95)
km0 <- survfit(Surv(tps,evt)~a_recode, data=s[s$a_recode==0,], conf.int=.95)
km1 <- survfit(Surv(tps,evt)~a_recode, data=s[s$a_recode==1,], conf.int=.95)

#pour IC95%
skmi0<-summary(km0, time=vec_time_IC-0.1)
skmi1<-summary(km1, time=vec_time_IC+0.1) #plus d'évènement apres 1.94 ans

if(recode == TRUE) {
  group0 <- paste0("\nIn group ", var, " < ",round(median(s$a),0), "\n ")
  group1 <- paste0("\nIn group ", var, " >= ",round(median(s$a),0), "\n ")
} else {
  group0 <- paste0("\nIn group ", var, " = 0\n ")
  group1 <- paste0("\nIn group ", var, " = 1\n ")
}
#survies aux tps choisis
cat(group0)
sv <- summary(km0, time=vec_time_IC)
df <- data.frame(time = sv$time, survival = sv$surv*100, LCI = sv$lower*100, UCI = sv$upper*100)
df[,2:4] <- round(df[,2:4], 0)
#cat(paste0("At ", df$time, " year, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
cat(paste0("At ", df$time, " months, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))

cat(group1)
sv <- summary(km1, time=vec_time_IC)
df <- data.frame(time = sv$time, survival = sv$surv*100, LCI = sv$lower*100, UCI = sv$upper*100)
df[,2:4] <- round(df[,2:4], 0)
#cat(paste0("At ", df$time, " year, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
cat(paste0("At ", df$time, " months, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))


  #pour table de survie
  #skm0 <- summary(km0, time=seq(0, 10, by=1))
  skm0 <- summary(km0, time=seq(0,max(s$tps),12))# tps en mois
  skm0 <- data.frame(time=skm0$time, n.risk=skm0$n.risk)
  skm1<-summary(km1, time=seq(0, 10, by=1))
  skm1<-summary(km1, time=seq(0,max(s$tps),12))
  skm1 <- data.frame(time=skm1$time, n.risk=skm1$n.risk)
  
  #preparation legende
  #if (recode == TRUE) 
    leg <- c(paste0(var, " < ", round(median(s$a),0)), paste0(var, " >= ", round(median(s$a),0)))
  #else 
    leg<-str_sub(names(km$strata),-1,-1)
  col <- hue_pal()(length(leg))
  
  #courbe de survie
  g <- ggsurv(km, CI=FALSE, order.legend=FALSE, surv.col=col, cens.col=col) +
    #changement des axes
    #scale_x_continuous(breaks=seq(0,max(s$tps),1), labels=0:(length(seq(0,max(s$tps),1))-1)) +#annee
    scale_x_continuous(breaks=seq(0,max(s$tps),12), labels=seq(0,max(s$tps),12)) +#mois
    scale_y_continuous(labels=percent) +
    #labs(x="Time of follow-up, year", title=.title) +
    #labs(x="Durée de suivi, année", y = "Survie, %", title=.title) +
    labs(x="Durée de suivi, mois", y = "Survie, %", title=.title) +
    #labs(x="Time of follow-up, months", title=.title) +
    #changement legende
    guides (linetype = FALSE) +
    scale_colour_discrete( labels = leg) +
    theme(legend.position="right", legend.title=element_blank(), axis.title = element_text(size=14)) +
    #espace autour du schéma
    theme(plot.margin = unit(c(1,1,3,2), "cm"), plot.title = element_text(size = 24, hjust = 0.5))#,
         # plot.title = element_text(hjust = 0.5), plot.title = element_text(lineheight = .1)) #top, right, bottom, left
  #theme(plot.margin = unit(c(1,1,3,2), "cm")) #top, right, bottom, left
  
  #intervalle de confiance
  for (i in 1:2) {
    g <- g + geom_segment(x = skmi0$time[i], y = skmi0$lower[i], xend = skmi0$time[i], yend = skmi0$upper[i], colour = col[1])
    g <- g + geom_segment(x = skmi0$time[i] - 0.1, y = skmi0$lower[i], xend = skmi0$time[i] + 0.1, yend = skmi0$lower[i], colour = col[1])
    g <- g + geom_segment(x = skmi0$time[i] - 0.1, y = skmi0$upper[i], xend = skmi0$time[i] + 0.1, yend = skmi0$upper[i], colour = col[1])
    
    g <- g + geom_segment(x = skmi1$time[i], y = skmi1$lower[i], xend = skmi1$time[i], yend = skmi1$upper[i], colour = col[2])
    g <- g + geom_segment(x = skmi1$time[i] - 0.1, y = skmi1$lower[i], xend = skmi1$time[i] + 0.1, yend = skmi1$lower[i], colour = col[2])
    g <- g + geom_segment(x = skmi1$time[i] - 0.1, y = skmi1$upper[i], xend = skmi1$time[i] + 0.1, yend = skmi1$upper[i], colour = col[2])
  }
  #risk table
  for (ii in 1:nrow(skm0)) {
    g <- g + annotation_custom(grob = textGrob(skm0$n.risk[ii]), xmin = skm0$time[ii], xmax = skm0$time[ii], ymin= - 1.5 )
  }
  for (ii in 1:nrow(skm1)) {
    g <- g + annotation_custom(grob = textGrob(skm1$n.risk[ii]), xmin = skm1$time[ii], xmax = skm1$time[ii], ymin= - 1.7 )
  }
  #display group text
  myx <- -10
  g <- g + annotation_custom(grob = textGrob(leg[1]), xmin = myx, xmax = myx, ymin= - 1.5 )
  g <- g + annotation_custom(grob = textGrob(leg[2]), xmin = myx, xmax = myx, ymin= - 1.7 )
  
  
      mod <- coxph(Surv(tps, evt) ~ a_recode, data = s)
  
    
    test <- summary (mod)
    pval <- round(test$sctest["pvalue"],3)#idem que logrank 
    pval <- ifelse(pval<0.05, paste0(pval, " *"), pval)
    pval <- ifelse(pval<0.01, "Test du logrank \n p<0.01 *",paste0("Test du logrank\n p=", pval))
    
    g <- g + annotate("text",
                      x=0.75*max(km$time),
                      y=0.75*max(km$surv),
                      label=pval)

  gt <- ggplotGrob(g)
  gt$layout$clip[gt$layout$name=="panel"] <- "off"
  grid.draw(gt)
  
  
  #Residus de schoenfeld
  
  check_RP(var=var, data=d, .time="time.vni", .evt="evt", recode = FALSE, resid = TRUE)
  
  #transformation du temps
  ti <- sort(unique(c(0,s$tps[s$evt==1])))
  slat <- s
  slat <- slat %>% select(PATIENT, tps, evt, DYSP_PAROLE) %>% filter(PATIENT %in% c("ID12", "ID2919", "ID8710"))
  slat <- slat %>% select(PATIENT, tps, evt, DYSP_PAROLE)
  slat$start <- 0
  slat$stop <- slat$tps
  slat$etat <- slat$evt
  slat$a_recode <-  slat$DYSP_PAROLE
  # slat$evt <- slat$evt
  slat <- survSplit(Surv(stop,etat)~.,slat,start="start",cut=ti)
  slat <- survSplit(Surv(tps,evt)~.,slat,cut=ti)
  survSplit(slat, end="stop", event="evt", start="start",cut=ti)
  
  add_vart_and_check(var, d, "time.vni", "evt", recode=FALSE, .transf="*t^3", vec_cut = NULL, ylim = NULL, resid = TRUE)
  
  
  #decoupe du temps
  transf5 <- c(6, 30)
  vec_cut <- c(6, 30)
  add_vart_and_check(data=d, .time="time.vni", .evt="evt", recode = FALSE, var="DYSP_PAROLE", vec_cut = transf5)
  HR_score(var, data=s, .time="time.vni", .evt="evt", quali = FALSE, recode=FALSE, .transf="6-30", .tps_clinique = 12)
  