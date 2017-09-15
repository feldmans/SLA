
#nombre total de patients dans la base
.l <- lapply(bdds, function(x) {
  tmp <- get (x)
  pat <- as.character(tmp[ ,"PATIENT"])
  return(pat)
})
length(unique(unlist(.l)))
length(unique(bdd6$PATIENT))
unique(bdd6$PATIENT)[!unique(bdd6$PATIENT) %in%unique(unlist(.l))]


#date de consult la plus ancienne 
all_d <- manage_date_ND(bdd6$DATEXAM)
head(sort(all_d[all_d>as_date("1900-01-01")]), 100)
#dates prospectives 
all_d <- manage_date_ND(bdd9$DATEXAM_V_M1)
head(sort(all_d[all_d>as_date("1900-01-01")]), 10)

#nombre de patients suivis prospectivement
DC_2.df <- read.csv2("data/date_evt_patientHorsSuiviNeuro.csv")
DC_2.df$date_evt[DC_2.df$date_evt==""] <- NA
DC_2.df <- DC_2.df[!is.na(DC_2.df$date_evt), ]
pat_prosp1 <- as.character(DC_2.df$PATIENT) 
pat_prosp2 <- unique(as.character(bdd9$PATIENT))
pat_prosp1[pat_prosp1 %in% pat_prosp2]
#table(pat_prosp1 %in% bdd7$PATIENT) #ces patients sont tous suivis en pneumo mais pas en neuro (or date de décès en neuro uniquement) 
#table(pat_prosp1 %in% bdd6$PATIENT) #ces patients ont tous eu une consultation neuro mais pas de suivi neuro (or date de décès en neuro uniquement) 
#mais on a récupérer leur ddn => on considère qu'ils sont bien prospectifs.
#=> ntotal 
length(unique(bdd7$PATIENT))
length(unique(bdd9$PATIENT))

#suivi neuro prospectif :
length(c(pat_prosp1, pat_prosp2))
length(c(bdd7[pat_prosp1, pat_prosp2))

table(pat_prosp1 %in% bdd7$PATIENT)

#exemple de RP
bl <- readRDS("data/bl.rds")
var = 'APPAREILLE_PP'
var = 'REVEIL_ETOUF'
var = 'TOUX_EFFICACE'
data = d
.time = 'time.vni'
.evt = 'evt'

recode = FALSE

s <- data
s$a <- s[ ,var]
s$evt <- s[ ,.evt]
#s$tps <- (s[ ,.time]/365.25) + 0.001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner
s$tps <- (s[ ,.time]/365.25*12) + 0.001

s <- s[!is.na(s$a),]
s$a_recode <- s$a
.title <- paste0("RP of ", var)
mod <- coxph(Surv(tps, evt) ~ a_recode, data = s)

#Test de Harrell
z <- cox.zph(mod, transform = "rank")
pval <- round(z$table[,3],3)
#non signif si p>=0.05

#résidus de Shoenfeld
z <- cox.zph(mod, transf="identity")
plot(z, main=paste0(.title, "\nHarrell test p = ",pval), resid = TRUE, ylab = paste0("Beta(t) for ", var),
     xlab = "Time, months",ylim = c(-3,3))
abline(h=0, col="red")
abline(h=coef(mod), col="blue")
