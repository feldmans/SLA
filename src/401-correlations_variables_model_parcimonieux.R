source("src/libraries_SLA.R")

ac <- readRDS("data/all.cox_noNA.rds")
#ac <- readRDS("data/ac_transf20170814.rds")
cor(ac$PACO2_PP, ac$PACO2_PP_CL1)

ac <- ac %>% group_by(PATIENT) %>% slice(1) %>% ungroup
ac <- data.frame(ac)
"MORPHO_PP_CHOICE_1", "PACO2_PP", "PACO2_PP_CL1", "ALS",  "BMI_c3"
"TOUX_EFFICACE", "LIEUDEB_rec2"

nosel <- c("CVF_ERS", "BPCO_PP", "RESP_PARADOX", "R_MUSCL_ACCES", 
           "SAS_PREEXIST_PP", "DYSP_PAROX", "REVEIL_MULTI",  "FERM_BOUCHE")
varQq <- c("SLAtillvni",  "FAUS_ROUTE", "REVEIL_ETOUF",  
           "ENC_BRONCHIQ", "agevni",  "sla_familiale",   
           "SEX", "DYSP_PAROLE")
varQ <- c("SLAtillvni","agevni")
varbin <- c("FAUS_ROUTE", "REVEIL_ETOUF",  
            "ENC_BRONCHIQ", "sla_familiale",   
            "SEX", "DYSP_PAROLE")

mat <- round(cor(ac[,varQq]),3)
dim(ac)
couples<-lapply(c(0.2,0.4),function(w){
  #pour supprimer les doublons
  mat2<- lower.tri(mat,diag=FALSE)
  rownames(mat2)<-rownames(mat)
  colnames(mat2) <- colnames(mat)
  mat2 <- ifelse(mat2==TRUE,mat,0)
  #pour chercher les coefficients de corrélation superieur à w
  w_r <- which(abs(mat2)>=w )
  #pour trouver les noms de ligne et colonne de ces coefficients
  which_couple <- lapply(w_r,function(x){
    k <- arrayInd(x, dim(mat2))
    d<-data.frame(var1=rownames(mat2)[k[,1]], var2=colnames(mat2)[k[,2]],r=mat2[x])
    return(d)
  })
  #Je colle les listes
  which_couple <- data.frame(do.call(rbind,which_couple))
  #Je nomme les 2 listes de niveau supérieur selon la valeur du coefficient
  return(which_couple)
})
couples_rename <- couples
colnames(couples_rename[[1]])<- c("variable 1","variable 2", "coefficient de corrélation") 
couples_rename[[1]]

#ok, aucune correlations entre variables du modele 

#chi2 pour TOUX_EFFICACE et LIEUDEB_rec2

varbase <- "TOUX_EFFICACE"
varbase <- "LIEUDEB_rec2"

dfall <- do.call(rbind, lapply(c(varbin, "TOUX_EFFICACE", "LIEUDEB_rec2"), function(varbase){
  dfa <- data.frame(do.call(rbind, lapply(varbin, function(var) {
    print(var)
    print(addmargins(table(ac[ ,varbase], ac[ ,var])))
    chi <- chisq.test(ac[ ,varbase], ac[ ,var])
    return(c(var, round(chi$p.value, 4)))
  })), stringsAsFactors = FALSE)
  dfa$var <- varbase
  return(dfa)
}))

dfall %>% filter(X2<0.05)



#pas de chi2 pour SLA familiale, condition non remplie. Mais aucune raison que ce soit correle (pas de logique)
#correlation de TOUX_EFFICACE avec FAUS_ROUTE et ENC_BRONCHIQ
#Je decide de supprimer toux efficace car FAUS_ROUTE et ENC_BRONCHIQUE moins subjectives
chisq.test(ac$FAUS_ROUTE, ac$ENC_BRONCHIQ)#non pas correle
#Sex et faus route sont liees a lieudeb mais je les garde car toutes importantes
#Je retire PACO2 sous ventilation et je garde celle sans ventilation
#Je rajoute agevni et sex


#entre quali et quanti
sapply(c("TOUX_EFFICACE","LIEUDEB_rec2"),function(.i){
  pl <- lapply (var[! var %in% "sexe"], function(.x) {
    ggplot(rt, aes(x = get(.i), y = get(.x))) +
      geom_boxplot(fill = "lightsteelblue2", color = "lightsteelblue4") +
      scale_x_discrete() + xlab(.i) + ylab(.x) +ggtitle (paste0("dispersion de ",.x," selon ",.i))
  })
  ml <- marrangeGrob(pl,ncol=2,nrow=3,top = NULL)
  print(ml)

qplot(ac$CVF_ERS)#pas normal
qplot(ac$agevni)#a peu pres normal
cor.test(ac$CVF_ERS, ac$agevni)
