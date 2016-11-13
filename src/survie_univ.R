library(dplyr)
library(stringr)

manage_date_ND <- function(vec){ #vec doit être un vecteur avec éléments de la forme 04/04/1989 ou "04/04/1989"
  vec <- as.character(vec)
  exist_year <-!is.na(str_sub(vec, 7, 10))
  vec[exist_year] <- gsub("ND/ND", "01/07",vec,fixed=T)
  vec[exist_year] <- gsub("ND", "15",vec,fixed=T)
  vec_d <- as.Date(vec,"%d/%m/%Y")
  return(vec_d)
}

who_is_date_ND <- function(vec_name,vec_date) {
  vec_date <- as.character(vec_date)
  exist_year <-!is.na(str_sub(vec_date, 7, 10))
  exist_month <- !is.na(str_sub(vec_date,4,5))
  name_ND <- vec_name[grep("ND",vec_date[exist_year])]
  date_ND <- vec_date[grep("ND",vec_date[exist_year])]
  df_ND <- data.frame(name = name_ND, date = date_ND,
                      N_ND = ifelse ( date_ND %in% vec_date[exist_month], "day", "day_month"))
  return(df_ND)
}

#source("src/flow_chart.R")
sel_names <- readRDS("data/selected_names.rds")

DCD <- read.csv2("C:/Users/4051268/Documents/sauvegarde data/sla/data/20150827/DCD.csv")

dcd <- DCD %>% filter(PATIENT %in% sel_names)
dcd <- droplevels(dcd)

#-------------------------------------------------------------
#Gestion des dates ND

#Je sors les ND:
who_is_date_ND (dcd$PATIENT,dcd$DATE_DCD)
# Je remplace ND jour et mois par 01/07 (pas de cas ici) et ND jour uniquement par 15 :
dcd$date_dcd <- manage_date_ND(dcd$DATE_DCD)
table(!is.na(dcd$date_dcd[dcd$FORM != "ID"]))




#Pas de NA pour les dates (mais 2 patients avec ND pour le jour)
#table(dcd [dcd$FORM != "ID", "DATE_DCD"],useNA = "a")
# dcd$date_dcd <- as.Date(as.character(dcd$DATE_DCD),"%d/%m/%Y")
# table(dcd [dcd$FORM != "ID", "date_dcd"],useNA = "a") #2 dates qui avaient ND pour les jours ont été suprrimées

#2 patients avec jour ND
# dcd$date_dcd[str_sub(dcd$date_dcd, 1, 2)=="ND"]
# #nom des patients avec DCD ND
# dcd_ccl <- dcd [dcd$FORM != "ID", ]
# dateDCna <- dcd_ccl [is.na(dcd_ccl$date_dcd),]



#je sélectionne les lignes avec la date du décès
dcd %>% filter(CENTRE != "SLA01") #pe centre non renseigné dans VISIS_SUPL
head(dcd)

#lignes MODULE PV FORM ID  : centre 
table(table(dcd %>% filter (MODULE=="PV" & FORM=="ID") %>% select(DATE_DCD)))
# lignes FORM autre que ID (conclusion de l'examen) : date du décès
