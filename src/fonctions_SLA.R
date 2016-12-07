library(stringr)
library(dplyr)


manage_date_ND <- function(vec){ #vec doit être un vecteur avec éléments de la forme 04/04/1989(facteur) ou "04/04/1989"(character)
  if (all(!is.na(as.Date(as.character(vec[!is.na(vec)]), tz = 'UTC', format = '%Y-%m-%d')))){
    vec_d <- vec #cad si c'était déjà en format date
  } else {
    
    vec <- as.character(vec)
    #browser()
    exist_year <-!is.na(str_sub(vec, 7, 10))
    
    if (!length(grep("ND",vec[exist_year]))==0) {
      vec[exist_year] <- gsub("ND/ND", "01/07",vec[exist_year],fixed=T)
      vec[exist_year] <- gsub("ND", "15",vec[exist_year],fixed=T)
    }
    vec_d <- as.Date(vec,"%d/%m/%Y") 
    
  }
  return(vec_d)
}

#si ND dans la colonne, sort tous les ND, si pas de ND dans la colonne, renvoie une ligne non ND pour la colonne
who_is_date_ND <- function(vec_name,vec_date) {
  #browser()
  vec_date <- as.character(vec_date)
  exist_year <-!is.na(str_sub(vec_date, 7, 10))
  exist_month <- !is.na(str_sub(vec_date,4,5))
  name_ND <- vec_name[grep("ND",vec_date[exist_year])]
  if (length(name_ND)!=0){
    date_ND <- vec_date[grep("ND",vec_date[exist_year])]
    df_ND <- data.frame(name = name_ND, date = date_ND,
                        N_ND = ifelse ( date_ND %in% vec_date[exist_month], "day", "day_month"))
  } else {
    df_ND <- data.frame(name = "no_ND", date = "no_ND",
                        N_ND = "no_ND")
  }
  
  return(df_ND)
}

print_double_unique <- function(data,name_pat){ #name_pat entre guillemet!
  #browser()
  wt1 <-data[data$PATIENT==name_pat, ]
  wt <- wt1[1:(nrow(wt1)-1), ] == wt1[2:nrow(wt1),] #compare ligne du dessus à ligne du dessous
  st <- apply(wt,2,all)
  diff_coll <- c("PATIENT",names(st[st==F & !is.na(st)]))
  if ( all(diff_coll=="PATIENT") ) res <- "no difference"
  else {
    res <- wt1[,diff_coll]
    #res[, colnames(res)[grep("dat",colnames(res),ignore.case=T)]] <- for (i in colnames(res)[grep("dat",colnames(res),ignore.case=T)]){
    #  manage_date_ND(res[,i]) #ne marche pas, tant pis
    #}
  }
  write.table(print(res),file="clipboard",sep="\t",dec=",",row.names=FALSE) 
  return (res)
}

print_double <- function(data,vec_doublons,range_col){
  #browser()
  if ( ncol(data)<length(range_col) ) range_col <- 1:ncol(data)
  if (range_col[1]==1) wt1 <- arrange(data[data$PATIENT %in% vec_doublons, range_col], PATIENT)
  else wt1 <- arrange(data[data$PATIENT %in% vec_doublons, c(1,range_col)], PATIENT) #voir plus tard pour les repêcher
  wt <- wt1[1:(nrow(wt1)-1), ] == wt1[2:nrow(wt1),] #suppose que chaque ligne est présente 2 fois uniquement
  
  wt <- wt[(1:nrow(wt))[(1:nrow(wt)) %in% seq(1,100,2)], ]
  st <- apply(wt,2,all)
  diff_coll <- c("PATIENT",names(st[st==F & !is.na(st)]))
  if ( all(diff_coll=="PATIENT") ) res <- "no difference"
  else res <- wt1[,diff_coll]
  write.table(print(res),file="clipboard",sep="\t",dec=",",row.names=FALSE) 
}

#Impression des doublons sur excel
print.report <- function(.name,data,file,sheet) {
  xlsx.addHeader(file, sheet,.name)
  res <- print_double_unique (data,.name)
  if (is.null(ncol(res))) {
    xlsx.addParagraph (file, sheet, res)
    xlsx.addLineBreak(sheet, 1)
  } else {
    xlsx.addTable(file,sheet,res)
    xlsx.addLineBreak(sheet, 1)
  } 
} 



# get_date_max_fun <- function (data,version=NULL){
#   data$PATIENT <- as.character(data$PATIENT)
#   #Je transforme les facteurs en date
#   
#   for (i in colnames(data)){
#     data[,i] <- manage_date_ND(data[,i])
#   }
#   
#   #Je repere les colonnes non NA
#   is.datenonNA <- apply(data,2,function(.x)!is.na(.x)) #si true = non NA
#   COLDATEnonNA <- colnames(is.datenonNA)[apply(is.datenonNA,2,sum)>0]
#   
#   data <- data[ ,COLDATEnonNA]
#   
#   #Date max par patient
#   get_date_max <- function (data,version){
#     data <- data.frame(data)
#     data$datemax <- sapply(seq(nrow(data)), function(i) {
#       if(version==1){
#         j <- which.max(data[i,])
#         #c(paste(i, j, data[i,j],sep='/')) #version vérif
#         data[i,j]#version non verif
#         #c(paste(rownames(data)[i], colnames(data)[j], data[i,j],sep='/'))
#       } else {
#         if (version==2 | is.null(version)) max(as.numeric(data[i,]),na.rm=T)
#       }
#     })
#     if (version==2 | is.null(version)) data$datemax <- as.Date(as.numeric(data$datemax),origin="1970-01-01")
#     
#     #browser()
#     return(data$datemax)
#   }
#   
#   data$datemax <- get_date_max(data,version)
#   
#   return(data$datemax)
#   #return(data[,c("PATIENT","datemax")])
#   #return(data) #version verif
# }
#
# get_date_max_fun <- function (data){
#   data$PATIENT <- as.character(data$PATIENT)
#   #Je transforme les facteurs en date
#   
#   for (i in !colnames(data) %in% "PATIENT"){
#     data[,i] <- manage_date_ND(data[,i])
#   }
#   
#   #Je repere les colonnes non NA
#   is.datenonNA <- apply(data,2,function(.x)!is.na(.x)) #si true = non NA
#   COLDATEnonNA <- colnames(is.datenonNA)[apply(is.datenonNA,2,sum)>0]
#   
#   data <- data[ ,COLDATEnonNA]
#   
#   #Date max par patient
#   get_date_max <- function (data){
#     data <- data.frame(data)
#     data$datemax <- sapply(seq(nrow(data)), function(i)max(as.numeric(data[i,]),na.rm=T))
#     data$datemax <- as.Date(as.numeric(data$datemax),origin="1970-01-01")
#     return(data$datemax)
#   }
#   
#   data$datemax <- get_date_max(data)
#   
#   #return(data$datemax)
#   return(data[,c("PATIENT","datemax")])
#   #return(data) #version verif
# }

get_date_max_fun <- function (data) {
  colnamesDAT <- colnames(data)[grep("DAT",colnames(data))]
  
  for (i in colnamesDAT){
    #browser()
   data[,i] <- manage_date_ND(data[,i])
  }
  #browser()
  is.datenonNA <- apply(data,2,function(.x)!is.na(.x)) #si true = non NA
  COLDATEnonNA <- colnames(is.datenonNA)[apply(is.datenonNA,2,sum)>0]
  COLDATEnonNA <- COLDATEnonNA[!COLDATEnonNA %in% "PATIENT"]
  #databis <- data[, colnames(data)[colnames(data) %in% COLDATEnonNA & !colnames(data)%in% "PATIENT"] ]
  
  pick_date <- lapply(1: nrow(data),function(.x){
    if (.x %in% seq(1,nrow(data),by=100))print(paste(.x,"/",nrow(data)))
    #browser()
    .l <- data[.x, COLDATEnonNA ]
    .l <- .l[!is.na(.l)]#transforme en vecteur, indispensable pour l'étape d'après
    #.l <- .l[!is.na(as.Date(as.character(.l), tz = 'UTC', format = '%Y-%m-%d'))]#Prend en charge les elements qui ne seraient pas des dates
    if (length(.l)!=0) date <- max(.l)
    else date <- NA
    
    return(date)
  })
  data$max <- as.vector(do.call(rbind,pick_date))
  #browser()
  return(data[,c("PATIENT","max")])
}


# for (i in colnames(v)[-1]){
#   v[,i] <- manage_date_ND(v[,i])
# }
# 
# pick_date <- lapply(1: nrow(v),function(.x){
#   #browser()
#   .l <- v[.x,]
#   .l <- .l[!is.na(.l)]#transforme en vecteur, indispensable pour l'étape d'après
#   .l <- .l[!is.na(as.Date(as.character(.l), tz = 'UTC', format = '%Y-%m-%d'))]#Prend en charge les elements qui ne seraient pas des dates
#   
#   if (length(.l)!=0) date <- max(.l)
#   else date <- NA
#   
#   return(date)
# })
# v$max <- as.vector(do.call(rbind,pick_date))
