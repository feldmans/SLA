# anonymate_sla <- function(x) {
#   if(is.factor(x)) {
#     y <- ceiling(length(levels(x))/26)
#     levels(x) <- sample(paste0(unlist(lapply(LETTERS,function(x)rep(x,20)[1:y])), 1:y), length(levels(x)),replace=FALSE)
#   } else {
#     x <- as.factor(x)
#     y <- ceiling(length(levels(x))/26)
#     levels(x) <- sample(paste0(unlist(lapply(LETTERS,function(x)rep(x,20)[1:y])), 1:y), length(levels(x)),replace=FALSE)
#   }
#   return(as.character(x))
# }


manage_date_ND <- function(vec){ #vec doit être un vecteur avec éléments de la forme 04/04/1989(facteur) ou "04/04/1989"(character)
  if (all(!is.na(as.Date(as.character(vec[!is.na(vec)]), tz = 'UTC', format = '%Y-%m-%d')))){
    vec_d <- as_date(vec) #cad si c'était déjà en format date (pouvant être ou non une date)
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
  if ( all(diff_coll=="PATIENT") ) res <- "no difference2"
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
  if ( all(diff_coll=="PATIENT") ) res <- "no difference2"
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



# get_date_max_fun <- function (data) {
#   colnamesDAT <- colnames(data)[grep("DAT",colnames(data))]
#   
#   for (i in colnamesDAT){
#    data[,i] <- manage_date_ND(data[,i])
#   }
# 
#   is.datenonNA <- apply(data,2,function(.x)!is.na(.x)) #si true = non NA
#   COLDATEnonNA <- colnames(is.datenonNA)[apply(is.datenonNA,2,sum)>0]
#   COLDATEnonNA <- COLDATEnonNA[!COLDATEnonNA %in% "PATIENT"]
# 
#   
#   pick_date <- lapply(1: nrow(data),function(.x){
#     
#     if (.x %in% seq(1,nrow(data),by=100))print(paste(.x,"/",nrow(data)))
#     .l <- data[.x, COLDATEnonNA ]
#     .l <- .l[!is.na(.l)]#transforme en vecteur, indispensable pour l'étape d'après
#     if (length(.l)!=0) browser() #date <- max(.l)
#     else date <- NA
#     
#     return(date)
#   })
#   data$max <- as.vector(do.call(rbind,pick_date))
# 
#   return(data[,c("PATIENT","max")])
# }

get_min_max <- function (data, fun = c("min","max")){ 
  if (fun=="max"){
    print("getting max for each row")
    pick_date <- lapply(1: nrow(data),function(.x){
      .l <- data[.x, !colnames(data)%in%"PATIENT"]
      .l <- .l[!is.na(.l)]#transforme en vecteur, indispensable pour l'étape d'après
      if (length(.l)!=0) date <- max(.l)
      else date <- NA
      return(date)
    })
    data$max <- as.vector(do.call(rbind,pick_date))
    # #si bug c'est cette ligne :
    # data2$max <- as_date(data2$max)
    res <- data[,c("PATIENT","max")]
  } else {
    print("getting min for each row")
    pick_date <- lapply(1: nrow(data),function(.x){
      .l <- data[.x, !colnames(data)%in%"PATIENT"]
      .l <- .l[!is.na(.l)]#transforme en vecteur, indispensable pour l'étape d'après
      if (length(.l)!=0) date <- min(.l)
      else date <- NA
      return(date)
    })
    data$min <- as.vector(do.call(rbind,pick_date))
    # #si bug c'est cette ligne :
    # data2$max <- as_date(data2$max)
    res <- data[,c("PATIENT","min")]
  }
  return(res)
}


get_date_max_fun <- function (data2, string, fun) { #fun = max ou min
  colnamesDAT <- colnames(data2)[grep(string,colnames(data2))]
  #browser()
  if(length(colnamesDAT)==0) return (list(print(paste0("No string '",string,"'.")),
                                          print(paste0("No colnames with '",string,"'."))))
  
  data <- data2[ ,colnamesDAT] #data = retrait de la colonne PATIENT 
  #browser()
  if(length(colnamesDAT)==1) {
    data <- manage_date_ND(data)
    if(any(!is.na(data))) {
      data2$max <- data
      res <- data2[,c("PATIENT","max")]
    } else{
      res <- "coldate but NA"
    } 
  } else {
    print("converting dataframe in data format")
    for (i in colnames(data))  data[,i] <- manage_date_ND(data[,i])
    
    is.datenonNA <- apply(data,2,function(.x)!is.na(.x)) #si true = non NA
    COLDATEnonNA <- colnames(is.datenonNA)[apply(is.datenonNA,2,sum)>0]
    COLDATEnonNA <- COLDATEnonNA[!COLDATEnonNA %in% "PATIENT"]
    
    res <- get_min_max (data = data2[ ,c("PATIENT", COLDATEnonNA)], fun) 
    
    
    # if (fun=="max"){
    #   print("getting max for each row")
    #   pick_date <- lapply(1: nrow(data),function(.x){
    #     #if (.x %in% seq(1,nrow(data),by=100))print(paste(.x,"/",nrow(data)))
    #     .l <- data[.x, COLDATEnonNA ]
    #     .l <- .l[!is.na(.l)]#transforme en vecteur, indispensable pour l'étape d'après
    #     if (length(.l)!=0) date <- max(.l)
    #     else date <- NA
    #     return(date)
    #   })
    #   data2$max <- as.vector(do.call(rbind,pick_date))
    #   # #si bug c'est cette ligne :
    #   # data2$max <- as_date(data2$max)
    #   
    #   res <- data2[,c("PATIENT","max")]
    # } else {
    #   print("getting min for each row")
    #   pick_date_min <- lapply(1: nrow(data),function(.x){
    #     #if (.x %in% seq(1,nrow(data),by=100))print(paste(.x,"/",nrow(data)))
    #     .l <- data[.x, COLDATEnonNA ]
    #     .l <- .l[!is.na(.l)]#transforme en vecteur, indispensable pour l'étape d'après
    #     if (length(.l)!=0) date <- min(.l)
    #     else date <- NA
    #     return(date)
    #   })
    #   data2$min <- as.vector(do.call(rbind,pick_date))
    #   # #si bug c'est cette ligne :
    #   # data2$max <- as_date(data2$max)
    #   res <- data2[,c("PATIENT","min")]
    # }
  }
  return(list(res,colnamesDAT))
}



get_ddn <- function (.path,.string,fun="max") {
  #browser()
  df.tmp <- read.csv2(.path)
  .table <- get_date_max_fun(df.tmp,.string)
  max_ddn <- .table[[1]]
  
  #si bug, supprimer ces ligne :
  if (is.data.frame(max_ddn)) {
    max_ddn$PATIENT <- as.character(max_ddn$PATIENT)
    max_ddn$max <- as_date (max_ddn$max)
  }
  
  #si bug, décommenter cette ligne:
  #if (is.data.frame(max_ddn)) max_ddn$PATIENT <- as.character(max_ddn$PATIENT)
  
  colnames_date <- .table[[2]]
  list(ddn = max_ddn,colonnes_date = colnames_date, path= .path)
  #list(.table,.path)
}



#to look for repetitive (or not) information (date or no)
# get_repet_var_fun <- function (data2,string1, string2=NULL) {
#   if (is.null(string2)){
#     colnamesDAT <- colnames(data2)[grep(string1, colnames(data2), ignore.case=T)] 
#   } else {
#     #browser()
#     num_row_string1 <- grep(string1, colnames(data2), ignore.case=T)
#     num_row_string2 <- grep(string2, colnames(data2), ignore.case=T)
#     both_strings <- num_row_string1[num_row_string1 %in% num_row_string2]
#     colnamesDAT <- colnames(data2)[both_strings] 
#   }
#   
#   if(length(colnamesDAT)==0) {
#     if (is.null(string2)) return (cat(paste0(paste0("No string '",string1,"'."),"\n",
#                                              paste0("No colnames with '",string1,"'."),"\nNB: not case sensitive")))
#     else {
#       #browser()
#       return (cat(paste0(paste0(length(num_row_string1)," string '",string1,"' without string '",string2,"'."),"\n",
#                          paste0(length(num_row_string2)," string '",string2,"' without string '",string1,"'."),"\nNB: not case sensitive")))
#     }
#   } else {
#     data <- data2[ ,colnamesDAT]
#     
#     if(length(colnamesDAT)==1) {
#       
#       if(any(!is.na(data))) {
#         data <- as.character(data) 
#         data <- ifelse(data=="",NA,data)
#         data <- data.frame(data) ; colnames(data) <- colnamesDAT
#         data$PATIENT <- as.character(data2$PATIENT)
#         which_nonNA <- data[!is.na(data[,colnamesDAT]), c("PATIENT",colnamesDAT)]
#         n_row_nonNA <- nrow(data[!is.na(data[,colnamesDAT]), c("PATIENT",colnamesDAT)])
#         #res 
#         which_colnonNA <- data[,c("PATIENT",colnamesDAT)]
#         return(list(all_data_rept_var = data[ ,c("PATIENT",colnamesDAT)], col_nonNA = which_colnonNA, col_and_row_nonNA = which_nonNA, nrow_nonNA = n_row_nonNA, which_col_withcolNA = colnamesDAT, which_col_nonNA = colnamesDAT))
#         
#       } else{
#         res <- "matching srting but column NA"
#         return(list(res,colnamesDAT))
#       }
#       
#     } else {
#       for (i in colnames(data)) {
#         data[,i] <- as.character(data[,i])
#         data[,i] <- ifelse(data[,i]=="",NA,data[,i])
#       }
#       #browser()
#       is.datenonNA <- apply(data,2,function(.x)!is.na(.x)) #si true = non NA
#       COLDATEnonNA <- colnames(is.datenonNA)[apply(is.datenonNA,2,sum)>0]
#       #COLDATEnonNA <- COLDATEnonNA[!COLDATEnonNA %in% "PATIENT"]
#       
#       data$PATIENT <- as.character(data2$PATIENT)
#       which_nonNA <- data[apply(apply(data[,-which(colnames(data) %in% "PATIENT")],2,function(x)!is.na(x)),1,sum)>0, c("PATIENT",COLDATEnonNA)]
#       n_row_nonNA <- nrow(data[apply(apply(data[,-which(colnames(data) %in% "PATIENT")],2,function(x)!is.na(x)),1,sum)>0, c("PATIENT",COLDATEnonNA)])
#       which_colnonNA <- data[ , c("PATIENT",COLDATEnonNA)]
#       
#       return(list(all_data_rept_var = data[ ,c("PATIENT",colnamesDAT)], col_nonNA = which_colnonNA, col_and_row_nonNA = which_nonNA, nrow_nonNA = n_row_nonNA, which_col_withcolNA = colnamesDAT, which_col_nonNA = COLDATEnonNA))
#     }
#     
#   }
#   
# }

# get_repet_var_fun(bdd7,"ALS_PAROL")
# get_repet_var_fun(bdd7,"als","PAROL") #selectionne colonnes avec les 2 strings, sans se préoccuper du case
# get_repet_var_fun(bdd7,"PAp","pop") #selectionne colonnes avec les 2 strings, sans se préoccuper du case
# get_repet_var_fun(bdd7,"pop") #selectionne colonnes avec les 2 strings, sans se préoccuper du case



#voir dans chaque base, quelle colonne contient l'info et la retourner si spécifié
# which_col <- function(data,string1,string2=NULL, type="explo"){
#   num <- as.integer(str_sub(data,-1,-1))
#   bdd <- get(data)
#   if(type=="explo") return(c(data, .dir_csv[num], get_repet_var_fun(bdd,string1,string2)$which_col_withcolNA))
#   if (type=="merge"){
#     return(unique(get_repet_var_fun(bdd,string1,string2)$col_and_row_nonNA))
#     #return(head(unique(get_repet_var_fun(bdd,string1,string2)$col_and_row_nonNA)))
#   }
# }


#to look for repetitive (or not) information (date or no) NB: string3= unwanted string
get_repet_var_fun <- function (data2,string1, string2=NULL, string3=NULL) {
  
  if (is.null(string2) & is.null(string3)){
    colnamesDAT <- colnames(data2)[grep(string1, colnames(data2), ignore.case=T)] 
  } else {
    if (!is.null(string2) & is.null(string3)){
      num_row_string1 <- grep(string1, colnames(data2), ignore.case=T)
      num_row_string2 <- grep(string2, colnames(data2), ignore.case=T)
      both_strings <- num_row_string1[num_row_string1 %in% num_row_string2]
      colnamesDAT <- colnames(data2)[both_strings]   
    } else {
      if (is.null(string2) & !is.null(string3)){
        num_row_string1 <- grep(string1, colnames(data2), ignore.case=T)
        num_row_string3 <- grep(string3, colnames(data2), ignore.case=T)
        string1_not3 <- num_row_string1[!num_row_string1 %in% num_row_string3]
        colnamesDAT <- colnames(data2)[string1_not3]   
      } else {#string2 et string3 existent
        num_row_string1 <- grep(string1, colnames(data2), ignore.case=T)
        num_row_string2 <- grep(string2, colnames(data2), ignore.case=T)
        num_row_string3 <- grep(string3, colnames(data2), ignore.case=T)
        both_strings <- num_row_string1[num_row_string1 %in% num_row_string2]
        both_strings_not3 <- both_strings[!both_strings %in% num_row_string3]
        colnamesDAT <- colnames(data2)[both_strings_not3]   
      }
    }
  }
  
  if(length(colnamesDAT)==0) {
    
    
    if (is.null(string2)) return (cat(paste0(paste0("No string '",string1,"'."),"\n",
                                             paste0("No colnames with '",string1,"'."),"\nNB: not case sensitive")))
    else {
      if(is.null(string3)){
        return (cat(paste0(paste0(length(num_row_string1)," string '",string1,"' without string '",string2,"'."),"\n",
                           paste0(length(num_row_string2)," string '",string2,"' without string '",string1,"'."),"\nNB: not case sensitive")))
      }
    }
    
  } else {
    
    data <- data2[ ,colnamesDAT]
    
    if(length(colnamesDAT)==1) {
      
      if(any(!is.na(data))) {
        data <- as.character(data) 
        data <- ifelse(data=="",NA,data)
        data <- data.frame(data) ; colnames(data) <- colnamesDAT
        data$PATIENT <- as.character(data2$PATIENT)
        which_nonNA <- data[!is.na(data[,colnamesDAT]), c("PATIENT",colnamesDAT)]
        n_row_nonNA <- nrow(data[!is.na(data[,colnamesDAT]), c("PATIENT",colnamesDAT)])
        #res 
        which_colnonNA <- data[,c("PATIENT",colnamesDAT)]
        return(list(all_data_rept_var = data[ ,c("PATIENT",colnamesDAT)], col_nonNA = which_colnonNA, col_and_row_nonNA = which_nonNA, nrow_nonNA = n_row_nonNA, which_col_withcolNA = colnamesDAT, which_col_nonNA = colnamesDAT))
        
      } else{
        res <- "matching srting but column NA"
        return(list(res,colnamesDAT))
      }
      
    } else {
      for (i in colnames(data)) {
        data[,i] <- as.character(data[,i])
        data[,i] <- ifelse(data[,i]=="",NA,data[,i])
      }

        #browser()
        is.datenonNA <- apply(data,2,function(.x)!is.na(.x)) #si true = non NA
        COLDATEnonNA <- colnames(is.datenonNA)[apply(is.datenonNA,2,sum)>0]
        #COLDATEnonNA <- COLDATEnonNA[!COLDATEnonNA %in% "PATIENT"]
        
        data$PATIENT <- as.character(data2$PATIENT)
        which_nonNA <- data[apply(apply(data[,-which(colnames(data) %in% "PATIENT")],2,function(x)!is.na(x)),1,sum)>0, c("PATIENT",COLDATEnonNA)]
        n_row_nonNA <- nrow(data[apply(apply(data[,-which(colnames(data) %in% "PATIENT")],2,function(x)!is.na(x)),1,sum)>0, c("PATIENT",COLDATEnonNA)])
        which_colnonNA <- data[ , c("PATIENT",COLDATEnonNA)]
        return(list(all_data_rept_var = data[ ,c("PATIENT",colnamesDAT)], col_nonNA = which_colnonNA, col_and_row_nonNA = which_nonNA, nrow_nonNA = n_row_nonNA, which_col_withcolNA = colnamesDAT, which_col_nonNA = COLDATEnonNA))
    }
    
  }
  
}

# get_repet_var_fun(bdd7,"ALS_PAROL")
# get_repet_var_fun(bdd7,"als","PAROL") #selectionne colonnes avec les 2 strings, sans se préoccuper du case
# get_repet_var_fun(bdd7,"PAp","pop") #selectionne colonnes avec les 2 strings, sans se préoccuper du case
# get_repet_var_fun(bdd7,"pop") #selectionne colonnes avec les 2 strings, sans se préoccuper du case



#voir dans chaque base, quelle colonne contient l'info et la retourner si spécifié
which_col <- function(data,string1,string2=NULL, string3=NULL, type="explo", keep_col_NA=FALSE){
  num <- as.integer(str_sub(data,-1,-1))
  bdd <- get(data)
  if(type=="explo") return(c(data, .dir_csv[num], get_repet_var_fun(bdd, string1, string2, string3)$which_col_withcolNA))
  if (type=="merge"){
    if (keep_col_NA==FALSE){
      return(unique(get_repet_var_fun(bdd,string1,string2,string3)$col_and_row_nonNA))  
    } else {
      return(unique(get_repet_var_fun(bdd,string1,string2,string3)$all_data_rept_var))
    }
    
    #return(head(unique(get_repet_var_fun(bdd,string1,string2)$col_and_row_nonNA)))
  }
}

matching_table <- data.frame(path = c("DCD","demo","Diag","neuro_mobilite","VNI","pat","pre","trt","visite"), bdd=c(1,2,3,4,5,6,7,8,9))

find_table <- function(path_or_number){
  #browser()
  if (is.numeric(path_or_number)) res <- matching_table %>% filter (bdd==path_or_number)
  else res <- matching_table %>% filter(path==path_or_number)
  return (res)
}

scan_notebook <- function(string, onepath_or_pathlist){
  for (i in onepath_or_pathlist){
    fic<-i
    x<-scan(i, what=as.character(), sep="\n")
    j<-grep(string, tolower(x),ignore.case = TRUE);print(c(i,x[j]))
  }
}

#verif graphique des risques proportionnels en ggplot2


#RECUPERATION VARIABLES REPETEES

#pour récupere des dates dans la base neuro (bdd9)
get_var_blf0_suivif1_neuro_date <- function(Ws1, Ws2){
  #browser()
  Ws <- c(Ws1, Ws2)
  print(Ws)
  vs <- c("DATEXAM", "DATEXAM_V")
  extws <- c("base", "suivi")

  #base
  v <- c(vs[1], Ws[1])
  w<-c("date", "x")
  y<-bdd6[, c("PATIENT", v)]
  names(y)[-1]<-w
  y$ext <- extws[1]
  y$qui <- Ws[1]
  y$f<- 0 #f0 car base
  Yv<-y
  # Yv<-Yv[order(Yv$PATIENT, Yv$f),]
  Yv$date<-as.Date(as.character(Yv$date), "%d/%m/%Y")


  #option verif_date :Ws <- c("DATE_NUTRI", "DATE_NUTRI_V") #pour verifier que la date nutri est la même que la date exam
  Yv$x<-as.Date(as.character(Yv$x), "%d/%m/%Y")

  # Yv<-Yv[!is.na(Yv$x) & !is.na(Yv$date),]
  YYv<-Yv

  #suivi
  v <- c(vs[2], Ws[2])
  r<-c("", paste("_M", 1:100, sep=""))
  vr<-expand.grid(list(v=v, r=r))
  vr$f<-as.numeric(gsub("_M", "", vr$r, fix=T))
  vr$f[is.na(vr$f)]<-0
  vr$vr<-paste(vr$v, vr$r, sep="")
  vr<-vr[vr$vr%in%names(bdd9),]
  w<-c("date", "x")
  vrs<-split(vr, vr$f)

  for (iv in 1:length(vrs)) {
    #browser()
    #print(iv)
    y<-bdd9[, c("PATIENT", vrs[[iv]]$vr)]
    names(y)[-1]<-w
    if (ncol(y)<3) next
    y$ext<-extws[ie]
    #y$qui<-W #erreur
    y$qui<-Ws[1]
    y$f<-vrs[[iv]]$f[1]
    if (iv==1) {
      Yv<-y
    } else {
      Yv<-rbind(Yv, y)
    }
  }
  #browser()
  # Yv<-Yv[order(Yv$PATIENT, Yv$f),]
  Yv$date<-as.Date(as.character(Yv$date), "%d/%m/%Y")


  #option verif_date :Ws <- c("DATE_NUTRI", "DATE_NUTRI_V")
  Yv$x<-as.Date(as.character(Yv$x), "%d/%m/%Y")

  # Yv<-Yv[!is.na(Yv$x) & !is.na(Yv$date),]
  head(Yv)
  YYv<-rbind(YYv, Yv)
  YYv<-YYv[order(YYv$PATIENT, YYv$f),]
  YYv<-YYv[!is.na(YYv$x) & !is.na(YYv$date),]
  if (nrow(YYv)==0) return(NULL)
  #y<-merge(YYv, vnisla[, c("PATIENT", "date")], by="PATIENT", all=F, suff=c("", ".vni"))
  y<-merge(YYv, vnisla[, c("PATIENT", "datevni")], by="PATIENT", all=F)
  #y$del<-as.numeric(y$date-y$date.vni)
  y$del<-as.numeric(y$date-y$datevni)
  y<-y[!is.na(y$del),]
  y<-y[order(y$PATIENT, y$date),]

  #selection des variables de suivi : toutes
  yp<-y[y$del>0,]
  head(yp)
  summary(yp)
  if(nrow(yp)!=0) yp$f<-1 #1 pour suivi (en opposition à baseline)

  #selection valeur de baseline
  yn<-y[y$del<=0,]
  head(yn)
  yn<-yn[order(yn$PATIENT, -yn$del),] #delai le plus petit en première ligne
  head(yn)
  id<-unique(yn$PATIENT);id
  dim(yn)
  yn<-yn[match(id, yn$PATIENT),] #prend la première ligne de yn qui matche avec id (donc prend ligne correspondant au délai le plus petit pour chaque patient)
  dim(yn)
  head(yn)
  summary(yn)
  if(nrow(yn)!=0)yn$f<-0 #0 pour baseline

  #je rassemble les tableaux baseline(yn) et suivi(yp)
  y<-rbind(yp, yn)
  y<-y[order(y$PATIENT, y$del),]
  head(y)
  return(y)
}


#pour récupérer variables pneumo(bdd7) répétées qui n'ont pas de baseline
get_var_suivi_nobl_pneumo <- function(vec){
  vs<-c("DATE_SOUSVENT_SV")
  Ws <- vec
  extvs<-c("SV")
  extws<-c("SV")
  print(Ws)
  for (ie in 1:length(extvs)) {
    v<-c(vs[ie], Ws[ie])
    r<-c("", paste("_F", 1:100, sep=""))
    vr<-expand.grid(list(v=v, r=r))
    vr$f<-as.numeric(gsub("_F", "", vr$r, fix=T))
    vr$f[is.na(vr$f)]<-0
    vr$vr<-paste(vr$v, vr$r, sep="")
    vr<-vr[vr$vr%in%names(bdd7),]
    dim(vr)
    w<-c("date", "x")
    vrs<-split(vr, vr$f)
    #names(vrs)
    #vrs$"0"
    #vrs$"2"
    for (iv in 1:length(vrs)) { 
      y<-bdd7[, c("PATIENT", vrs[[iv]]$vr)]
      names(y)[-1]<-w
      y$ext<-extws[ie]
      y$qui<-Ws[1]
      y$f<-vrs[[iv]]$f[1]
      if (iv==1) {
        Yv<-y
      } else {
        Yv<-rbind(Yv, y)
      }
    }
    Yv<-Yv[order(Yv$PATIENT, Yv$f),]
    Yv$date<-as.Date(as.character(Yv$date), "%d/%m/%Y")
    Yv$x<-as.numeric(as.character(Yv$x))
    Yv<-Yv[!is.na(Yv$x) & !is.na(Yv$date),]
    head(Yv)
    if (ie==1) {
      YYv<-Yv
    } else {
      YYv<-rbind(YYv, Yv)
    }
  }
  y<-merge(YYv, vnisla[, c("PATIENT", "datevni")], by="PATIENT", all=F)
  
  #colonne delai(del) entre la date d'exam et la date de vni
  y$del<-as.numeric(y$date-y$datevni)
  y<-y[!is.na(y$del),]
  
  y<-y[order(y$PATIENT, y$date),]
  
  #selection des variables de suivi mais je ne selection pas les valeurs de baseline (yn<-y[y$del<=0,])
  yp<-y[y$del>0,] 
  if(nrow(yp)!=0) yp$f<-1 #1 pour suivi (en opposition à baseline)
  
  y <- yp
  y<-y[order(y$PATIENT, y$del),]
  head(y)
  
  return(y)
}

#pour récupérer variables pneumo(bdd7) répétées qui ont une baseline
get_var_bl_suivi_pneumo <- function(vec){
  vs<-c("DATE_RESP_PP", "DATE_PREVENT_PP", "DATE_SOUSVENT_SV")
  #Ws <- c(varPP, varPV, varSV)
  Ws <- vec
  extvs<-c("PP", "PP", "SV")
  extws<-c("PP", "PV", "SV")
  print(Ws)
  for (ie in 1:length(extvs)) {
    #browser()
    # ie<-ie+1  
    v<-c(vs[ie], Ws[ie])
    #v<-paste(v, c(extvs[ie], extws[ie]), sep="_")
    r<-c("", paste("_F", 1:100, sep=""))
    vr<-expand.grid(list(v=v, r=r))
    vr$f<-as.numeric(gsub("_F", "", vr$r, fix=T))
    vr$f[is.na(vr$f)]<-0
    vr$vr<-paste(vr$v, vr$r, sep="")
    vr<-vr[vr$vr%in%names(bdd7),]
    dim(vr)
    w<-c("date", "x")
    vrs<-split(vr, vr$f)
    names(vrs)
    vrs$"0"
    vrs$"2"
    for (iv in 1:length(vrs)) { 
      #browser()
      y<-bdd7[, c("PATIENT", vrs[[iv]]$vr)]
      names(y)[-1]<-w
      #y$ext<-ext #erreur
      y$ext<-extws[ie]
      #y$qui<-W #erreur
      y$qui<-Ws[1]
      y$f<-vrs[[iv]]$f[1]
      if (iv==1) {
        Yv<-y
      } else {
        Yv<-rbind(Yv, y)
      }
    }
    #browser()
    Yv<-Yv[order(Yv$PATIENT, Yv$f),]
    Yv$date<-as.Date(as.character(Yv$date), "%d/%m/%Y")
    Yv$x<-as.numeric(as.character(Yv$x))
    Yv<-Yv[!is.na(Yv$x) & !is.na(Yv$date),]
    head(Yv)
    if (ie==1) {
      YYv<-Yv
    } else {
      YYv<-rbind(YYv, Yv)
    }
  }
  #y<-merge(YYv, vnisla[, c("PATIENT", "date")], by="PATIENT", all=F, suff=c("", ".vni"))
  y<-merge(YYv, vnisla[, c("PATIENT", "datevni")], by="PATIENT", all=F)
  
  #colonne delai(del) entre la date d'exam et la date de vni
  #y$del<-as.numeric(y$date-y$date.vni)
  y$del<-as.numeric(y$date-y$datevni)
  y<-y[!is.na(y$del),]
  
  y<-y[order(y$PATIENT, y$date),]
  head(y)
  
  #selection des variables de suivi : toutes
  yp<-y[y$del>0,] 
  head(yp)
  summary(yp)
  if(nrow(yp)!=0) yp$f<-1 #1 pour suivi (en opposition à baseline)
  
  #selection valeur de baseline
  yn<-y[y$del<=0,]
  head(yn)
  yn<-yn[order(yn$PATIENT, -yn$del),] #delai le plus petit en première ligne 
  head(yn)
  id<-unique(yn$PATIENT);id
  dim(yn)
  yn<-yn[match(id, yn$PATIENT),] #prend la première ligne de yn qui matche avec id (donc prend ligne correspondant au délai le plus petit pour chaque patient)
  dim(yn)
  head(yn)
  summary(yn)
  if(nrow(yn)!=0) yn$f<-0 #0 pour baseline
  
  #je rassemble les tableaux baseline(yn) et suivi(yp)
  y<-rbind(yp, yn)
  y<-y[order(y$PATIENT, y$del),]
  head(y)
  #browser()
  return(y)
}


#pour récupérer variables pneumo(bdd7) répétées qui ont une baseline mais avec une fin en CL1
get_var_bl_suivi_pneumo2 <- function(vec){
  #browser()
  vs<-c("DATE_RESP_PP", "DATE_PREVENT_PP", "DATE_SOUSVENT_SV") 
  Ws <- vec
  extvs<-c("PP", "PP", "SV")
  extws<-c("PP", "PV", "SV")
  print(Ws)
  for (ie in 1:length(extvs)) {
    #browser()
    v<-c(vs[ie], Ws[ie])
    if(ie==3){
      #recuperer date prevent
      r<-c("", paste("_F", 1:100, sep=""))
      vr1 <- expand.grid(list(v=v[1], r=r))
      vr1$f<-as.numeric(gsub("_F", "", vr1$r, fix=T))
      #recuperer SV_Fx_CL1
      r<-c("", paste("_F", 1:100, "_CL1", sep=""))
      vr2 <-expand.grid(list(v=v[2], r=r))
      vr2$f<-gsub("_F", "", vr2$r, fix=T)
      vr2$f<-as.numeric(gsub("_CL1", "", vr2$f, fix=T))
      #rassembler    
      vr <- rbind(vr1, vr2)
    } else {
      r<-c("", paste("_F", 1:100, sep=""))
      vr<-expand.grid(list(v=v, r=r))
      vr$f<-as.numeric(gsub("_F", "", vr$r, fix=T))
    }
    vr$f[is.na(vr$f)]<-0
    vr$vr<-paste(vr$v, vr$r, sep="")
    vr<-vr[vr$vr%in%names(bdd7),]
    vr <- vr[order(vr$f),]
    
    dim(vr)
    w<-c("date", "x")
    vrs<-split(vr, vr$f)
    names(vrs)
    vrs$"0"
    vrs$"2"
    
    for (iv in 1:length(vrs)) { 
      y<-bdd7[, c("PATIENT", vrs[[iv]]$vr)]
      names(y)[-1]<-w
      y$ext<-extws[ie]
      y$qui<-paste0(Ws[1],"_CL1")
      y$f<-vrs[[iv]]$f[1]
      if (iv==1) {
        Yv<-y
      } else {
        Yv<-rbind(Yv, y)
      }
    }
    #browser()
    Yv<-Yv[order(Yv$PATIENT, Yv$f),]
    Yv$date<-as.Date(as.character(Yv$date), "%d/%m/%Y")
    Yv$x<-as.numeric(as.character(Yv$x))
    Yv<-Yv[!is.na(Yv$x) & !is.na(Yv$date),]
    head(Yv)
    if (ie==1) {
      YYv<-Yv
    } else {
      YYv<-rbind(YYv, Yv)
    }
  }
  #y<-merge(YYv, vnisla[, c("PATIENT", "date")], by="PATIENT", all=F, suff=c("", ".vni"))
  y<-merge(YYv, vnisla[, c("PATIENT", "datevni")], by="PATIENT", all=F)
  
  #colonne delai(del) entre la date d'exam et la date de vni
  #y$del<-as.numeric(y$date-y$date.vni)
  y$del<-as.numeric(y$date-y$datevni)
  y<-y[!is.na(y$del),]
  
  y<-y[order(y$PATIENT, y$date),]
  head(y)
  
  #selection des variables de suivi : toutes
  yp<-y[y$del>0,] 
  head(yp)
  summary(yp)
  if(nrow(yp)!=0) yp$f<-1 #1 pour suivi (en opposition à baseline)
  
  #selection valeur de baseline
  yn<-y[y$del<=0,]
  head(yn)
  yn<-yn[order(yn$PATIENT, -yn$del),] #delai le plus petit en première ligne 
  head(yn)
  id<-unique(yn$PATIENT);id
  dim(yn)
  yn<-yn[match(id, yn$PATIENT),] #prend la première ligne de yn qui matche avec id (donc prend ligne correspondant au délai le plus petit pour chaque patient)
  dim(yn)
  head(yn)
  summary(yn)
  if(nrow(yn)!=0) yn$f<-0 #0 pour baseline
  
  #je rassemble les tableaux baseline(yn) et suivi(yp)
  y<-rbind(yp, yn)
  y<-y[order(y$PATIENT, y$del),]
  head(y)
  #browser()
  return(y)
}


#Récupérer les baseline pneumo qui dépendent de la date de vni
get_var_bl_depvni_pneumo <- function(vec){
  vs<-c("DATE_RESP_PP", "DATE_PREVENT_PP")
  #Ws <- c(varPP, varPV, varSV)
  Ws <- vec
  extvs<-c("PP", "PP")
  extws<-c("PP", "PV")
  print(Ws)
  
  for (ie in 1:length(extvs)) {
    #browser()
    # ie<-ie+1  
    v<-c(vs[ie], Ws[ie])
    #v<-paste(v, c(extvs[ie], extws[ie]), sep="_")
    r<-c("", paste("_F", 1:100, sep=""))
    vr<-expand.grid(list(v=v, r=r))
    vr$f<-as.numeric(gsub("_F", "", vr$r, fix=T))
    vr$f[is.na(vr$f)]<-0
    vr$vr<-paste(vr$v, vr$r, sep="")
    vr<-vr[vr$vr%in%names(bdd7),]
    dim(vr)
    w<-c("date", "x")
    vrs<-split(vr, vr$f)
    names(vrs)
    vrs$"0"
    vrs$"2"
    for (iv in 1:length(vrs)) { 
      #browser()
      y <- bdd7[, c("PATIENT", vrs[[iv]]$vr)]
      names(y)[-1] <- w
      #y$ext<-ext #erreur
      y$ext <- extws[ie]
      #y$qui<-W #erreur
      y$qui <- str_sub(Ws[1], 1, -4)
      y$f <- vrs[[iv]]$f[1]
      if (iv==1) {
        Yv<-y
      } else {
        Yv<-rbind(Yv, y)
      }
    }
    #browser()
    Yv<-Yv[order(Yv$PATIENT, Yv$f),]
    Yv$date<-as.Date(as.character(Yv$date), "%d/%m/%Y")
    Yv$x<-as.numeric(as.character(Yv$x))
    Yv<-Yv[!is.na(Yv$x) & !is.na(Yv$date),]
    head(Yv)
    if (ie==1) {
      YYv<-Yv
    } else {
      YYv<-rbind(YYv, Yv)
    }
  }
  #browser()
  #y<-merge(YYv, vnisla[, c("PATIENT", "date")], by="PATIENT", all=F, suff=c("", ".vni"))
  y<-merge(YYv, vnisla[, c("PATIENT", "datevni")], by="PATIENT", all=F)
  
  #colonne delai(del) entre la date d'exam et la date de vni
  #y$del<-as.numeric(y$date-y$date.vni)
  y$del<-as.numeric(y$date-y$datevni)
  y<-y[!is.na(y$del),]
  
  y<-y[order(y$PATIENT, y$date),]
  head(y)
  
  # #selection des variables de suivi : toutes
  # yp<-y[y$del>0,] 
  # head(yp)
  # summary(yp)
  # if(nrow(yp)!=0) yp$f<-1 #1 pour suivi (en opposition à baseline)
  
  #selection valeur de baseline
  yn<-y[y$del<=0,]
  head(yn)
  yn<-yn[order(yn$PATIENT, -yn$del),] #delai le plus petit en première ligne 
  head(yn)
  id<-unique(yn$PATIENT);id
  dim(yn)
  yn<-yn[match(id, yn$PATIENT),] #prend la première ligne de yn qui matche avec id (donc prend ligne correspondant au délai le plus petit pour chaque patient)
  dim(yn)
  head(yn)
  summary(yn)
  if(nrow(yn)!=0) yn$f<-0 #0 pour baseline
  y <- yn
  head(y)
  #browser()
  return(y)
}

#Recuperer les variables neuro répétées
get_var_blf0_suivif1_neuro <- function(Ws1, Ws2){
  #browser()
  Ws <- c(Ws1, Ws2)
  print(Ws)
  vs <- c("DATEXAM", "DATEXAM_V")
  extws <- c("base", "suivi")
  
  #base
  v <- c(vs[1], Ws[1])
  w<-c("date", "x")
  y<-bdd6[, c("PATIENT", v)]
  names(y)[-1]<-w
  y$ext <- extws[1]
  y$qui <- Ws[1]
  y$f<- 0 #f0 car base
  Yv<-y
  # Yv<-Yv[order(Yv$PATIENT, Yv$f),]
  Yv$date<-as.Date(as.character(Yv$date), "%d/%m/%Y")
  
  #option var non date
  Yv$x<-as.numeric(as.character(Yv$x))
  # #option verif_date :Ws <- c("DATE_NUTRI", "DATE_NUTRI_V") #pour verifier que la date nutri est la même que la date exam
  # Yv$x<-as.Date(as.character(Yv$x), "%d/%m/%Y")
  
  # Yv<-Yv[!is.na(Yv$x) & !is.na(Yv$date),]
  YYv<-Yv
  
  #suivi
  v <- c(vs[2], Ws[2])
  r<-c("", paste("_M", 1:100, sep=""))
  vr<-expand.grid(list(v=v, r=r))
  vr$f<-as.numeric(gsub("_M", "", vr$r, fix=T))
  vr$f[is.na(vr$f)]<-0
  vr$vr<-paste(vr$v, vr$r, sep="")
  vr<-vr[vr$vr%in%names(bdd9),]
  w<-c("date", "x")
  vrs<-split(vr, vr$f)
  
  for (iv in 1:length(vrs)) { 
    #browser()
    #print(iv)
    y<-bdd9[, c("PATIENT", vrs[[iv]]$vr)]
    names(y)[-1]<-w
    if (ncol(y)<3) next 
    #y$ext<-extws[ie]#pas de ie car plus de boucle
    y$ext<-NA
    #y$qui<-W #erreur
    y$qui<-Ws[1]
    y$f<-vrs[[iv]]$f[1]
    if (iv==1) {
      Yv<-y
    } else {
      Yv<-rbind(Yv, y)
    }
  }
  #browser()
  # Yv<-Yv[order(Yv$PATIENT, Yv$f),]
  Yv$date<-as.Date(as.character(Yv$date), "%d/%m/%Y")
  
  #option var non date
  Yv$x<-as.numeric(as.character(Yv$x))
  # #option verif_date :Ws <- c("DATE_NUTRI", "DATE_NUTRI_V") 
  # Yv$x<-as.Date(as.character(Yv$x), "%d/%m/%Y")
  
  # Yv<-Yv[!is.na(Yv$x) & !is.na(Yv$date),]
  head(Yv)
  YYv<-rbind(YYv, Yv)
  YYv<-YYv[order(YYv$PATIENT, YYv$f),]
  YYv<-YYv[!is.na(YYv$x) & !is.na(YYv$date),]
  if (nrow(YYv)==0) return(NULL)
  #y<-merge(YYv, vnisla[, c("PATIENT", "date")], by="PATIENT", all=F, suff=c("", ".vni"))
  y<-merge(YYv, vnisla[, c("PATIENT", "datevni")], by="PATIENT", all=F)
  #y$del<-as.numeric(y$date-y$date.vni)
  y$del<-as.numeric(y$date-y$datevni)
  y<-y[!is.na(y$del),]
  y<-y[order(y$PATIENT, y$date),]
  
  #selection des variables de suivi : toutes
  yp<-y[y$del>0,] 
  head(yp)
  summary(yp)
  if(nrow(yp)!=0) yp$f<-1 #1 pour suivi (en opposition à baseline)
  
  #selection valeur de baseline
  yn<-y[y$del<=0,]
  head(yn)
  yn<-yn[order(yn$PATIENT, -yn$del),] #delai le plus petit en première ligne 
  head(yn)
  id<-unique(yn$PATIENT);id
  dim(yn)
  yn<-yn[match(id, yn$PATIENT),] #prend la première ligne de yn qui matche avec id (donc prend ligne correspondant au délai le plus petit pour chaque patient)
  dim(yn)
  head(yn)
  summary(yn)
  if(nrow(yn)!=0)yn$f<-0 #0 pour baseline
  
  #je rassemble les tableaux baseline(yn) et suivi(yp)
  y<-rbind(yp, yn)
  y<-y[order(y$PATIENT, y$del),]
  head(y)
  return(y)
}


#=======================================
#DESCRIPTION

des_quali <- function(var, data){
  data <- data[,var]
  names_levels <- levels(as.factor(data))
  a <- lapply(names_levels, function(x) {
    tmp <- as.numeric(table(data)[x])
    tmpbis <- round(as.numeric(prop.table(table(data))[x]),3)*100
    tmptot <- paste0(tmp," (",tmpbis,"%)")
    
    nNA <- table(is.na(data))
    pNA <- round(prop.table(table(is.na(data))),3)
    if (is.na(nNA[2]))  {
      if (which(names_levels==x)==1) nNA <- paste0 (0," (0%)")
      else nNA <- ""
    }
    else {
      if (which(names_levels==x)==1){
        nNA <- as.numeric (nNA[names(nNA)==TRUE])
        pNA <- as.numeric (pNA[names(pNA)==TRUE])*100
        nNA <- paste0(nNA," (",pNA,"%)")  
      }
      else nNA <- ""
    }
    cbind(tmptot,nNA)
    
  })
  a <- do.call(rbind,a)
  #a <- cbind (a,nNA)
  rownames(a) <- paste0(var,"_",names_levels) 
  colnames(a) <- c("values","missing values")
  # a <- rbind (a,nNA)
  # rownames(a)[-nrow(a)] <- paste0(i,"_",names_levels) 
  return(a)
}

des_quanti <- function(var, data){ 
  data <- data[,var]
  med <- round(median (data,na.rm=T),2)
  quant <- round(quantile(data,na.rm=T),2)
  Q1 <- quant[2]
  Q3 <- quant[4]
  a <- paste0(med," (",Q1,"-",Q3,")")
  #browser()
  
  nNA <- table(is.na(data))
  pNA <- round(prop.table(table(is.na(data))),3)
  if (is.na(nNA[2]))  nNA <- paste0 (0," (0%)")
  else {
    nNA <- as.numeric (nNA[names(nNA)==TRUE])
    pNA <- as.numeric (pNA[names(pNA)==TRUE])*100
    nNA <- paste0(nNA," (",pNA,"%)")
  }
  # a <- rbind (a,nNA)
  # rownames(a)[-nrow(a)] <- paste0(i,"*") 
  a <- cbind (a,nNA)
  rownames(a) <- paste0(var,"*")
  colnames(a) <- c("values","missing values")
  return(a)
}

des_all <- function(var, data){
  print(var)
  #browser()
  vec <- data[ ,var]
  if (any(!is.na(as.numeric(as.character(vec)))) & length(levels(as.factor(vec))) != 2) res <- describe_quantitative(var, data)#génère warning si character
  else {
    if (class(vec)=="Date") res <- NULL 
    else res <- describe_qualitative(var, data)
  }
  return(res)
}

#--

describe_qualitative <- function(vec_var, .data){
  table_var_quali <- lapply(vec_var, function(i){
    data <- .data[,i]
    names_levels <- levels(as.factor(data))
    a <- lapply(names_levels, function(x) {
      tmp <- as.numeric(table(data)[x])
      tmpbis <- round(as.numeric(prop.table(table(data))[x]),3)*100
      tmptot <- paste0(tmp," (",tmpbis,"%)")
      
      nNA <- table(is.na(data))
      pNA <- round(prop.table(table(is.na(data))),3)
      if (is.na(nNA[2]))  {
        if (which(names_levels==x)==2) nNA <- paste0 (0," (0%)") #NA pour ligne 1
        else nNA <- ""
      }
      else {
        if (which(names_levels==x)==2){   #NA pour ligne 1
          nNA <- as.numeric (nNA[names(nNA)==TRUE])
          pNA <- as.numeric (pNA[names(pNA)==TRUE])*100
          nNA <- paste0(nNA," (",pNA,"%)")  
        }
        else nNA <- ""
      }
      cbind(tmptot,nNA)
      
    })
    a <- do.call(rbind,a)
    #a <- cbind (a,nNA)
    rownames(a) <- paste0(i,"_",names_levels) 
    colnames(a) <- c("valeur","missing values")
    # a <- rbind (a,nNA)
    # rownames(a)[-nrow(a)] <- paste0(i,"_",names_levels) 
    return(a)
  })
  table_var_quali <- do.call(rbind,table_var_quali)
  table_var_quali <- data.frame(table_var_quali)
  table_var_quali$range <- NA
  colnames(table_var_quali) <- c("valeur", "missing values", "range")
  return (table_var_quali)
}

describe_quantitative <- function(vec_var, .data){
  table_var_quanti <- lapply(vec_var, function(i){ #median ou moyenne? (sachant qu'on ne verifie pas normalite des baselines)
    data <- .data[,i]
    if (!is.numeric(data)) {
      a <- data.frame("not num", "", "") 
      colnames(a) <- c("valeur", "missing values", "range")
      rownames(a) <- i
    } else {
      med <- round(median (data,na.rm=T),2)
      quant <- round(quantile(data,na.rm=T),2)
      Q1 <- quant[2]
      Q3 <- quant[4]
      a <- paste0(med," (",Q1,"-",Q3,")")
      #browser()
      
      nNA <- table(is.na(data))
      pNA <- round(prop.table(table(is.na(data))),3)
      if (is.na(nNA[2]))  nNA <- paste0 (0," (0%)")
      else {
        nNA <- as.numeric (nNA[names(nNA)==TRUE])
        pNA <- as.numeric (pNA[names(pNA)==TRUE])*100
        nNA <- paste0(nNA," (",pNA,"%)")
      }
      
      myrange <- range(data, na.rm=T)
      myrange <- paste0(myrange[1]," - ",myrange[2])
      # a <- rbind (a,nNA)
      # rownames(a)[-nrow(a)] <- paste0(i,"*") 
      a <- cbind (a, nNA, myrange)
      rownames(a) <- paste0(i,"*")
      colnames(a) <- c("valeur", "missing values", "range")
    }
    return(a)
  })
  table_var_quanti <- do.call(rbind,table_var_quanti)
  return (table_var_quanti)
}

describe_all <- function(var, data){
  print(var)
#browser()
  vec <- data[ ,var]
  if (any(!is.na(as.numeric(as.character(vec)))) & length(levels(as.factor(vec))) != 2) res <- describe_quantitative(var, data)#génère warning si character
  else {
    if (class(vec)=="Date") res <- NULL 
    else res <- describe_qualitative(var, data)
  }
  return(res)
}

#===========================
#analyse survie simple

#VERIF LOGLINEARITE

# var <- "SNIP_cmH2O"
# .time <- "time.vni"
# .evt <- "evt" 
# data <- sla

check_loglin <- function(var, data, .time, .evt){
s <- data
s$a <- s[ ,var]
s$evt <- s[ ,.evt]
#s$tps <- (s[ ,.time]/365.25) + 0.001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner
s$tps <- (s[ ,.time]/365.25*12) + 0.001

s <- s[!is.na(s$a),]

# 1/ plot résidus du modèle vide en fonction de la variable explicative
cox1<-coxph(Surv(tps,evt)~1, data=s)
r<-residuals(cox1, "martingale")
lw<-lowess(r~s$a)
plot(s$a, r, main=paste0("Loglinearity of ",var))
lines(lw)

# 2/ découpe la variable le variable en fonction du temps avec une polynome de degré 2
cox2<-coxph(Surv(tps,evt)~poly(a, df=2, raw=T), data=s)
tabcox <- summary(cox2)
pval <- tabcox$coefficients[2,"Pr(>|z|)"]
paste0("pvalue polynome degre 2 : ", round(pval,3))
}


#HYP DES RISQUES PROP

check_RP <- function(var, data, .time, .evt, type="quanti", recode = TRUE){
  s <- data
  s$a <- s[ ,var]
  s$evt <- s[ ,.evt]
  #s$tps <- (s[ ,.time]/365.25) + 0.001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner
  s$tps <- (s[ ,.time]/365.25*12) + 0.001
  
  s <- s[!is.na(s$a),]
  
  if (type=="quanti" & recode==TRUE) {
    s$a_recode <- ifelse (s$a < median(s$a), 0, 1)
    .title <- paste0 ("RP of ", var, " superior to ", round(median(s$a),0))
  } else {
    s$a_recode <- s$a
    .title <- paste0("RP of ", var)
  }
  mod <- coxph(Surv(tps, evt) ~ a_recode, data = s)
  
    #Test de Harrell
  z <- cox.zph(mod, transform = "rank")
  cat("Test de Harrell\n\n")
  print(z)
  pval <- round(z$table[,3],3)
  cat(paste0("\nTest de Harrell p value: ", pval))
  #non signif si p>=0.05
  
  #résidus de Shoenfeld
  z <- cox.zph(mod, transf="identity")
  plot(z, main=paste0(.title, "\nHarrell test p = ",pval), resid = FALSE, ylab = paste0("Beta(t) for ", var))
  abline(h=0, col="red")
  abline(h=coef(mod), col="blue")
  #text(x = (max(s$tps)*1/2), y = z$table[,1]+ 2, labels = paste0("Harrell test p = ", pval))
  return(data.frame(var=var,pval=pval))
  #non significatif si l'IC contient a tout moment la courbe rouge
}


#RECODAGE EN FONCTION DU TEMPS ET VERIF
add_vart_and_check <- function(var, data, .time, .evt, type="quanti", recode=TRUE, .transf="log", vec_cut = NULL){
  s <- data
  s$a <- s[ ,var]
  s$evt <- s[ ,.evt]
  #s$tps <- (s[ ,.time]/365.25) + 0.001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner
  s$tps <- (s[ ,.time]/365.25*12) + 0.001
  s <- s[!is.na(s$a),]
  
  if (type=="quanti" & recode==TRUE) {
    s$a_recode <- ifelse (s$a < median(s$a), 0, 1)
    .title <- paste0 ("RP of ", var, " superior to ", round(median(s$a),0))
  } else {
    s$a_recode <- s$a
    .title <- paste0("RP of ", var)
  }
  
  ti <- sort(unique(c(0,s$tps[s$evt==1])))
  slat <- s
  slat$start <- 0
  slat$stop <- slat$tps
  slat$evt <- slat$evt
  slat <- survSplit(Surv(stop,evt)~.,slat,start="start",cut=ti)
  
  #OPTION 1 : transformation du temps
  if(is.null(vec_cut)){ 
    transf <- .transf 
    print(paste(var, transf, sep="-"))
    if (transf=="log") slat$at<-slat$a_recode*log(slat$stop)
    if (transf=="sqrt")slat$at<-slat$a_recode*sqrt(slat$stop)
    if (transf=="*t")slat$at<-slat$a_recode*(slat$stop)
    if (transf=="/t")slat$at<-slat$a_recode/(slat$stop)
    if (transf=="*t^2") slat$at <-slat$a_recode*(slat$stop^2)
    if (transf=="*t^0.7") slat$at <-slat$a_recode*(slat$stop^0.7)
    if (transf=="log10") slat$at <-slat$a_recode*log10(slat$stop)
    if (transf=="*t^0.3") slat$at <-slat$a_recode*(slat$stop^0.3)
    if (transf=="*t^3") slat$at <-slat$a_recode*(slat$stop^3)
    f <- "Surv(start, stop, evt) ~ a_recode + at + cluster(PATIENT)"
    
    coxt <- coxph(as.formula(f), data=slat)
    test <- summary(coxt)
    pval <-  round(test$coefficients[ ,"Pr(>|z|)"], 3)
    coefbeta <- round(test$coefficients[ ,"coef"], 5)
    name_param <- rownames(test$coefficients)
    
    #La transformation du temps convient-elle?
    
    #non : coef de at non significatif => la transformation n'est pas bonne
    if (as.numeric(pval["at"])>0.05){ 
      #cat(paste0("\n",transf,": at not significant (p>=0.05), ",transf," doesn't fit, don't look at shoenfeld nor Harrell test"))
      return (data.frame(variable = var, param = name_param, transf= .transf, beta = coefbeta, pvalue = pval, Harrell = NA, 
                         curve = NA, AIC = NA, robust = NA, probust = NA))
    
    #peut-être : coef de at est significatif => on test les RP : Les 2 tests doivent être satisfaits
    } else { 
      .AIC <- round(extractAIC(coxt)[2],2) #le premier paramètre correspond au modèle vide, le deuxième est l'AIC du modèle. Plus l'AIC est petit, plus la vraisemblance est grande comparé au nombre de paramètres et mieux c'est.
      rscore <- round(test$robscore["test"],2)
      prscore <- round(test$robscore["pvalue"],4)
      #test de Harrell 
      zit <- cox.zph(coxt, transform = "rank")
      pval_H <- round(zit$table[,3],3) #Les 3 p doivent etre >=0.05
      Harrell <- if (all(as.numeric(pval_H)>0.05)) "ok" else "NOT ok"
      #------
      #plot #résidus de shoenfeld doivent être horizontaux
      zt <- cox.zph(coxt, transf="identity")
      for (i in 1:(nrow(zt$table)-1)){
        iz<-i
        plot(zt[iz], resid = FALSE, main = paste0("plot shoenfeld for ", var, rownames(zt[iz]$table), " with ",transf," transformation\nHarrell :", Harrell, "\nAIC = ", .AIC))
        abline(h=0, col="red")
      }
      #------
      return (data.frame(variable = var, param = name_param, transf = .transf, beta = coefbeta, pvalue = pval, Harrell = all(as.numeric(pval_H)>0.05),
                         curve = NA,  AIC = .AIC, robust = rscore, probust = prscore))
    }
    
  #OPTION 2 : découpage du temps
  } else { 
    print(var)
    b <- vec_cut
    name_cut <- paste(b, collapse = "-")
    
    for (i in 1:(length(b)+1)){
      if (i == 1) tmp <-  slat$a_recode * ifelse(slat$stop<=b[1], 1, 0)
      if(i <= length(b) & i!= 1) tmp <- slat$a_recode * ifelse(slat$stop>b[i-1] & slat$stop<=b[i], 1, 0)
      if(i == (length(b)+1)) tmp <-  slat$a_recode * ifelse(slat$stop>b[i-1], 1, 0)
      slat[ ,paste0("at",i)] <- tmp
    }
    vat<-paste("at",  1:(length(b)+1), sep="")
    x<-slat[, vat]
    sx<-colSums(x) #interval de temps sans evt
    wat<-vat[sx>0] #on supprime interval de temps quand pas d'evenement
    f<-paste("Surv(start, stop, evt) ~ ", paste(wat, collapse="+"),"+cluster(PATIENT)", sep="")  #on ne met pas a_recode car les at couvre deja  toutes les perdiodes
    

    coxt <- coxph(as.formula(f), data=slat)
    test <- summary(coxt)
    pval <-  round(test$coefficients[ ,"Pr(>|z|)"], 3) #on veut la pvalue du test robuste et le rscore et pas la pvalue des coefficients
    coefbeta <- round(test$coefficients[ ,"coef"], 5)
    name_param <- rownames(test$coefficients)
    .AIC <- round(extractAIC(coxt)[2],2)
    rscore <- round(test$robscore["test"],2)
    prscore <- round(test$robscore["pvalue"],4)
   
    
    #on ne fait que la courbe et pas de test de Harrell pour la découpe du temps
    zt <- cox.zph(coxt, transf="identity")
    for (i in 1:(nrow(zt$table)-1)){
      iz<-i
      plot(zt[iz], resid = FALSE, main = paste0("plot shoenfeld for", var, " cut ", name_cut, "\nAIC = ", .AIC))
      abline(h=0, col="red")
      abline(h=bhat[i], col="blue", lty=2)
      abline(v=b)
    }
    return(data.frame(variable = var, param = name_param, cut = name_cut, beta = coefbeta, pvalue = pval, 
                      curve = NA,  AIC = .AIC, robust = rscore, probust = prscore))
  }
}

#HR et IC
Test_score_HR_IC <- function(var="SNIP_perc_pred", data=sla, .time="time.vni", .evt="evt", type="quanti", recode=TRUE, dep_temps = FALSE, .transf=NULL, vec_time=NULL) {
  
  s <- data
  s$a <- s[ ,var]
  s$evt <- s[ ,.evt]
  #s$tps <- (s[ ,.time]/365.25) + 0.001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner
  s$tps <- (s[ ,.time]/365.25*12) + 0.001 
  s <- s[!is.na(s$a),]
  if (type== "quanti" & recode==TRUE) {
    cat("Loglinearity Hypothesis is not verified, quantitative variable cut at the median \n")
    #cat(paste0("a = ", var))
    #cat("\ns$a_recode <- ifelse (s$a < median(s$a), 0, 1)")
    s$a_recode <- ifelse (s$a < median(s$a), 0, 1)
    cat(paste0("median of ", var, " = ", median(s$a),"\n"))
    
  } else {
    if (type=="quanti" & recode==FALSE) cat("Loglinearity Hypothesis is assumed verified \n")
    #cat(paste0("a = ", var))
    s$a_recode <- s$a
    #cat("\ns$a_recode <- s$a\n")
  }
  
  
  #TEST DU SCORE ET HR [95%IC]
  
  if (dep_temps==TRUE){ #NON RESPECT DES RISQUES PROP=> ajout variable dependant du temps
    cat("Proportionality Hypothesis is not verified ")
    ti <- sort(unique(c(0,s$tps[s$evt==1])))
    slat <- s
    slat$start <- 0
    slat$stop <- slat$tps
    slat$evt <- slat$evt
    slat <- survSplit(Surv(stop,evt)~.,slat,start="start",cut=ti)
    
    transf <- .transf
    cat(paste0("=> transformation function of time is added. \nTransformation = ",transf))
    
    if (transf=="log") slat$at<-slat$a_recode*log(slat$stop)
    if (transf=="sqrt")slat$at<-slat$a_recode*sqrt(slat$stop)
    if (transf=="*t")slat$at<-slat$a_recode*(slat$stop)
    if (transf=="/t")slat$at<-slat$a_recode/(slat$stop)
    if (transf=="*t^2") slat$at <-slat$a_recode*(slat$stop^2)
    if (transf=="*t^0.7") slat$at <-slat$a_recode*(slat$stop^0.7)
    if (transf=="*t^3") slat$at <-slat$a_recode*(slat$stop^3)
    
    mod <- coxph(Surv(tps, evt) ~ a_recode + at , data = slat)
    test <- summary (mod)
    
    S <- vcov(mod)
    b <- coef(mod)
    t <- vec_time #choisir le temps en mois
    if (transf=="*t^0.7") t_t <- t^0.7
    if (transf=="log") t_t <- log(t)
    if (transf=="*t^2") t_t <- t^2 
    if (transf=="*t") t_t <- t
    if (transf=="*t^3") t_t <- t^3
    
    variance <- S[1,1]+S[2,2]*(t_t)^2+2*S[1,2]*(t_t)
    m <- b[1]+b[2]*(t_t) #coef de l'HR
    
    
    cat("\n\nmod <- coxph(Surv(tps, evt) ~ ", var, " + ", var, "(time), data)")
    cat(paste0("\n\nScore test: ", round(test$sctest["pvalue"],3)))
    HR <- round(exp(m),3)
    IC <- round(exp(m + qnorm(0.975)*sqrt(variance) * c(-1,1)),3)
    #cat(paste0("\nHR[95%CI] = ",HR, " [", IC[1], "-", IC[2], "] ", "pour t = ", t, " an" ))
    cat(paste0("\nHR[95%CI] = ",HR, " [", IC[1], "-", IC[2], "] ", "pour t = ", t, " months" ))
    param <- round(test$coefficients[,1],4)
    cat(paste0("\ncoefficient of ", c(var,paste0(var, "(time)")), ": ", param))
    
  } else { #RESPECT DE L'HYP DES RISQUES PROP
    cat("Proportionality Hypothesis is assumed verified")
    mod <- coxph(Surv(tps, evt) ~ a_recode, data = s)
    test <- summary (mod)
    cat("\n\nmod <- coxph(Surv(tps, evt) ~ ", var, ", data)")
    cat(paste0("\n\nScore test: ", round(test$sctest["pvalue"],3)))
    HRIC <- round(test$conf.int,2)
    param <- round(test$coefficients[,1],4)
    
    
    if(type=="quali2"){
      lev <- levels(s$a_recode)
      cat("\nref=",lev[1],"\n\n")
      for (i in 1:(length(levels(s$a_recode))-1)){
        cat(paste0("\nHR[95%CI] ", "of class ", lev[i+1]," = ", HRIC[i, 1] , " [", HRIC[i, 3], "-", HRIC[i, 4], "] (if PHH verified)" ))
        cat(paste0("\ncoefficient of ", lev[i+1],": ", param[i]))
      }
    } else {
      cat(paste0("\nHR[95%CI] = ",HRIC[1] , " [", HRIC[3], "-", HRIC[4], "] (if PHH verified)" ))
      cat(paste0("\ncoefficient of ", var,": ", param))
    }
    
  }
}

#COURBE DE SURVIE VARIABLE BINAIRE

#pour courbe , tps en année

#Attention a été modifiée pour être sortie avec marrange Grob. Il faut maintenant faire print de la fonction pour l'afficher(ou marrange grob)
draw_surv_bin <- function(var, data, .time, .evt, vec_time_IC= c(1, 3), type = "quanti", surv_only=FALSE, pvalue = TRUE, dep_temps=FALSE, .transf=NULL) {

  s <- data
  s$a <- s[ ,var]
  s <- s[!is.na(s$a),]
  .title <- paste0("Survival by ", var)
  
  if (type=="quanti") {
    s$a_recode <- ifelse (s$a < median(s$a), 0, 1)
    #.title <- paste0 ("Survival by ", var, " superior to ", round(median(s$a),0))
  } else {
    s$a_recode <- s$a
  }
  
  s$evt <- s[ ,.evt]
  s$tps <- (s[ ,.time]/365.25) + 0.001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner
  
  km <- survfit(Surv(tps,evt)~a_recode, data=s, conf.int=.95)
  km0 <- survfit(Surv(tps,evt)~a_recode, data=s[s$a_recode==0,], conf.int=.95)
  km1 <- survfit(Surv(tps,evt)~a_recode, data=s[s$a_recode==1,], conf.int=.95)
  
  #pour IC95%
  skmi0<-summary(km0, time=vec_time_IC-0.1)
  skmi1<-summary(km1, time=vec_time_IC+0.1) #plus d'évènement apres 1.94 ans
  
  if(type=="quali") {
    group0 <- paste0("\nIn group ", var, " = 0\n ")
    group1 <- paste0("\nIn group ", var, " = 1\n ")
  } else {
    group0 <- paste0("\nIn group ", var, " < ",round(median(s$a),0), "\n ")
    group1 <- paste0("\nIn group ", var, " >= ",round(median(s$a),0), "\n ")
  }
  #survies aux tps choisis
  cat(group0)
  sv <- summary(km0, time=vec_time_IC)
  df <- data.frame(time = sv$time, survival = sv$surv*100, LCI = sv$lower*100, UCI = sv$upper*100)
  df[,2:4] <- round(df[,2:4], 0)
  cat(paste0("At ", df$time, " year, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
  #cat(paste0("At ", df$time, " months, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
  
  cat(group1)
  sv <- summary(km1, time=vec_time_IC)
  df <- data.frame(time = sv$time, survival = sv$surv*100, LCI = sv$lower*100, UCI = sv$upper*100)
  df[,2:4] <- round(df[,2:4], 0)
  cat(paste0("At ", df$time, " year, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
  #cat(paste0("At ", df$time, " months, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
  
  if(surv_only==FALSE){
    #pour table de survie
    skm0 <- summary(km0, time=seq(0, 10, by=1))
    skm0 <- data.frame(time=skm0$time, n.risk=skm0$n.risk)
    skm1<-summary(km1, time=seq(0, 10, by=1))
    skm1 <- data.frame(time=skm1$time, n.risk=skm1$n.risk)
    
    #preparation legende
    if(type=="quali")leg<-str_sub(names(km$strata),-1,-1)
    
    if(type=="quanti")leg <- c(paste0(var, " < ", round(median(s$a),0)), paste0(var, " >= ", round(median(s$a),0)))
    col <- hue_pal()(length(leg))
    
    #courbe de survie
    g <- ggsurv(km, CI=FALSE, order.legend=FALSE, surv.col=col, cens.col=col) +
      #changement des axes
      scale_x_continuous(breaks=seq(0,max(s$tps),1), labels=0:(length(seq(0,max(s$tps),1))-1)) +
      #scale_x_continuous(breaks=seq(0,max(s$tps),12), labels=0:(length(seq(0,max(s$tps),12))-1)) +
      scale_y_continuous(labels=percent) +
      labs(x="Time of follow-up, year", title=.title) +
      #labs(x="Time of follow-up, months", title=.title) +
      #changement legende
      guides (linetype = FALSE) +
      scale_colour_discrete( labels = leg) +
      theme(legend.position="right", legend.title=element_blank()) +
      #espace autour du schéma
      theme(plot.margin = unit(c(1,1,3,2), "cm")) #top, right, bottom, left
    
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
    g <- g + annotation_custom(grob = textGrob(leg[1]), xmin = -1.7, xmax = -1.7, ymin= - 1.5 )
    g <- g + annotation_custom(grob = textGrob(leg[2]), xmin = -1.7, xmax = -1.7, ymin= - 1.7 )
    
    if (pvalue==TRUE){
      if(dep_temps==TRUE){
        ti <- sort(unique(c(0,s$tps[s$evt==1])))
        s$tps <- (s[ ,.time]/365.25*12) + 0.001
        slat <- s
        slat$start <- 0
        slat$stop <- slat$tps
        slat$evt <- slat$evt
        slat <- survSplit(Surv(stop,evt)~.,slat,start="start",cut=ti)
        transf <- .transf
        if (transf=="log") slat$at<-slat$a_recode*log(slat$stop)
        if (transf=="sqrt")slat$at<-slat$a_recode*sqrt(slat$stop)
        if (transf=="*t")slat$at<-slat$a_recode*(slat$stop)
        if (transf=="/t")slat$at<-slat$a_recode/(slat$stop)
        if (transf=="*t^2") slat$at <-slat$a_recode*(slat$stop^2)
        if (transf=="*t^0.7") slat$at <-slat$a_recode*(slat$stop^0.7)
        mod <- coxph(Surv(tps, evt) ~ a_recode + at , data = slat)
      } else {
        mod <- coxph(Surv(tps, evt) ~ a_recode, data = s)
      }
      
      test <- summary (mod)
      pval <- round(test$sctest["pvalue"],3)
      pval <- ifelse(pval<0.05, paste0(pval, " *"), pval)
      pval <- ifelse(pval<0.01, "Score test \n p<0.01 *",paste0("Score test \n p=", pval))
      g <- g + annotate("text",
                        x=0.75*max(km$time),
                        y=0.75*max(km$surv),
                        label=pval)
    }
    
    gt <- ggplotGrob(g)
    gt$layout$clip[gt$layout$name=="panel"] <- "off"
    grid.draw(gt)
    return(gt)
    
  } else {
    sv <- summary(km0, time=vec_time_IC)
    df <- data.frame(group = 0, time = sv$time, survival = sv$surv*100, LCI = sv$lower*100, UCI = sv$upper*100)
    df[,3:5] <- round(df[,3:5], 0)
    df0 <- df
    
    sv <- summary(km1, time=vec_time_IC)
    df <- data.frame(group = 1, time = sv$time, survival = sv$surv*100, LCI = sv$lower*100, UCI = sv$upper*100)
    df[,3:5] <- round(df[,3:5], 0)
    df1 <- df
    df <- rbind(df0, df1)
    return(df)
    
  }
}

# draw_surv_bin <- function(var, data, .time, .evt, vec_time_IC= c(1, 3), type = "quanti") {
#   s <- data
#   s$a <- s[ ,var]
#   s <- s[!is.na(s$a),]
#   
#   if (type=="quanti") {
#     s$a_recode <- ifelse (s$a < median(s$a), 0, 1)
#     .title <- paste0 ("Survival by ", var, " superior to ", round(median(s$a),0))
#   } else {
#     s$a_recode <- s$a
#     .title <- paste0("survival by ", var)
#   }
#   
#   s$evt <- s[ ,.evt]
#   s$tps <- (s[ ,.time]/365.25) + 0.001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner
#   
#   km <- survfit(Surv(tps,evt)~a_recode, data=s, conf.int=.95)
#   km0 <- survfit(Surv(tps,evt)~a_recode, data=s[s$a_recode==0,], conf.int=.95)
#   km1 <- survfit(Surv(tps,evt)~a_recode, data=s[s$a_recode==1,], conf.int=.95)
#   
#   #pour IC95%
#   
#   skmi0<-summary(km0, time=vec_time_IC-0.1)
#   skmi1<-summary(km1, time=vec_time_IC+0.1) #plus d'évènement apres 1.94 ans
#   
#   #survies aux tps choisis
#   cat("In group 0\n")
#   sv <- summary(km0, time=vec_time_IC)
#   df <- data.frame(time = sv$time, survival = sv$surv*100, LCI = sv$lower*100, UCI = sv$upper*100)
#   df[,2:4] <- round(df[,2:4], 0)
#   cat(paste0("At ", df$time, " year, survival[IC95%] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
#   
#   cat("\nIn group 1\n")
#   sv <- summary(km1, time=vec_time_IC)
#   df <- data.frame(time = sv$time, survival = sv$surv*100, LCI = sv$lower*100, UCI = sv$upper*100)
#   df[,2:4] <- round(df[,2:4], 0)
#   cat(paste0("At ", df$time, " year, survival[IC95%] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
#   
#   
#   #pour table de survie
#   skm0 <- summary(km0, time=seq(0, 10, by=1))
#   skm0 <- data.frame(time=skm0$time, n.risk=skm0$n.risk)
#   skm1<-summary(km1, time=seq(0, 10, by=1))
#   skm1 <- data.frame(time=skm1$time, n.risk=skm1$n.risk)
#   
#   #preparation legende
#   leg<-str_sub(names(km$strata),-1,-1)
#   col <- hue_pal()(length(leg))
#   
#   #courbe de survie
#   g <- ggsurv(km, CI=FALSE, order.legend=FALSE, surv.col=col, cens.col=col) +
#     #changement des axes
#     scale_x_continuous(breaks=seq(0,max(s$tps),1), labels=0:(length(seq(0,max(s$tps),1))-1)) +
#     scale_y_continuous(labels=percent) +
#     labs(x="Time of follow-up, year", title=.title) +
#     #changement legende
#     guides (linetype = FALSE) +
#     scale_colour_discrete( labels = leg) +
#     theme(legend.position="right", legend.title=element_blank()) +
#     #espace autour du schéma
#     theme(plot.margin = unit(c(1,1,3,2), "cm")) #top, right, bottom, left
#   #intervalle de confiance
#   for (i in 1:2) {
#     g <- g + geom_segment(x = skmi0$time[i], y = skmi0$lower[i], xend = skmi0$time[i], yend = skmi0$upper[i], colour = col[1])
#     g <- g + geom_segment(x = skmi0$time[i] - 0.1, y = skmi0$lower[i], xend = skmi0$time[i] + 0.1, yend = skmi0$lower[i], colour = col[1])
#     g <- g + geom_segment(x = skmi0$time[i] - 0.1, y = skmi0$upper[i], xend = skmi0$time[i] + 0.1, yend = skmi0$upper[i], colour = col[1])
#   }
#   for (i in 1:2) {
#     g <- g + geom_segment(x = skmi1$time[i], y = skmi1$lower[i], xend = skmi1$time[i], yend = skmi1$upper[i], colour = col[2])
#     g <- g + geom_segment(x = skmi1$time[i] - 0.1, y = skmi1$lower[i], xend = skmi1$time[i] + 0.1, yend = skmi1$lower[i], colour = col[2])
#     g <- g + geom_segment(x = skmi1$time[i] - 0.1, y = skmi1$upper[i], xend = skmi1$time[i] + 0.1, yend = skmi1$upper[i], colour = col[2])
#   }
#   #risk table
#   for (ii in 1:nrow(skm0)) {
#     g <- g + annotation_custom(grob = textGrob(skm0$n.risk[ii]), xmin = skm0$time[ii], xmax = skm0$time[ii], ymin= - 1.5 )
#   }  
#   for (ii in 1:nrow(skm1)) {
#     g <- g + annotation_custom(grob = textGrob(skm1$n.risk[ii]), xmin = skm1$time[ii], xmax = skm1$time[ii], ymin= - 1.7 )
#   } 
#   #display group text
#   g <- g + annotation_custom(grob = textGrob(leg[1]), xmin = -1.7, xmax = -1.7, ymin= - 1.5 )
#   g <- g + annotation_custom(grob = textGrob(leg[2]), xmin = -1.7, xmax = -1.7, ymin= - 1.7 )
#   
#   gt <- ggplotGrob(g)
#   gt$layout$clip[gt$layout$name=="panel"] <- "off"
#   grid.draw(gt)
#   
# }
#fonction survie quali à plusiuers clkasses(à fusionner avec celle du dessus à l'occasion)
surv_lieudeb <- function(surv_only=FALSE, pvalue=TRUE){
  vec_time_IC=1
  s <- sla
  s <- s[!is.na(s$LIEUDEB_recode),]
  s$tps <- (s$time.vni/365.25) + 0.001 
  
  var <- "beginning of onset"
  km <- survfit(Surv(tps,evt)~LIEUDEB_recode, data=s, conf.int=.95)
  
  for (i in 1:length(km$strata)){
    .km <- survfit(Surv(tps,evt)~LIEUDEB_recode, data=s[s$LIEUDEB_recode==levels(s$LIEUDEB_recode)[i], ], conf.int=.95)
    assign(paste0("km",i), .km)
    #pour IC95%
    am <- i/10
    .skmi <- summary(.km, time=vec_time_IC-am)
    assign(paste0("skmi",i), .skmi)
    #pour table de survie
    .skm <- summary(.km, time=seq(0, 10, by=1))
    .skm <- data.frame(time=.skm$time, n.risk=.skm$n.risk)
    assign(paste0("skm",i), .skm)
    
    #survies aux tps choisis
    cat(paste0("\nIn group ", var, " = ", levels(s$LIEUDEB_recode)[i], "\n "))
    sv <- summary(.km, time=vec_time_IC)
    df <- data.frame(time = sv$time, survival = sv$surv*100, LCI = sv$lower*100, UCI = sv$upper*100)
    df[,2:4] <- round(df[,2:4], 0)
    cat(paste0("At ", df$time, " year, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
    #cat(paste0("At ", df$time, " months, survival[95%CI] ", df$survival, "% [",df$LCI,"% - ",df$UCI, "%]\n"))
  }
  
  if (surv_only==FALSE){
    #preparation legende
    leg<-names(km$strata)
    leg <- str_sub(leg,16,-1)
    col <- hue_pal()(length(leg))
    .title <- paste0("Survival by ", var)
    
    #courbe de survie
    g <- ggsurv(km, CI=FALSE, order.legend = FALSE, surv.col=col, cens.col=col) +
      #changement des axes
      scale_x_continuous(breaks=seq(0,max(s$tps),1), labels=0:(length(seq(0,max(s$tps),1))-1)) +
      scale_y_continuous(labels=percent) +
      labs(x="Time of follow-up, year", title=.title) +
      #changement legende
      guides (linetype = FALSE) +
      scale_colour_discrete( labels = leg) +
      theme(legend.position="right", legend.title=element_blank()) +
      #espace autour du schéma
      theme(plot.margin = unit(c(0,1,4,2), "cm"), plot.title = element_text(size = 16, face = "bold"),
            axis.title.y = element_text(size = 12), axis.title.x = element_text(size = 12),
            legend.title = element_text(size=14), legend.text = element_text(size=12)) #top, right, bottom, left
    
    #intervalle de confiance
    for (j in 1:length(km$strata)){
      .skmi <- get(paste0(paste0("skmi",j)))
      for (i in 1:2) {
        g <- g + geom_segment(x = .skmi$time[i], y = .skmi$lower[i], xend = .skmi$time[i], yend = .skmi$upper[i], colour = col[j])
        g <- g + geom_segment(x = .skmi$time[i] - 0.1, y = .skmi$lower[i], xend = .skmi$time[i] + 0.1, yend = .skmi$lower[i], colour = col[j])
        g <- g + geom_segment(x = .skmi$time[i] - 0.1, y = .skmi$upper[i], xend = .skmi$time[i] + 0.1, yend = .skmi$upper[i], colour = col[j])
      }
    }
    
    #risk table
    #position_y <- c(-1.2, -1.5, -1.7, -1.9, -2.1)
    position_y <- c(-1.4, -1.5, -1.6, -1.7, -1.8)
    for (j in 1:5){
      .skm <- get(paste0(paste0("skm",j)))
      .pos <- position_y[j]
      for (ii in 1:nrow(.skm)) {
        g <- g + annotation_custom(grob = textGrob(.skm$n.risk[ii]), xmin = .skm$time[ii], xmax = .skm$time[ii], ymin= .pos )
      } 
    }
    
    #display group text
    for (j in 1:5){
      g <- g + annotation_custom(grob = textGrob(leg[j]), xmin = -2.1, xmax = -2.1, ymin = position_y[j])
    }
    if (pvalue==TRUE){
      mod <- coxph(Surv(tps, evt) ~ LIEUDEB_recode, data = s)
      test <- summary (mod)
      pval <- round(test$sctest["pvalue"],3)
      pval <- ifelse(pval<0.05, paste0(pval, " *"), pval)
      pval <- ifelse(pval<0.01, "Score test \n p<0.01 *",paste0("Score test \n p=", pval))
      g <- g + annotate("text",
                        x=0.75*max(km$time),
                        y=0.75*max(km$surv),
                        label=pval)
    }
    gt <- ggplotGrob(g)
    gt$layout$clip[gt$layout$name=="panel"] <- "off"
    grid.draw(gt) 
    
  }
}

#=======================================================
#fonctions analyse var répétées

check_loglin_dt <- function(modele=NULL, var=NULL, data=NULL, .time=NULL, .evt=NULL){
  browser()
  s <- data
  s$a <- s[ ,var]
  s$evt <- s[ ,.evt]
  #s$tps <- (s[ ,.time]/365.25) + 0.001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner
  s$tps <- (s[ ,.time]/365.25*12) + 0.001
  
  s <- s[!is.na(s$a),]
  
  #1/ découpe la variable le variable en fonction du temps avec une polynome de degré 2
  cox2<-coxph(Surv(tps,evt)~poly(a, df=2, raw=T), data=s)
  tabcox <- summary(cox2)
  pval <- tabcox$coefficients[2,"Pr(>|z|)"]
  paste0("pvalue polynome degre 2 : ", round(pval,3))
  return(pval)
}

check_RP_dt <- function(modele=NULL, var=NULL, data=NULL, .time=NULL, .evt=NULL, type="quanti", recode = TRUE){
  
  if(!is.null(modele)){
    mod <- modele
    .title <- paste0("RP of ", var)
  } else {
    s <- data
    s$a <- s[ ,var]
    s$evt <- s[ ,.evt]
    #s$tps <- (s[ ,.time]/365.25) + 0.001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner
    s$tps <- (s[ ,.time]/365.25*12) + 0.001
    
    s <- s[!is.na(s$a),]
    
    if (type=="quanti" & recode==TRUE) {
      s$a_recode <- ifelse (s$a < median(s$a), 0, 1)
      .title <- paste0 ("RP of ", var, " superior to ", round(median(s$a),0))
    } else {
      s$a_recode <- s$a
      .title <- paste0("RP of ", var)
    }
    mod <- coxph(Surv(tps, evt) ~ a_recode, data = s)
  }
  
  
  #Test de Harrell
  z <- cox.zph(mod, transform = "rank")
  cat("Test de Harrell\n\n")
  print(z)
  pval <- round(z$table[,3],3)
  cat(paste0("\nTest de Harrell p value: ", pval))
  #non signif si p>=0.05
  
  #résidus de Shoenfeld
  z <- cox.zph(mod, transf="identity")
  plot(z, main=paste0(.title, "\nHarrell test p = ", pval))
  abline(h=0, col="red")
  abline(h=coef(mod), col="blue")
  #non significatif si l'IC contient a tout moment la courbe rouge
  
  return(pval)
}


#RECODAGE EN FONCTION DU TEMPS ET VERIF
add_vart_and_check_dt <- function(data_split=NULL, modele=NULL, var, data=NULL, .time=NULL, .evt=NULL, type="quanti", recode=TRUE, .transf="log"){
  if(!is.null(data_split)){
    slat <- data_split
    .title <- paste0("RP of ", var)
    slat$a_recode <- slat$x
    slat$at <- slat$xt
    
  } else {
    s <- data
    s$a <- s[ ,var]
    s$evt <- s[ ,.evt]
    #s$tps <- (s[ ,.time]/365.25) + 0.001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner
    s$tps <- (s[ ,.time]/365.25*12) + 0.001
    
    s <- s[!is.na(s$a),]
    
    if (type=="quanti" & recode==TRUE) {
      s$a_recode <- ifelse (s$a < median(s$a), 0, 1)
      .title <- paste0 ("RP of ", var, " superior to ", round(median(s$a),0))
    } else {
      s$a_recode <- s$a
      .title <- paste0("RP of ", var)
    }
    
    ti <- sort(unique(c(0,s$tps[s$evt==1])))
    slat <- s
    slat$start <- 0
    slat$stop <- slat$tps
    slat$evt <- slat$evt
    slat <- survSplit(Surv(stop,evt)~.,slat,start="start",cut=ti)
  }
  
  transf <- .transf 
  print(transf)
  
  if (transf=="log") slat$at<-slat$a_recode*log(slat$stop)
  if (transf=="sqrt")slat$at<-slat$a_recode*sqrt(slat$stop)
  if (transf=="*t")slat$at<-slat$a_recode*(slat$stop)
  if (transf=="/t")slat$at<-slat$a_recode/(slat$stop)
  if (transf=="*t^2") slat$at <-slat$a_recode*(slat$stop^2)
  if (transf=="*t^0.7") slat$at <-slat$a_recode*(slat$stop^0.7)
  if (transf=="log10") slat$at <-slat$a_recode*log10(slat$stop)
  if (transf=="*t^0.3") slat$at <-slat$a_recode*(slat$stop^0.3)
  if (transf=="*t^3") slat$at <-slat$a_recode*(slat$stop^3)
  
  slat$a <- slat$a_recode
  coxt <- coxph(Surv(start, stop, evt) ~ a + at, data=slat)
  test <- summary(coxt)
  pval_at <- test$coefficients["at","Pr(>|z|)"]
  #print(paste0("pval for time dependant coefficient : ", pval_at))
  if (pval_at>0.05){
    #print(paste0("at non significant (p>=0.05), ",transf," doesn't fit, don't look at shoenfeld nor Harrell test"))
    res <- paste0("\n",transf,": at not significant (p>=0.05), ",transf," doesn't fit, don't look at shoenfeld nor Harrell test")
    return (list(res, NA))
  } else {
    #print(paste0("at significant (p<0.05), ", transf," may fit, check shoenfeld and Harrell test"))
    res1 <- paste0("\n",transf, ": at significant (p<0.05), ", transf," may fit, check shoenfeld and Harrell test")
    
    #résidus de Shoenfeld non significatif?
    zt <- cox.zph(coxt, transf="identity")
    for (i in 1:(nrow(zt$table)-1)){
      iz<-i
      plot(zt[iz], main=paste0("plot shoenfeld for ",rownames(zt[iz]$table), "(", var, ")", " with ",transf," transformation"))
      abline(h=0, col="red")
    }
    
    zit <- cox.zph(coxt, transform = "rank")
    zit
    zit <- cox.zph(coxt, transform = "rank")
    pval <- round(zit$table[,3],3)
    #if (all(as.numeric(pval)>0.05)) print(paste0("Harrell test not significant : if curve ok too, ", transf, "fit"))
    #else print(paste0("Harrell test significant : even if curve ok, ", transf, " do not fit"))
    #print("-----------------------")
    #Les 3 p doivent etre >=0.05 
    
    if (all(as.numeric(pval)>0.05)) res2 <- paste0(" => Harrell test not significant : if curve ok too, ", transf, " fit")
    else res2 <- paste0(" => Harrell test significant : even if curve ok, ", transf, " do not fit")
    
    res3 <- paste(res1,res2, sep ="\n" )
    
    if(pval_at<0.05 & all(as.numeric(pval)>0.05)){
      keep <- .transf
    } else {
      keep <- NA
    }
    return(list(res3, keep))
  }
}

Test_score_HR_IC <- function(var="SNIP_perc_pred", data=sla, .time="time.vni", .evt="evt", type="quanti", recode=TRUE, dep_temps = FALSE, .transf=NULL, vec_time=NULL) {
  
  s <- data
  s$a <- s[ ,var]
  s$evt <- s[ ,.evt]
  #s$tps <- (s[ ,.time]/365.25) + 0.001 # au cas ou un temps vaut 0 ce qui empêche survsplit de fonctionner
  s$tps <- (s[ ,.time]/365.25*12) + 0.001 
  s <- s[!is.na(s$a),]
  if (type== "quanti" & recode==TRUE) {
    cat("Loglinearity Hypothesis is not verified, quantitative variable cut at the median \n")
    #cat(paste0("a = ", var))
    #cat("\ns$a_recode <- ifelse (s$a < median(s$a), 0, 1)")
    s$a_recode <- ifelse (s$a < median(s$a), 0, 1)
    cat(paste0("median of ", var, " = ", median(s$a),"\n"))
    
  } else {
    if (type=="quanti" & recode==FALSE) cat("Loglinearity Hypothesis is assumed verified \n")
    #cat(paste0("a = ", var))
    s$a_recode <- s$a
    #cat("\ns$a_recode <- s$a\n")
  }
  
  
  #TEST DU SCORE ET HR [95%IC]
  
  if (dep_temps==TRUE){ #NON RESPECT DES RISQUES PROP=> ajout variable dependant du temps
    cat("Proportionality Hypothesis is not verified ")
    ti <- sort(unique(c(0,s$tps[s$evt==1])))
    slat <- s
    slat$start <- 0
    slat$stop <- slat$tps
    slat$evt <- slat$evt
    slat <- survSplit(Surv(stop,evt)~.,slat,start="start",cut=ti)
    
    transf <- .transf
    cat(paste0("=> transformation function of time is added. \nTransformation = ",transf))
    
    if (transf=="log") slat$at<-slat$a_recode*log(slat$stop)
    if (transf=="sqrt")slat$at<-slat$a_recode*sqrt(slat$stop)
    if (transf=="*t")slat$at<-slat$a_recode*(slat$stop)
    if (transf=="/t")slat$at<-slat$a_recode/(slat$stop)
    if (transf=="*t^2") slat$at <-slat$a_recode*(slat$stop^2)
    if (transf=="*t^0.7") slat$at <-slat$a_recode*(slat$stop^0.7)
    if (transf=="*t^3") slat$at <-slat$a_recode*(slat$stop^3)
    
    mod <- coxph(Surv(tps, evt) ~ a_recode + at , data = slat)
    test <- summary (mod)
    
    S <- vcov(mod)
    b <- coef(mod)
    t <- vec_time #choisir le temps en mois
    if (transf=="*t^0.7") t_t <- t^0.7
    if (transf=="log") t_t <- log(t)
    if (transf=="*t^2") t_t <- t^2 
    if (transf=="*t") t_t <- t
    if (transf=="*t^3") t_t <- t^3
    
    variance <- S[1,1]+S[2,2]*(t_t)^2+2*S[1,2]*(t_t)
    m <- b[1]+b[2]*(t_t) #coef de l'HR
    
    
    cat("\n\nmod <- coxph(Surv(tps, evt) ~ ", var, " + ", var, "(time), data)")
    cat(paste0("\n\nScore test: ", round(test$sctest["pvalue"],3)))
    HR <- round(exp(m),3)
    IC <- round(exp(m + qnorm(0.975)*sqrt(variance) * c(-1,1)),3)
    #cat(paste0("\nHR[95%CI] = ",HR, " [", IC[1], "-", IC[2], "] ", "pour t = ", t, " an" ))
    cat(paste0("\nHR[95%CI] = ",HR, " [", IC[1], "-", IC[2], "] ", "pour t = ", t, " months" ))
    param <- round(test$coefficients[,1],4)
    cat(paste0("\ncoefficient of ", c(var,paste0(var, "(time)")), ": ", param))
    
  } else { #RESPECT DE L'HYP DES RISQUES PROP
    cat("Proportionality Hypothesis is assumed verified")
    mod <- coxph(Surv(tps, evt) ~ a_recode, data = s)
    test <- summary (mod)
    cat("\n\nmod <- coxph(Surv(tps, evt) ~ ", var, ", data)")
    cat(paste0("\n\nScore test: ", round(test$sctest["pvalue"],3)))
    HRIC <- round(test$conf.int,2)
    param <- round(test$coefficients[,1],4)
    
    
    if(type=="quali2"){
      lev <- levels(s$a_recode)
      cat("\nref=",lev[1],"\n\n")
      for (i in 1:(length(levels(s$a_recode))-1)){
        cat(paste0("\nHR[95%CI] ", "of class ", lev[i+1]," = ", HRIC[i, 1] , " [", HRIC[i, 3], "-", HRIC[i, 4], "] (if PHH verified)" ))
        cat(paste0("\ncoefficient of ", lev[i+1],": ", param[i]))
      }
    } else {
      cat(paste0("\nHR[95%CI] = ",HRIC[1] , " [", HRIC[3], "-", HRIC[4], "] (if PHH verified)" ))
      cat(paste0("\ncoefficient of ", var,": ", param))
    }
    
  }
}


