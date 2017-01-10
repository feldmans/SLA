anonymate_sla <- function(x) {
  if(is.factor(x)) {
    y <- ceiling(length(levels(x))/26)
    levels(x) <- sample(paste0(unlist(lapply(LETTERS,function(x)rep(x,20)[1:y])), 1:y), length(levels(x)),replace=FALSE)
  } else {
    x <- as.factor(x)
    y <- ceiling(length(levels(x))/26)
    levels(x) <- sample(paste0(unlist(lapply(LETTERS,function(x)rep(x,20)[1:y])), 1:y), length(levels(x)),replace=FALSE)
  }
  return(as.character(x))
}


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
which_col <- function(data,string1,string2=NULL, string3=NULL, type="explo"){
  num <- as.integer(str_sub(data,-1,-1))
  bdd <- get(data)
  if(type=="explo") return(c(data, .dir_csv[num], get_repet_var_fun(bdd,string1,string2,string3)$which_col_withcolNA))
  if (type=="merge"){
    return(unique(get_repet_var_fun(bdd,string1,string2,string3)$col_and_row_nonNA))
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

ggcoxzph.1var <- function (fit, resid = TRUE, se = TRUE, df = 4, nsmo = 40, var2, 
                           point.col = "red", point.size = 1, point.shape = 19, point.alpha = 1, 
                           font.main = c(16, "plain", "black"), font.x = c(14, "plain", 
                                                                           "black"), font.y = c(14, "plain", "black"), font.tickslab = c(12, 
                                                                                                                                         "plain", "black"), ggtheme = theme_classic2()) 
{
  #browser()
  x <- fit
  if (!methods::is(x, "cox.zph")) 
    stop("Can't handle an object of class ", class(x))
  xx <- x$x
  yy <- x$y
  d <- nrow(yy)
  df <- max(df)
  nvar <- ncol(yy)
  pred.x <- seq(from = min(xx), to = max(xx), length = nsmo)
  temp <- c(pred.x, xx)
  lmat <- splines::ns(temp, df = df, intercept = TRUE)
  pmat <- lmat[1:nsmo, ]
  xmat <- lmat[-(1:nsmo), ]
  qmat <- qr(xmat)
  if (qmat$rank < df) 
    stop("Spline fit is singular, try a smaller degrees of freedom")
  if (se) {
    bk <- backsolve(qmat$qr[1:df, 1:df], diag(df))
    xtx <- bk %*% t(bk)
    seval <- d * ((pmat %*% xtx) * pmat) %*% rep(1, df)
  }
  #ylab <- paste("Beta(t) for", dimnames(yy)[[2]])
  ylab <- paste("Beta(t) for", var2)
  #if (missing(var)) 
    var <- 1:nvar
  # else {
  #   if (is.character(var)) 
  #    # browser()
  #     var <- match(var, dimnames(yy)[[2]])
  #   if (any(is.na(var)) || max(var) > nvar || min(var) < 
  #       1) 
  #     stop("Invalid variable requested")
  # }
  if (x$transform == "log") {
    xx <- exp(xx)
    pred.x <- exp(pred.x)
  }
  else if (x$transform != "identity") {
    #browser()
    xtime <- as.numeric(dimnames(yy)[[1]])
    indx <- !duplicated(xx)
    apr1 <- approx(xx[indx], xtime[indx], seq(min(xx), max(xx), 
                                              length = 17)[2 * (1:8)])
    temp <- signif(apr1$y, 2)
    apr2 <- approx(xtime[indx], xx[indx], temp)
    xaxisval <- apr2$y
    xaxislab <- rep("", 8)
    for (i in 1:8) xaxislab[i] <- format(temp[i])
  }
  #browser()
  plots <- list()
  plots <- lapply(var, function(i) {
    invisible(pval <- round(x$table[i, 3], 3))
    gplot <- ggplot() + ggtitle(paste0("Schoenfeld Individual Test p: ", 
                                       pval)) + ggtheme
    #browser()
    y <- yy[, i]
    yhat <- pmat %*% qr.coef(qmat, y)
    if (resid) 
      yr <- range(yhat, y)
    else yr <- range(yhat)
    if (se) {
      temp <- 2 * sqrt(x$var[i, i] * seval)
      yup <- yhat + temp
      ylow <- yhat - temp
      yr <- range(yr, yup, ylow)
    }
    if (x$transform == "identity") {
      gplot <- gplot + geom_line(aes(x = pred.x, y = yhat)) + 
        xlab("Time") + ylab(ylab[i]) + ylim(yr)
    }
    else if (x$transform == "log") {
      gplot <- gplot + geom_line(aes(x = log(pred.x), y = yhat)) + 
        xlab("Time") + ylab(ylab[i]) + ylim(yr)
    }
    else {
      gplot <- gplot + geom_line(aes(x = pred.x, y = yhat)) + 
        xlab("Time") + ylab(ylab[i]) + scale_x_continuous(breaks = xaxisval, 
                                                          labels = xaxislab) + ylim(yr)
    }
    if (resid) 
      gplot <- gplot + geom_point(aes(x = xx, y = y), col = point.col, 
                                  shape = point.shape, size = point.size, alpha = point.alpha)
    if (se) {
      gplot <- gplot + geom_line(aes(x = pred.x, y = yup), 
                                 lty = "dashed") + geom_line(aes(x = pred.x, y = ylow), 
                                                             lty = "dashed")
    }
    # gplot <- .labs(p = gplot, font.main = font.main, font.x = font.x, 
    #                font.y = font.y)
    # gplot <- .set_ticks(gplot, font.tickslab = font.tickslab)
  })
  #browser()
  names(plots) <- var
  #class(plots) <- c("ggcoxzph", "list")
  if(nrow(x$table)>1) attr(plots, "global_pval") <- x$table["GLOBAL", 3]
  plots
}


