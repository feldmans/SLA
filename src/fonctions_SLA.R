library(stringr)
library(dplyr)


manage_date_ND <- function(vec){ #vec doit être un vecteur avec éléments de la forme 04/04/1989(facteur) ou "04/04/1989"(character)
  vec <- as.character(vec)
  #browser()
  exist_year <-!is.na(str_sub(vec, 7, 10))
  
  if (!length(grep("ND",vec[exist_year]))==0) {
    vec[exist_year] <- gsub("ND/ND", "01/07",vec,fixed=T)
    vec[exist_year] <- gsub("ND", "15",vec,fixed=T)
  }
  vec_d <- as.Date(vec,"%d/%m/%Y")
  return(vec_d)
}

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