doc <- docx (title="survie univariée SLA")

addParagraph(doc,"Survie univariée SLA", stylename = "TitleDoc")
addTOC(doc)

addPageBreak(doc)

#addParagraph(doc,"DATE DE DEBUT = DATE DES 1er SYMPTOMES", stylename = "Titre1")
addTitle(doc,"DATE DE DEBUT = DATE DES 1er SYMPTOMES", level=1)


#Description
addTitle(doc,"Description", level=2) 
addParagraph(doc, c("", ""))

addParagraph(doc, value= c('range date de début : ', paste(as.character(deb1)[1],"–",as.character(deb1)[2])), stylename="Normal")

addPlot( doc = doc, fun =  function()plot(idc.suiv,xscale=365.25, yscale= 100, xlab="time (years)"))
addParagraph( doc, value = "Courbe de suivi", stylename = "rPlotLegend")

addPlot( doc = doc, fun =  function()plot(idc.surv,xscale=365.25, yscale= 100, xlab="time (years)"))
addParagraph( doc, value = "Courbe de survie", stylename = "rPlotLegend")

addParagraph(doc, value= c('médiane de suvie : ', paste0(med1," jours soit ", round(med1/325.25,2), " années."), stylename="Normal"))


#survie selon baseline
addTitle(doc,"survie selon baseline", level=2)
addParagraph(doc, c("", ""))

table_var_quali <- lapply(var_quali, function(i){
  data <- sla[,i]
  names_levels <- levels(as.factor(data))
  a <- lapply(names_levels, function(x) {
    tmp <- as.numeric(table(data)[x])
    tmpbis <- round(as.numeric(prop.table(table(data))[x]),3)*100
    tmptot <- paste0(tmp," (",tmpbis,"%)")
    
    nNA <- table(is.na(data))
    pNA <- round(prop.table(table(is.na(data))),3)
    if (is.na(nNA[2]))  {
      if (which(names_levels==x)==1) nNA <- paste0 (0," (0%)")
      else nNA <- NA
    }
    else {
      if (which(names_levels==x)==1){
        nNA <- as.numeric (nNA[names(nNA)==TRUE])
        pNA <- as.numeric (pNA[names(pNA)==TRUE])*100
        nNA <- paste0(nNA," (",pNA,"%)")  
      }
      else nNA <- NA
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

table_var_quanti <- lapply(var_quanti, function(i){ #median ou moyenne? (sachant qu'on ne vérifie pas normalité des baselines)
  data <- sla[,i]
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
  rownames(a) <- paste0(i,"*")
  colnames(a) <- c("valeur","missing values")
  return(a)
})
table_var_quanti <- do.call(rbind,table_var_quanti)

table_var <- rbind(table_var_quali,table_var_quanti)

MyFTable <- FlexTable(data=table_var,add.rownames=T)
setZebraStyle(MyFTable,odd="#E5E8E8",even="white")#"#E1EEf4"
addFlexTable(doc,MyFTable)
addParagraph(doc, value = "caractéristique des patients", stylename = "rTableLegend")

writeDoc(doc , file = "C:/Users/4051268/Documents/writing/survie_univ_SLA.docx")



#if( requireNamespace("ggplot2", quietly = TRUE) ){
#addTitle( doc, "Courbe de suivi", level = 2 )

# create a ggplot2 plot
myplot = plot(idc.suiv,xscale=365.25, yscale= 100, xlab="time (years)")

# Add myplot into object doc
addPlot( doc = doc, fun =  function()plot(idc.suiv,xscale=365.25, yscale= 100, xlab="time (years)"))
#addPlot( doc = doc, fun = print, x=myplot) #pour ggplot2

# Add a legend below the plot
addParagraph( doc, value = "Courbe de suivi", stylename = "rPlotLegend")
#}