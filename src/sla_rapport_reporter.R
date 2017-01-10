source("src/survie_univ.R")

mydoc <- docx (title="survie univariée SLA")

addParagraph(mydoc,"Survie univariée SLA", stylename = "TitleDoc")
addTOC(mydoc)

addPageBreak(mydoc)

#addParagraph(mydoc,"DATE DE DEBUT = DATE DES 1er SYMPTOMES", stylename = "Titre1")
addTitle(mydoc,"DATE DE DEBUT = DATE DES 1er SYMPTOMES", level=1)


#Description
addTitle(mydoc,"Description", level=2) 
addParagraph(mydoc, c("", ""))

addParagraph(mydoc, value= c('range date de début : ', paste(as.character(deb1)[1],"–",as.character(deb1)[2])), stylename="Normal")

addPlot( doc = mydoc, fun =  function()plot(sym.suiv,xscale=365.25, yscale= 100, xlab="time (years)"))
addParagraph( mydoc, value = "Courbe de suivi", stylename = "rPlotLegend")

addPlot( doc = mydoc, fun =  function()plot(sym.surv,xscale=365.25, yscale= 100, xlab="time (years)"))
addParagraph( mydoc, value = "Courbe de survie", stylename = "rPlotLegend")

addParagraph(mydoc, value= c('médiane de suvie : ', paste0(med1," jours soit ", round(med1/325.25,2), " années."), stylename="Normal"))


#survie selon baseline
addTitle(mydoc,"survie selon baseline", level=2)
addParagraph(mydoc, c("", ""))


#Description des baselines
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
addFlexTable(mydoc,MyFTable)
addParagraph(mydoc, value = "caractéristique des patients", stylename = "rTableLegend")
addParagraph(mydoc, c("", ""))

#Modèle de Cox 
addTitle(mydoc,"Modèle de Cox : conditions de validité", level=3)
addParagraph(mydoc, "")
addTitle(mydoc,"Risques proportionnels", level=4)
addParagraph(mydoc, "")


#https://github.com/davidgohel/ReporteRs/issues/44


p <- lapply(var_bl[-4],function(x){
 b <- cox.zph(coxph(Surv(sla$time.sym,sla$censor)~sla[,x]))
 #browser()
 ggcoxzph.1var(b,var=x)
})
myggplot2 = p
#addPlot(doc, print,x= myggplot2)

base_legend = "Risque en fonction du temps pour "
#addSection(mydoc, ncol = 2, space_between = 0.1)
for( index in seq_along(p) ){
  mylegend = pot(base_legend, textBoldItalic() )
  mylegend = paste0(base_legend, var_bl[-4][index] )
  doc = addPlot( doc = mydoc, fun = print, x = p[[index]], width=6, heigth=2)
  doc = addParagraph( mydoc, value = mylegend, stylename = "rPlotLegend")
}
#addSection(mydoc,  ncol = 1 )

b <- cox.zph(coxph(Surv(sla$time.sym,sla$censor)~sla[,var_bl[4]]))
p <- ggcoxzph.1var(b,var2=var_bl[4])
base_legend = "Risque en fonction du temps pour "
for( index in seq_along(p) ){
  mylegend = pot(base_legend, textBoldItalic() )
  mylegend = paste0(base_legend, var_bl[-4][index] )
  doc = addPlot( doc = mydoc, fun = print, x = p[[index]], width=6, heigth=2)
  doc = addParagraph( mydoc, value = mylegend, stylename = "rPlotLegend")
}


addParagraph(mydoc, "")
addTitle(mydoc,"Loglinéarité", level=4)
addParagraph(mydoc, "")


xa <- lapply(var_quanti, function(i){
  a <- coxph(Surv(sla$time.vni,sla$censor)~sla[,i])
  res <- residuals(a, type="martingale")
  X <- sla[,i]
  X <- X[!is.na(sla[,i])]
  par(mfrow=c(2,2))
  #plot(sla[!is.na(sla[,i]),i],res,xlab=i,ylab="residuals")
  plot(X,res,xlab=i,ylab="residuals")
  abline(h=0,lty=2)
  #lines(lowess(sla[!is.na(sla[,i]),i],res,iter=0))
  lines(lowess(X,res,iter=0))
  
  b <- coef(a)
  plot(X,b*X+res,xlab=i,ylab="component+residuals")
  abline(lm(b*X+res~X),lty=2)
  lines(lowess(X,b*X+res,iter=0))
})

pl <- lapply(colnames(dl0)[-1], function(.x) qplot(as.factor(dl0[,.x]),xlab=NULL, main=.x, fill=I("navajowhite3"), col=I("pink4")))
ml <- marrangeGrob(xa,ncol=3,nrow=2,top = NULL)
print(ml)





writeDoc(mydoc , file = "C:/Users/4051268/Documents/writing/survie_univ_SLA.docx")



