

.dir <- dir("C:/Users/4051268/Documents/sauvegarde data/sla/data/",full.names = T, recursive = T)

.dir_csv <- .dir[str_sub(.dir, -3, -1)=="csv"]
.dir_sas <- .dir[str_sub(.dir, -3, -1)=="sas"]

#Pour charger toutes les bases de données disponibles (se nommeront bdd 1 à 9)
for (i in .dir_csv) {
  print(i)
  num <- which(.dir_csv==i)
  a <- read.csv2(i)
  assign(paste0("bdd",num),a)
}
bdds <- paste0("bdd",1:9)