
# Créer les dossiers où seront stockés les fichiers
dir.create("Data Base/nhanes2013_2014", recursive = TRUE, showWarnings = FALSE)
dir.create("Data Base/nhanes2015_2016", recursive = TRUE, showWarnings = FALSE)

# Définir la base URL 2013-2014 et 2015-2016
base_url_2013_2014 <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/"
base_url_2015_2016 <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/"

# Vecteur des noms de fichiers (sans l’extension ni la partie _H ou _I)
files_base_names <- c("DEMO", "DSQTOT", "DR1TOT", "DR2TOT", "BMX", "MGX", "TST", "TCHOL", "HDL", "TRIGLY", "GLU", "INS", "COT", "BIOPRO", "CUSEZN", "BPQ", "VID", "RXQ_RX", "PAQ", "SMQ", "ALQ", "SLQ", "DPQ")

# On ajoute l'extension
files_2013_2014 <- paste0(files_base_names, "_H.xpt")
files_2015_2016 <- paste0(files_base_names, "_I.xpt")

# Fonction de téléchargement
download_nhanes_file <- function(base_url, file_name, dest_folder){
  dest_file <- file.path(dest_folder, file_name) #création du chemin de destination
  
  if(!file.exists(file_name)){
    tryCatch({ #tryCatch pour qu'il ne s'arrête pas dès le premier echec
      download.file(paste0(base_url, file_name), destfile = dest_file, mode = "wb") #téléchargement avec concaténation du lien complet
      message(paste("Téléchargé: ", file_name)) #Indique que le fichier a bien été téléchargé
    }, error = function(e){
      message(paste("Échec du téléchargement pour:", file_name))
      })
  } else {
    message(paste("Déjà présent: ", file_name)) #si jamais le fichier est déjà présent
  }
}



# Boucles de téléchargement

for (f in files_2013_2014) {
  download_nhanes_file(base_url_2013_2014, f, "Data Base/nhanes2013_2014")
}

for (f in files_2015_2016) {
  download_nhanes_file(base_url_2015_2016, f, "Data Base/nhanes2015_2016")
}
