# 📦 LIBRAIRIES
  
# Liste des packages nécessaires au projet
  packages <- c("haven", "dplyr", "purrr", "tools","tidyr", "stargazer", "broom", "gt", "knitr", "kableExtra", "ggplot2", "tinytex", "tidyverse", "rmarkdown","zoo", "ggpubr", "gmodels", "car", "gtsummary", "flextable", "lmtest", "ResourceSelection")
  
  # Fonction qui installe un package s'il n'est pas déjà installé
  install_if_missing <- function(pkg) {
    if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
  }
  
  # Applique la fonction d'installation à tous les packages
  invisible(lapply(packages, install_if_missing))
  
  # Charge tous les packages en mémoire (après installation)
  lapply(packages, library, character.only = TRUE)
  
  # Définit la langue du système à "français" pour les messages (utile pour certains affichages ou erreurs)
  Sys.setenv(LANGUAGE = "fr") # Affichage en français


# 📁 FONCTION POUR CHARGER ET FUSIONNER LES FICHIERS NHANES D'UN CYCLE

load_and_merge_nhanes <- function(path){
  fichiers <- list.files(path=path, pattern="(DEMO|DSQTOT|DR1TOT|DR2TOT|BMX|MGX|TST|TCHOL|HDL|TRIGLY|GLU|INS|COT|BIOPRO|CUSEZN|BPQ|VID|RXQ_RX|PAQ|SMQ|ALQ|SLQ|DPQ)_[HI]\\.xpt$",
                         full.names=TRUE, recursive=FALSE) #On cherche dans le dossiers tous les datasets .xpt nécessaires à notre étude
  
  data_list <- lapply(fichiers, read_xpt) # créer une liste de tous les datasets utiles
  names(data_list) <- tools::file_path_sans_ext(basename(fichiers)) #attribue un nom à chaque élément de la liste data_list
  
  # Filtrer les datasets qui contiennent SEQN
  data_list_seqn <- data_list[sapply(data_list, function(df) is.data.frame(df) && "SEQN" %in% names(df))]

  # Extraire les individus sous théapie de remplacement de testostérone (TRT) de RXQ_RX si elle existe
  if(length(grep("RXQ_RX_[HI]\\.xpt$", fichiers) > 0 )){
    codes_TRT <- c("d03245", "d03873", "d03389", "d00295", "d00558", "d04273") # Molécules: méthyltestostérone, anastrozole, testostérone, testostérone + (progesterone) + oestradiol qui prises lors d'une TRT
    rxq_rx_name <- names(data_list_seqn)[grep("^RXQ_RX_[HI]$", names(data_list_seqn))][1] # Récupère le bon nom de RXQ_RX (H ou I selon l'année)
    RXQ_TRT <- data_list_seqn[[rxq_rx_name]] %>% #extrait un jeu de donnée présent dans data_list_seqn qu'on nomme rxq_rx_name
      filter(!is.na(RXDDRGID)) %>% # garde que les RXDDRGID non manquants
      group_by(SEQN) %>% #regroupe par participants
      summarise(sous_TRT = as.integer(any(RXDDRGID %in% codes_TRT)), .groups = "drop") #crée une variable `sous_TRT` qui vaut 1 si au moins un des médicaments est un traitement de substitution (TRT), 0 sinon (as.integer), et enlève l'effet du group by après summarisation (.groups = "drop")
    data_list_seqn[rxq_rx_name] <- NULL 
  }

  # Fusionner toutes les autres tables par SEQN
  merged_data <- Reduce(function(x,y) merge(x,y, by="SEQN", all=TRUE), data_list_seqn)
  
  # # Ajouter la variable TRT si elle existe
  if(!is.null(RXQ_TRT)){
    merged_data <- merge(merged_data, RXQ_TRT, by="SEQN", all.x=TRUE)
    merged_data$sous_TRT[is.na(merged_data$sous_TRT)] <- FALSE
  } else {
    merged_data$sous_TRT <- FALSE
  }
  
  return(merged_data)
}


# ⚖️ FONCTION POUR COMPARER LES COLONNES DE 2 DATAFRAMES A PARTIR D'UNE LISTE DE VARS

compare_colnames <- function(vars, df1, df2){
  
  # Récupère les noms des objects passés
  name1 <- deparse(substitute(df1))
  name2 <- deparse(substitute(df2))
  
  # Filtre les variables spécifiées pour voir celles qui sont dans chaque dataframe
  vars_in_df1 <- vars[vars %in% names(df1)]
  vars_in_df2 <- vars[vars %in% names(df2)]
  
  # Compare les deux df
  only_in_df1 <- setdiff(vars_in_df1, vars_in_df2)
  only_in_df2 <- setdiff(vars_in_df2, vars_in_df1)
  absent_in_both <- setdiff(vars, union(vars_in_df1, vars_in_df2))
  
  # Retourne un résultat avec des noms clairs
  result <- list()
  result[[paste0("Absent dans ", name1, " et ", name2)]] <- absent_in_both
  result[[paste0("Absent dans ", name2, " mais présent dans ", name1, ": ")]] <- only_in_df1
  result[[paste0("Absent dans  ", name1, " mais présent dans ", name2, ": ")]] <- only_in_df2
  
  return(result)
}

# ✂️ FONCTION SUPPRIMER LES VALEURS EXTRÊMES PAR POURCENTILE

remove_pourcentile <- function(df, variables, lower_pct, upper_pct){
  df_final <- df
  total_initial <- nrow(df)
  for(var in variables){
    n_before <- nrow(df_final) #Nombre initial d'obs
    lower_bound <- quantile(df_final[[var]], lower_pct) #Calcule de bornes
    upper_bound <- quantile(df_final[[var]], upper_pct)
    df_final <- df_final[df_final[[var]] >= lower_bound & df_final[[var]] <= upper_bound, ] #Filtrage
    n_after <- nrow(df_final) #Nombre après filtrage
    cat(sprintf("🧪 Variables '%s' : %d observation supprimées (%.2f%%)\n", 
                var, n_before-n_after, 100*(n_before-n_after)/total_initial))
  }
  return(df_final)
}

  # 🤓 VARIABLES INTERESSANTES

vars <- c(
  #-----------------Demo Data--------------------
  # -----DEMO-----
  "SEQN", #ID 0%
  "RIAGENDR", #Genre 0%
  "RIDAGEYR", #Age (années) 0%
  "RIDRETH3", #Race 0%
  
  #----------------Diet Data---------------------
  # -----DSQTOT-----
  # "DSQTZINC", #Zinc (mg) 76% val manq
  # "DSQTVD", #Vit D (D2 + D3) (mcg) 69% val manq
  # "DSQTMAGN", #Magnesium (mg) 84% val manq
  
  # -----DR1TOT----- 1er jour de questionnement sur habitudes alimentaires
  "DR1TKCAL", #kcal 16%
  # "DR1TPROT", #Protéines totales (gm) 16%
  # "DR1TTFAT", #Lipides totaux (gm) 16%
  # "DR1TCHOL", #Cholestérol (mg) 16%
  "DR1TSUGR", #Sucres totaux (gm) 16%
  # "DR1TSODI", #Sodium (mg) 16%
  # "DR1TPOTA", #Potassium (mg) 16%
  "DR1TALCO", #Alcohol (gm) 16%
  "DR1TMAGN", #Magnesium (mg) 16%
  "DR1TZINC", #Zinc (mg) 16%
  # "DR1_320Z", #Total plain water drank yesterday (gm) 15%
  
  
  # -----DR2TOT----- 2e jour de questionnement, 3 à 10j après 1er
  "DR2TKCAL", #kcal 29%
  # "DR2TPROT", #Protéines totales (gm) 29%
  # "DR2TTFAT", #Lipides totaux (gm) 29%
  # "DR2TCHOL", #Cholestérol (mg) 29%
  "DR2TSUGR", #Sucres totaux (gm) 29%
  # "DR2TSODI", #Sodium (mg) 29%
  # "DR2TPOTA", #Potassium (mg) 29%
  "DR2TALCO", #Alcohol (gm) 29%
  "DR2TMAGN", #Magnesium (mg) 29%
  "DR2TZINC", #Zinc (mg) 29%
  # "DR2_320Z", #Total plain water drank yesterday (gm) 28%
  
  
  #------------------Exam Data-------------------
  # -----BMX-----
  "BMXBMI", #IMC (kg/m**2) 12%
  # "BMXWT", #Poid (Kg) 5%
  # "BMXHT", #Taille (cm) 11%
  "BMXWAIST", #Tour taille (cm) 16%
  # "BMXARMC", #Circonf bras BMXARMC 9%
  
  # -----MGX-----  
  # "MGDCGSZ", #Somme max_2 force/mains (kg) n'existe pas pour 2015_2016
  
  #------------------Labo Data--------------------
  # -----TST-----
  "LBXTST", #Testosterone totale (ng/dL) 28%
  "LBXEST", #Estradiol (pg/mL) 28%
  # "LBDESTLC", #Estradiol Comment Code 28%
  "LBXSHBG", #SHBG (nmol/L) 33%
  # "LBDSHGLC", #SHBG Comment Code 33%
  
  # -----TCHOL-----
  "LBXTC", #Total Cholesterol( mg/dL) 26%
  # "LBDTCSI", #Total Cholesterol( mmol/L) 26%
  
  # -----HDL-----
  "LBDHDD", #HDL Cholesterol (mg/dL) 26%
  
  # -----TRIGLY-----
  # "LBXTR", #Triglyceride (mg/dL) 70%
  # "LBDTRSI", #Triglyceride (mmol/L) 71%
  # "LBDLDL", #LDL-cholesterol (mg/dL) 71%
  # "LBDLDLSI", #LDL-cholesterol (mmol/L) 71%
  
  # -----GLU-----
  # "LBXGLU", #Glucose (mg/dL) 70%
  
  # -----INS-----
  # "LBXSAL", #Insuline (uU/mL) 36%
  
  # -----VITD-----
  "LBXVIDMS", #Vitamine D (nmol/L) 18%
  
  # -----CUSEZN-----
  # "LBXSZN", #Serum Zinc (ug/dL) 75%
  # "LBDSZNSI", #Serum Zinc (umol/L) 75%
  
  # -----BIOPRO-----
  "LBDSALSI", #Albumin (g/L) 36%
  # "LBXSCR", #Creatinine (mg/dL) 36%
  "LBDSTPSI", #Total protein (g/L) 36%
  
  # -----COT-----
  "LBXCOT", #Cotinine, Serum (ng/mL) (nicotine) 22%
  
  #------------------Questionnary data----------------
  
  # -----BPQ-----
  # "BPQ080", #Doctor told you - high cholesterol level 37%
  
  # -----PAQ-----
  "PAQ650", #Vigorous recreational activities 30%
  "PAQ665", #Moderate recreational activities 30%
  "PAD680", #Minutes sedentary activity 30%
  
  # -----SMQ-----
  # "SMQ040", #Tabagisme actuel 75%
  
  # -----ALQ-----
  # "ALQ120Q", #Fréquence consommation d’alcool 56%
  
  # -----SLQ-----
  "SLD010H", "SLD012", #(2015) #How much sleep do you get (hours)? 37%
  # "SLQ050", #Ever told doctor had trouble sleeping? 37%
  
  # -----DPQ_H-----
  # "DPQ010", #Have little interest in doing things 48%
  # "DPQ020", #Feeling down, depressed, or hopeless 48%
  # "DPQ030", #Trouble sleeping or sleeping too much 48%
  # "DPQ040", #Feeling tired or having little energy 48%
  # "DPQ070", #Trouble concentrating on things 48%
  # 
  # -----RXQ_RX-----
  "sous_TRT" #Variable créée dans la fonction load_and_merge_nhanes 0%
)
#-------------------------------------------------------------------------------------------------------------#

# 🧬 CHARGEMENT DES CYCLES

chemin_2013_2014 <- file.path(getwd(), "Data Base/nhanes2013_2014")
chemin_2015_2016 <- file.path(getwd(), "Data Base/nhanes2015_2016")
chemin_2013_2014

data_base_2013_2014 <- load_and_merge_nhanes(chemin_2013_2014)
data_base_2015_2016 <- load_and_merge_nhanes(chemin_2015_2016)

# Check sur la présence des variables
compare_colnames(vars, data_base_2013_2014, data_base_2015_2016)

# Ajustements
names(data_base_2015_2016)[names(data_base_2015_2016)=="SLD012"] <- "SLD010H"


# 🔗 CONCATENATION DES TABLES SEULEMENT SUR LES COLONNES COMMUNES A PARTIR D'UNE LISTE DE COLONNES: VARS

colonnes_comm_vars <- intersect(vars,intersect(names(data_base_2013_2014), names(data_base_2015_2016)))
data_base_2013_2016 <- rbind(data_base_2013_2014[, colonnes_comm_vars], data_base_2015_2016[, colonnes_comm_vars])
setdiff(vars, names(data_base_2013_2016)) #pour voir que toutes les variables sont bien importées sauf exceptions

# 🧹 TRAITEMENT DES VARIABLES

# Traimement des valeurs manquantes
any(duplicated(data_base_2013_2016$SEQN)) # Vérification des doublons d'obs au sein de seqn
colMeans(is.na(data_base_2013_2016)) #On affiche les taux de val manquantes par colonnes
tolerance <- 0.4 # seuil de tolerance
data_base_2013_2016 <- data_base_2013_2016[, colMeans(is.na(data_base_2013_2016))<=tolerance] #On garde les colonnes <= seuil
data_base_2013_2016 <- drop_na(data_base_2013_2016) #Suppression de toutes les valeurs manquantes

# Garder la population qu'on souhaite analyser
str(data_base_2013_2016) #Structure générale de la BDD
data_base_2013_2016 <- data_base_2013_2016[data_base_2013_2016$RIAGENDR == 1,] #On ne garde que les hommmes
data_base_2013_2016 <- subset(data_base_2013_2016, RIDAGEYR > 17 & RIDAGEYR < 71) # Agés de 18 à 71 ans
data_base_2013_2016 <- data_base_2013_2016[data_base_2013_2016$sous_TRT != 1, ] # Je garde ceux sous TRT

# Créer les variables composites
# 1- créer les moyennes sur DR1 et DR2
data_base_2013_2016$DRTKCAL <- (data_base_2013_2016$DR1TKCAL + data_base_2013_2016$DR2TKCAL) / 2 # Moyenne des apports caloriques
data_base_2013_2016$DRTSUGR <- (data_base_2013_2016$DR1TSUGR + data_base_2013_2016$DR2TSUGR) / 2 # Moyenne des sucres totaux
data_base_2013_2016$DRTALCO <- (data_base_2013_2016$DR1TALCO + data_base_2013_2016$DR2TALCO) / 2 # Moyenne de l'alcool
data_base_2013_2016$DRTMAGN <- (data_base_2013_2016$DR1TMAGN + data_base_2013_2016$DR2TMAGN) / 2 # Moyenne du magnésium
data_base_2013_2016$DRTZINC <- (data_base_2013_2016$DR1TZINC + data_base_2013_2016$DR2TZINC) / 2 # Moyenne du zinc

data_base_2013_2016 <- data_base_2013_2016[, -grep("DR[1_2]", names(data_base_2013_2016))] #Suppression des anciennes variables

# Suppression des valeurs extrêmes
print(sapply(data_base_2013_2016, min)) #On affiche les valeurs extrêmes
print(sapply(data_base_2013_2016, max))
variable_supp_extr <- c("RIAGENDR", "RIDAGEYR", "RIDRETH3", "DRTKCAL", "DRTSUGR", "DRTALCO","DRTMAGN","DRTZINC", "BMXBMI", 
                        "BMXWAIST", "LBXTST", "LBXEST", 
"LBXSHBG", "LBXTC", "LBDHDD", "LBXVIDMS", "LBDSALSI", "LBDSTPSI", "LBXCOT", "PAQ650", "PAQ665", "PAD680", "SLD010H") # Pas de bilan lipidique dans les analyses car trop de valeurs manquantes, comme les troubles de la dépression
dataset_final <- remove_pourcentile(data_base_2013_2016, variable_supp_extr, 0.005, 0.995)

# 2- Création des variables dichotomiques
unique(dataset_final$PAQ665) # renvoie valeurs distinctes dans la colonne
dataset_final$PAQ650 <- ifelse(dataset_final$PAQ650 == 2, 0, 1) # créer les dichotomiques pour le sport ou non (1,0)
dataset_final$PAQ665 <- ifelse(dataset_final$PAQ665 == 2, 0, 1)
unique(dataset_final$RIDRETH3)
dataset_final$RIDRETH3 <- as.factor(dataset_final$RIDRETH3) # informer R que c'est une variable qualitative
levels(data_base_2013_2016$RIDRETH3) # on vérifie les modalités
dataset_final$hypogonadisme <- ifelse(dataset_final$LBXTST < 300, 1, 0) # si l'individus souffre d'hypogonadisme ou non (variable à étudier)


# 📊 STATISTIQUES DESCRIPTIVES

labels <- c(
  RIDAGEYR = "Âge (années)",
  LBXTST = "Testostérone (ng/dL)",
  LBXEST = "Estradiol (pg/mL)",
  DRTKCAL = "Calories consommées (kcal)",
  DRTSUGR = "Sucres consommés (g)",
  DRTALCO = "Alcool consommé (g)",
  DRTMAGN = "Magnésium consommé (mg)",
  DRTZINC = "Zinc consommé (mg)",
  PAQ650 = "Activité physique modérée",
  PAQ665 = "Activité physique intense",
  PAD680 = "Durée d’activité assise (min)",
  SLD010H = "Temps de sommeil (heures)",
  BMXBMI = "IMC (kg/m²)",
  BMXWAIST = "Tour de taille (cm)",
  LBXSHBG = "SHBG (protéine liant les hormones sexuelles)",
  LBXTC = "Cholestérol total (mg/dL)",
  LBDHDD = "Cholestérol HDL (mg/dL)",
  LBXVIDMS = "Vitamine D (nmol/L)",
  LBDSALSI = "Albumine (g/L)",
  LBDSTPSI = "Protéines totales (g/L)",
  LBXCOT = "Cotinine (nicotine, ng/mL)"
)

#----------Moyenne et médiane sur toutes les variables:----------

# Sélection des variables numériques uniquement
vars_num <- dataset_final[, c("RIDAGEYR", "LBXTST", "DRTKCAL", "DRTSUGR", "DRTALCO", "DRTMAGN", "DRTZINC", 
                              "PAQ650", "PAQ665", "PAD680", "SLD010H", "BMXBMI", "BMXWAIST", 
                              "LBXSHBG", "LBXTC", "LBDHDD", "LBXVIDMS", "LBDSALSI", 
                              "LBDSTPSI", "LBXCOT")]
# Stats générales sur nos variables
summary_stats <- data.frame(
  Variable = labels[colnames(vars_num)],
  Minimum = sapply(vars_num, min, na.rm = TRUE),
  Maximum = sapply(vars_num, max, na.rm = TRUE),
  Moyenne = sapply(vars_num, mean, na.rm = TRUE),
  Mediane = sapply(vars_num, median, na.rm = TRUE),
  SD = sapply(vars_num, sd, na.rm = TRUE)
)

knitr::kable(summary_stats, digits = 2, caption = "Statistiques descriptives des variables numériques") %>%
  kableExtra::kable_styling(full_width = FALSE)
cat("Nombre d'observations :", nrow(vars_num), "\n")
cat("Nombre de variables :", ncol(vars_num), "\n")


#----------Croisement âge / testostérone----------

# Tests de normalité de la répartition de l'age et du taux de testostérone
shapiro.test(dataset_final$RIDAGEYR)
shapiro.test(dataset_final$RIDAGEYR)

# Calculer la moyenne de testostérone par âge
mean_testo_age <- dataset_final %>%
  group_by(RIDAGEYR) %>%
  summarise(moyenne_testo = mean(LBXTST, na.rm = TRUE)) %>%
  arrange(RIDAGEYR) %>%
  mutate(moyenne_mobile = zoo::rollmean(moyenne_testo, k = 5, fill = NA, align = "center"))

# Graphique moyenne testostérone en fonction de l'âge avec moyenne mobile
ggplot(mean_testo_age, aes(x = RIDAGEYR)) +
  geom_line(aes(y = moyenne_testo), color = "blue", size = 1) +
  geom_point(aes(y = moyenne_testo), color = "darkblue") +
  geom_line(aes(y = moyenne_mobile), color = "green", size = 1, linetype = "solid") +
  labs(
    x = "Âge (années)",
    y = "Taux moyen de testostérone (ng/dL)",
    title = "Évolution moyenne de la testostérone selon l'âge",
    subtitle = "Ligne rouge : moyenne mobile (k=3)"
  ) +
  theme_minimal()

# Test de corrélaton de Spearman:
cor.test(dataset_final$RIDAGEYR, dataset_final$LBXTST, method = "spearman")

#------------- Croisemment cholesterol HDL / Testostérone -------------

ggplot(dataset_final, aes(x = LBDHDD, y = LBXTST)) +
  stat_density2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_viridis_c() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = labels["LBDHDD"], 
       y = labels["LBXTST"],
       title = paste("Taux de", labels["LBXTST"], "en fonction du", labels["LBDHDD"], "dans le sang"))

# Calcul de la corrélation de Spearman
cor.test(dataset_final$LBDHDD, dataset_final$LBXTST, method = "spearman")

#------------- Croisemment vitamine D / Testostérone -------------

ggplot(dataset_final, aes(x = LBXVIDMS, y = LBXTST)) + stat_density2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_viridis_c() + geom_smooth(method = "lm", se=FALSE) + labs(
    x = labels["LBXVIDMS"],
    y = labels["LBXTST"],
    title = paste("Taux de", labels["LBXTST"], "en fonction du taux de", labels["LBXVIDMS"], "dans le sang")
  )

# Calcul de la corrélation de Spearman
cor.test(dataset_final$LBXVIDMS, dataset_final$LBXTST, method = "spearman")

#------------ Tour de taille (cm) / testostérone ----------

ggplot(dataset_final, aes(x = BMXWAIST, y = LBXTST)) + geom_point(shape = 21, color = "black", fill = "white",
                                                                  size = 2, stroke = 0.7) + geom_smooth(method = "lm", color="green") + labs(
  x = labels["BMXWAIST"],
  y = labels["LBXTST"],
  title = paste(labels["LBXTST"], "en fonction du", labels["BMXWAIST"])
)

# Calcul de la corrélation de Spearman
cor_test <- cor.test(dataset_final$BMXWAIST, dataset_final$LBXTST, method = "spearman")

# Affichage des résultats
cat("Corrélation de Spearman rho =", round(cor_test$estimate, 3), "\n")
cat("p-value =", signif(cor_test$p.value, 3), "\n")

#------------ Croisement temps de sommeil / testostérone ----------

# Création de groupe pour simplifier le diagramme à boite à moustaches
testo_sommeil <- dataset_final %>%
  mutate(sleep_group = cut(SLD010H,
                           breaks = c(0, 5, 6, 7, 8, Inf),
                           labels = c("<5h", "5-6h", "6-7h", "7-8h", ">8h")))

ggplot(testo_sommeil, aes(x = sleep_group, y = LBXTST)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Temps de sommeil (heures)", y = "Testostérone (ng/dL)",
       title = "Testostérone selon groupes de sommeil")

# test ANOVA
anova_result <- aov(LBXTST ~ sleep_group, data = testo_sommeil)
summary(anova_result)

#------------------activité physique / Hypogonadisme -----------------

# Hypogonadisme (0 = non, 1 = oui)
dataset_final$hypogonadisme_lab <- factor(dataset_final$hypogonadisme,
                                          levels = c(0, 1),
                                          labels = c("Pas d'hypogonadisme", "Hypogonadisme"))

# PAQ650 (activité physique modérée, 0 = non, 1 = oui)
dataset_final$PAQ650_lab <- factor(dataset_final$PAQ650,
                                   levels = c(0, 1),
                                   labels = c("Pas d'activité", "Activité modérée"))


# Test de khi²:

tab <- table(dataset_final$hypogonadisme_lab, dataset_final$PAQ650_lab)
test_chi2 <- chisq.test(tab)


#-------------------Taux estradiol / Hypogonadisme-------------------
ggplot(dataset_final, aes(x = LBXEST, fill = factor(hypogonadisme))) +
  geom_density(alpha = 0.5) +
  labs(x = labels["LBXEST"],
       fill = "Hypogonadisme",
       title = "Densité du taux d'œstradiol selon l'hypogonadisme") +
  theme_minimal()

# test de Wolcoxon pour tester la relation entre oestradiol et hypogonadisme
wilcox.test(LBXEST ~ hypogonadisme, data = dataset_final)


# 🧑🔬 MODELES STATISTIQUES

#-----------------------------Modèle MCO sur le mode de vie--------------------------------
modele_mode_vie <- lm(LBXTST ~ RIDAGEYR + DRTKCAL + DRTSUGR + DRTALCO + DRTMAGN + DRTZINC + PAQ650 + PAQ665 + PAD680 + 
                        SLD010H , data = dataset_final)

# colinéarité dans notre modèle ? 
#≈ 1 pas de multico, 1-5 légère, >5 élevée

vif(modele_mode_vie)
summary(modele_mode_vie)

#-------------------Modèle MCO sur les charactéristiques physique et les analyses laboratoires------------------
dataset_final$RIDRETH3 <- factor(dataset_final$RIDRETH3,
                                    levels = c(1, 2, 3, 4, 6, 7), # on force RIDRETH3 a être catégorielle 
                                    labels = c("Mexicain Americain", "Autres Hispaniques", "Non-Hispanique Blanc",
                                               "Non-Hispanique Noir", "Non-Hispanique Asiatique", "Autre et multi ethnie")) 

dataset_final$RIDRETH3 <- relevel(dataset_final$RIDRETH3, ref = "Non-Hispanique Blanc") # la modalité de référence

modele_physiologique <- lm(LBXTST ~ RIDAGEYR + RIDRETH3 + BMXBMI + BMXWAIST + LBXSHBG + LBXTC + LBDHDD + LBXVIDMS +
                             LBDSALSI + LBDSTPSI + LBXCOT, data = dataset_final) # méthodes des MCO (moindres carrés ordinaires)

# colinéarité:
vif(modele_physiologique)
#Les résultats du VIF montrent une multicolinéarité modérée principalement entre BMXBMI (IMC) et BMXWAIST (tour de taille), mais dans l’ensemble, les niveaux restent acceptables, ce qui permet de conserver toutes les variables dans les modèles sans risque majeur.

summary(modele_physiologique)

#--------------------------Modèle logistique global-------------------------------

modele_logistique <- glm(hypogonadisme ~ RIDAGEYR + DRTKCAL + DRTSUGR + DRTALCO + DRTMAGN + DRTZINC + PAQ650 + PAQ665 + PAD680 + 
                     SLD010H + RIDRETH3 + BMXBMI + BMXWAIST + LBXSHBG + LBXTC + LBDHDD + LBXVIDMS + LBDSALSI + LBDSTPSI + LBXCOT,
                   family = binomial, data = dataset_final)

vif(modele_logistique)

summary(modele_logistique)

exp(cbind(OR = coef(modele_logistique), confint(modele_logistique))) # résumé clair et interprétable d’un modèle de régression logistique
