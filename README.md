# 🇫🇷 Analyse des déterminants du taux de testostérone chez les hommes 🧪

Ce projet explore les facteurs influençant la testostérone totale à partir des données NHANES 2013–2016, via des modèles de régression linéaire et logistique. L’objectif est d’identifier les déterminants biologiques et comportementaux liés au taux de testostérone et à l’hypogonadisme, et de proposer des pistes d’interprétation selon la littérature scientifique.

## Méthodologie 📊
- Données : NHANES 2013–2016
- Techniques : régression MCO, erreurs robustes (HC3), tests d'ajustement, analyse logistique
- Outils : R, packages `gtsummary`, `car`, `lmtest`, `sandwich`, `pROC`, `ggplot2`, `tidyverse`, `dplyr`, `ResourceSelection`, `gmodels`, `flextable`, `bpurr`

## Résultats clés 🔍
- Âge, SHBG et tour de taille sont des prédicteurs majeurs de la testostérone totale.
- L’activité physique modérée semble protectrice.
- Le modèle logistique affiche une AUC de 0.84.

## Mode d'emploi 📁

1. **Téléchargement des données**  
   Commencez par exécuter le script **`Download_data_nhanes.R`** qui télécharge automatiquement les fichiers NHANES nécessaires pour les années 2013–2016.

2. **Analyse et rapport**  
   Une fois les données téléchargées, lancez le fichier **`Rapport.rmd`** pour générer l’analyse complète et le rapport final au format HTML.

## À propos 📁
Le fichier `Rapport.html` contient l’analyse complète et détaillée.

---

**Auteur** : Maxime HERVE  
**Date** : Juin 2025  

----------------------------------------------------------------------------------------------------------------------------------------------------

# 🇬🇧 Analysis of Testosterone Determinants in Men 🧪

This project explores the factors influencing total testosterone based on NHANES 2013-2016 data, using linear and logistic regression models. The aim is to identify the biological and behavioural determinants linked to testosterone levels and clinical hypogonadism, and to propose interpretations based on the scientific literature.

## Methodology 📊
- Data: NHANES 2013-2016
- Techniques: OLS regression, robust errors (HC3), goodness-of-fit tests, logistic analysis
- Tools: R, packages `gtsummary`, `car`, `lmtest`, `sandwich`, `pROC`, `ggplot2`, `tidyverse`, `dplyr`, `ResourceSelection`, `gmodels`, `flextable`, `bpurr`

## Key results 🔍
- Age, SHBG and waist circumference are major predictors of total testosterone.
- Moderate physical activity appears protective.
- The logistic model displays an AUC of 0.84.

## Operating instructions 📁

1. **Download data** 
 Start by running the **`Download_data_nhanes.R`** script which automatically downloads the NHANES files required for the years 2013-2016.

2. **Analysis and report** 
 Once the data has been downloaded, run the **`Report.rmd`** file to generate the full analysis and final report in HTML format.

## About 📁
The `Rapport.html` file contains the full and detailed analysis.
