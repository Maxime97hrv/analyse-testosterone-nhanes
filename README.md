# 🇫🇷 Analyse des déterminants du taux de testostérone chez les hommes 🧪

### <a href="https://maxime97hrv.github.io/analyse-testosterone-nhanes/Rapport.html" target="_blank">Consulter le rapport</a>

## Résumé 📖
Ce projet explore les facteurs influençant la testostérone totale à partir des données NHANES 2013–2016, via des modèles de régression linéaire et logistique. L’objectif est d’identifier les déterminants biologiques et comportementaux liés au taux de testostérone et à l’hypogonadisme, et de proposer des pistes d’interprétation selon la littérature scientifique.

## Compétences techniques (hard-skills):
- **Programmation et analyse de données**: utilisation de R pour l'ensemble de l'analyse (importation des données, préparation, nettoyage et panipulation avec dplyr).
- **Visualisation de données**: Création de graphiques informatifs pour croiser les variables (avec ggplot2), production de visualisations de haute qualité.
- **Modélisation statistique et machine learning**: Modèle de régression linéaire, Modèle de régression logistique pour prédire l'hypogonadisme, un problème de classification binaire.
- **Evaluation de modèles**: Courbe de ROC et calcul d'AUC pour démontrer la performance prédictive du modèle.
- **Communication des résultats**: Rédaction de l'ensemble du rapport et des codes dans RMarkdown afin de créer un document reproductible, intégrant le code et les résultats de manière transparente.

## Compétences non techniques (soft-skills):
- **Résolution de Problèmes 🧠** : J'ai identifié un problème de santé publique et y ai répondu par une analyse rigoureuse, de la conception à la modélisation.
- **Pensée Critique 🤔** : J'ai analysé les données de manière critique et j'ai interprété les résultats de manière à en tirer des conclusions médicalement pertinentes.
- **Communication de Résultats 📝** : J'ai organisé les résultats de mon analyse dans un rapport clair et concis, afin de les rendre accessibles à un public non technique.
- **Autonomie et Gestion de Projet 🗓️** : J'ai mené ce projet de bout en bout, de la définition de la problématique à sa publication sur GitHub, démontrant ma capacité à travailler de manière autonome.

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
   Commencez par exécuter le script **`Download_data_nhanes.R`** qui télécharge automatiquement les fichiers NHANES nécessaires pour les années 2013–2016. Sinon, vous pouvez directement télécharger l'ensemble du projet et mettre le dossier `Data Base` dans la même racine que les fichiers .R, vous n'aurez donc pas besoin de lancer le programme de téléchargement `Download_Data_nhanes.R`.

2. **Analyse et rapport**  
   Une fois les données téléchargées, lancez le fichier **`Rapport.rmd`** pour générer l’analyse complète et le rapport final au format HTML.

## À propos 📁
Le fichier `Rapport.html` contient l’analyse complète et détaillée.

---

**Auteur** : Maxime HERVE  
**Date** : Juin 2025  

----------------------------------------------------------------------------------------------------------------------------------------------------

# 🇬🇧 Analysis of Testosterone Determinants in Men 🧪

### <a href="https://maxime97hrv.github.io/analyse-testosterone-nhanes/Rapport.html" target="_blank">Read the report</a>

## Executive Summary 📖
This project explores the factors influencing total testosterone based on NHANES 2013-2016 data, using linear and logistic regression models. The aim is to identify the biological and behavioural determinants linked to testosterone levels and clinical hypogonadism, and to propose interpretations based on the scientific literature. Vous n'aurez donc pas besoin de lancer le programme de téléchargement `Download_Data_nhanes.R`.

## Hard Skills:
- **Programming and Data Analysis:** I used R for the entire analysis, including data import, preparation, cleaning, and manipulation with the dplyr package.
- **Data Visualization:** I created informative charts to cross-analyze variables using ggplot2 to produce high-quality visualizations.
- **Statistical Modeling and Machine Learning:** I built a linear regression model and a logistic regression model to predict hypogonadism, which is a binary classification problem.
- **Model Evaluation:** I used a ROC curve and calculated the AUC (Area Under the Curve) to demonstrate the model's predictive performance.
- **Results Communication:** I wrote the full report and code in RMarkdown to create a reproducible document that transparently integrates code and results.

## Soft Skills:
- **Problem-Solving 🧠 :** I identified a public health problem and addressed it with a rigorous analysis, from initial design to final modeling.
- **Critical Thinking 🤔 :** I critically analyzed the data and interpreted the results to draw medically relevant conclusions.
- **Results Communication 📝 :** I organized my analysis in a clear and concise report to make the findings accessible to a non-technical audience.
- **Autonomy and Project Management 🗓 :** I led this project from start to finish, from defining the problem to publishing it on GitHub, demonstrating my ability to work autonomously.

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
 Start by running the **`Download_data_nhanes.R`** script which automatically downloads the NHANES files required for the years 2013-2016. Alternatively, you can download the whole project directly and put the `Data Base` folder in the same root as the .R files, so you won't need to run the `Download_Data_nhanes.R` download program.

2. **Analysis and report** 
 Once the data has been downloaded, run the **`Report.rmd`** file to generate the full analysis and final report in HTML format.

## About 📁
The `Rapport.html` file contains the full and detailed analysis.
