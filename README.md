COMMENCER PAR LE FICHIER `finess.R` PUIS `jointure.R` cela trie les données pour avoir une base de données fusionnée pour `ACP/AFC/ACM/CLUSTERING`

Puis pour visualiser ce que j'ai commencé pour ACP ET ACM :

- ouvrir `Presentation.RMD` et lancer les cellules une par une. 

```# ============================================================================
# ----------- SCRIPT DE TRIE DES DONNÉES -----------
# ============================================================================
# Prérequis : 
# 1. Les fichiers de données présent dans ce readme : https://github.com/Maikooh/Challenge_Open_Data/
# EN PARTICULIER à placer dans un dossier data: 
# - Le fichier .RDS qui se trouve dans data : https://github.com/Maikooh/Challenge_Open_Data/tree/main/data
# - Le Json de ce lien : https://data.ameli.fr/explore/dataset/demographie-ages-moyens-part-des-femmes-part-des-plus-de-60-ans/export/
#
#
# 2. Le script finess.R du repo : https://github.com/Maikooh/Challenge_Open_Data/blob/main/finess.R
# - normalement présent dans ce repo aussi
#
# 3. Placer ainsi : 
#Repertoire de travail/
#├── acp_dashboard_test.R
#├── acm_dashboard_test2.R
#├── Presentation.Rmd
#├── jointure.R
#├── finess.R
#└──data/                       # Données (à créer)
#    ├── finess_geolocalise.csv  # Généré par finess.R
#    ├── demographie-effectifs-et-les-densites.rds
#    └── demographie-ages-moyens-part-des-femmes-part-des-plus-de-60-ans.json
#
#
# 4. Lancer dans l'ordre
# - finess.R 
# - jointure.R  
#
#
# 5. Vous devriez obtenir après les 2 scripts: 
#Repertoire de travail/
#├── acp_dashboard_test.R
#├── acm_dashboard_test2.R
#├── Presentation.Rmd
#├── jointure.R
#├── finess.R
#├──data/                      
#|   ├── finess_geolocalise.csv  # Généré par finess.R
#|   ├── demographie-effectifs-et-les-densites.rds
#|   └── demographie-ages-moyens-part-des-femmes-part-des-plus-de-60-ans.json
#└──datafinal/                       
#    ├── database.rds
#    └── ages_moyens_nationals.rds
# ============================================================================
```
