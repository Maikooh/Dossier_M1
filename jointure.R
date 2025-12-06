# ============================================================================
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


# ----------- 1. CHARGEMENT DES BIBLIOTHÈQUES -----------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(DT)
library(leaflet)
library(sf)
library(ggplot2)
library(plotly)
library(jsonlite)
library(tidyr)

# ----------- 2. DÉTERMINATION DU CHEMIN DE BASE -----------
if (basename(getwd()) == "R") {
  chemin_base <- dirname(getwd())
} else {
  chemin_base <- getwd()
}

# ----------- 3. CHARGEMENT DES DONNÉES -----------

# --- 3.1. FINESS ---
finess_data <- tryCatch({
  data_temp <- read.csv(
    file.path(chemin_base, "data/finess_geolocalise.csv"),
    encoding = "UTF-8",
    stringsAsFactors = FALSE
  ) |>
    mutate(
      longitude = as.numeric(longitude),
      latitude  = as.numeric(latitude),
      annee     = as.numeric(annee),
      
      libcategetab = case_when(
        libcategetab == "603" ~ "Maison de santé",
        libcategetab == "462" ~ "Lieux de Vie et d'Accueil",
        TRUE ~ libcategetab
      )
    ) |>
    ## --- Filtrer les départements invalides ---
    filter(!grepl("^9[A-J]$", departement)) |>
    filter(departement %in% c(
      sprintf("%02d", 1:19),        # 01 à 19
      sprintf("%02d", 21:95),       # 21 à 95
      "2A", "2B",                   # Corse
      "971", "972", "973", "974", "976"  # DOM-TOM
    ))
  
  cat("FINESS chargé:", nrow(data_temp), "établissements\n")
  data_temp
}, error = function(e) {
  cat("Erreur FINESS:", e$message, "\n")
  NULL
})


# --- 3.2. Fonction de chargement JSON ---
charger_json_local <- function(nom_fichier, chemin_base) {
  chemins_possibles <- c(
    file.path(chemin_base, "data", nom_fichier),
    file.path(chemin_base, "data/processed", nom_fichier)
  )
  
  chemin_complet <- NULL
  for (chemin in chemins_possibles) {
    if (file.exists(chemin)) {
      chemin_complet <- chemin
      break
    }
  }
  
  if (is.null(chemin_complet)) {
    cat("Fichier introuvable:", nom_fichier, "\n")
    return(NULL)
  }
  
  tryCatch({
    data <- fromJSON(chemin_complet, flatten = TRUE)
    
    if ("records" %in% names(data)) {
      df <- data$records
      if ("fields" %in% names(df))
        df <- df$fields
      
      cat("",
          nom_fichier,
          ":",
          format(nrow(df), big.mark = " "),
          "enregistrements\n")
      return(df)
    } else {
      df <- as.data.frame(data)
      cat("",
          nom_fichier,
          ":",
          format(nrow(df), big.mark = " "),
          "enregistrements\n")
      return(df)
    }
  }, error = function(e) {
    cat("Erreur:", nom_fichier, ":", e$message, "\n")
    return(NULL)
  })
}


# --- 3.3. Données des professionnels de santé ---
demographie_effectifs <- tryCatch({
  data_temp <- readRDS(file.path(
    chemin_base,
    "data/demographie-effectifs-et-les-densites.rds"
  )) |>
    mutate(
      annee   = as.numeric(as.character(annee)),
      effectif = as.numeric(as.character(effectif)),
      densite  = as.numeric(as.character(densite))
    )
  
  cat("Démographie effectifs chargé:",
      nrow(data_temp),
      "enregistrements\n")
  data_temp
}, error = function(e) {
  cat("Erreur démographie effectifs:", e$message, "\n")
  NULL
})

ages_moyens <- charger_json_local(
  "demographie-ages-moyens-part-des-femmes-part-des-plus-de-60-ans.json",
  chemin_base
)

if (!is.null(ages_moyens)) {
  if ("part_des_60_ans_et_plus" %in% names(ages_moyens)) {
    ages_moyens$part_des_60_ans_et_plus <- as.numeric(as.character(ages_moyens$part_des_60_ans_et_plus))
  }
}


# --- 3.4. Données géographiques ---
france_depts <- tryCatch({
  cat("Chargement des données géographiques...\n")
  
  data_temp <- st_read(
    "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson",
    quiet = TRUE
  )
  
  cat("Données géographiques chargées:",
      nrow(data_temp),
      "départements\n")
  data_temp
  
}, error = function(e) {
  cat("Erreur chargement GeoJSON:", e$message, "\n")
  cat("Les cartes ne fonctionneront pas correctement\n")
  NULL
})

# ----------- 4. Fusion et préparation des données -----------
cat("\n", rep("=", 80), "\n")
cat("CRÉATION DE LA BASE DE DONNÉES FINALE\n")
cat(rep("=", 80), "\n\n")

if (!dir.exists("datafinal")) {
  dir.create("datafinal", recursive = TRUE)
  cat("Dossier 'datafinal' créé\n\n")
}

# ----------- 5. VÉRIFICATION DES DONNÉES SOURCES -----------
cat("ÉTAPE 1 : Vérification des données sources\n")
cat(rep("-", 80), "\n")

required_data <- c("finess_data",
                   "demographie_effectifs",
                   "ages_moyens",
                   "france_depts")
missing_data  <- required_data[!sapply(required_data, exists)]

if (length(missing_data) > 0) {
  stop(
    "Erreur : Données manquantes : ",
    paste(missing_data, collapse = ", "),
    "\n→ Exécute d'abord source('global.R') pour charger les données"
  )
}

cat("Toutes les données sources sont présentes\n")
cat("   - finess_data           :", format(nrow(finess_data), big.mark = " "), "lignes\n")
cat("   - demographie_effectifs :",
    format(nrow(demographie_effectifs), big.mark = " "),
    "lignes\n")
cat("   - ages_moyens           :", format(nrow(ages_moyens), big.mark = " "), "lignes\n")
cat("   - france_depts          :", format(nrow(france_depts), big.mark = " "), "lignes\n\n")


# ----------- 6. NETTOYAGE ET HARMONISATION -----------
cat("ÉTAPE 2 : Nettoyage et harmonisation\n")
cat(rep("-", 80), "\n")

clean_prof <- function(x) {
  x %>%
    tolower() %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    gsub("[^a-z0-9 ]", " ", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws()
}

harmoniser_dept <- function(x) {
  x <- trimws(as.character(x))
  
  ifelse(
    x %in% c("2A", "2B") |
      grepl("^97[0-9]$", x) | grepl("^98[0-9]$", x),
    x,
    sprintf("%02s", x)
  )
}

demographie_effectifs <- demographie_effectifs %>%
  mutate(
    profession_sante = clean_prof(profession_sante),
    departement      = harmoniser_dept(departement)
  ) %>%
  filter(departement != "999")

ages_moyens <- ages_moyens %>%
  mutate(annee = as.numeric(annee),
         profession_sante = clean_prof(profession_sante))

finess_data <- finess_data %>%
  mutate(departement = harmoniser_dept(departement))

france_depts <- france_depts %>%
  mutate(code = harmoniser_dept(code))

# ----------- 7. AGRÉGATION DES DONNÉES -----------
cat("ÉTAPE 3 : Agrégation des données\n")
cat(rep("-", 80), "\n")

#Nouvelle classification des établissements
finess_agg <- finess_data %>%
  mutate(categetab = as.character(categetab)) %>%
  group_by(departement) %>%
  summarise(
    nb_etablissements = n(),
    
    ## --- HÔPITAUX PUBLICS ---
    nb_hopitaux = sum(categetab %in% c("355", "101", "292", "698", "106"), 
                      na.rm = TRUE),
    
    ## --- ÉTABLISSEMENTS DE SOINS privés/semi-privés ---
    nb_etabl_soins = sum(categetab %in% c("365", "362", "128", "129", "122"), 
                         na.rm = TRUE),
    
    ## --- STRUCTURES AMBULATOIRES ---
    nb_ambulatoire = sum(categetab %in% c("603", "124"), 
                         na.rm = TRUE),
    
    ## --- HOSPITALISATION À DOMICILE ---
    nb_had = sum(categetab == "127", na.rm = TRUE),
    
    ## --- DÉTAIL pour analyses spécifiques ---
    nb_msp = sum(categetab == "603", na.rm = TRUE),
    nb_centres_sante = sum(categetab == "124", na.rm = TRUE),
    nb_lva = sum(grepl("lieux.*vie|accueil", tolower(libcategetab)), na.rm = TRUE),
    
    .groups = "drop"
  )

demog_agg <- demographie_effectifs %>%
  group_by(annee, departement, profession_sante) %>%
  summarise(
    effectif_total   = sum(effectif, na.rm = TRUE),
    densite_moyenne  = mean(densite, na.rm = TRUE),
    .groups = "drop"
  )


# ----------- 8. FUSION PROGRESSIVE -----------
cat("ÉTAPE 4 : Fusion progressive\n")
cat(rep("-", 80), "\n")

fusion_etape1 <- demog_agg

fusion_etape2 <- fusion_etape1 %>%
  left_join(finess_agg, by = "departement")

fusion_finale <- fusion_etape2 %>%
  left_join(france_depts %>% select(code, nom, geometry),
            by = c("departement" = "code"))


# ----------- 9. CRÉATION DES MODALITÉS POUR ACM -----------
cat("ÉTAPE 5 : Création des modalités\n")
cat(rep("-", 80), "\n")

creer_modalites_equilibrees <- function(x, k = 4, labels = NULL) {
  x_clean <- x[!is.na(x)]
  n_unique <- length(unique(x_clean))
  
  if (n_unique < k) {
    k_adapte <- max(2, n_unique)
    
    if (is.null(labels)) {
      labels_adaptes <- if (k_adapte == 2) {
        c("Faible", "Élevé")
      } else if (k_adapte == 3) {
        c("Faible", "Moyen", "Élevé")
      } else {
        paste0("Classe_", 1:k_adapte)
      }
    } else {
      labels_adaptes <- labels[1:k_adapte]
    }
    
    if (n_unique == 1) {
      return(factor(rep("Valeur unique", length(x)), levels = "Valeur unique"))
    }
    
    breaks <- unique(quantile(x, seq(0, 1, length.out = k_adapte + 1), na.rm = TRUE))
    
    if (length(breaks) < 2) {
      return(factor(rep("Valeur unique", length(x)), levels = "Valeur unique"))
    }
    
    return(cut(
      x,
      breaks = breaks,
      labels = labels_adaptes,
      include.lowest = TRUE,
      right = TRUE
    ))
  }
  
  breaks <- quantile(x, seq(0, 1, length.out = k + 1), na.rm = TRUE)
  
  if (length(unique(breaks)) < k + 1) {
    breaks <- unique(breaks)
    k_reel <- length(breaks) - 1
    
    labels_adaptes <- if (is.null(labels))
      paste0("Classe_", 1:k_reel)
    else
      labels[1:k_reel]
    
    return(cut(
      x,
      breaks = breaks,
      labels = labels_adaptes,
      include.lowest = TRUE,
      right = TRUE
    ))
  }
  
  cut(
    x,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = TRUE
  )
}

database <- fusion_finale %>%
  group_by(annee, profession_sante) %>%
  mutate(
    effectif_classe = creer_modalites_equilibrees(
      effectif_total,
      k = 4,
      labels = c("Très faible", "Faible", "Moyen", "Élevé")
    ),
    
    densite_classe = creer_modalites_equilibrees(
      densite_moyenne,
      k = 4,
      labels = c("Désert médical", "Sous-doté", "Correct", "Bien doté")
    ),
    
    etabl_classe = creer_modalites_equilibrees(
      nb_etablissements,
      k = 3,
      labels = c("Peu équipé", "Moyennement équipé", "Bien équipé")
    ),
    
    ## --- Nouvelles variables de présence ---
    presence_hopital = factor(ifelse(nb_hopitaux > 0, "Oui", "Non"), 
                              levels = c("Non", "Oui")),
    
    presence_etabl_soins = factor(ifelse(nb_etabl_soins > 0, "Oui", "Non"), 
                                  levels = c("Non", "Oui")),
    
    presence_ambulatoire = factor(ifelse(nb_ambulatoire > 0, "Oui", "Non"), 
                                  levels = c("Non", "Oui")),
    
    msp_classe = creer_modalites_equilibrees(
      nb_msp,
      k = 3,
      labels = c("Aucune/Peu", "Moyen", "Nombreuses")
    ),
    
    region_type = case_when(
      departement %in% c("75", "77", "78", "91", "92", "93", "94", "95") ~ "Île-de-France",
      departement %in% c("971", "972", "973", "974", "976") ~ "Outre-mer",
      departement %in% c("2A", "2B") ~ "Corse",
      TRUE ~ "Province"
    ),
    
    presence_lva = factor(ifelse(nb_lva > 0, "Oui", "Non"), levels = c("Non", "Oui")),
    
    ## --- Offre dominante enrichie ---
    offre_dominante = case_when(
      nb_hopitaux > nb_etabl_soins + nb_ambulatoire ~ "Hospitalier public",
      nb_etabl_soins > nb_hopitaux + nb_ambulatoire ~ "Établ. soins privés",
      nb_ambulatoire > nb_hopitaux + nb_etabl_soins ~ "Ambulatoire",
      nb_hopitaux + nb_etabl_soins + nb_ambulatoire == 0 ~ "Aucune structure",
      TRUE ~ "Mixte"
    ),
    
    niveau_equipement = creer_modalites_equilibrees(
      nb_etablissements,
      k = 4,
      labels = c("Très faible", "Faible", "Bon", "Excellent")
    ),
    
    densite_pro_classe = case_when(
      is.na(densite_moyenne) ~ "Non renseigné",
      densite_moyenne < 50   ~ "Très faible",
      densite_moyenne < 100  ~ "Faible",
      densite_moyenne < 150  ~ "Correct",
      TRUE ~ "Élevé"
    ),
    
    profil_infra = case_when(
      nb_hopitaux >= 2 & nb_etabl_soins >= 1 ~ "Pôle hospitalier complet",
      nb_hopitaux >= 1 & nb_ambulatoire >= 2 ~ "Mixte hôpital-ambulatoire",
      nb_ambulatoire >= 3                    ~ "Réseau ambulatoire fort",
      nb_etablissements <= 1                 ~ "Sous-équipé",
      TRUE                                   ~ "Standard"
    ),
    
    type_territoire = {
      q25 <- quantile(effectif_total, 0.25, na.rm = TRUE)
      q75 <- quantile(effectif_total, 0.75, na.rm = TRUE)
      
      case_when(
        region_type == "Île-de-France" ~ "Métropole capitale",
        region_type == "Outre-mer"     ~ "Outre-mer",
        region_type == "Corse"         ~ "Insulaire",
        q25 == q75                     ~ "Standard",
        effectif_total >= q75          ~ "Urbain dense",
        effectif_total >= q25          ~ "Urbain moyen",
        TRUE                            ~ "Rural"
      )
    }
  ) %>% ungroup()


# ----------- 10. CALCUL D'INDICATEURS COMPLÉMENTAIRES -----------
cat("ÉTAPE 6 : Calcul des indicateurs\n")
cat(rep("-", 80), "\n")

database <- database %>%
  mutate(
    taux_couv_hosp = ifelse(
      effectif_total > 0,
      (nb_hopitaux / effectif_total) * 1000,
      NA_real_
    ),
    
    ## --- Ratio ambulatoire ajusté ---
    ratio_ambu_hosp = ifelse(nb_hopitaux + nb_etabl_soins > 0, 
                             nb_ambulatoire / (nb_hopitaux + nb_etabl_soins), 
                             NA_real_),
    
    ## --- Diversité enrichie ---
    diversite_offre = ((nb_hopitaux > 0) +
                         (nb_etabl_soins > 0) +
                         (nb_ambulatoire > 0) +
                         (nb_had > 0) +
                         (nb_lva > 0)) / 5,
    
    ## --- ratio public/privé ---
    ratio_public_prive = ifelse(nb_etabl_soins > 0, 
                                nb_hopitaux / nb_etabl_soins, 
                                NA_real_),
    
    ## --- Correction est_metropole ---
    est_domtom    = region_type == "Outre-mer",
    est_metropole = !est_domtom
  )


# ----------- 11. VALIDATION FINALE -----------
cat("ÉTAPE 7 : Validation finale\n")
cat(rep("-", 80), "\n")

cat("Structure finale :\n")
cat("   - Lignes :", format(nrow(database), big.mark = " "), "\n")
cat("   - Colonnes :", ncol(database), "\n")
cat("   - Années :", paste(range(database$annee), collapse = " à "), "\n")
cat("   - Départements :", length(unique(database$departement)), "\n")
cat("   - Professions :", length(unique(database$profession_sante)), "\n\n")

# ----------- 12. SAUVEGARDE -----------
cat("ÉTAPE 8 : Sauvegarde\n")
cat(rep("-", 80), "\n")

saveRDS(database, "datafinal/database.rds")
saveRDS(ages_moyens, "datafinal/ages_moyens_national.rds")

# ----------- 13. RAPPORT FINAL -----------
cat(rep("=", 80), "\n")
cat("CRÉATION DE LA BASE DE DONNÉES TERMINÉE AVEC SUCCÈS !\n")
cat(rep("=", 80), "\n\n")

cat("RÉSUMÉ DES NOUVELLES VARIABLES :\n")
cat("   - nb_etabl_soins     : Établissements de soins privés/semi-privés\n")
cat("   - nb_ambulatoire     : Structures ambulatoires (MSP + Centres santé)\n")
cat("   - nb_had             : Hospitalisation à domicile\n")
cat("   - ratio_public_prive : Ratio hôpitaux publics / établ. soins\n")
cat("   - est_metropole      : Corrigé (inverse de est_domtom)\n\n")

# ----------- 14. VÉRIFICATIONS FINALES -----------
cat("VÉRIFICATIONS RAPIDES :\n")
cat(rep("-", 80), "\n")

cat("Lignes              :", format(nrow(database), big.mark = " "), "\n")
cat("Colonnes            :", ncol(database), "\n")
cat("Années              :", paste(range(database$annee), collapse = " à "), "\n")
cat("Départements        :", length(unique(database$departement)), "\n")
cat("Professions         :", length(unique(database$profession_sante)), "\n")
cat("NA effectif_total   :", sum(is.na(database$effectif_total)), "\n")
cat("NA densite_moyenne  :", sum(is.na(database$densite_moyenne)), "\n")
cat("NA geometry         :", sum(is.na(database$geometry)), "\n\n")

cat("Distribution effectif_classe :\n")
print(table(database$effectif_classe))
cat("\n")

cat("Distribution densite_classe :\n")
print(table(database$densite_classe))
cat("\n")

cat("Distribution region_type :\n")
print(table(database$region_type))
cat("\n")

cat("Distribution est_metropole vs est_domtom :\n")
print(table(database$est_metropole, database$est_domtom, useNA = "always"))
cat("\n")

## --- Vérifier doublons ---
nb_doublons <- database %>%
  group_by(annee, departement, profession_sante) %>%
  filter(n() > 1) %>%
  nrow()

cat("Doublons détectés   :", nb_doublons, "\n\n")

cat(rep("=", 80), "\n")
cat("SCRIPT TERMINÉ - Prêt pour ACM/ACP/AFC/CLUSTERING ! \n")
cat(rep("=", 80), "\n")