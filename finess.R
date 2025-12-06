library(dplyr)
library(readr)
library(stringr)
library(tidyr)

cat("=== CHARGEMENT COMPLET DU FICHIER FINESS ===\n\n")

# URL du fichier
url <- "https://www.data.gouv.fr/fr/datasets/r/98f3161f-79ff-4f16-8f6a-6d571a80fea2"

cat("📥 Téléchargement du fichier complet...\n")
cat("   (Ceci peut prendre 1-2 minutes)\n\n")

# Lire TOUTES les lignes (pas de limite n_max)
lignes_completes <- readLines(url)

cat(sprintf("✅ %s lignes lues\n\n", format(length(lignes_completes), big.mark = " ")))

# Retirer les commentaires et métadonnées
lignes <- lignes_completes[!str_detect(lignes_completes, "^#|^finess;etalab")]

cat("=== ANALYSE DE LA STRUCTURE ===\n\n")

# Compter les différents types de lignes
n_struct <- sum(str_detect(lignes, "^structureet"))
n_geo <- sum(str_detect(lignes, "^geolocalisation"))

cat(sprintf("📊 Lignes 'structureet' : %s\n", format(n_struct, big.mark = " ")))
cat(sprintf("📊 Lignes 'geolocalisation' : %s\n\n", format(n_geo, big.mark = " ")))

if (n_geo == 0) {
  cat("❌ ERREUR : Aucune ligne de géolocalisation trouvée !\n")
  cat("   Le fichier ne contient peut-être pas cette section.\n")
  stop("Pas de géolocalisation disponible")
}

# Trouver où commence la section géolocalisation
premiere_geo <- which(str_detect(lignes, "^geolocalisation"))[1]
cat(sprintf("📍 Première ligne de géolocalisation : ligne n°%s\n\n", 
            format(premiere_geo, big.mark = " ")))

# Exemple de lignes
cat("=== EXEMPLES DE DONNÉES ===\n\n")
cat("Exemple de ligne 'structureet' :\n")
cat(substr(lignes[1], 1, 150), "...\n\n")

cat("Exemple de ligne 'geolocalisation' :\n")
cat(substr(lignes[premiere_geo], 1, 150), "...\n\n")

cat("=== PARSING DES DONNÉES ===\n\n")

# Séparateur (point-virgule d'après le diagnostic précédent)
sep <- ";"

# 1. Parser structureet
cat("1️⃣ Parsing des établissements...")
lignes_structure <- lignes[str_detect(lignes, "^structureet")]

df_structure <- tibble(ligne = lignes_structure) %>%
  separate(ligne, into = c(
    "type", "nofinesset", "nofinessej", "rs", "rslongue", "complrs",
    "compldistrib", "numvoie", "typvoie", "voie", "compvoie", "lieuditbp",
    "commune", "departement", "libdepartement", "ligneacheminement",
    "telephone", "telecopie", "categetab", "libcategetab", "categagretab",
    "libcategagretab", "siret", "codeape", "codemft", "libmft",
    "codesph", "libsph", "dateouv", "dateautor", "datemaj", "numuai"
  ), sep = sep, fill = "right", extra = "drop")

cat(sprintf(" ✅ %s établissements\n", format(nrow(df_structure), big.mark = " ")))

# 2. Parser géolocalisation
cat("2️⃣ Parsing de la géolocalisation...")
lignes_geo <- lignes[str_detect(lignes, "^geolocalisation")]

df_geo <- tibble(ligne = lignes_geo) %>%
  separate(ligne, into = c(
    "type", "nofinesset", "coordxet", "coordyet", "sourcecoordet", "datemaj_geo"
  ), sep = sep, fill = "right", extra = "drop") %>%
  mutate(
    coordxet = as.numeric(coordxet),
    coordyet = as.numeric(coordyet)
  )

cat(sprintf(" ✅ %s coordonnées\n", format(nrow(df_geo), big.mark = " ")))

# Statistiques sur les coordonnées
n_valid <- sum(!is.na(df_geo$coordxet) & !is.na(df_geo$coordyet))
cat(sprintf("   → %s coordonnées valides (%.1f%%)\n\n", 
            format(n_valid, big.mark = " "),
            100 * n_valid / nrow(df_geo)))

# 3. Jointure
cat("3️⃣ Fusion des données...")
df_final <- df_structure %>%
  left_join(
    df_geo %>% select(nofinesset, coordxet, coordyet, sourcecoordet),
    by = "nofinesset"
  )

n_avec_coords <- sum(!is.na(df_final$coordxet) & !is.na(df_final$coordyet))
cat(sprintf(" ✅ %s établissements avec coordonnées\n\n", 
            format(n_avec_coords, big.mark = " ")))

# 4. Conversion Lambert 93 → WGS84
cat("=== CONVERSION DES COORDONNÉES ===\n\n")

# Vérifier le système de coordonnées
mean_x <- mean(df_geo$coordxet, na.rm = TRUE)
mean_y <- mean(df_geo$coordyet, na.rm = TRUE)

cat(sprintf("Coordonnées moyennes : X=%.2f, Y=%.2f\n", mean_x, mean_y))

if (mean_x > 100000) {
  cat("→ Système : Lambert 93 (projection française)\n")
  cat("→ Conversion en WGS84 (lat/lon)...\n\n")
  
  # Installer sf si nécessaire
  if (!requireNamespace("sf", quietly = TRUE)) {
    cat("📦 Installation du package 'sf'...\n")
    install.packages("sf")
  }
  
  library(sf)
  
  # Conversion
  df_avec_coords <- df_final %>%
    filter(!is.na(coordxet), !is.na(coordyet))
  
  df_sf <- df_avec_coords %>%
    st_as_sf(coords = c("coordxet", "coordyet"), crs = 2154, remove = FALSE) %>%
    st_transform(crs = 4326)
  
  coords <- st_coordinates(df_sf)
  df_sf <- df_sf %>%
    mutate(
      longitude = coords[,1],
      latitude = coords[,2]
    ) %>%
    st_drop_geometry()
  
  # Rejoindre avec les lignes sans coordonnées
  df_final <- df_final %>%
    select(-coordxet, -coordyet) %>%
    left_join(
      df_sf %>% select(nofinesset, longitude, latitude, coordxet, coordyet),
      by = "nofinesset"
    )
  
  cat("✅ Conversion terminée\n")
  cat(sprintf("   Longitude moyenne : %.4f°\n", mean(df_final$longitude, na.rm = TRUE)))
  cat(sprintf("   Latitude moyenne : %.4f°\n\n", mean(df_final$latitude, na.rm = TRUE)))
  
} else {
  cat("→ Système : WGS84 (déjà en lat/lon)\n\n")
  df_final <- df_final %>%
    rename(longitude = coordxet, latitude = coordyet)
}

# 5. Nettoyage final
df_final <- df_final %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(
    annee = lubridate::year(lubridate::ymd(datemaj))
  )

cat("=== SAUVEGARDE ===\n\n")

# Sauvegarder
output_path <- "data/finess_geolocalise.csv"
write_csv(df_final, output_path)

cat(sprintf("✅ Fichier sauvegardé : %s\n", output_path))
cat(sprintf("📊 %s établissements géolocalisés\n\n", format(nrow(df_final), big.mark = " ")))


cat("\n✅ TERMINÉ !\n\n")
cat("Vous pouvez maintenant supprimer finess.R et utiliser ce fichier de données dans votre application Shiny :\n")