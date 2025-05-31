# Installation du package nécessaire
#install.packages("haven")
#install.packages("dplyr")
#install.packages("viridis")
#install.packages(c("sf", "haven", "dplyr", "tmap", "tidyr"))
#install.packages("ggplot2")
# install.packages("patchwork")

# 1. Chargement des bibliothèques
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(sf)
library(tmap)
library(patchwork) 

# Transferts reçus

data_path <- "C:/Users/LENOVO/Desktop/second semestre/ISEP2025/ISEP2025/projet_8_visualisation/Base/"
data5  <- read_dta(paste0(data_path, "s16a_me_sen2018.dta"))

# Création des variables intermédiaires

data5$frequence <- ifelse(data5$s16aq09b == 1, 1,
                    ifelse(data5$s16aq09b == 2, 0.0001, NA))

data5 <- data5 %>%
  mutate(superficie = coalesce(frequence, 0) * coalesce(s16aq09a, 0))

# Superficie totale par ménage

data5 <- data5 %>%
  group_by(vague, grappe, menage) %>%
  mutate(superficie_menage = sum(superficie, na.rm = TRUE)) %>%
  ungroup()

superficie_menage <- data5 %>%
  group_by(vague, grappe, menage) %>%
  summarise(superficie_menage = first(superficie_menage), .groups = "drop")


# Exporter le data frame en format .dta
write_dta(superficie_menage, "superficie_menage.dta")


data6  <- read_dta(paste0(data_path, "superficie_menage.dta"))

data7  <- read_dta(paste0(data_path, "s00_me_SEN2018.dta"))

# Sélectionner uniquement les colonnes nécessaires
data6_sel <- data6[, c("vague", "grappe", "menage", "superficie_menage")]
data7_sel <- data7[, c("vague", "grappe", "menage", "s00q01")]

# Fusionner avec un left join pour conserver toutes les observations de data7
data_menage <- merge(data7_sel, data6_sel, 
                by = c("vague", "grappe", "menage"), 
                all.x = TRUE)

# Remplacer les valeurs NA de superficie_menage par 0
data_menage$superficie_menage[is.na(data_menage$superficie_menage)] <- 0

noms_regions <- c("DAKAR", "ZIGUINCHOR", "DIOURBEL", "SAINT LOUIS", "TAMBACOUNDA",
                  
                  "KAOLACK", "THIES", "LOUGA", "FATICK", "KOLDA",
                  
                  "MATAM", "KAFFRINE", "KEDOUGOU", "SEDHIOU")



# Créer la variable region dans data8

data_menage$region <- noms_regions[data7$s00q01]

# Exporter le data frame en format .dta
write_dta(data_menage, "data_menage.dta")

data8  <- read_dta(paste0(data_path, "data_menage.dta"))


# 1. Lecture du shapefile
shapefile <- st_read(paste0(data_path, "Limite_Région.shp"))

# 2. Table de correspondance code ↔ nom
correspondance <- data.frame(
  region = as.character(1:14),
  NOMREG = c("DAKAR", "ZIGUINCHOR", "DIOURBEL", "SAINT LOUIS", "TAMBACOUNDA",
             "KAOLACK", "THIES", "LOUGA", "FATICK", "KOLDA",
             "MATAM", "KAFFRINE", "KEDOUGOU", "SEDHIOU")
)

# 3. Ajouter les noms de région dans data8
data8 <- data8 %>%
  mutate(NOMREG = region) 

# 4. Créer une base avec les moyennes par région
superficie_par_region <- data8 %>%
  group_by(NOMREG) %>%
  summarise(moyenne_superficie = mean(superficie_menage, na.rm = TRUE)) %>%
  ungroup()


# 5. Fusionner cette base avec le shapefile
shapefile_merged <- shapefile %>%
  left_join(superficie_par_region, by = "NOMREG")



# Formater la moyenne avec 2 décimales en texte
shapefile_merged <- shapefile_merged %>%
  mutate(
    moyenne_superficie_fmt = sprintf("%.2f", moyenne_superficie)
  )

tmap_mode("plot")

tm_shape(shapefile_merged) +
  tm_polygons(
    col = "moyenne_superficie",
    palette = "Blues",
    title = "Superficie moyenne (ha)",
    style = "quantile",
    border.col = "white"
  ) +
  # Noms des régions en haut (+ymod)
  tm_text("NOMREG", size = 0.43, col = "black", just = "left") +
  # Moyennes formatées en bas (-ymod)
  tm_text("moyenne_superficie_fmt", size = 0.45, col = "black", ymod = -1.0, fontface = "bold") +
  tm_layout(
    main.title = "Moyenne des superficies des ménages par région",
    main.title.size = 1.2,
    legend.outside = TRUE,
    frame = FALSE
  )



# 7. Créer l'objet carte
carte_superficie <- tm_shape(shapefile_merged) +
  tm_polygons(
    col = "moyenne_superficie",
    palette = "Blues",
    title = "Superficie moyenne (ha)",
    style = "quantile",
    border.col = "white"
  ) +
  tm_text("NOMREG", size = 0.43, col = "black", just = "left") +
  tm_text("moyenne_superficie_fmt", size = 0.45, col = "black", ymod = -1.0, fontface = "bold") +
  tm_layout(
    main.title = "Moyenne des superficies des ménages par région",
    main.title.size = 1.2,
    legend.outside = TRUE,
    frame = FALSE
  )

# 8. Sauvegarder la carte en image PNG
# Créer le dossier "outputs" s'il n'existe pas
if (!dir.exists("outputs")) dir.create("outputs")

# Enregistrer la carte
tmap_save(carte_superficie, filename = "outputs/carte_superficie_par_region.png", 
          width = 2000, height = 1600, dpi = 300)


#############################proprietaire ############################


s16a <- read_dta(paste0(data_path, "s16a_me_SEN2018.dta"))  # Parcelles
s00  <- read_dta(paste0(data_path, "s00_me_SEN2018.dta"))   # Localisation

# 3. Créer la variable de superficie corrigée (avec fréquence)
s16a <- s16a %>%
  mutate(
    frequence = ifelse(s16aq09b == 1, 1, ifelse(s16aq09b == 2, 0.0001, NA)),
    superficie = coalesce(s16aq09a, 0) * coalesce(frequence, 0)
  )

# 4. Ne garder que les parcelles en propriété (code 1)
proprietaires <- s16a %>%
  filter(s16aq10 == 1) %>%  # Code 1 = propriétaire
  select(grappe, menage, superficie)

# 5. Ajouter les informations régionales
s00_sel <- s00 %>%
  select(grappe, menage, region = s00q01) %>%
  mutate(region = as.character(region))

# Fusion des données
proprietaires_region <- proprietaires %>%
  left_join(s00_sel, by = c("grappe", "menage"))

# 6. Calcul de la superficie moyenne des parcelles en propriété par région
moyenne_par_region <- proprietaires_region %>%
  group_by(region) %>%
  summarise(superficie_moyenne = mean(superficie, na.rm = TRUE), .groups = "drop")

# 7. Table de correspondance code ↔ nom
correspondance <- data.frame(
  region = as.character(1:14),
  NOMREG = c("DAKAR", "ZIGUINCHOR", "DIOURBEL", "SAINT LOUIS", "TAMBACOUNDA",
             "KAOLACK", "THIES", "LOUGA", "FATICK", "KOLDA",
             "MATAM", "KAFFRINE", "KEDOUGOU", "SEDHIOU")
)

# 8. Ajouter les noms de région
moyenne_par_region <- moyenne_par_region %>%
  left_join(correspondance, by = "region")

# 9. Graphique
ggplot(moyenne_par_region, aes(x = reorder(NOMREG, superficie_moyenne), y = superficie_moyenne)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = round(superficie_moyenne, 2)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Superficie moyenne des parcelles en propriété par région",
    x = "Région",
    y = "Superficie moyenne (ha)"
  ) +
  theme_minimal()




# 1. Importation des données
s16a <- read_dta(paste0(data_path, "s16a_me_SEN2018.dta"))  # Parcelles
s00  <- read_dta(paste0(data_path, "s00_me_SEN2018.dta"))   # Localisation

# 2. Créer la variable de superficie corrigée
s16a <- s16a %>%
  mutate(
    frequence = ifelse(s16aq09b == 1, 1, ifelse(s16aq09b == 2, 0.0001, NA)),
    superficie = coalesce(s16aq09a, 0) * coalesce(frequence, 0)
  )

# 3. Ne garder que les parcelles en propriété (code 1)
proprietaires <- s16a %>%
  filter(s16aq10 == 1) %>%  # Code 1 = propriétaire
  select(grappe, menage, superficie)

# 4. Ajouter les informations régionales
s00_sel <- s00 %>%
  select(grappe, menage, region = s00q01) %>%
  mutate(region = as.character(region))

# 5. Fusion des données
proprietaires_region <- proprietaires %>%
  left_join(s00_sel, by = c("grappe", "menage"))

# 6. Calcul de la superficie moyenne par région
moyenne_par_region <- proprietaires_region %>%
  group_by(region) %>%
  summarise(superficie_moyenne = mean(superficie, na.rm = TRUE), .groups = "drop")

# 7. Table de correspondance code ↔ nom
correspondance <- data.frame(
  region = as.character(1:14),
  NOMREG = c("DAKAR", "ZIGUINCHOR", "DIOURBEL", "SAINT LOUIS", "TAMBACOUNDA",
             "KAOLACK", "THIES", "LOUGA", "FATICK", "KOLDA",
             "MATAM", "KAFFRINE", "KEDOUGOU", "SEDHIOU")
)

# 8. Ajouter les noms de région
moyenne_par_region <- moyenne_par_region %>%
  left_join(correspondance, by = "region")

# 9. Créer le graphique
p_superficie_propriete <- ggplot(moyenne_par_region, aes(x = reorder(NOMREG, superficie_moyenne), y = superficie_moyenne)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = round(superficie_moyenne, 2)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Superficie moyenne des parcelles en propriété par région",
    x = "Région",
    y = "Superficie moyenne (ha)"
  ) +
  theme_minimal()

# 10. Sauvegarder dans un dossier
dir.create("outputs", showWarnings = FALSE)
ggsave("outputs/superficie_proprietaires_par_region.png", plot = p_superficie_propriete, width = 10, height = 6)


####################CARTE ACTIVITE PASTORALE###############################


# Chargement des fichiers
s16a <- read_dta(paste0(data_path, "s16a_me_SEN2018.dta")) # Données agricoles
s16b <- read_dta(paste0(data_path, "s16b_me_SEN2018.dta")) 
s17  <- read_dta(paste0(data_path, "s17_me_SEN2018.dta"))  # Données élevage
s00  <- read_dta(paste0(data_path, "s00_me_SEN2018.dta"))  # Données géographiques
shapefile <- st_read(paste0(data_path, "Limite_Région.shp"))

# 3. Préparer les données de localisation
s00 <- s00 %>%
  rename(region = s00q01) %>%
  select(grappe, menage, region) %>%
  mutate(region = as.character(region))
s17 <- s17 %>%
  rename(espece = s17q02, nb_animaux = s17q05)

# 4. Appliquer les coefficients UBT
coefficients <- c(
  "1" = 1,
  "2" = 0.1,
  "3" = 0.1,
  "4" = 1.25,
  "5" = 0.80,
  "6" = 0.80,
  "7" = 0.20,
  "8" = 0.02,
  "9" = 0.01,
  "10" = 0.015,
  "11" = 0.01
)

elevage <- s17 %>%
  mutate(nb_animaux_pond = nb_animaux * coefficients[as.character(espece)]) %>%
  group_by(grappe, menage) %>%
  summarise(nb_animaux = sum(nb_animaux_pond, na.rm = TRUE), .groups = "drop")

# 5. Joindre localisation
elevage_geo <- left_join(elevage, s00, by = c("grappe", "menage"))

# 6. Calcul de la moyenne du cheptel par ménage par région
cheptel_moyen_region <- elevage_geo %>%
  group_by(region) %>%
  summarise(moyenne_UBT = mean(nb_animaux, na.rm = TRUE), .groups = "drop")

# 7. Correspondance code ↔ nom des régions
correspondance <- data.frame(
  region = as.character(1:14),
  NOMREG = c("DAKAR", "ZIGUINCHOR", "DIOURBEL", "SAINT LOUIS", "TAMBACOUNDA",
             "KAOLACK", "THIES", "LOUGA", "FATICK", "KOLDA",
             "MATAM", "KAFFRINE", "KEDOUGOU", "SEDHIOU")
)

# Créer un dossier de sortie s'il n'existe pas
dir.create("outputs", showWarnings = FALSE)

# Sauvegarder la carte en image PNG
tmap_save(
  tm = tm_shape(map_cheptel_moyen) +
    tm_polygons("moyenne_UBT", palette = "Blues", title = "Moyenne UBT par ménage") +
    tm_text("etiquette", size = 0.43, col = "black", just = "left") +
    tm_title("Taille moyenne du cheptel par ménage (UBT) - par région") +
    tm_layout(legend.outside = TRUE),
  filename = "outputs/carte_moyenne_cheptel_UBT_par_region.png",
  width = 2000, height = 1400, dpi = 300
)


###################cheptel par région ######################################
# Créer une étiquette combinée : nom + total UBT
map_cheptel <- map_cheptel %>%
  mutate(
    etiquette = paste0(NOMREG, "\n", round(total_UBT, 1))
  )

# Affichage de la carte
tmap_mode("plot")
tm_shape(map_cheptel) +
  tm_polygons("total_UBT", palette = "Blues", title = "Cheptel (UBT)") +
  tm_text("etiquette", size = 0.43, col = "black", just="left") +
  tm_title("Répartition du cheptel pondéré (UBT) par région") +
  tm_layout(legend.outside = TRUE)

# Créer un dossier de sortie s'il n'existe pas
dir.create("outputs", showWarnings = FALSE)

# Sauvegarder la carte du cheptel total en PNG
tmap_save(
  tm = tm_shape(map_cheptel) +
    tm_polygons("total_UBT", palette = "Blues", title = "Cheptel (UBT)") +
    tm_text("etiquette", size = 0.43, col = "black", just = "left") +
    tm_title("Répartition du cheptel pondéré (UBT) par région") +
    tm_layout(legend.outside = TRUE),
  filename = "outputs/carte_total_cheptel_UBT_par_region.png",
  width = 2000, height = 1400, dpi = 300
)


##################cheptel par ménage par région #####################################

cheptel_moyen_region <- left_join(cheptel_moyen_region, correspondance, by = "region")

# 8. Joindre avec le shapefile
map_cheptel_moyen <- left_join(sen_map, cheptel_moyen_region, by = "NOMREG")
# Créer une étiquette combinée avec le nom de la région et la moyenne UBT
map_cheptel_moyen <- map_cheptel_moyen %>%
  mutate(etiquette = paste0(NOMREG, "\n", round(moyenne_UBT, 1)))

# Affichage de la carte avec les noms + moyennes
tmap_mode("plot")
tm_shape(map_cheptel_moyen) +
  tm_polygons("moyenne_UBT", palette = "Blues", title = "Moyenne UBT par ménage") +
  tm_text("etiquette", size = 0.43, col = "black", just="left") +
  tm_title("Taille moyenne du cheptel par ménage (UBT) - par région") +
  tm_layout(legend.outside = TRUE)




# 3. Charger les données nécessaires
s16a <- read_dta(paste0(data_path, "s16a_me_SEN2018.dta"))  # Données agricoles
s17  <- read_dta(paste0(data_path, "s17_me_SEN2018.dta"))   # Données élevage
s00  <- read_dta(paste0(data_path, "s00_me_SEN2018.dta"))   # Localisation

# 4. Créer la variable "superficie" (ajuster la fréquence)
s16a <- s16a %>%
  mutate(frequence = ifelse(s16aq09b == 1, 1,
                            ifelse(s16aq09b == 2, 0.0001, NA)),
         superficie = coalesce(frequence, 0) * coalesce(s16aq09a, 0))

# 5. Superficie totale par ménage
superficie_menage <- s16a %>%
  group_by(grappe, menage) %>%
  summarise(superficie_menage = sum(superficie, na.rm = TRUE), .groups = "drop")

# 6. Appliquer les coefficients UBT
coefficients <- c(
  "1" = 1, "2" = 0.1, "3" = 0.1, "4" = 1.25,
  "5" = 0.80, "6" = 0.80, "7" = 0.20,
  "8" = 0.02, "9" = 0.01, "10" = 0.015, "11" = 0.01
)

s17 <- s17 %>%
  rename(espece = s17q02, nb_animaux = s17q05)

elevage_menage <- s17 %>%
  mutate(nb_animaux_pond = nb_animaux * coefficients[as.character(espece)]) %>%
  group_by(grappe, menage) %>%
  summarise(nb_ubt = sum(nb_animaux_pond, na.rm = TRUE), .groups = "drop")

# 7. Localisation : récupérer le milieu (urbain/rural)
localisation <- s00 %>%
  select(grappe, menage, milieu = s00q04) %>%
  mutate(milieu = ifelse(milieu == 1, "Urbain", "Rural"))

# 8. Fusion finale
donnees_menage <- localisation %>%
  left_join(superficie_menage, by = c("grappe", "menage")) %>%
  left_join(elevage_menage, by = c("grappe", "menage")) %>%
  mutate(
    superficie_menage = replace_na(superficie_menage, 0),
    nb_ubt = replace_na(nb_ubt, 0)
  )

# 9. Moyennes par milieu
moyennes_milieu <- donnees_menage %>%
  group_by(milieu) %>%
  summarise(
    `Superficie moyenne (ha)` = mean(superficie_menage, na.rm = TRUE),
    `Cheptel moyen (UBT)` = mean(nb_ubt, na.rm = TRUE),
    .groups = "drop"
  )

# 10. Passage en format long pour ggplot
moyennes_long <- pivot_longer(moyennes_milieu,
                              cols = -milieu,
                              names_to = "Indicateur",
                              values_to = "Valeur")

# 11. Graphique final
ggplot(moyennes_long, aes(x = Indicateur, y = Valeur, fill = milieu)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(Valeur, 2)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +
  labs(title = "Comparaison des moyennes par milieu de résidence",
       x = "", y = "Valeur moyenne", fill = "Milieu") +
  theme_minimal()

# ... (tout votre code précédent reste inchangé)

# 11. Graphique final


graphique_milieu <- ggplot(moyennes_long, aes(x = Indicateur, y = Valeur, fill = milieu)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(Valeur, 2)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +
  labs(title = "Comparaison des moyennes par milieu de résidence",
       x = "", y = "Valeur moyenne", fill = "Milieu") +
  theme_minimal()

# Affichage
print(graphique_milieu)

# Sauvegarde automatique
ggsave("outputs/graphique_moyennes_milieu.png", graphique_milieu, width = 8, height = 5, dpi = 300)
