
#I-CHARGEMENT DES BIBLIOTHEQUES

#install.packages(c("sf", "haven", "dplyr", "tmap", "tidyr"))
#install.packages("ggplot2")
# install.packages("patchwork")
library(sf)
library(haven)
library(dplyr)
library(tidyr)
library(tmap)
library(ggplot2)
library(patchwork) 


################1. Importation des données ########################
# Chemins d'accès (à adapter selon votre configuration)
data_path <- "C:/Users/LENOVO/Desktop/second semestre/ISEP2025/ISEP2025/projet_cartographie_Shiny/"

# Chargement des fichiers
s16a <- read_dta(paste0(data_path, "s16a_me_SEN2018.dta")) # Données agricoles
s16b <- read_dta(paste0(data_path, "s16b_me_SEN2018.dta")) 
s17  <- read_dta(paste0(data_path, "s17_me_SEN2018.dta"))  # Données élevage
s00  <- read_dta(paste0(data_path, "s00_me_SEN2018.dta"))  # Données géographiques



################2. Nettoyage et renommage des données ########################

  #Données de localisation
s00 <- s00 %>%
  rename(region = s00q01, departement = s00q02, milieu = s00q04) %>%
  select(grappe, menage, region, departement, milieu)

  #Données agricoles
s16a <- s16a %>%
  rename(culture = s16aq08, superficie = s16aq09a)

  #Données élevage
s17 <- s17 %>%
  rename(espece = s17q02, nb_animaux = s17q05)



################3. Agrégation par ménage ########################
  
  #Nombre de parcelles
agriculture <- s16a %>%
  group_by(grappe, menage) %>%
  summarise(nb_parcelles = n(), .groups = "drop")
  
  #Nombre d’animaux
elevage <- s17 %>%
  group_by(grappe, menage) %>%
  summarise(nb_animaux = sum(nb_animaux, na.rm = TRUE), .groups = "drop")



################4. Fusion et jointures ########################

  #Fusion agriculture + élevage
merged_data <- full_join(agriculture, elevage, by = c("grappe", "menage"))

  #Ajout des infos géographiques
merged_geo <- left_join(merged_data, s00, by = c("grappe", "menage"))

  #Remplacer les NA par 0
merged_geo <- merged_geo %>%
  mutate(
    nb_parcelles = replace_na(nb_parcelles, 0),
    nb_animaux = replace_na(nb_animaux, 0)
  )



################5. Préparation pour la cartographie ########################

  #Agrégation par région
region_data <- merged_geo %>%
  group_by(region) %>%
  summarise(
    total_parcelles = sum(nb_parcelles, na.rm = TRUE),
    total_animaux = sum(nb_animaux, na.rm = TRUE)
  )

  #Lecture shapefile
sen_map <- st_read(paste0(data_path, "Limite_Région.shp"))

  #Correspondance code ↔ nom de région
region_data <- region_data %>%
  mutate(region = toupper(region))

correspondance <- data.frame(
  region = as.character(1:14),
  NOMREG = c("DAKAR", "ZIGUINCHOR", "DIOURBEL", "SAINT LOUIS", "TAMBACOUNDA",
             "KAOLACK", "THIES", "LOUGA", "FATICK", "KOLDA",
             "MATAM", "KAFFRINE", "KEDOUGOU", "SEDHIOU")
)

region_data <- left_join(region_data, correspondance, by = "region")

map_data <- left_join(sen_map, region_data, by = "NOMREG")



################6. Carte – Activités agricoles ########################

tmap_mode("plot")

tm_shape(map_data) +
  tm_polygons("total_parcelles", palette = "Greens", title = "Parcelles agricoles") +
  tm_text("NOMREG", size = 0.6, col = "black", just = "left") +
  tm_text("total_parcelles", size = 0.6, col = "black", ymod = -1.0) +
  tm_title("Distribution des activités agricoles par région") +
  tm_layout(legend.outside = TRUE)



################8. Moyennes de parcelles ########################

  #Moyenne nationale
mean(merged_geo$nb_parcelles, na.rm = TRUE)

  #Moyenne par région
moyenne_par_region <- merged_geo %>%
  group_by(region) %>%
  summarise(moyenne_parcelles = mean(nb_parcelles, na.rm = TRUE)) %>%
  mutate(region = as.character(region)) %>%
  left_join(correspondance, by = "region")

  #Graphe
ggplot(moyenne_par_region, aes(x = reorder(NOMREG, moyenne_parcelles), y = moyenne_parcelles)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = round(moyenne_parcelles, 1)), 
            vjust = -0.3, size = 3) +
  coord_flip() +
  labs(title = "Nombre moyen de parcelles par ménage",
       x = "Région", y = "Moyenne de parcelles") +
  theme_minimal()



# Étape 1 : Ajouter les libellés + joindre les infos régionales
occupation_data <- s16a %>%
  rename(mode_occupation = s16aq10) %>%
  left_join(s00, by = c("grappe", "menage")) %>%
  filter(!is.na(mode_occupation)) %>%
  mutate(
    region = as.character(region),
    mode_occupation = recode(as.character(mode_occupation), 
                             "1" = "Propriétaire",
                             "2" = "Prêt gratuit",
                             "3" = "Fermage",
                             "4" = "Métayage",
                             "5" = "Gage",
                             "6" = "Autre")
  )

# Étape 2 : Compter le total de parcelles et celles en propriété
occupation_pourcentage <- occupation_data %>%
  group_by(region, mode_occupation) %>%
  summarise(nb = n(), .groups = "drop") %>%
  group_by(region) %>%
  mutate(total_region = sum(nb)) %>%
  filter(mode_occupation == "Propriétaire") %>%
  mutate(pourcentage = round((nb / total_region) * 100, 1)) %>%
  left_join(correspondance, by = "region")


ggplot(occupation_pourcentage, aes(x = reorder(NOMREG, pourcentage), y = pourcentage)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = paste0(pourcentage, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "Part des parcelles en propriété par région",
       x = "Région", y = "Pourcentage") +
  theme_minimal()


  #1. Ajouter les infos régionales
parcelles_occupation <- s16a %>%
  select(grappe, menage, mode_occupation = s16aq10) %>%
  left_join(s00, by = c("grappe", "menage")) %>%
  filter(!is.na(mode_occupation)) %>%
  mutate(region = as.character(region))

 # Ménages agricoles : au moins une parcelle (toutes modalités confondues)
menages_agricoles <- parcelles_occupation %>%
  distinct(region, grappe, menage)

# 3. Ménages propriétaires : au moins une parcelle avec code 1
menages_proprietaires <- parcelles_occupation %>%
  filter(mode_occupation == 1) %>%
  distinct(region, grappe, menage)

# 4. Comptage + pourcentage
propriete_par_region <- menages_agricoles %>%
  group_by(region) %>%
  summarise(
    total_menages = n()
  ) %>%
  left_join(
    menages_proprietaires %>%
      group_by(region) %>%
      summarise(nb_proprietaires = n()),
    by = "region"
  ) %>%
  mutate(
    nb_proprietaires = replace_na(nb_proprietaires, 0),
    pourcentage = round((nb_proprietaires / total_menages) * 100, 1)
  ) %>%
  left_join(correspondance, by = "region")


################9. Carte – Cheptel ########################
  
  #1.Ajouter étiquette
map_data <- map_data %>%
  mutate(label_cheptel = paste0(NOMREG, "\n", round(total_animaux, 1)))

  #2.Carte
tmap_mode("plot")

tm_shape(map_data) +
  tm_polygons("total_animaux", palette = "Blues", title = "Cheptel") +
  tm_text("label_cheptel", size = 0.6, col = "black") +
  tm_title("Distribution du cheptel par région") +
  tm_layout(legend.outside = TRUE)



################10. Barplot – Total cheptel par région ########################
  
ggplot(region_data, aes(x = reorder(NOMREG, total_animaux), y = total_animaux)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = round(total_animaux, 0)),
            vjust = -0.3, size = 3) +
  coord_flip() +
  labs(title = "Nombre total d’animaux par région",
       x = "Région", y = "Animaux") +
  theme_minimal()

  #1 Charger les données élevage + localisation
elevage_menages <- s17 %>%
  select(grappe, menage, nb_propres = s17q06) %>%  # ou renomme selon ton nom réel
  left_join(s00, by = c("grappe", "menage")) %>%
  filter(!is.na(nb_propres)) %>%
  mutate(region = as.character(region))

  #2 Tous les ménages ayant renseigné une activité d’élevage
menages_elevage <- elevage_menages %>%
  distinct(region, grappe, menage)

  #3 Ménages éleveurs : ceux ayant déclaré au moins une espèce avec animaux propres > 0
menages_eleveurs <- elevage_menages %>%
  filter(nb_propres > 0) %>%
  distinct(region, grappe, menage)

  #4 : Calcul des effectifs + pourcentage
elevage_par_region <- menages_elevage %>%
  group_by(region) %>%
  summarise(total_menages = n()) %>%
  left_join(
    menages_eleveurs %>%
      group_by(region) %>%
      summarise(nb_eleveurs = n()),
    by = "region"
  ) %>%
  mutate(
    nb_eleveurs = replace_na(nb_eleveurs, 0),
    pourcentage = round((nb_eleveurs / total_menages) * 100, 1)
  ) %>%
  left_join(correspondance, by = "region")

ggplot(elevage_par_region, aes(x = reorder(NOMREG, pourcentage), y = pourcentage)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(pourcentage, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "Part des ménages éleveurs par région",
       x = "Région", y = "Pourcentage de ménages") +
  theme_minimal()






################11. Moyennes de cheptel ########################

  #Moyenne nationale
mean(merged_geo$nb_animaux, na.rm = TRUE)

  #Moyenne par région
moyenne_cheptel_region <- merged_geo %>%
  group_by(region) %>%
  summarise(moyenne_cheptel = mean(nb_animaux, na.rm = TRUE)) %>%
  mutate(region = as.character(region)) %>%
  left_join(correspondance, by = "region")

  #Graphe
ggplot(moyenne_cheptel_region, aes(x = reorder(NOMREG, moyenne_cheptel), y = moyenne_cheptel)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(moyenne_cheptel, 1)), 
            vjust = -0.0, size = 3) +
  coord_flip() +
  labs(title = "Taille moyenne du cheptel par ménage",
       x = "Région", y = "Nombre moyen d’animaux") +
  theme_minimal()

moyennes_long

################12. Comparaison par type de milieu ########################
moyennes_milieu <- merged_geo %>%
  mutate(milieu = as_factor(milieu)) %>%
  group_by(milieu) %>%
  summarise(
    moyenne_parcelles = mean(nb_parcelles, na.rm = TRUE),
    moyenne_cheptel = mean(nb_animaux, na.rm = TRUE),
    .groups = "drop"
  )


# Graphique 1 : Moyenne de parcelles par milieu
p1 <- ggplot(moyennes_milieu, aes(x = milieu, y = moyenne_parcelles, fill = milieu)) +
  geom_col() +
  geom_text(aes(label = round(moyenne_parcelles, 1)), vjust = -0.3, size = 3) +
  labs(title = "Nombre moyen de parcelles par ménage selon le milieu",
       x = "Milieu", y = "Nombre moyen") +
  theme_minimal() +
  theme(legend.position = "none")

# Graphique 2 : Moyenne de cheptel par milieu
p2 <- ggplot(moyennes_milieu, aes(x = milieu, y = moyenne_cheptel, fill = milieu)) +
  geom_col() +
  geom_text(aes(label = round(moyenne_cheptel, 1)), vjust = -0.3, size = 3) +
  labs(title = "taille moyenne du Cheptel par ménage selon le milieu",
       x = "Milieu", y = "taille moyenne") +
  theme_minimal() +
  theme(legend.position = "none")

# Afficher côte à côte
p1 + p2



