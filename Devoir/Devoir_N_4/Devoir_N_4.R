# Chargement des bibliothèques nécessaires
library(haven)
library(tidyverse)
library(dplyr)


# 1. Importation des données
s01_me_SEN2018 <- read_dta("C:/Users/LENOVO/Desktop/second semestre/ISEP2025/ISEP2025/Devoir/Devoir_N_4/s01_me_SEN2018.dta")
s00_me_SEN2018 <- read_dta("C:/Users/LENOVO/Desktop/second semestre/ISEP2025/ISEP2025/Devoir/Devoir_N_4/s00_me_SEN2018.dta")

# 2. Suppression des doublons
s01_me_SEN2018 <- s01_me_SEN2018 %>%
  group_by(grappe, menage, s01q00a) %>%
  distinct()

# Vérification du nombre d'observations après suppression
print(dim(s01_me_SEN2018))

# 3. Création de la variable "ifmember"
s01_me_SEN2018 <- s01_me_SEN2018 %>%
  mutate(ifmember = ifelse(s01q12 == 1 | s01q13 == 1, 1, 0))

# Somme des membres du ménage
print(sum(s01_me_SEN2018$ifmember, na.rm = TRUE))  # 66109 < 66119

# 4. Calcul de la taille du ménage
s01_me_SEN2018 <- s01_me_SEN2018 %>%
  group_by(grappe, menage) %>%
  mutate(hhsize = sum(ifmember, na.rm = TRUE))

# Statistiques descriptives de la taille des ménages
print(summary(s01_me_SEN2018$hhsize))

# Calcul de la taille moyenne du ménage
taille_moyenne <- mean(s01_me_SEN2018$hhsize, na.rm = TRUE)
message("Nous avons bien calculé la taille moyenne de l'échantillon : ", taille_moyenne)

# Fusionner les deux bases
s01_me_SEN2018 <- merge(s01_me_SEN2018, s00_me_SEN2018, by = c("grappe", "menage"), all = TRUE)

