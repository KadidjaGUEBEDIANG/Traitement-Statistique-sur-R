#1. Fonctions avec if, for, while, foreach (si installé)
analyseNombre <- function(n) {
  # Vérifie si le nombre est positif, négatif ou nul
  if (n > 0) {
    print("Le nombre est positif")
  } else if (n < 0) {
    print("Le nombre est négatif")
  } else {
    print("Le nombre est nul")
  }
  
  # Boucle for pour afficher les n premiers entiers
  print("Boucle for :")
  for (i in 1:n) {
    print(i)
  }
  
  # Boucle while pour compter à rebours
  print("Boucle while :")
  while (n > 0) {
    print(n)
    n <- n - 1
  }
}




# 2. Recherche de nombres premiers

estPremier <- function(n) {
  if (n < 2) return(FALSE)
  for (i in 2:sqrt(n)) {
    if (n %% i == 0) return(FALSE)
  }
  return(TRUE)
}
#resourdre une équation du second degrés
resoudreEquation2ndDegre <- function(a, b, c) {
  delta <- b^2 - 4*a*c
  if (delta < 0) {
    return("Pas de solution réelle")
  } else if (delta == 0) {
    x <- -b / (2*a)
    return(paste("Une solution double:", x))
  } else {
    x1 <- (-b + sqrt(delta)) / (2*a)
    x2 <- (-b - sqrt(delta)) / (2*a)
    return(paste("Deux solutions :", x1, "et", x2))
  }
}


# 4. Résoudre un système d’équations

resoudreSysteme <- function() {
  A <- matrix(c(2, 3, 1, -1), nrow=2)
  B <- c(5, -1)
  sol <- solve(A, B)
  return(sol)
}

# 6. Inverser une matrice

inverserMatrice <- function(M) {
  if (det(M) == 0) {
    return("La matrice n'est pas inversible")
  }
  return(solve(M))
}




# Définir une matrice carrée 2x2
matrice <- matrix(c(2, 1, 3, 4), nrow = 2, byrow = TRUE)

# Appel de la fonction
inverse <- inverserMatrice(matrice)

# Affichage du résultat
print("Matrice d'origine :")
print(matrice)

print("Inverse de la matrice :")
print(inverse)


### Optimisation
optimiserFonction <- function() {
  f <- function(x) (x - 3)^2 + 1
  result <- optim(par = 0, fn = f)
  return(result$par)  # Point du minimum
}


### Etude de cas primaire - Aider à resoudre les 04 opérations

operationPrimaire <- function(a, b) {
  return(list(
    addition = a + b,
    soustraction = a - b,
    multiplication = a * b,
    division = if (b != 0) a / b else "Division par zéro"
  ))
}

# Appel de la fonction avec les nombres 10 et 2
resultat <- operationPrimaire(10, 2)

# Affichage des résultats
print("Résultats des opérations :")
print(resultat)
