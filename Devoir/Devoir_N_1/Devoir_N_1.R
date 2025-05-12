
# Ici on commente

## Creation de vecteur
x <- c(1:7)
typeof(x)
y <- as.character(x)


## sur il est possible de l'utiliser comme calculatrice
1+1
sum(x)
sum(y)

#NA
x1 <- c(x, NA,9)
x1
print(x1)

sum(x1, na.rm = TRUE) #NA.REMOVE enlève les valeurs manquantes
help(sum)

#homework
#jeu de donnée
#vecteurs etc.


################04/03/2025#################

#Vecteur caractère (character)
x_char <- c("a","b","c","d")
print(x_char)
#>[1] "a" "b" "c" "d"
typeof(x)
##vecteur logique
x_log <- c(TRUE, FALSE, TRUE, FALSE)
print(x_log)
typeof(x_log)
sum(x_log)
# r affecte 1 a TRUE et 0 a FALSE


#Vecteur complex(complex)
x_complex <- c(1+2i, 3+4i)
print(x_complex)
typeof(x_complex)


#Facteur (factor)
x_fact <- factor(c("homme", "femme", "homme", "femme"))
print(x_fact)
levels(x_fact)#c'est pour trouver les modalités

#VECTEUR RAW(binaire)
x_raw <- raw(4)
print(x_raw)
typeof(x_raw)

#Facteurs
sexe <- rep (c("homme","femme"),7)# rep signifie repliquer
length(rep)

#liste
ma_liste <- list(
  nombre=42,
  texte="Bonjour",
  logique=TRUE,
  vecteur=c(1,2,3)
)
print(ma_liste)


#dataframe, c'est un tableau à deux dimension
mon_df <- data.frame(
  id=1:3,
  nom=c("Alice","Bob","Charlie"),
  age=c(25,30, 35),
  actif=c(TRUE, FALSE, TRUE)
)
print(mon_df)

un_autre_df <- data.frame(
  id=1:3,
  actif=rep(c(TRUE),3)
)
###########HOMEWORK: faire un dataframe
#session 2
#Créer un Data.Frame de 10 variables, en y ajoutant des facteurs; 
#Type de variable (qualitative et quantitative)

dataFrameetliste <- data.frame(ma_liste)
print(dataFrameetliste)


####Matrice
ma_matrice <- matrix(1:9, nrow=3,ncol=3)
print(ma_matrice)
ma_matrice <- matrix(1:9, ncol=3,nrow=3)
print(ma_matrice)
#conclusion: ça ne change pas 


#est ce qu'on peut combiner des listes pour en faire une matrice? David
dim (ma_matrice)
x1 <- 1:5
x2 <- -1:-5
x3 <- c(-2:2)
mat <- matrix(c(x1,x2,x3),ncol=5,nrow=3)
mat2<- matrix(c(x1,x2,x3),ncol=3,nrow=5)
mat2

#TRANSPOSER UNE MATRICE
###méthode1 Thioub
mat3<-t(matrix(c(x1,x2,x3),ncol=3,nrow=5))
mat3
###méthode2 Pahichou
mat4 <- cbind((matrix(c(x1,x2,x3),ncol=3,nrow=5)))
mat4

####homework
data.frame(
  id=c(1:7),
  Prénom=c("Michel-Marie","Cheikh","Herman","Mouhamet","Boris","Bamba","Kadidja"),
  nom=c("Tevoedjre","Thioub","Ngake","Seck","Vlan","Fall","Guebediang"),
  genre=c("h","h","h","h","h","h","f"),
  taille=round(runif(7,0.5,0.99)+1,2),
  classe=list("isep2"),
  note=round(rnorm(7,12,2),2),
  salaire=runif(7,1500,20000),
  école=list("ensae"),
  statut=list("célibataire")
)

#### créer un array 
mon_array <- array (1:24,dim=c(2,3,4))
print(mon_array)

###opérateur spéciaux
#opérateur pipe
library (magrittr)
1:10 %>% mean() %>%round(2)

#Opérateur natif (/>)
1:10 |> mean() |> round (2)

#INDEXATION
##