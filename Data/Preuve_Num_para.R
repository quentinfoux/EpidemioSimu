###### Démonstration ######


# On veut récupérer la somme des paramètres lambda et mu dans une liste en les faisant chacun varier par pas de 0.01 
somme <- NULL
vrai_lbd <- NULL
lambda1 <- NULL
p = 1
for (i in 1:100){
  for (j in 1:100){
    lbd <- i/100
    mu <- j/100
    p10 = (1-lbd)*mu
    p12 = (1-mu)*lbd
    lbd1 = (1 - p10 + p12 + sqrt(round((1-p10+p12)**2-4*p12,10)))/2
    lbd2 = (1 - p10 + p12 - sqrt(round((1-p10+p12)**2-4*p12,10)))/2
    lambda1[p] <- round(lbd1,10) # Liste des lambda1
    vrai_lbd[p] <- lbd # Réelle valeur de lambda
    somme[p] <- lbd + mu 
    p = p + 1
  }
}


# On compare ensuite la valeur réelle de lambda avec celle de lambda1. 
# Si c'est la même, on place la somme des paramètres associés dans une nouvelle liste somme_lbd1. 
# Sinon, on rentre la valeur de cette somme dans une autre liste nommée somme_lbd2.
somme_lbd1 <- NULL
somme_lbd2 <- NULL
p = 1
l = 1
for (i in 1:length(vrai_lbd)){
  if (lambda1[i] == vrai_lbd[i]){
    somme_lbd1[p] <- somme[i] # Somme lambda + mu lorsque lambda1 est égal à lambda
    p = p + 1
  }else{
    somme_lbd2[l] <- somme[i] # Somme lambda + mu lorsque lambda2 est égal à lambda
    l = l + 1
  }
}


# On vérifie enfin si les conditions sont respectées
all(somme_lbd1 >= 1 ) # TRUE 
all(somme_lbd2 <= 1) # TRUE