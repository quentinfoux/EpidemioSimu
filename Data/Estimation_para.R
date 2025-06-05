# Simulation d'une chaine de Markov en population infinie 
simulation2 <- function(lambda, mu, alpha, EDep, n){
  X <- NULL
  X[1] <- EDep
  proba <- c(1-alpha, alpha)
  i = 1
  while (i<n & X[i]<=100000000){
    if (X[i] != 0){
      Gn <- rbinom(1, X[i], prob = mu)
      In <- rbinom(1, X[i], prob = lambda)
      X[i+1] <- X[i] + In - Gn
      i = i+1
    }else{
      Un <- sample(c(0,1), 1, prob = proba)
      X[i+1] <- X[i] + Un
      i = i+1
    }
  }
  if (X[i] == 0){
    cat("L'état 0 est atteint en", i, "étapes \n")
    return(X)
  }else{ 
    cat("Diverge vers l'infini, dernier état :", X[i], "en", i, "étapes \n") 
    return(X)
  }
}


# Réalise plusieurs simulations de notre chaîne de Markov. 
Mult_simu2 <-function(lambda, mu, alpha, EDep, n){
  simu <- list()
  for (i in 1:1000){
    simu[i] <- list(simulation2(lambda, mu, alpha, EDep, n))
  }
  return(simu)
}


# Compte le nombre de passages d'un état à un autre des probabilités   associées. 
Compt_prob <- function(simu){
  p10 = 0
  p12 = 0
  p01 = 0
  for (i in 1:(length(simu)-1)){
    if (simu[i] == 1 & simu[i+1] == 0){
      p10 = p10 + 1
    }else if (simu[i] == 1 & simu[i+1] == 2){
      p12 = p12 + 1
    }else if(simu[i] == 0 & simu[i+1] == 1){
      p01 = p01 + 1
    }
  }
  return(c(p10,p12,p01))
}


# Estimation, par Monte Carlo, des différentes probabilités.
MC2 <- function(simu){
  p10 <- NULL
  p12 <- NULL
  p01 <- NULL
  for (i in 1:length(simu)){
    p10[i] <- Compt_prob(simu[[i]])[1]
    p12[i] <- Compt_prob(simu[[i]])[2]
    p01[i] <- Compt_prob(simu[[i]])[3]
  }
  return(c(mean(p10),mean(p12),mean(p01)))
}


# Compte le nombre de fois où l'on passe par l'état 1 dans chaque simulation et 
# en retourne la moyenne.
Compt_etat1 <- function(simu){
  etat1 <- NULL
  for (i in 1:length(simu))
    etat1[i] <- length(which(simu[[i]] == 1))
  return(mean(etat1))
}


# Compte le nombre de fois où l'on passe par l'état 0 dans chaque simulation et 
# en retourne la moyenne.
Compt_etat0 <- function(simu){
  etat0 <- NULL
  for (i in 1:length(simu))
    etat0[i] <- length(which(simu[[i]] == 0))
  return(mean(etat0))
}


# Renvoie les paramètres estimés lambda, mu et alpha.
Parametre <- function(p10, p12, p01, lambda, mu){
  if (lambda + mu <= 1){ # Condition que l'on a implémentée
    lambda = (1-p10+p12-sqrt((1-p10+p12)**2-(4*p12)))/2
    mu = 2*p10/(1+p10-p12+sqrt((1-p10+p12)**2-(4*p12)))
    alpha = p01
  }else{
    lambda = (1-p10+p12+sqrt((1-p10+p12)**2-(4*p12)))/2
    mu = 2*p10/(1+p10-p12-sqrt((1-p10+p12)**2-(4*p12)))
    alpha = p01
  }
  return(c(lambda, mu, alpha))
}


# Fonction genérale qui calcule les estimations des paramètres
Estimation <- function(lambda, mu, alpha, Edep, n){
  set.seed(20)
  simu <- Mult_simu2(lambda,mu,alpha,Edep,n) # Liste de 1000 simulations différentes
  Nb_moy <- MC2(simu) # Nombre moyen de passages entre les états des probabilités associées
  etat1 <- Compt_etat1(simu) # Réalise la moyenne du nombre de passages par l'état 1
  etat0 <- Compt_etat0(simu) # Réalise la moyenne du nombre de passages par l'état 0
  prob_est <- c(Nb_moy[1:2]/etat1,Nb_moy[3]/etat0) # Estimation des probabilités
  para <- Parametre(prob_est[1], prob_est[2], prob_est[3], lambda, mu) # Estimation des paramètres 
  return(cat("lambda estimé :", para[1], 
             "mu estimé :", para[2],
             "alpha estimé :", para[3]))