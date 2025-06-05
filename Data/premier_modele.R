######## Premier modele #######


# Simulation d'une chaîne de Markov en population infinie.
simulation <- function(lambda,mu,EDep){
  X <- NULL 
  X[1] <- EDep # État de départ
  i = 1
  while (X[i] != 0 & X[i]<=100000000){ 
    Gn <- rbinom(1, X[i], prob = mu)
    In <- rbinom(1, X[i], prob = lambda)
    X[i+1] <- X[i] + In - Gn
    i = i+1
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
Mult_simu <-function(lambda, mu, EDep){
  simu <- list()
  for (i in 1:1000){
    simu[i] <- list(simu(lambda, mu, EDep))
  }
  return(simu)
}



# Compte le nombre de simulations atteignant l'état 0.
Maladie <- function(simu){
  L = 0
  for (i in 1:1000){
    if ((simu[[i]][length(simu[[i]])] == 0) == TRUE){
      L = L + 1
    }
  }
  return(cat(L,"simulations, sur 1000, ont menés à la fin de la maladie."))
}

set.seed(20)
simulation1 <- Mult_simu(0.5,0.5,250)
Maladie(simulation1) # 1000/1000 -> 100%
set.seed(20)
simulation2 <- Mult_simu(0.8,0.8,250)
Maladie(simulation2) # 1000/1000 -> 100%
set.seed(20)
simulation3 <- Mult_simu(0.301,0.3,250)
Maladie(simulation3) # 294/1000 -> 29,4% 


#  Estimation du temps moyen de retour en 0 (pour mu >= lambda) par Monte-Carlo
MC <- function(simu){
  liste <- NULL 
  p = 1
  for (i in 1:1000){
    if (simu[[i]][length(simu[[i]])] == 0){ # Vérifie si l'état 0 a été atteint dans la simulation i
      liste[p] <- which(simu[[i]] == 0) # Rentre le nombre de pas nécessaire pour parvenir à l'état absorbant dans la simulation i
      p = p+1
    }
  }
  res <- cat("Le temps moyen de retour en 0 est de", round(mean(liste)), "étapes") 
  return(res)
}


####### Graphiques ######

par(oma = c(4,1,1,1), mfrow = c(2, 2), mar = c(2, 2, 1, 1))

set.seed(60)
un <- simu3(0.8,0.8,250)
deux <- simu3(0.5,0.5,250)
trois <- simu3(0.2,0.4,250)
quatre <- simu3(0.4,0.2,250)
plot(un, type="l", col="gold", xlab='n', ylab=expression(X[n]), ylim =c(0,1500), xlim=c(0,1500))
lines(deux, type="l", col="red")
lines(trois, type="l", col="blue")
lines(quatre, type="l", col="green")

set.seed(25)
un <- simu3(0.8,0.8,250)
deux <- simu3(0.5,0.5,250)
trois <- simu3(0.2,0.4,250)
quatre <- simu3(0.4,0.2,250)
plot(un, type="l", col="gold", xlab='n', ylab=expression(X[n]), ylim =c(0,1500), xlim=c(0,1500))
lines(deux, type="l", col="red")
lines(trois, type="l", col="blue")
lines(quatre, type="l", col="green")

set.seed(45)
un <- simu3(0.8,0.8,250)
deux <- simu3(0.5,0.5,250)
trois <- simu3(0.2,0.4,250)
quatre <- simu3(0.4,0.2,250)
plot(un, type="l", col="gold", xlab='n', ylab=expression(X[n]), ylim =c(0,1500), xlim=c(0,1500))
lines(deux, type="l", col="red")
lines(trois, type="l", col="blue")
lines(quatre, type="l", col="green")

set.seed(10)
un <- simu3(0.8,0.8,250)
deux <- simu3(0.5,0.5,250)
trois <- simu3(0.2,0.4,250)
quatre <- simu3(0.4,0.2,250)
plot(un, type="l", col="gold", xlab='n', ylab=expression(X[n]), ylim =c(0,1500), xlim=c(0,1500))
lines(deux, type="l", col="red")
lines(trois, type="l", col="blue")
lines(quatre, type="l", col="green")


mtext("n", side=1, line=0, outer=TRUE, cex=1)
mtext(expression(X[n]), side=2, line=0, outer=TRUE, cex=1)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
legend = c(expression(lambda*" = "*mu*" : 0.8"), expression(lambda*" = "*mu*" : 0.5"), expression(lambda*" : 0.2 | "*mu*" : 0.4"), expression(lambda*" : 0.4 | "*mu*" : 0.2"))
plot(1 , type = 'l', bty = 'n', , xaxt = 'n', yaxt = 'n')
legend('bottom',legend = legend, col = c("gold","red","blue","green"), lwd = 3, xpd = TRUE, seg.len=1, cex = 1.3,  horiz = TRUE , bty = 'n')


###### Histogrammes ######


library(ggplot2)

# 0.2/0.4
set.seed(20)
simul <- Mult_simu(0.2,0.4,250)

# 0.8/0.8
set.seed(20)
simul2 <- Mult_simu(0.8,0.8,250)


temps1 <- NULL
for (i in 1:1000){
  temps1[i] <- which(simul[[i]] == 0)
}

temps2 <- NULL
for (i in 1:1000){
  temps2[i] <- which(simul2[[i]] == 0)
}

temps3 <- NULL
p = 1
for (i in 1:1000){
  if (temps2[i] <= 50000){
    temps3[p] <- temps2[i]
    p = p + 1
  }
}

# Histogramme du temps moyen d'extinction
ggplot(as.data.frame(temps1), aes(x=temps1)) + 
  geom_histogram(colour="darkblue", fill="lightblue", bins = 47) + 
  geom_vline(aes(xintercept=mean(temps1)),color="firebrick3", linetype="dashed", size=1)+
  labs(x="Temps d'extinction de l'épidémie", y = "Nombre de simulations", caption = "Moyenne : - - - - -") +
  theme(
    plot.caption = element_text(hjust = 0, size = 14, color = "firebrick3")
  )

ggplot(as.data.frame(temps3), aes(x=temps3)) + 
  geom_histogram(colour="darkblue", fill="lightblue", bins = 100) + 
  geom_vline(aes(xintercept=mean(temps2)),color="firebrick3", linetype="dashed", size=1)+
  labs(x="Temps d'extinction de l'épidémie", y = "Nombre de simulations", caption = "Moyenne : - - - - -") +
  theme(
    plot.caption = element_text(hjust = 0, size = 14, color = "firebrick3")
  )