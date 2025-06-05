#requirements
import numpy as np
import math
import pandas as pd

#définition de la probabilité binomiale
def binom(k,i,lbd):
    if i >= k and k >= 0:
        x = math.comb(i,k)*pow(lbd,k)*pow(1-lbd,i-k)
    else :
        x = 0
    return x
    

# Calcul des probas
def somme(i,j,lbd,mu):
    s = 0
    for k in range(i+1):
        s =  s + binom(k,i,lbd)*binom(k-j+i,i,mu)
    return(s)

def somme2(i,N,lbd,mu):
    s = 0
    for k in range(i+1):
        for l in range(N):
            s =  s + binom(k,i,lbd)*binom(k-l+i,i,mu)
    return(1-s)


# Matrice de transition de (X_n)
def MT(N, lbd, mu):
    Matrice=np.zeros((N+1,N+1))
    Matrice[0,0] = 1
    for k in range(1,N+1):
        for j in range(N):
            Matrice[k,j] = somme(k,j,lbd,mu)
        Matrice[k,N] = somme2(k,N,lbd,mu)
    return Matrice


# Temps moyen de retour en 0 pour N = 2
def T_moy(M):
    V0 = 0
    V1 = 1 / (1 - M[1,2]) + (M[1,1] * (1 - M[1,2] + M[2,1])) / (( 1 - M[1,2]) * ( 1 - M[1,2] - M[2,2] + M[1,2] * M[2,2] - M[1,1] * M[2,1]))
    V2 = (1 - M[1,2] + M[2,1]) / ( 1 - M[1,2] - M[2,2] + M[1,2] * M[2,2] - M[1,1] * M[2,1])
    Vi = [V0, V1, V2]
    print(f'\n V0 = {Vi[0]}',f'\n V1 = {Vi[1]}', f'\n V2 = {Vi[2]}')
    return Vi

