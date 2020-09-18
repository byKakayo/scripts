#Spectral Clustering recebe:
#Base de dados 
#Quantidade de grupos
spectral_clustering <- function(dt, k, sig) {
  #Definindo vari�veis
  S <- dt
  n <- nrow(dt)
  W <- matrix(rep(0,n^2) ,nrow = n ,ncol=n)
  D <- diag(n)
  
  #Calculando matriz W
  for (i in 1:n){
    for(j in 1:n){
      if (i != j){
        W[i,j] <- exp( - sqrt(sum((S[i,]-S[j,])^2)) / 2*sig)
      }
    }  
  } 
  
  #Calculando matriz diagonal
  for (i in 1:n){ 
    D[i,i] <- sum (W[i,])
  }
  
  #Definindo L
  aux = solve(sqrt(D))
  L <- aux %*% W %*% aux 
  
  #Autovetores de L (funcao eigen ja ordena)
  eig <- eigen(L)$vectors
  X <- eig[,(1 : k)]
  
  #Definindo matriz de comunidade
  Y <- matrix(0,nrow=n,ncol=k)
  com <- matrix(0,nrow=n,ncol=1)
  
  #Encontra as comunidades
  for(i in 1:n){
    for(j in 1:k){
      Y[i,j] <- X[i,j] / (sqrt (sum(X[i,j])^2))
    }
  }
  
  return(Y)
}