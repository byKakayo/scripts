#Analise da base de treino
guess.knn <- function(x, train, trainlabels, k) {
  #Distância euclidiana
  xmatrix <- matrix(as.numeric(x), nrow=nrow(train), ncol=length(x), byrow=T)
  xmatrix <- (abs(as.matrix(train)-xmatrix))^2
  diffs <- (rowSums(xmatrix))^(1/2)

  #Ordena as distâncias
  diffs <- data.frame(dist=diffs,label=trainlabels)
  diffs <- (diffs[order(diffs$dist),])
  diffs <- diffs[1:k,]
  
  guess <- names(sort(-table(diffs$label)))[1]
  
  return(guess)
}

#Função KNN que recebe:
#Base de treino
#Base de teste
#Classificação real da base de treino
#Quantidade de grupos
knn <- function(train, test, trainlabels, k) {
  #Formatando a base de treino
  subsample <- sample(1:nrow(train), nrow(train), replace=F)
  train <- train[subsample,]
  trainlabels <- trainlabels[subsample]
  
  #Atribuir os valores das classes da base teste
  kn <- apply(test, 1, function(x) guess.knn(x, train, trainlabels, k))
  
  return(kn)
}