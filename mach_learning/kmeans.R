#Função K-means que recebe:
#Base de dados
#Quantidade de grupos
#Critério de parada
k_means <- function(dataset, k, threshold=1e-3) {
  #Definindo os centroides iniciais
  ids = sample(1:nrow(dataset), size=k)
  centroid = as.data.frame(dataset[ids,])
  div = 2 * threshold
  
  #Divergente > que o limiar
  while (div > threshold) {
    #Para cada centróide
    dists = NULL

    for (i in 1:k) {
      #Calcular a distancia euclidiana de cada ponto do
      #Espaço em relação a cada centróide
      euclidean = apply(dataset, 1, function(row){sqrt(sum((row - centroid[i,])^2))})
      dists = cbind(dists, euclidean)
    }
    
    #Descobrir qual centróide cada ponto está associado
    clusters = apply(dists, 1, function(row) { which.min(row) } )
    
    #Atualizar a posição dos centróides
    div = 0

    for (i in 1:k) {
      ids = which(clusters == i)
      position = colMeans(as.data.frame(dataset[ids,]))
      div = div + sqrt(sum((centroid[i,] - position)^2))
      centroid[i,] = position
    }
    
    div = div / k
  }
  
  #Cria dataFrame com os dados
  km = list()
  km$k = k
  km$centroid = centroid
  km$cluster = clusters
  
  return(km)
}

