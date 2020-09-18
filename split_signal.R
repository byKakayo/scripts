#Discretizar padrão 1D
split_signal <- function(L, S) {
  #Li é uma matrix cujas linhas são os splits para cada nó do autômato
  #Gera a matrix com zeros
  Li <-matrix(0, nrow=lenght(S), ncol=dim(L)[1])
  rownames(Li) <-S

  #Percorre o padrão
  for (i in 1:dim(L)[1]) {
    #Adiciona 1 no split correspondente ao sinal
    Li[L[i, 2], i] = 1
  }
  
  return(Li)
}