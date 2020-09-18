#Funcao responsavel por gerar o padrao que recebe:
#Vetor com os nos do automato
#Matriz de transicao dos nos
#Valor n iteracoes do padrao
#Estado inicial do padrao
generate_pattern <- function(nos, mt, n, e) {
  #Adicionar estado inicial ao vetor
  ve <-matrix(c(0,e), ncol = 2)

  #Loop para gerar o padrao 
  for (i in 1:(n-1)) {
    #Gerar numero aleatorio entre 0 e 1
    x <-runif(1, 0, 1)
    
    #Contador que percorre as linhas do estado atual
    j = 1
    
    #Probabilidade de ir p/ primeiro n�
    p = mt[nos[j],e]
    
    #Teste da probabilidade
    while (x > p) {
      j = j + 1
      p = p + mt[nos[j],e]
    }

    #Atualizar o n� atual e adiciona ao vetor
    e = nos[j]
    ve <-rbind(ve, c(i, e))
  }

  #Retorna um vetor com os estados
  return(ve)
}
