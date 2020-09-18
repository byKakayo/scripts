#Detectar distâncias entre símbolos no sinal discretizado
scan_intersymbols <- function(L, n) {
  #Inicializa o vetor de distâncias
  Ls <-vector()

  for (i in 1:n) {
    #Encontra a ocorrência do sinal
    if (L[i] == 1) {
      bs = 0
      
      #Incrementa enquanto não encontra outra ocorrência
      while ((L[i] == 0) & (i < n)) {
        i = i + 1
      }

      #Caso do último ser o sinal
      if ((i == n) & (L[i] == 1)) {
        bs = n 
      #Caso do último ser um espaço  
      }else if ((i == n) & (L[i] == 0)) {
        bs = 0
      }else if ((i < n) & (L[i] == 1)) {
        bs = i  
      }
      if (bs > 0) {
        Ls <-c(Ls, bs)
      }
    }

    #Incremento
    i = i + 1
  }

  return(Ls)
}