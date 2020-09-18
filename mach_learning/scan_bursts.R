#Detectar bursts no sinal discretizado
scan_bursts <- function(L, n) {
  #Inicializa o vetor de bursts
  Ls <-vector()

  for (i in 1:n) {
    #Encontra a ocorr�ncia do sinal
    if (L[i] == 1) {
      i0 = 1
      
      #Incrementa enquanto há ocorrência do sinal
      while ((L[i] == 1) & (i < n)) {
        i = i + 1
      }

      #Caso do último ser o sinal
      if ((i == n) & (L[i] == 1)) {
        i = n + 1
      }

      bs = i - i0

      if (bs > 0) {
        Ls <- append(Ls,bs)
      }
    }

    #Incremento
    i = i + 1
  }

  return(Ls)
}