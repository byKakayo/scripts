#Gera matriz de visibilidade do sinal
visibility <- function(L, n) {
  A <-matrix( rep( 0, len=(n*n)), nrow = n)
  
  for (j in 2:n) {
    for (i in 1:(j-1)) {
      flag = 1
      k = i + 1

      while ((k <= (j-1)) & (flag == 1)) {
        aux = L[j] + (L[i] - L[j])*(j-k)/(j-i)
        if (L[k] >= aux) {
          flag = 0
        }
        k = k + 1
      }

      if (flag == 1) {
        A[i,j] = 1
        A[j,i] = 1
      }
    }
  }

  return(A)
}
