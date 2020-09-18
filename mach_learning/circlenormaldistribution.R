#Gera um gráfico circular com distribuição normalizada 
circlenormaldistribution <- function(n, r=1) {
  # inicializa os vetores x e y
  x <- c()
  y <- c()

  #Loop para preencher os vetores x e y
  while (length(x) < n) {
    #Gera um ponto aleatorio
    p = runif(2, min=-r, max=r)

    #Verifica r
    if ( p[1]**2 + p[2]**2 <= r**2 ) {
      # adiciona o ponto nos vetores x e y
      x <- c(x, p[1])
      y <- c(y, p[2])
    }
  }

  return(data.frame("x"=x, "y"=y))
}