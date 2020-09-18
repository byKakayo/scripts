#Aplica deformação nos eixos
deformation <- function(x, y, xaxis=1, yaxis=1) {
  #Deforma x e y
  x = x*xaxis
  y = y*yaxis
  
  return(data.frame("x"=x, "y"=y))
}