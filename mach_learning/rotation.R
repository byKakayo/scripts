#Rotação
rotation <- function(x, y, theta=0) {
  #Aplica uma rotação de ângulo theta no sentido anti-horário
  x = x*cos(theta) + y*sin(theta)
  y = y*cos(theta) + x*sin(theta)
  
  return(data.frame("x"=x, "y"=y))
}