rm(list=ls())
Fx <- function(x) tan(pi*x)-sin(pi*x)
F1x <- function(x) asin(tan(pi* x))/pi

puntoFijo <- function(a,b)
{
  d<- 0.1
  x1<-a
  it<-0
  c<-abs(x1+d-x1)/abs(x1+d)
  while(x1<b)
  {
    it<-it+1
    resultado<- F1x(x1)
    if((x1<=resultado)&&(x1+d>=resultado)) 
      break
    x1<-x1+d
    cat("x= ", x1, " E=", c, "iteraciones: ", it,"\n")
  }
  c<-abs(x1+d-x1)/abs(x1+d)
  while (c>1.e-9) 
  {
    it<-it+1
    resultado<- F1x(x1)
    if((x1<=resultado)&&(x1+d>resultado)) 
      d<-d/10
    x1<-Fx(x1+d)
    c<-1-abs(d)/abs(x1+d)
    cat("x= ", x1, " E=", c, "iteraciones: ", it,"\n")
  }
  return(x1)
}
invocacion<-function(a,b)
{
  sol1 <- puntoFijo(a,b)
  
  sol2 <- puntoFijo(sol1+1,b+a)
  cat("solucion 1:", sol1)
  cat(" solucion 2: " , sol2)
}
invocacion(0,2)