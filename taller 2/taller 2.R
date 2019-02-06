# Remueve todos los objetos creados
rm(list=ls())
Fx <- function(x) exp(x) - pi*x

# Halla la raiz de Fx
biseccion <- function(a,b) {
  x<-seq(a,b,0.1)
  x<-b
  d<-(a+b)/2
  i<-0
  error<-abs(a-b)/2
  while (error > 1.e-4) {
    i<-i+1
    if (Fx(x) == 0) break
    if (Fx(x)*Fx(a) < 0) b <- x else {a <- x}
    d<-x
    x<-(a+b)/2
    error<-abs(a-b)/2
    cat("X=",x,"\tE=",error, " \titeracion: ",i ,"\n" )
  }
}
biseccion(0,1)

# Remueve todos los objetos creados
rm(list=ls())
Fx <- function(x) exp(x) - pi*x
F1x <- function(x) exp(x) - pi

# Halla la raiz de Fx
secante <- function(x0,x1) {
  x<-(Fx(x1)*x0-Fx(x0)*x1)/(Fx(x1)-Fx(x0))
  error <-1
  i<-0
  while (error > 1.e-4) 
  {
    i<-i+1
    x0<-x1
    x1<-x
    x<-(Fx(x1)*x0-Fx(x0)*x1)/(Fx(x1)-Fx(x0))
    if (Fx(x) == 0) break
    error<-abs(Fx(x)/F1x(x))
    cat("X=",x,"\t","E=",error," \titeracion: ",i ,"\n")
  }
}
secante(0,1)

  # Remueve todos los objetos creados
  rm(list=ls())
  Fx <- function(x) exp(x) - pi*x
  F1x <- function(x) exp(x)/pi
  
  # Halla la raiz de Fx
  puntoFijo <- function(a,b)
  {
    x1<-a
    it<-0
    while(x1<b)
    {
      
      resultado<- F1x(x1)
      if((x1<=resultado)&&(x1+0.1>=resultado)) 
        break
      x1<-x1+0.1
    }
    c<-abs(x1+0.1-x1)/abs(x1+0.1)
    while (c>1.e-4) 
    {
      it<-it+1
      resultado<- F1x(x1)
      if((x1<=resultado)&&(x1+0.01>=resultado)) 
        break
      x1<-Fx(x1+0.01)
      c<-abs(x1+0.01-x1)/abs(x1+0.01)
      cat("punto fijo: ", x1, " error: ", c, "iteraciones: ", it,"\n")
    } 
  }
  puntoFijo(0,1)
