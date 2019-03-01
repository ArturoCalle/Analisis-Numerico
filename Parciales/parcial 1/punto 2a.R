rm(list=ls())
Fx <-function(x) tan(pi*x)-sin(pi*x)
ecuacion<- function(x,b)
{
  x<-((x-b)-((Fx(x-b)*((x-b)-(x-2*b)))/(Fx(x-b)-(Fx(x-2*b)))))
  return(x)
}
metodo <- function(a)
{
  iteracion<-0
  b<-0.1
  x<- a
  while(abs(Fx(x))>1.e-9)
  {
    x<-ecuacion(x,b)
    iteracion<-iteracion+1
    if((Fx(x)<0))
    {
      if(Fx(x+b)>0)
      {
        b<- b/-10
      }
    }
    else 
    {
      if((Fx(x)>0))
      {
        if(Fx(x+b)<0)
        {
          b<- b/-10
        }
      }
    }
    cat("fx: ", Fx(x), " x: ", x," iteracion: ", iteracion, "\n" )
  }
  return(x)
}
invocacion<-function(a)
{
  sol1<- metodo(a)
  sol2<- metodo(a+sol1)
  cat("solucion 1: ", sol1,"solucion 2: ", sol2, "\n")
  
}

invocacion(0.5)

