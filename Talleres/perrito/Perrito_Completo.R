rm(list=ls())

InterpolBarLagr<-function(Ini, Fin){
  X <- seq(x[Ini], x[Fin], length=90)
  Interpol <- barylag(x[Ini:Fin], y[Ini:Fin], X)
  lines(X, Interpol, col="black")
}

y=c(3,  3.7 ,3.9 ,
    #cola 1-3
    7.12 ,4.45 ,
    #cuerpo 4-5
    7   ,5.6  ,
    #Cabeza pt1 6-7
    5.95 ,5.3,
    #cabeza pt2 8-9
    
    3, 3,
    #Cola 10-11
    3.1,3.5, 3.2,
    #pata 1.1
    2, 2.4, 3, 3.2,
    #pata1.2
    2, 1.6 ,1.9, 1.6 ,1.9, 1.7, 2,
    #pata2
    2 , 1.6 ,
    #cuerpo
    2 , 
    #oreja
    1.9,1.5,1.8,
    #pata
    2, 3, 5.3
    #cabeza 
)


x=c(1,  2   ,5   ,
    #cola 1-3
    10   ,17.6 ,
    #cuerpo 4-5
    20 ,24.5 ,
    #cabeza pt1 6-7
    26.5  ,28,
    #cabeza pt2 8-9
    
    1, 7.5,
    #Cola 10-11
    8 ,8.5,  9 ,
    #pata 12-14
    8.4, 8.5, 8.9 , 9,
    #pata1.2 15-18
    8.4, 9.3 , 11, 12.3 , 13.5, 14.1, 15,
    #pata2 19-25
    18 ,18.6,
    #cuerpo 26-27
    19.2, 
    #oreja 28
    20 ,24.4,25,
    #pata 29-31
    27, 27.9,28
    #cabeza 32.34
)




length(x) 

length(y) 


plot(x,y, pch=19, cex=0.5, col = "red", asp=1,xlab="X", ylab="Y", main="Perrito ")

#lines(x, y, col="blue")



InterpolBarLagr(1,3)
InterpolBarLagr(3,5)
InterpolBarLagr(5,7)
InterpolBarLagr(7,9)
#ARRIBA

InterpolBarLagr(10,11)
InterpolBarLagr(11,14)
InterpolBarLagr(15,18)
InterpolBarLagr(19,25)
InterpolBarLagr(25,26)
InterpolBarLagr(26,28)
InterpolBarLagr(28,31)
InterpolBarLagr(31,33)
InterpolBarLagr(33,34)
#ABAJO