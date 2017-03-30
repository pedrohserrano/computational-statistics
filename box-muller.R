
## n=n??mero de observaciones
## min=a  max=b  extremos de la densidad
n=10000
p1<-runif(n,0,1)#, min=0, max=1)
p2<-runif(n,0,1)#, min=0, max=1)

x<-sqrt(-2*log(p1))*cos(1*360*p2)
y<- #inversa de la acumulada de la exponencial
#come un vector de probabilidades y genera un vector de x
hist(x)





