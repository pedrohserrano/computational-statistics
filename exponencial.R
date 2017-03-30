
## n=n??mero de observaciones
## min=a  max=b  extremos de la densidad
n=10000
l<-.5
p<-runif(n)#, min=0, max=1)
y<-(l^-1)*log((1-p)^-1)  #inversa de la acumulada de la exponencial
#come un vector de probabilidades y genera un vector de x
hist(y)





