library(plyr)
mc.intervals<-function(Phi,N,samp=runif,alpha=0.05,interval=c(),
                       control=list()){
  # N es un vector que tiene diferentes tama??os de muestra para estimar
  #alpha determina los intervalos de confianza 1-a
  #sample es a funci??n 
##-----------------variable de controlp method
  if (lenght(interval)>1){ #la implementaci??n del m??todo variable de control practica
    sample<-function(nsim) runif(nsim,interval[1],interval[2])
    Phi.old<-Phi
    Phi<-function(x)(interval[2]-interval[1])*Phi.old(x)
  }
##---------------variab control 1)funcion 2)media del verdadero valor 3) 
  
  
  
results.list<-lapply(N,function(nsim){
  #montecarlo step
  X<-sapply(FUN=samp,nsim) #numero de simulaciones
  PhiX <- sapply(X,Phi) #evalua phi en cada X_i es un vector
  estim<-mean(PhiX) #estima nuestra esperanza guapa
  S2<-var(PhiX) #estima la varianza de phi(x_i)
#----#aqui va el metodo de variables de control
  if(length(control>0
            c<-sapply)) #falta codigo
#--------------
  quant<-qnorm(alpha/2, lower.tail = FALSE)
  int.upper<-estim+sqrt(S2/nsim)*quant
  int.lower<-estim-sqrt(S2/nsim)*quant
  return(data.frame(N=nsim,Estimate=estim,LI=int.lower,UI=int.upper))
})
  results.table<-ldply(results.list) #le das un vector y se la aplica las veces de simulaciones
  return(results.table)
}



set.seed(110104)
Phi<-function(x) 2*sqrt(4-x^2)
samp<-function(nsim) runif(nsim,0,2)
N<-seq(from=1000,to=100000,by=1000)
data<-mc.intervals(Phi = Phi,N=N,samp=samp)
data

data[]


#en la tarea 
if(length((interval)))