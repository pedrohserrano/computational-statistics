
##importance en la practica  
a<-0
b<-2
nsim<-100
#crudo
U<-runif(nsim,a,b)
mean((a-b)*dnorm(U))

phi #queremos integrar

#prioritario
U<-rexp(nsim,rate=4) #si muestreas en la exponencial
mean((dnorm(U)/dexp(U))) #pogas en la exponencial


#IMPLEMENTACI??N PRACTICA DE IMPORTANCE SAMPLING
pnorm(2)-1/2
plot(dnorm, xlim=c(0,10))

#opci??n crudo
nsim <- 5000
u <- runif (nsim, 0,10) 
phi <- function (x) 10*dnorm(x)
estim <- mean (phi(U))
estim

plot(dnorm,xlim=c(0,10))

#OPCI??N PRIORITARIA
nsim <- 5000
#usamos el m??todo de la inversa
u <- runif (nsim, 0 ,10)
#exponencial (LAMBDA=1) truncada a [0,2]
x <- -log((1-(1-exp(-10)))*u)
#la densidad de la exponencial truncada
fun <- function(x) (dexp(x)/(1-exp(-10)))
#monte carlo
phi <- function (x) 10*dnorm(x)
estim <- mean (phi(U))

plot(dnorm)


##del importance que ya est?? arriba
w <- function(x) dunif(x, 0, 2)/(-log((1-(1-exp(-10)))*x))##dexp(x, rate=1)
phi <- function(x,m) m*exp(-m*x) ##k/(1+x^2*m)

importance <- function(nsim,m){
  x <- runif (nsim, 0 ,10) ##rexp(nsim,rate = 1)
  y=phi(x,m)*dunif(x, 0, 2)
  mean(y)
}

data_norm <- importance(1000,100)
plot(importance(1000,100))


Consideren la famlia de funciones (s??, es la densidad de una exponencial pero ignoren ese hecho por completo).
f_m(x)=m*exp(-m*x)


Y queremos calcular int_0^2 f_m(x) dx. El valor real de la integral es f??cil de calcular, pues es (1-exp(-2m)).
Quiero que prueben y comparen las siguientes estrategias y grafiquen los errores. Los errores como de costumbre pueden graficarlos en funci??n del n??mero de simulaciones. 

-- Monte Carlo crudo generando uniformes en [0,2].

-- Modifiquen su m??todo de la funci??n inversa para generar una muestra de una distribuci??n exponencial con par??metro lambda truncada a 0,2. (HINT: calculen F(x | X \leq 2 ) = P(X\leq x |X\leq 2)=P(X \leq x)/P(X\leq 2) = P(X \leq x)/(1-exp(-2lambda)), por lo que su m??todo de la funci??n inversa ya creado solo se modifica por una constante).

-- Para subir m??s puntos es su tarea consideren incluir en la comparaci??n la densidad beta con una elecci??n inteligente de par??metros como en las notas. 

-- Ahora, para los que vayan por el 10. ??Intervalos de confianza para las estimaciones de cada estrategia?


X~exp(\lambda)
y=x|x<=2

F_y(y=P(Y<=y))
P(x)


se termino de revisar el important sampling
Vari



#IMPLEMENTACI??N PRACTICA DE IMPORTANCE SAMPLING


#opcion crudo
m = 1
nsim = 5000
alpha=.05

means_crudo<-function (nsim,m=1,alpha=.05){
x <- runif (nsim, 0,1) 
phi <- function (x,m)  m*exp(-m*x)
theta_crudo <- mean (phi(x,m))
#theta_crudo
theta_crudo_var <- var (phi(x,m))
#theta_crudo_var
quant<-qnorm(alpha/2, lower.tail = FALSE)
int.upper<-theta_crudo+sqrt(theta_crudo_var/nsim)*quant
int.lower<-theta_crudo-sqrt(theta_crudo_var/nsim)*quant
return(c(theta_crudo,theta_crudo_var,int.upper,int.lower))
}

temp1<-means_crudo(nsim,m)


#opcion importance
m = 1
nsim = 5000
alpha=.05

means_importance<-function (nsim,m=1,alpha=.05){
u <- runif (nsim, 0 ,1)
x <- -log(1-u*(1-exp(-2*m))) #inversa de exp truncada en 2
g <- function(x,m) m*exp(-x*m)/(1-exp(-2*m)) #funcion elegida como peso de importancia
phi <- function(x,m) m*exp(-m*x) #funcion a estimar
importance <- function(x,m) phi(x,m)/g(x,m)
theta_importance <- mean (importance(x,m))
#theta_importance
theta_imp_var <- var(importance(x,m))
#theta_imp_var
quant<-qnorm(alpha/2, lower.tail = FALSE)
int.upper<-theta_importance+sqrt(theta_imp_var/nsim)*quant
int.lower<-theta_importance-sqrt(theta_imp_var/nsim)*quant
return (c(theta_importance,theta_imp_var,int.upper,int.lower))
}

temp2<-means_importance(nsim,m)

#mejora de varianza, checar
mejora_var<-(temp1[2]/temp2[2])-1

#se dan el numero de iteraciones
n<-seq(from=10,to=1000,by=10)

#graficar los estimadores
vec_crudo <- sapply(n,means_crudo)

plot(n,vec_crudo[1,],type = 'l',col='red',par=mfcol(c(2,2)))
plot(n,vec_crudo[2,],type = 'l')

vec_importance <- sapply(n,means_importance)
plot(n,vec_importance[1,],type = 'l')
plot(n,vec_importance[2,],type = 'l')
