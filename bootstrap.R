library(boot)

#parametros
sample.size<-50
x<-runif(sample.size)
fun<-function(x) var(x) ##esta funcion define lo que nos interesa
nboot<-1000 #numero de remuestreo bootstrap

##bootsatrap-mc el basico
vec<-numeric(nboot)
for (i in 1:nboot){ #se puede hacer com apply
  x.b<-sample(x, sample.size, replace = TRUE)
  vec[i]<-fun(x.b)
}

hit(x)
var(x) #estimador sin remuestreo
hist(vec) #distribucion dek estimador fun
summary(vec)
1/12 #varianza de la uniforme
var(vec) #varianza del estimador de la varianza

#importa la calidad de la representacion de la muestra

##AHORA LO VEMOS USANDO EL PAQUETE BOOT
my.fun<-function(x,ind) fun(x[ind]) #hay que definir una funcion que defina 2 argumentos
#i es un vector de indices
boot.res<-boot(x,my.fun,R=nboot) #se come una muestra original y luego una funcion estadistica
boot.res$t #el equivalente al vec que hicimos
hist(boot.res$t)
plot(boot.res)
summary(boot.res)
#si en la muestra original se le pone una muestra chica se parece mas a la chi cuadrada
alpha<-.05
#1)metodo de percentiles
int.quant<-quantile(vec, c(alpha/2, 1-alpha/2))
int.quant<-quantile(boot.res$t, c(alpha/2, 1-alpha/2))

int.quant-bootres$t0

#?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
library(boot)

#parametros
n<-10
x<-rexp(n, rate=1)
mean(x)

b<-10

##bootsatrap-mc el basico
bootstrap.means <-numeric(b)
for (i in 1:b){ #se puede hacer com apply
  x.b<-sample(x, size=n, replace = TRUE)
  bootstrap.means[i]<-mean(x.b)
}

hist(bootstrap.means)
var(bootstrap.means)

var(x) #estimador sin remuestreo
hist(vec) #distribucion dek estimador fun
summary(vec)
1/12 #varianza de la uniforme
var(vec) #varianza del estimador de la varianza

#importa la calidad de la representacion de la muestra

##AHORA LO VEMOS USANDO EL PAQUETE BOOT
my.fun<-function(x,ind) fun(x[ind]) #hay que definir una funcion que defina 2 argumentos
#i es un vector de indices
boot.res<-boot(x,my.fun,R=nboot) #se come una muestra original y luego una funcion estadistica
boot.res$t #el equivalente al vec que hicimos
hist(boot.res$t)
plot(boot.res)
summary(boot.res)
#si en la muestra original se le pone una muestra chica se parece mas a la chi cuadrada
alpha<-.05
#1)metodo de percentiles
int.quant<-quantile(vec, c(alpha/2, 1-alpha/2))
int.quant<-quantile(boot.res$t, c(alpha/2, 1-alpha/2))

int.quant-bootres$t0
