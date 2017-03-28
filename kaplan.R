#Por unica ves, despues de instalar R, se deben instalar las siguientes librerias


install.packages('survival')
install.packages('KMsurv')
install.packages('coin')

# Siempre que empiece una sesion y requiera realizar analisis de supervivencia es recomendable cargar las
# librerias 

help()
library()
library(MASS)
library(graphics)
library(lattice)
library(foreign)
library(survival)
library(splines)
library(KMsurv)
require(grDevices)
library(stats)
library(coin)  #SurvTest
library(mvtnorm)
library(modeltools)
library(stats4)

#Cambiar el directorio de trabajo

#para importar datos tipo excel. Debe ser un archivo de valores separados por comas
aml<-read.csv("AML.csv", header=T)
aml

#para ver la columna tiempo de la base de datos aml
aml$time

# creando un objeto survival
aml.surv<-Surv(aml$time, aml$status)
aml.surv

# creando una base de datos de la poblacion con quimio=1
aml1<-aml[aml$quimio==1, ]
aml1

aml1.surv<-Surv(aml1$time, aml1$status)
aml1.surv

#Estimando el Kaplan-Meier para los dos grupos

aml.km<-survfit(aml.surv~1, type="kaplan-meier", data=aml)

#Para conocer los valores del funci�n de supervivencia y los intervalos de confianza
summary(aml.km)

# Para obtener los valores de la funcion de supervivencia y los tiempos de falla
aml.km$surv
aml.km$time
aml.km$n.event
aml.km$n.ris

#Gr�fica de la funci�n de supervivencia
plot(aml.km,xlab="Tiempo",ylab="Funci�n de Supervivencia", main="Funci�n de Supervivencia Kaplan Meier",col=3,cex=1.25)
legend(locator(1), legend="KM", lty=1,col=3)


#Riesgo acumulado

H.hat.aml<- -log(aml.km$surv)

#Papel de probabilidad Weibull

x<- log(-log(aml.km$surv))
y<-log(aml.km$time)
plot(x,y, pch=16, xlab="Ln(t)", ylab=" Log(-Log(S(t)))",main="Papel de Probabilidad Weibull")

aml1<-aml[aml$quimio==1, ]
aml1.surv<-Surv(aml1$tiempo, aml1$censura)
aml1.km<-survfit(aml1.surv~1, type="kaplan-meier", data=aml1)

aml2<-aml[aml$quimio==0, ]
aml2.surv<-Surv(aml2$tiempo, aml2$censura)
aml2.km<-survfit(aml2.surv~1, type="kaplan-meier", data=aml2)

plot(aml1.km,xlab="Tiempo",ylab="Funci�n de Supervivencia", main="Funci�n de Supervivencia Kaplan Meier", conf.int=F)

par(new=TRUE)
plot(aml2.km,xlab="Tiempo",ylab="Funci�n de Supervivencia", main="Funci�n de Supervivencia Kaplan Meier", conf.int=F)
par(mfrow=c(2,1))


Para incorporar la variable quimio
aml.km<-survfit(aml.surv~quimio, type="kaplan-meier", data=aml)
plot(aml.km, conf.int=F, xlab="time", col=1:2,lab=c(10,10,7), cex=2, lty=1:2)
legend(locator(1), legend=c("No Quimio", "Quimio"), lty=1:2, col=1:2)

y<-log(aml1.km$time)
qqnorm(y, pch=16)

y<-log(aml2.km$time)

qqnorm(y, pch=16, col=2)
curve(dweibull(x,3,2), 0, 5, lty=1)


ss<- -log(aml1.km$surv/(1-aml1.km$surv))
st<-log(aml1.km$time)
plot(st,ss, pch=16)
