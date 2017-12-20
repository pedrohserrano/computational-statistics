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

#se llama a la base de datos de los asegurados 
asegcan<-read.csv("Asegcan.csv", header=T)
asegcan
#se crea el objeto survival
asegcan.surv<-Surv(asegcan$Time, asegcan$C)
asegcan.surv


#veamos la supervivencia dependiendo de la frecuencia de pago

asegcan.km<-survfit(asegcan.surv~FP, type="kaplan-meier", data=asegcan)
asegcan.km
#Graficamos en un solo plot las 4 curvas de supervivencia
plot(asegcan.km, conf.int=F,main="Comparacion de curvas (Forma de Pago )", xlab="Dias", col=1:4,lab=c(10,10,7), cex=4, lty=1:4)
legend(locator(1), legend=c("Anual", "Semestral","Trimestral","Mensual"), lty=1:4, col=1:4)
#a simple vista podemos decir que mientras mayor sea su plazo de pago, más tardan en cancelar

#####################ajuste del modelo

#transformacion de edad
asegcan$edad<-asegcan$Age-min(asegcan$Age)
#se crean indicadoras en nuevas columnas del factor FP
asegcan$FP1[asegcan$FP==1]<-1
asegcan$FP1[asegcan$FP!=1]<-0
asegcan$FP2[asegcan$FP==2]<-1
asegcan$FP2[asegcan$FP!=2]<-0
asegcan$FP3[asegcan$FP==3]<-1
asegcan$FP3[asegcan$FP!=3]<-0
asegcan$FP4[asegcan$FP==4]<-1
asegcan$FP4[asegcan$FP!=4]<-0
asegcan

######################paso1
#significancia del modelo, se consideran todas las variables
asegcan.todas<-coxph(asegcan.surv~edad+FP1+FP2+FP3+EP+Sex+Premium+Policy+Coinsurance,asegcan,  method="breslow", na.action=na.exclude)
summary(asegcan.todas)
#el modelo si es significativo hay al menos una significativa
asegcan.fp<-coxph(asegcan.surv~FP,asegcan,  method="breslow", na.action=na.exclude)
asegcan.fp
#Significancia de variables solas
asegcan.ep<-coxph(asegcan.surv~EP,asegcan,  method="breslow", na.action=na.exclude)
asegcan.ep
#No es significativa
asegcan.sex<-coxph(asegcan.surv~Sex,asegcan,  method="breslow", na.action=na.exclude)
asegcan.sex
#No es significativa
asegcan.premium<-coxph(asegcan.surv~Premium,asegcan,  method="breslow", na.action=na.exclude)
asegcan.premium
#Si es significativa
asegcan.policy<-coxph(asegcan.surv~Policy,asegcan,  method="breslow", na.action=na.exclude)
asegcan.policy
#No es significativa
asegcan.co<-coxph(asegcan.surv~Coinsurance,asegcan,  method="breslow", na.action=na.exclude)
asegcan.co
#No es significativa
asegcan.fp<-coxph(asegcan.surv~FP,asegcan,  method="breslow", na.action=na.exclude)

asegcan.e<-coxph(asegcan.surv~edad,asegcan,  method="breslow", na.action=na.exclude)
asegcan.fp1<-coxph(asegcan.surv~FP1+FP2+FP3,asegcan,  method="breslow", na.action=na.exclude)


#por lo que el midelo ajustado será solo con la variable premium, considerando despues FP y edad 

###########################paso2
# no es necesario ya que solo se eligio una variable

###########################paso3
#vemos si hay relacion con las variables que se descartaron al principio
asegcan.premium.ep<-coxph(asegcan.surv~Premium+EP,asegcan,  method="breslow", na.action=na.exclude)
1-pchisq(-2*asegcan.premium$loglik[2]+2*asegcan.premium.ep$loglik[2],1)
#No es significativa extraprima
asegcan.premium.sex<-coxph(asegcan.surv~Premium+Sex,asegcan,  method="breslow", na.action=na.exclude)
1-pchisq(-2*asegcan.premium$loglik[2]+2*asegcan.sex$loglik[2],1)
#No es significativa el sexo
asegcan.premium.policy<-coxph(asegcan.surv~Premium+Policy,asegcan,  method="breslow", na.action=na.exclude)
1-pchisq(-2*asegcan.premium$loglik[2]+2*asegcan.policy$loglik[2],1)
#No es significativa la poliza (renovacion)
asegcan.premium.co<-coxph(asegcan.surv~Premium+Coinsurance,asegcan,  method="breslow", na.action=na.exclude)
1-pchisq(-2*asegcan.premium$loglik[2]+2*asegcan.co$loglik[2],1)
#No es significativa el si pago coaseguro
#no se agregan variables nuevas al modelo

##########################paso4
#agregamos las variables de interés del investigador
#modelo completo fp1,fp2,fp3, prima, edad
asegcan.age.fp1.fp2.fp3.premium<-coxph(asegcan.surv~FP1+FP2+FP3+edad+Premium,asegcan,  method="breslow", na.action=na.exclude)
#modelo reducido premium
asegcan.premium
1-pchisq(-2*asegcan.premium$loglik[2]+2*asegcan.age.fp1.fp2.fp3.premium$loglik[2],3)
#SI son significativas las variables al agregarlas al mismo tiempo
#con el estadístico se ve igual
-2*log(19.6/25.8)
qchisq(.05,2)

#modelo prima y fp
asegcan.fp1.premium<-coxph(asegcan.surv~FP1+Premium,asegcan,  method="breslow", na.action=na.exclude)
#modelo prima
asegcan.premium
1-pchisq(-2*asegcan.premium$loglik[2]+2*asegcan.fp1.premium$loglik[2],1)

asegcan.fp1.fp2.premium<-coxph(asegcan.surv~FP1+FP2+Premium,asegcan,  method="breslow", na.action=na.exclude)
asegcan.fp1.premium
1-pchisq(-2*asegcan.fp1.premium$loglik[2]+2*asegcan.fp1.fp2.premium$loglik[2],1)

#vemos la significancia en precencia de las demas
summary(asegcan.age.fp1.fp2.fp3.premium)
#ninguna lo es, pero no podemos descartar las que son de interés, tendremos que descartar prima, comprobamos si es significativa incluirla

#modelo completo fp1,fp2,fp3, prima, edad
asegcan.age.fp1.fp2.fp3.premium<-coxph(asegcan.surv~FP1+FP2+FP3+edad+Premium,asegcan,  method="breslow", na.action=na.exclude)
#modelo completo fp1,fp2,fp3, edad
asegcan.age.fp1.fp2.fp3<-coxph(asegcan.surv~FP1+FP2+FP3+edad,asegcan,  method="breslow", na.action=na.exclude)
1-pchisq(-2*asegcan.age.fp1.fp2.fp3$loglik[2]+2*asegcan.age.fp1.fp2.fp3.premium$loglik[2],1)
#no es significativa la variable prima para agregarla en el modelo

summary(asegcan.age.fp1.fp2.fp3)

########################paso4.1
#veamos si las interacciones entre las variables son significativas
#modelo completo 
asegcan.age.fp1.fp2.fp3.intertodas<-coxph(asegcan.surv~edad+FP1+FP2+FP3+FP1:edad+FP2:edad+FP3:edad,asegcan,  method="breslow", na.action=na.exclude)
summary(asegcan.age.fp1.fp2.fp3.intertodas)
asegcan.age.fp1.fp2.fp3
1-pchisq(-2*asegcan.age.fp1.fp2.fp3$loglik[2]+2*asegcan.age.fp1.fp2.fp3.intertodas$loglik[2],4)
#las interacciones no son significativas


#####################validacion del modelo
#funcion de supervivencia del modelo de riesgos proporcionales y comparacion con kaplan meier
plot(survfit(asegcan.age.fp1.fp2.fp3),col=2, main="Bandas de Confianza de la Funcion Supervivecia de la Cartera")
lines(survfit(asegcan.surv~1, type="kaplan-meier", asegcan),col=1)
legend(locator(1), legend=c("Supervivencia del Modelo de Cox", "Supervivencia con Kaplan Meier"), lty=2:1, col=2:1)

# checando riesgos proporcionales
modelo.ph<-cox.zph(asegcan.age.fp1.fp2.fp3)
modelo.ph
#no hay evidencia de la viloacion de supuestos

# Se grafican los residuos
par(mfrow=c(2,2))
plot(modelo.ph,main="Grafico de Residuales")

# Se grafica linealidad (solo )
modelo.marting<-residuals(asegcan.age.fp1.fp2.fp3,type='martingale')
modelo.marting
X<-as.matrix(asegcan[,c("edad")])
scatter.smooth(X[,1],modelo.marting, type="p",pch=".",xlab="Edad",ylab="Residuos martingalas",main="Prueba de linealidad") 

#outliers datos influyentes
dfbeta.modelo.ph<-residuals(asegcan.age.fp1.fp2.fp3,type="dfbeta")
par(mfrow=c(2,2))
for (j in 1:4){ plot(dfbeta.modelo.ph[,j],
main="Outliers",ylab=names(coef(asegcan.age.fp1.fp2.fp3))[j])
abline(h=0, lty=2) }


#######################estimacion de funcion de riesgo base
h0.asegcan<-basehaz(asegcan.age.fp1.fp2.fp3,centered=T)
h0.asegcan
 XX<-as.matrix(h0.asegcan)
plot(XX[,2],XX[,1],type="s",xlab="Tiempo",ylab="Riesgo",main="Estimacion de la Funcion de Riesgo Base",col=4)


#####################validacion del modelo con preium
#funcion de supervivencia del modelo de riesgos proporcionales y comparacion con kaplan meier
plot(survfit(asegcan.premium),col=2, main="Bandas de Confianza de la Funcion Supervivecia de la Cartera")
lines(survfit(asegcan.surv~1, type="kaplan-meier", asegcan),col=1)
legend(locator(1), legend=c("Supervivencia del Modelo de Cox", "Supervivencia con Kaplan Meier"), lty=2:1, col=2:1)

# checando riesgos proporcionales
modelop.ph<-cox.zph(asegcan.premium)
modelop.ph
#no hay evidencia de la viloacion de supuestos

# Se grafican los residuos
plot(modelop.ph,main="Grafico de Residuales")

# Se grafica linealidad (solo )
modelop.marting<-residuals(asegcan.premium,type='martingale')
modelop.marting
Xp<-as.matrix(asegcan[,c("edad")])
scatter.smooth(Xp[,1],modelo.marting, type="p",pch=".",xlab="Edad",ylab="Residuos martingalas",main="Prueba de linealidad") 

#outliers datos influyentes
dfbeta.modelop.ph<-residuals(asegcan.premium,type="dfbeta")
 plot(dfbeta.modelop.ph,main="Outliers",ylab=names(coef(asegcan.premium))[1])
abline(h=0, lty=2)


#######################estimacion de funcion de riesgo base
h0p.asegcan<-basehaz(asegcan.premium,centered=T)
h0p.asegcan
 XXp<-as.matrix(h0p.asegcan)
plot(XXp[,2],XXp[,1],type="s",xlab="Tiempo",ylab="Riesgo",main="Estimacion de la Funcion de Riesgo Base",col=4)
