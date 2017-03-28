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


mieloma<-read.csv("mielomamultiple.csv", header=T)
mieloma
mieloma.surv<-Surv(mieloma$Tiempo.de.vida, mieloma$Estatus)
mieloma.surv
mieloma.km<-survfit(mieloma.surv~Sexo, type="kaplan-meier", data=mieloma)

hazard.km <- 
function(mieloma){
time <- summary(mieloma)$time
ni <- summary(mieloma)$n.risk
di <- summary(mieloma)$n.event
surv <- summary(mieloma)$surv
stderr <- summary(mieloma)$std.err
hitilde <- di/ni
tau <- diff(time, lag = 1)#length of interval to right of ti
tau[length(tau) + 1] <- NA
hihat <- hitilde/tau
Hhat <-  -log(surv)
Htilde <- cumsum(hitilde)
sqri <- di/(ni^2)
se.Hhat <- stderr/surv
se.Htilde <- sqrt(cumsum(sqri))
hazardtable <- round(data.frame(time, ni, di, hihat, hitilde, Hhat,se.Hhat, Htilde, se.Htilde),4)
} 
hazard.km
#Calcularemos las funciones de riesgo acumulado para los distintos grupos
mieloma1<-mieloma[mieloma$Sexo==1,]
mieloma1.surv<-Surv(mieloma1$Tiempo.de.vida, mieloma1$Estatus)
mieloma1.km<-survfit(mieloma1.surv~1, type="kaplan-meier", data=mieloma1)
Riesgo1<-hazard.km(mieloma1.km)

mieloma2<-mieloma[mieloma$Sexo==2,]
mieloma2.surv<-Surv(mieloma2$Tiempo.de.vida, mieloma2$Estatus)
mieloma2.km<-survfit(mieloma2.surv~1, type="kaplan-meier", data=mieloma2)
Riesgo2<-hazard.km(mieloma2.km)

plot(Riesgo1$time,log(Riesgo1$Hhat),col=3,xlab="tiempo",ylab="Riesgo Acumulado",main="Comparación Funciones de Riesgo Acumulado (GROUP)")
lines(Riesgo1$time,log(Riesgo1$Hhat),lty=1)
lines(Riesgo2$time,log(Riesgo2$Hhat),lty=5)

prueba<-survdiff(mieloma.surv~Sexo, data=mieloma,rho=0)
prueba

mieloma.no<-coxph(mieloma.surv~1,mieloma,  method="breslow", na.action=na.exclude)
mieloma.edad<-coxph(mieloma.surv~Edad,mieloma,  method="breslow", na.action=na.exclude)
mieloma.edad
#NO es significativa
mieloma.bun<-coxph(mieloma.surv~Bun,mieloma,  method="breslow", na.action=na.exclude)
mieloma.bun
#SI es significativa
mieloma.ca<-coxph(mieloma.surv~Ca,mieloma,  method="breslow", na.action=na.exclude)
mieloma.ca
#NO es significativa
mieloma.hb<-coxph(mieloma.surv~Hb,mieloma,  method="breslow", na.action=na.exclude)
mieloma.hb
#SI es significativa
mieloma.pcells<-coxph(mieloma.surv~Pcells,mieloma,  method="breslow", na.action=na.exclude)
mieloma.pcells
#No es significativa
mieloma.protein<-coxph(mieloma.surv~Protein,mieloma,  method="breslow", na.action=na.exclude)
mieloma.protein

#queremos verificar bun hb y sexo

mieloma.bun.hb.sexo<-coxph(mieloma.surv~Bun+Hb+Sexo,mieloma,  method="breslow", na.action=na.exclude)
mieloma.bun.hb.sexo
summary(mieloma.bun.hb.sexo) #bun es la mas significativa en presencia de las otras

mieloma.hb.sexo<-coxph(mieloma.surv~Hb+Sexo,mieloma,  method="breslow", na.action=na.exclude)
1-pchisq(-2*mieloma.hb.sexo$loglik[2]+2*mieloma.bun.hb.sexo$loglik[2],1)
#Si es significativa la variable Bun

mieloma.bun.sexo<-coxph(mieloma.surv~Bun+Sexo,mieloma,  method="breslow", na.action=na.exclude)
1-pchisq(-2*mieloma.bun.sexo$loglik[2]+2*mieloma.bun.hb.sexo$loglik[2],1)
#Si es significativa la variable Bun

mieloma.hb.bun<-coxph(mieloma.surv~Hb+Bun,mieloma,  method="breslow", na.action=na.exclude)
1-pchisq(-2*mieloma.hb.bun$loglik[2]+2*mieloma.bun.hb.sexo$loglik[2],1)
#No es significativa la variable sexo

#Solo consideraremos las variables Hb y Bun
mieloma.hb.bun
summary(mieloma.hb.bun)
mieloma.bun
1-pchisq(-2*mieloma.bun$loglik[2]+2*mieloma.hb.bun$loglik[2],1)
 #Si es significativa la variable Hb en presencia de bun
mieloma.hb
1-pchisq(-2*mieloma.hb$loglik[2]+2*mieloma.hb.bun$loglik[2],1)
 #Si es significativa la variable bun en presencia de hb

mieloma.hb.sexo
summary(mieloma.hb.sexo)
mieloma.hb
1-pchisq(-2*mieloma.hb$loglik[2]+2*mieloma.hb.sexo$loglik[2],1)
#no es significativo hb en presencia de sexo

mieloma.inter.hb.bun<-coxph(mieloma.surv~Hb:Bun+Hb+Bun,mieloma,  method="breslow", na.action=na.exclude)
summary(mieloma.inter.hb.bun)
mieloma.hb.bun
1-pchisq(-2*mieloma.hb.bun$loglik[2]+2*mieloma.inter.hb.bun$loglik[2],1)
#No es signficativa la interaccion en presencia de las demás

hazard1.ph<-coxph(mieloma.surv~Bun+Hb,mieloma,  method="breslow", na.action=na.exclude)
# checando riesgos proporcionales
cox.hazard1.ph<-cox.zph(hazard1.ph)
cox.hazard1.ph

# Se grafican los residuos
par(mfrow=c(2,1))
plot(cox.hazard1.ph)

no se cumple el supuesto de riesgos proporcionales
# checando linealidad
No se puede checar la linealidad

#outliers
dfbeta.hazard1<-residuals(hazard1.ph,type="dfbeta")
par(mfrow=c(1,2))
for (j in 1:2){ plot(dfbeta.hazard1[,j],
ylab=names(coef(hazard1.ph))[j])
abline(h=0, lty=2) }


#########################
mieloma.ph<-coxph(mieloma.surv~Sexo+Edad+Bun+Ca+Hb+Pcells+Protein,mieloma,method="breslow",na.action=na.exclude)
mieloma.ph
summary(mieloma.ph)

mieloma.ph3<-coxph(mieloma.surv~Bun+Hb+Bun:Hb,data= mieloma,method="breslow",na.action=na.exclude)
mieloma.ph3
summary(mieloma.ph3)

#incluir la edad mínima
mieloma$edad<-mieloma$Edad-min(mieloma$Edad)
#inclutendo la variable edad
modelosexEdad<-coxph(mieloma.surv~Sexo+Edad, mieloma, method="breslow", na.action=na.exclude)
modelosexEdad
#en el paso 1 del modelo general se encuentran como candidatos Bun y Hb considerando 1 por 1 
mieloma.ph2<-coxph(mieloma.surv~Bun+Hb, mieloma, method="breslow", na.action=na.exclude)
summary(mieloma.ph2)
#si el p valor es menor que el nivel de significancia para alguna de las variables entonces esa variable es significativa para el modelo dado que la otra ya esataba


#funcion riesgo base
BRiesgo<-basehaz(modelo9.ph,centered=T) 
XX<-as.matrix(BRiesgo)
plot(XX[,2],XX[,1],type="s",xlab="Tiempo", ylab="Riesgo",main="Función  Riesgo Base")
grid()
legend(locator(1),legend="Baseline Hazard",lty=1)

