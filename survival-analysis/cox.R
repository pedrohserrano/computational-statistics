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

cns2<-read.csv("cns2.csv", header=T)
cns2.surv<-Surv(cns2$B3TODEATH, cns2$STATUS)

#Ejercicio 1

cns2.km<-survfit(cns2.surv~GROUP, type="kaplan-meier", data=cns2)
#Graficamos en un solo plot las 3 curvas de supervivencia
plot(cns2.km, conf.int=F,main="Comparación de curvas (GROUP)", xlab="time", col=1:2,lab=c(10,10,7), cex=2, lty=1:2)
legend(locator(1), legend=c("No prior radiation", "Prior radiation"), lty=1:2, col=1:2)

hazard.km <- 
function(data){
## Author: Mara Tableman  Date: 20 November 2002 
## Purpose:  To compute the two types of empirical hazards,
##           and the cumulative hazards along their s.e.'s
## Arguments:  data is survfit object
time <- summary(data)$time
ni <- summary(data)$n.risk
di <- summary(data)$n.event
surv <- summary(data)$surv
stderr <- summary(data)$std.err
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

#Calcularemos las funciones de riesgo acumulado para los distintos grupos
cns21<-cns2[cns2$GROUP==1,]
cns21.surv<-Surv(cns21$B3TODEATH, cns21$STATUS)
cns21.km<-survfit(cns21.surv~1, type="kaplan-meier", data=cns21)
Riesgo1<-hazard.km(cns21.km)

cns22<-cns2[cns2$GROUP==0,]
cns22.surv<-Surv(cns22$B3TODEATH, cns22$STATUS)
cns22.km<-survfit(cns22.surv~1, type="kaplan-meier", data=cns22)
Riesgo2<-hazard.km(cns22.km)

plot(Riesgo1$time,log(Riesgo1$Hhat),col=3,xlab="tiempo",ylab="Riesgo Acumulado",main="Comparación Funciones de Riesgo Acumulado (GROUP)")
lines(Riesgo1$time,log(Riesgo1$Hhat),lty=1)
lines(Riesgo2$time,log(Riesgo2$Hhat),lty=5)

prueba<-survdiff(cns2.surv~GROUP, data=cns2,rho=0)

#Ejercicio 2
cns2$LESSUP1<-(cns2$LESSUP==1)
cns2$LESSUP2<-(cns2$LESSUP==2)

cns2.no<-coxph(cns2.surv~1,cns2,  method="breslow", na.action=na.exclude)
cns2.sex<-coxph(cns2.surv~SEX,cns2,  method="breslow", na.action=na.exclude)
#No es significativa
cns2.age<-coxph(cns2.surv~AGE,cns2,  method="breslow", na.action=na.exclude)
#No es significativa
cns2.kps.pre.<-coxph(cns2.surv~KPS.PRE.,cns2,  method="breslow", na.action=na.exclude)
#Si es significativa
cns2.lessing<-coxph(cns2.surv~LESSING,cns2,  method="breslow", na.action=na.exclude)
#No es significativa
cns2.lesdeep<-coxph(cns2.surv~LESDEEP,cns2,  method="breslow", na.action=na.exclude)
#No es significativa
cns2.lessup<-coxph(cns2.surv~LESSUP1+LESSUP2,cns2,  method="breslow", na.action=na.exclude)
#No es significativa
cns2.lessup1<-coxph(cns2.surv~LESSUP1,cns2,  method="breslow", na.action=na.exclude)
#No es significativa
cns2.lessup2<-coxph(cns2.surv~LESSUP2,cns2,  method="breslow", na.action=na.exclude)
#No es significativa
cns2.proc<-coxph(cns2.surv~PROC,cns2,  method="breslow", na.action=na.exclude)
#No es significativa
cns2.rad4000<-coxph(cns2.surv~RAD4000,cns2,  method="breslow", na.action=na.exclude)
#Es significativa
cns2.chemoprior<-coxph(cns2.surv~CHEMOPRIOR,cns2,  method="breslow", na.action=na.exclude)
#No es significativa
cns2.response<-coxph(cns2.surv~RESPONSE,cns2,  method="breslow", na.action=na.exclude)
#Es significativa

#Paso 2
cns2.kps.pre.rad4000.response<-coxph(cns2.surv~KPS.PRE.+RAD4000+RESPONSE,cns2,  method="breslow", na.action=na.exclude)

cns2.rad4000.response<-coxph(cns2.surv~RAD4000+RESPONSE,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.rad4000.response$loglik[2]+2*cns2.kps.pre.rad4000.response$loglik[2],1)
#No es significativa la variable KPSE.PRE

cns2.kps.pre.response<-coxph(cns2.surv~KPS.PRE.+RESPONSE,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.kps.pre.response$loglik[2]+2*cns2.kps.pre.rad4000.response$loglik[2],1)
#No es significativa la variable RAD4000

cns2.kps.pre.rad4000<-coxph(cns2.surv~KPS.PRE.+RAD4000,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.kps.pre.rad4000$loglik[2]+2*cns2.kps.pre.rad4000.response$loglik[2],1)
#Es significativa la variable response

#Solo consideraremos las variables response y rad4000
cns2.rad4000.response<-coxph(cns2.surv~RAD4000+RESPONSE,cns2,  method="breslow", na.action=na.exclude)
cns2.response<-coxph(cns2.surv~RESPONSE,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.response$loglik[2]+2*cns2.rad4000.response$loglik[2],1)
 #No es significativa la variable RAD 4000

cns2.rad4000<-coxph(cns2.surv~RAD4000,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.rad4000$loglik[2]+2*cns2.rad4000.response$loglik[2],1)
 #Es significativa la variable response

#Solo consideraremos las variables response y kps.pre.
cns2.kps.pre.response<-coxph(cns2.surv~KPS.PRE.+RESPONSE,cns2,  method="breslow", na.action=na.exclude)
cns2.response<-coxph(cns2.surv~RESPONSE,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.response$loglik[2]+2*cns2.kps.pre.response$loglik[2],1)
 #No es significativa la variable kps.pre.

cns2.kps.pre.<-coxph(cns2.surv~KPS.PRE.,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.kps.pre.$loglik[2]+2*cns2.kps.pre.response$loglik[2],1)

#Es significativa la variable response

Paso #3
cns2.sex.response<-coxph(cns2.surv~SEX+RESPONSE,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.response$loglik[2]+2*cns2.sex.response$loglik[2],1)
 #No es significativa la variable sex

cns2.age.response<-coxph(cns2.surv~AGE+RESPONSE,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.response$loglik[2]+2*cns2.age.response$loglik[2],1)
#No es significativa la variable age

cns2.lessing.response<-coxph(cns2.surv~LESSING+RESPONSE,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.response$loglik[2]+2*cns2.lessing.response$loglik[2],1)
#No es significativa la variable lessing

cns2.lesdeep.response<-coxph(cns2.surv~LESDEEP+RESPONSE,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.response$loglik[2]+2*cns2.lesdeep.response$loglik[2],1)
#No es significativa la variable lesdeep

cns2.lessup.response<-coxph(cns2.surv~LESSUP1+LESSUP2+RESPONSE,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.response$loglik[2]+2*cns2.lessup.response$loglik[2],2)
#No es significativa la variable lessup

cns2.proc.response<-coxph(cns2.surv~PROC+RESPONSE,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.response$loglik[2]+2*cns2.proc.response$loglik[2],1)
#No es significativa la variable proc

cns2.chemoprior.response<-coxph(cns2.surv~CHEMOPRIOR+RESPONSE,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.response$loglik[2]+2*cns2.chemoprior.response$loglik[2],1)
#No es significativa la variable chempoprior

#Paso 4
cns2.group.response<-coxph(cns2.surv~GROUP+RESPONSE,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.response$loglik[2]+2*cns2.group.response$loglik[2],1)

cns2.group<-coxph(cns2.surv~GROUP,cns2,  method="breslow", na.action=na.exclude)

cns2.group.response.int<-coxph(cns2.surv~GROUP+RESPONSE+GROUP:RESPONSE,cns2,  method="breslow", na.action=na.exclude)
1-pchisq(-2*cns2.group.response$loglik[2]+2*cns2.group.response.int$loglik[2],1)
#No es significativa la interacción entre las variables group y response

modelo1.ph<-coxph(cns2.surv~GROUP+RESPONSE,cns2,  method="breslow", na.action=na.exclude)
# checando riesgos proporcionales
cox.modelo1.ph<-cox.zph(modelo1.ph)

# Se obtiene los estadísticos de prueba
cox.modelo1.ph
# Se grafican los residuos
par(mfrow=c(2,1))
plot(cox.modelo1.ph)

se cumple el supuesto de riesgos proporcionales
# checando linealidad
No se puede checar la linealidad

#outliers
dfbeta.modelo1<-residuals(modelo1.ph,type="dfbeta")
par(mfrow=c(1,2))
for (j in 1:2){ plot(dfbeta.modelo1[,j],
ylab=names(coef(modelo1.ph))[j])
abline(h=0, lty=2) }
