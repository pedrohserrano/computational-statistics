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
#Graficamos en un solo plot las 2 curvas de supervivencia
plot(cns2.km, conf.int=F,main="Comparativo Grupo 1 vs Grupo 0", xlab="A~os", col=1:2,lab=c(10,10,7), cex=2, lty=2:3)
legend(locator(1), legend=c("Grupo 0", "Grupo 1"), lty=2:3, col=1:2)
survdiff(cns2.surv~GROUP, data=cns2, rho=1)
summary(cns2.km)

##################
#veamos la significancia de las variable por separado
cns2.no<-coxph(cns2.surv~1,cns2,  method="breslow", na.action=na.exclude)
cns2.no
cns2.sex<-coxph(cns2.surv~SEX,cns2,  method="breslow", na.action=na.exclude)
cns2.sex
#No es significativa
cns2.kps.pre<-coxph(cns2.surv~KPS.PRE.,cns2,  method="breslow", na.action=na.exclude)
cns2.kps.pre
#Si es significativa
cns2.rad4000<-coxph(cns2.surv~RAD4000,cns2,  method="breslow", na.action=na.exclude)
cns2.rad4000
#Si es significativa
cns2.group<-coxph(cns2.surv~GROUP,cns2,  method="breslow", na.action=na.exclude)
cns2.group
#si es significativa

################

#Ejercicio 2
#modelo completo sex kps y group
cns2.sex.kps.pre.group<-coxph(cns2.surv~SEX+KPS.PRE.+GROUP,cns2,  method="breslow", na.action=na.exclude)
cns2.sex.kps.pre.group
#modelo reducido kps y group
cns2.kps.pre.group<-coxph(cns2.surv~KPS.PRE.+GROUP,cns2,  method="breslow", na.action=na.exclude)
cns2.kps.pre.group
#significancia de sex
1-pchisq(-2*cns2.kps.pre.group$loglik[2]+2*cns2.sex.kps.pre.group$loglik[2],1)
#Si es significativa la variable SEX dado que están KPS.PRE y GROUP en el modelo

#################

#Ejercicio 3
#modelo completo rad kps y group
cns2.rad4000.kps.pre.group<-coxph(cns2.surv~RAD4000+KPS.PRE.+GROUP,cns2,  method="breslow", na.action=na.exclude)
cns2.rad4000.kps.pre.group
#modelo reducido kps y group
cns2.kps.pre.group
#significancia de radiation
1-pchisq(-2*cns2.kps.pre.group$loglik[2]+2*cns2.rad4000.kps.pre.group$loglik[2],1)
#NO es significativa la variable RAD dado que están KPS.PRE y GROUP en el modelo

#################

#Ejercicio 4
#modelo completo sex rad kps y group
cns2.sex.rad4000.kps.pre.group<-coxph(cns2.surv~SEX+RAD4000+KPS.PRE.+GROUP,cns2,  method="breslow", na.action=na.exclude)
cns2.sex.rad4000.kps.pre.group
#modelo reducido kps y group
cns2.kps.pre.group
#significancia de radiation y sex
1-pchisq(-2*cns2.kps.pre.group$loglik[2]+2*cns2.sex.rad4000.kps.pre.group$loglik[2],1)
#SI son significativas las variables RAD y SEX dado que están KPS.PRE y GROUP en el modelo

#################

#Ejercicio 5
#modelo completo sex:kps,  sex kps y group
cns2.inter.sex.kps.pre.group<-coxph(cns2.surv~SEX:KPS.PRE.+SEX+KPS.PRE.+GROUP,cns2,  method="breslow", na.action=na.exclude)
cns2.inter.sex.kps.pre.group
#modelo reducido sex, kps y group
cns2.sex.kps.pre.group<-coxph(cns2.surv~SEX+KPS.PRE.+GROUP,cns2,  method="breslow", na.action=na.exclude)
cns2.sex.kps.pre.group
#significancia de interaccion sex con kps
1-pchisq(-2*cns2.sex.kps.pre.group$loglik[2]+2*cns2.inter.sex.kps.pre.group$loglik[2],1)

#modelo completo group:kps, sex kps y group
cns2.inter2.sex.kps.pre.group<-coxph(cns2.surv~GROUP:KPS.PRE.+SEX+KPS.PRE.+GROUP,cns2,  method="breslow", na.action=na.exclude)
cns2.inter2.sex.kps.pre.group
#modelo reducido sex, kps y group
cns2.sex.kps.pre.group
#significancia de interaccion group con kps
1-pchisq(-2*cns2.sex.kps.pre.group$loglik[2]+2*cns2.inter2.sex.kps.pre.group$loglik[2],1)

#modelo completo sex:group, sex kps y group
cns2.inter3.sex.kps.pre.group<-coxph(cns2.surv~SEX:GROUP+SEX+KPS.PRE.+GROUP,cns2,  method="breslow", na.action=na.exclude)
cns2.inter3.sex.kps.pre.group
#modelo reducido sex, kps y group
cns2.sex.kps.pre.group
#significancia de interaccion sex con group
1-pchisq(-2*cns2.sex.kps.pre.group$loglik[2]+2*cns2.inter3.sex.kps.pre.group$loglik[2],1)