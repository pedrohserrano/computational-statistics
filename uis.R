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
library(km)

uis<-read.csv("uis.csv",header=T)
uis.surv<-Surv(uis$time, uis$censor)
uis.surv
#survival para tratamientos
uis.km<-survfit(uis.surv~treat, type="kaplan-meier", data=uis)
plot(uis.km, conf.int=F,main="Comparación de curvas tratamiento", xlab="time", col=1:2,lab=c(10,10,7), cex=2, lty=1:2)
legend(locator(1), legend=c("corto", "largo"), lty=1:2, col=1:2)

#se crean indicadorasen nuevas columnas de que tipo de consumo es
uis$herycoc[uis$hercoc==1]<-1
uis$herycoc[uis$hercoc!=1]<-0

uis$her[uis$hercoc==2]<-1
uis$her[uis$hercoc!=2]<-0

uis$coc[uis$hercoc==3]<-1
uis$coc[uis$hercoc!=3]<-0

uis$ninguna[uis$hercoc==4]<-1
uis$ninguna[uis$hercoc!=4]<-0

#se crean las indicadoras en nuevas columnas de los datos historicos del consumo de drogas
uis$nunca[uis$ivhx==1]<-1
uis$nunca[uis$ivhx!=1]<-0

uis$tiempo[uis$ivhx==2]<-1
uis$tiempo[uis$ivhx!=2]<-0

uis$recien[uis$ivhx==3]<-1
uis$recien[uis$ivhx!=3]<-0

uis
#probemos variable por variable
uis.no<-coxph(uis.surv~1,uis,  method="breslow", na.action=na.exclude)
uis.no
uis.id<-coxph(uis.surv~id,uis,  method="breslow", na.action=na.exclude)
uis.age<-coxph(uis.surv~age,uis,  method="breslow", na.action=na.exclude)
uis.becktota<-coxph(uis.surv~becktota,uis,  method="breslow", na.action=na.exclude)
uis.ndrugtx<-coxph(uis.surv~ndrugtx,uis,  method="breslow", na.action=na.exclude)
uis.race<-coxph(uis.surv~race,uis,  method="breslow", na.action=na.exclude)
uis.treat<-coxph(uis.surv~treat,uis,  method="breslow", na.action=na.exclude)
uis.site<-coxph(uis.surv~site,uis,  method="breslow", na.action=na.exclude)
uis.los<-coxph(uis.surv~los,uis,  method="breslow", na.action=na.exclude)
uis.hercoc<-coxph(uis.surv~herycoc+her+coc+ninguna,uis,method="breslow", na.action=na.exclude)
uis.ivhx<-coxph(uis.surv~nunca+tiempo+recien)
uis.nunca<-coxph(uis.surv~nunca,uis,  method="breslow", na.action=na.exclude)
uis.tiempo<-coxph(uis.surv~tiempo,uis,  method="breslow", na.action=na.exclude)
uis.recien<-coxph(uis.surv~recien,uis,  method="breslow", na.action=na.exclude)

uis.id  #no
uis.age  #no
uis.becktota  #si
uis.ndrugtx  #si
uis.race  #si
uis.treat  #si
uis.site  #no
uis.los  #si
#hercoc en general no
uis.herycoc  #no
uis.her  #si
uis.coc  #no
uis.ninguna  #no
#el general si es significativo entonces al menos alguno es significativo
uis.nunca  #si
uis.tiempo  #no
uis.recien  #si

#ahora corremos el modelo general
#en el general nos da que al menos alguna es significativa
#despues se observa la significancia de cada una en presencia de las demas
#descartamos la que no sea (becktota)
#((((si en el global dice que hay que agregar e individualmente no es significativa entonces hay que recategorizar))))