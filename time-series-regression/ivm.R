#####cross sectional regression

#ordinary least squares,lets consider a data frame called ivm, containing for age, salary an exper. we want regress various forms of age exper on salary
#a linear (model) regression might be

op<-read.csv("ivm.csv",header=T)
op

#campos 
id<-op$id
bim<-op$bim
u_op<-op$u_op
pob_0a9<-op$pob_0a9
fam_ivm<-op$fam_ivm
fam_rec_ivm<-op$fam_rec_ivm
ivm<-op$ivm
rec_ivm<-op$rec_ivm
monto_ivm<-op$monto_ivm

###regresion ivm vs pob0a9
reg_ivm_pob_0a9<-lm(ivm~pob_0a9)
par(mfrow=c(2,2))
plot(reg_ivm_pob_0a9,main="Regresion IVM vs pob 0 a 9")
salida1<-summary(reg_ivm_pob_0a9)
salida1
#si es significativa

#regresion pob0a9 vs ivm
reg_pob_0a9_ivm<-lm(pob_0a9~ivm)
par(mfrow=c(2,2))
plot(reg_pob_0a9_ivm,main="Regresion pob 0 a 9 vs IVM")
summary(reg_pob_0a9_ivm)


#nombrando salidas
ssr_ivm_pob_0a9 <- deviance(reg_ivm_pob_0a9)
ll_ivm_pob_0a9 <- logLik(reg_ivm_pob_0a9)
gdl_ivm_pob_0a9<- reg_ivm_pob_0a9$df
fit_ivm_pob_0a9<- reg_ivm_pob_0a9$fitted.values
res_ivm_pob_0a9<-reg_ivm_pob_0a9$resid
s_ivm_pob_0a9<- salida1$sigma
r.cuadrado_ivm_pob_0a9<- salida1$r.squared
aic_ivm_pob_0a9<- AIC(reg_ivm_pob_0a9)
coef_ivm_pob_0a9<- reg_ivm_pob_0a9$coefficients
aic_ivm_pob_0a9
coef_ivm_pob_0a9


##########TIME SERIES REGRESION########
#se definen los campos a la funcion serie
ts.ivm<-ts(ivm)
ts.ivm
par(mfrow=c(1,1))
plot(ts.ivm,col=c(1,2),main="IVM vs pob 0 a 9")
#differences= wt=vt-v(t-1)
d_ivm<-diff(ts.ivm)

#para parametrizar el retrazo de la serie)
lag(ts.ivm)

q_ts.ivm<-ts.ivm-lag(ts.ivm,-1)

#optimizar el orden del modelo
fracdiff(ts.ivm,nar=2,nma=1)

#ajustar ar y ma de orden 1
ar1<-arima(ts.ivm,order=c(1,0,0))
ma1<-arima(ts.ivm,order=c(0,0,1))
ar1
ma1
arima<-arima(ts.ivm,order=c(1,0,1))
arima

#ploting autocorrelations, autocorrelogram function, and partial
par(mfrow=c(2,1))
acf(ts.ivm)
pacf(ts.ivm)
stl(ts.ivm)#no es periodica



#predicted values take only regresion objects, (y,#predictions)
predict(arima,6)

#durbin watson test autiocorrelaton (takes lm)
durbin.watson(reg_ivm_pob_0a9,max.lag=1)

#plotting regresions
par(mfrow=c(2,2))
plot(reg_ivm_pob_0a9)

#ploting empirical distributions
dens<-density(ts.)
dens
