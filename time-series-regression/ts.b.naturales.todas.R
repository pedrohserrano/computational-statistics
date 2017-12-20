##############
#non seasonal time series using ARIMA, must be stationary
#ARIMA(p,d,q)  d=ornden de la diferenciación, tantas veces hasta que se estacionaria
#A key tool in identifying a model is an estimate of the autocovariance function

#1 Plot the data. Identify any unusual observations.
#2 If necessary, transform the data (using a Box-Cox transformation) to stabilize the variance.
#3 If the data are non-stationary: take first differences of the data until the data are stationary.
#4 Examine the ACF/PACF: Is an AR(p) or MA(q) model appropriate?
#5 Try your chosen model(s), and use the AICc to search for a better model.
#6 Check the residuals from your chosen model by plotting the ACF of the residuals, and doing a portmanteau test of the residuals. If they do not look like white noise, try a modified model.
#7 Once the residuals look like white noise, calculate forecasts.
##############

b_naturales<-read.csv("b.nat.completo1.csv", header=T)
b_naturales<-read.csv("b.nat.completo.prueba7.csv", header=T)
b_naturales<-read.csv("b.nat.completo.prueba7b.csv", header=T)
b_naturales<-read.csv("b.nat.completo.prueba4A.csv", header=T)
b_naturales
names(b_naturales)

b_nat<-b_naturales$b.naturales
ts.b_nat<-ts(b_nat)
plot(ts.b_nat,main="Bajas Historico",ylab="Bajas",lwd=2,col="blue")
adf.test(ts.b_nat, alternative = "stationary") #estacionaria

#selecting appropiate values p,d,q for ARIMA model
au.arima<-auto.arima(ts.b_nat) 
au.arima

#prueba 1 arima(3,1,0) recomendado
#necesita 1ra diferencia
dif1.b_nat<-diff(ts.b_nat,differences=1)


#plotting autocorrelations an partial one for most clarity
par(mfrow=c(1,2))
acf(ts.b_nat,main="Autocorrelograma Bajas",lwd=2) #no stationary
pacf(ts.b_nat,main="Autocorrelograma Parcial",lwd=2)

#prueba 1
arima1<-arima(dif1.b_nat,order=c(3,0,0))
#prueba 2
arima2<-arima(dif1.b_nat,order=c(0,0,3))
#prueba 3
arima3<-arima(dif1.b_nat,order=c(4,0,0)) #no funciona ar5
#prueba 4
arima4<-arima(ts.b_nat,order=c(3,0,0)) #no funciona ar4
#prueba 5
arima5<-arima(ts.b_nat,order=c(3,1,0)) 

#prueba 7 (replica de la prueba 4 considerando b_naturales prueba7)
arima7<-arima(ts.b_nat,order=c(3,0,0))
#prueba 7a (replica de la prueba 7 considerando b_naturales prueba7 y autoarima) arima(110) se usa 1ra dif
arima7a<-arima(dif1.b_nat,order=c(1,0,0))
#prueba 7b (replica de la prueba 7 considerando b_naturales prueba7b y autoarima) arima(110) se usa 1ra dif
arima7b<-arima(dif1.b_nat,order=c(1,0,0))

#prueba 4Aa  (replica de la prueba 4 considerando b_naturales prueba4A )
arima4Aa<-arima(ts.b_nat,order=c(3,0,0)) 
arima4Aa
#prueba 4Ab (replica de la prueba 4 considerando b_naturales prueba4A y autoarima) arima(310) se usa 1ra dif
arima4Ab<-arima(dif1.b_nat,order=c(3,0,0)) #no funciona ar4
arima4Ab

#plot prueba 1
par(mfrow=c(1,1))
res.arima1<-residuals(arima1)  #distribution of residuals
hist(res.arima1, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Errores Bajas")
dens.arima1<-density(res.arima1)
lines(dens.arima1,lwd=2,col="blue")

#plot prueba 2
par(mfrow=c(1,1))
res.arima2<-residuals(arima2)  #distribution of residuals
hist(res.arima2, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Errores Bajas")
dens.arima2<-density(res.arima2)
lines(dens.arima2,lwd=2,col="blue")

#plot prueba 3
par(mfrow=c(1,1))
res.arima3<-residuals(arima3)  #distribution of residuals
hist(res.arima3, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Errores Bajas")
dens.arima3<-density(res.arima3)
lines(dens.arima3,lwd=2,col="blue")

#plot prueba 4
par(mfrow=c(1,1))
res.arima4<-residuals(arima4)  #distribution of residuals
hist(res.arima4, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Errores Bajas")
dens.arima4<-density(res.arima4)
lines(dens.arima4,lwd=2,col="blue")

#plot prueba 5
par(mfrow=c(1,1))
res.arima5<-residuals(arima5)  #distribution of residuals
hist(res.arima5, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Errores Bajas")
dens.arima5<-density(res.arima4)
lines(dens.arima5,lwd=2,col="blue")

#plot prueba 7
par(mfrow=c(1,1))
res.arima7<-residuals(arima7)  #distribution of residuals
hist(res.arima7, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Errores Bajas")
dens.arima7<-density(res.arima7)
lines(dens.arima7,lwd=2,col="blue")

#plot prueba 7a
par(mfrow=c(1,1))
res.arima7a<-residuals(arima7a)  #distribution of residuals
hist(res.arima7a, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Errores Bajas")
dens.arima7a<-density(res.arima7a)
lines(dens.arima7a,lwd=2,col="blue")

#plot prueba 7b
par(mfrow=c(1,1))
res.arima7b<-residuals(arima7b)  #distribution of residuals
hist(res.arima7b, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Errores Bajas")
dens.arima7b<-density(res.arima7b)
lines(dens.arima7b,lwd=2,col="blue")

#plot prueba 4Aa
par(mfrow=c(1,1))
res.arima4Aa<-residuals(arima4Aa)  #distribution of residuals
hist(res.arima4Aa, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Errores Bajas")
dens.arima4Aa<-density(res.arima4Aa)
lines(dens.arima4Aa,lwd=2,col="blue")

#plot prueba 4Ab
par(mfrow=c(1,1))
res.arima4Ab<-residuals(arima4Ab)  #distribution of residuals
hist(res.arima4Ab, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Errores Bajas")
dens.arima4Ab<-density(res.arima4Ab)
lines(dens.arima4Ab,lwd=2,col="blue")

#forecasting prueba 1
pred.arima1<-predict(arima1,9)
for.arima1<-forecast(arima1,9,level=.95)
plot(forecast(arima1,9),lwd=2,main="Prediccion de 9 Periodos Bajas")
data.frame(pred.arima1$pred) #new values for series but we need 1rst difference
for.arima1$lower
for.arima1$upper

#forecasting prueba 2
pred.arima2<-predict(arima2,9)
for.arima2<-forecast(arima2,9,level=.95)
plot(forecast(arima2,9),lwd=2,main="Prediccion de 9 Periodos Bajas")
data.frame(pred.arima2$pred) #new values for series but we need 1rst difference
for.arima2$lower
for.arima2$upper

#forecasting prueba 3
pred.arima3<-predict(arima3,9)
for.arima3<-forecast(arima3,9,level=.95)
plot(forecast(arima3,9),lwd=2,main="Prediccion de 9 Periodos Bajas")
data.frame(pred.arima3$pred) #new values for series but we need 1rst difference
for.arima3$lower
for.arima3$upper

#forecasting prueba 4
pred.arima4<-predict(arima4,9)
for.arima4<-forecast(arima4,9,level=.6)
plot(forecast(arima4,9),lwd=2,main="Prediccion de 9 Periodos Bajas")# 95% conf para caer
plot(forecast(arima4,9,level=.6),lwd=2,main="Prediccion de 9 Periodos Bajas")
data.frame(pred.arima4$pred) #new values for series but we need 1rst difference
for.arima4$lower
for.arima4$upper

#forecasting prueba 5  ( es la que tiene menor variación con respecto a los reales )
pred.arima5<-predict(arima5,9)
for.arima5<-forecast(arima5,9,level=.95)
plot(forecast(arima5,9),lwd=2,main="Prediccion de 9 Periodos Bajas")
data.frame(pred.arima5$pred) #new values for series but we need 1rst difference
for.arima5$lower
for.arima5$upper

#vemos outliers
fam_act<-b_naturales$fam.activas
lm.b_nat <- lm(b_nat~fam_act)

par(mfrow=c(1,1))
avPlots(lm.b_nat) #added variable plot
cutoff <- 4/((nrow(b_naturales)-length(lm.b_nat$coef)-2)) 
plot(lm.b_nat, which=4, cook.levels=cutoff, main="Observaciones Influyentes",lwd=2)  #identify values
influencePlot(lm.b_nat,id.method="identify", main="Área de influencia", lwd=2, sub="El tamaño del circulo es proporcional a la distancia del modelo Cook" )

#forecasting prueba 7
pred.arima7<-predict(arima7,9)
for.arima7<-forecast(arima7,9,level=.95)
plot(forecast(arima7,9),lwd=2,main="Prediccion de 9 Periodos Bajas")
data.frame(pred.arima7$pred) #new values for series but we need 1rst difference
for.arima7$lower
for.arima7$upper

#forecasting prueba 7a
pred.arima7a<-predict(arima7a,9)
for.arima7a<-forecast(arima7a,9,level=.95)
plot(forecast(arima7a,9),lwd=2,main="Prediccion de 9 Periodos Bajas")
data.frame(pred.arima7a$pred) #new values for series but we need 1rst difference
for.arima7a$lower
for.arima7a$upper

#forecasting prueba 7b
pred.arima7b<-predict(arima7b,9)
for.arima7b<-forecast(arima7b,9,level=.95)
plot(forecast(arima7b,9),lwd=2,main="Prediccion de 9 Periodos Bajas")
data.frame(pred.arima7b$pred) #new values for series but we need 1rst difference
for.arima7b$lower
for.arima7b$upper

#forecasting prueba 4Ab
pred.arima4Aa<-predict(arima4Aa,9)
for.arima4Aa<-forecast(arima4Aa,9,level=.5)
plot(forecast(arima4Aa,9,level=.5),lwd=2,main="Prediccion de 9 Periodos Bajas")
data.frame(pred.arima4Aa$pred) #new values for series but we need 1rst difference
for.arima4Aa$lower
for.arima4Aa$upper

#forecasting prueba 4Ab
pred.arima4Ab<-predict(arima4Ab,9)
for.arima4Ab<-forecast(arima4Ab,9,level=.95)
plot(forecast(arima4Ab,9),lwd=2,main="Prediccion de 9 Periodos Bajas")
data.frame(pred.arima4Ab$pred) #new values for series but we need 1rst difference
for.arima4Ab$lower
for.arima4Ab$upper

