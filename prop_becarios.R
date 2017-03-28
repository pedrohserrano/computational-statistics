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

prop_becs<-read.csv("prop_becarios.csv", header=T)
prop_becs
names(prop_becs)
prop_becs4<-read.csv("prop_becarios4.csv", header=T)
prop_becs4
becs_hist<-read.csv("becarios_hist.csv", header=T)
becs_hist
becs_hist4<-read.csv("becarios_hist4.csv", header=T)
becs_hist4
becs_fam<-read.csv("ind_becarios_fams.csv", header=T)
becs_fam
becs_fam4<-read.csv("ind_becarios_fams4.csv", header=T)
becs_fam4

#define time series function
ts.pm<-ts(prop_becs$primaria_mujeres)
ts.ph<-ts(prop_becs$primaria_hombres)
ts.sm<-ts(prop_becs$secundaria_mujeres)
ts.sh<-ts(prop_becs$secundaria_hombres)
ts.em<-ts(prop_becs$ems_mujeres)
ts.eh<-ts(prop_becs$ems_hombres)
ts.em4<-ts(prop_becs4$EMS.Mujeres)
ts.eh4<-ts(prop_becs4$EMS.Hombres)
ts.becs<-ts(becs_hist$becarios)
ts.becs4<-ts(becs_hist4$becarios_4)
ts.becs_fam<-ts(becs_fam$indice.becario)
ts.fam<-ts(becs_fam$familias)
ts.fam4<-ts(becs_fam4$Familias)

#plotting time series
plot(ts.pm,main="Primaria Mujeres Historico",ylab="Proporción",lwd=2,col="blue")
plot(ts.ph,main="Primaria Hombres Historico",ylab="Proporción",lwd=2,col="blue")
plot(ts.sm,main="Secundaria Mujeres Historico",ylab="Proporción",lwd=2,col="blue")
plot(ts.sh,main="Secundaria Hombres Historico",ylab="Proporción",lwd=2,col="blue")
plot(ts.em,main="EMS Mujeres Historico",ylab="Proporción",lwd=2,col="blue")
plot(ts.eh,main="EMS Hombres Historico",ylab="Proporción",lwd=2,col="blue")
plot(ts.em4,main="EMS Mujeres Periodo 4 Historico",ylab="Proporción",lwd=2,col="blue")
plot(ts.eh4,main="EMS Hombres Periodo 4 Historico",ylab="Proporción",lwd=2,col="blue")
plot(ts.becs,main="Becarios Historico",ylab="Proporción",lwd=2,col="blue")
plot(ts.becs4,main="Becarios Periodo 4 Historico",ylab="Proporción",lwd=2,col="blue")
plot(ts.becs_fam,main="Numero de Becarios por familia Historico",ylab="Proporción",lwd=2,col="blue")
plot(ts.fam,main="Familias Historico",ylab="Cierre",lwd=2,col="blue")
plot(ts.fam4,main="Familias Historico",ylab="Cierre",lwd=2,col="blue")

#test Augmented Dickey-Fuller, for objecttive stationary
#The null-hypothesis for an ADF test is that the data are non-stationary. So large are indicative of non-stationarity, and small p-values suggest stationarity. Using the usual 5% threshold, differencing is required if the p-value is greater than 0.05.
adf.test(ts.pm, alternative = "stationary") #no stationary
adf.test(ts.ph, alternative = "stationary")
adf.test(ts.sm, alternative = "stationary")
adf.test(ts.sh, alternative = "stationary")
adf.test(ts.em, alternative = "stationary")
adf.test(ts.eh, alternative = "stationary")
adf.test(ts.em4, alternative = "stationary")
adf.test(ts.eh4, alternative = "stationary")
adf.test(ts.becs, alternative = "stationary")
adf.test(ts.becs4, alternative = "stationary")
adf.test(ts.becs_fam, alternative = "stationary")
adf.test(ts.fam, alternative = "stationary")
adf.test(ts.fam4, alternative = "stationary")

#selecting appropiate values p,d,q for ARIMA model
auto.arima(ts.pm, stationary=T)
auto.arima(ts.ph, stationary=T) 
auto.arima(ts.sm, stationary=T)  
auto.arima(ts.sh, stationary=T) 
auto.arima(ts.em, stationary=T)
auto.arima(ts.eh, stationary=T)
auto.arima(ts.em4, stationary=T)
auto.arima(ts.eh4, stationary=T)
auto.arima(ts.becs, stationary=T)
auto.arima(ts.becs4, stationary=T)
auto.arima(ts.becs_fam, stationary=T)
auto.arima(ts.fam, stationary=T)
auto.arima(ts.fam4, stationary=T)

#plotting autocorrelations an partial one for most clarity
#The data may follow an ARIMA(p,d,0) model if the ACF and PACF plots of the differenced data show the following patterns:
#---the ACF is exponentially decaying or sinusoidal
#---there is a significant spike at lag p in PACF, but none beyond lag p.
#The data may follow an ARIMA(0,d,q) model if the ACF and PACF plots of the differenced data show the following patterns:
#the PACF is exponentially decaying or sinusoidal
#there is a significant spike at lag q in ACF, but none beyond lag q.
par(mfrow=c(1,2))

acf(ts.pm,main="Autocorrelograma Primaria Mujeres",lwd=2) 
pacf(ts.pm,main="Autocorrelograma Parcial",lwd=2)

acf(ts.ph,main="Autocorrelograma Primaria Mujeres",lwd=2) 
pacf(ts.ph,main="Autocorrelograma Parcial",lwd=2)

acf(ts.sm,main="Autocorrelograma Primaria Mujeres",lwd=2) 
pacf(ts.sm,main="Autocorrelograma Parcial",lwd=2)

acf(ts.sh,main="Autocorrelograma Primaria Mujeres",lwd=2) 
pacf(ts.sh,main="Autocorrelograma Parcial",lwd=2)

acf(ts.em,main="Autocorrelograma Primaria Mujeres",lwd=2) 
pacf(ts.em,main="Autocorrelograma Parcial",lwd=2)

acf(ts.eh,main="Autocorrelograma Primaria Mujeres",lwd=2) 
pacf(ts.eh,main="Autocorrelograma Parcial",lwd=2)

acf(ts.em4,main="Autocorrelograma Primaria Mujeres",lwd=2) 
pacf(ts.em4,main="Autocorrelograma Parcial",lwd=2)

acf(ts.eh4,main="Autocorrelograma Primaria Mujeres",lwd=2) 
pacf(ts.eh4,main="Autocorrelograma Parcial",lwd=2)

acf(ts.becs,main="Autocorrelograma Primaria Mujeres",lwd=2) 
pacf(ts.becs,main="Autocorrelograma Parcial",lwd=2)

acf(ts.becs4,main="Autocorrelograma Primaria Mujeres",lwd=2) 
pacf(ts.becs4,main="Autocorrelograma Parcial",lwd=2)

acf(ts.becs_fam,main="Autocorrelograma Primaria Mujeres",lwd=2) 
pacf(ts.becs_fam,main="Autocorrelograma Parcial",lwd=2)

acf(ts.fam,main="Autocorrelograma Familias",lwd=2) 
pacf(ts.fam,main="Autocorrelograma Parcial",lwd=2)

acf(ts.fam4,main="Autocorrelograma Familias",lwd=2) 
pacf(ts.fam4,main="Autocorrelograma Parcial",lwd=2)

#selec own arima model

ar1.pm<-arima(ts.pm,order=c(1,0,0))
ar1.ph<-arima(ts.ph,order=c(1,0,0))
ar1.sm<-arima(ts.sm,order=c(1,0,0))
ar1.sh<-arima(ts.sh,order=c(1,0,0))
ar1.em<-arima(ts.em,order=c(1,0,0))
ar1.eh<-arima(ts.eh,order=c(1,0,0))
ar1.em4<-arima(ts.em4,order=c(1,0,0))
ar1.eh4<-arima(ts.eh4,order=c(1,0,0))
ar1.becs<-arima(ts.becs,order=c(1,0,0))
ar1.becs4<-arima(ts.becs4,order=c(1,0,0))
ar1.becs_fam<-arima(ts.becs_fam,order=c(1,0,0))
ar1.fam<-arima(ts.fam,order=c(1,0,0))
ar1.fam4<-arima(ts.fam4,order=c(1,0,0))

#plot density of residuals
par(mfrow=c(1,2))

res.pm<-residuals(ar1.pm)  #distribution of residuals
hist(res.pm, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Prop Primaria Mujeres")
dens.pm<-density(res.pm)
lines(dens.pm,lwd=2,col="blue")

res.ph<-residuals(ar1.ph)  #distribution of residuals
hist(res.ph, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Prop Primaria Hombres")
dens.ph<-density(res.ph)
lines(dens.ph,lwd=2,col="blue")

res.sm<-residuals(ar1.sm)  #distribution of residuals
hist(res.sm, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Prop Secundaria Mujeres")
dens.sm<-density(res.sm)
lines(dens.sm,lwd=2,col="blue")

res.sh<-residuals(ar1.sh)  #distribution of residuals
hist(res.sh, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Prop Secundaria Mujeres")
dens.sh<-density(res.sh)
lines(dens.sh,lwd=2,col="blue")

res.em<-residuals(ar1.em)  #distribution of residuals
hist(res.em, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Prop EMS Mujeres")
dens.em<-density(res.em)
lines(dens.em,lwd=2,col="blue")

res.eh<-residuals(ar1.eh)  #distribution of residuals
hist(res.eh, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Prop EMS Hombres")
dens.eh<-density(res.eh)
lines(dens.eh,lwd=2,col="blue")

res.em4<-residuals(ar1.em4)  #distribution of residuals
hist(res.em4, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Prop EMS Mujeres")
dens.em4<-density(res.em4)
lines(dens.em4,lwd=2,col="blue")

res.eh4<-residuals(ar1.eh4)  #distribution of residuals
hist(res.eh4, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Prop EMS Hombres")
dens.eh4<-density(res.eh4)
lines(dens.eh4,lwd=2,col="blue")

res.becs<-residuals(ar1.becs)  #distribution of residuals
hist(res.becs, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Prop EMS Hombres")
dens.becs<-density(res.becs)
lines(dens.becs,lwd=2,col="blue")

res.becs4<-residuals(ar1.becs4)  #distribution of residuals
hist(res.becs4, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Prop EMS Hombres")
dens.becs4<-density(res.becs4)
lines(dens.becs4,lwd=2,col="blue")

res.becs_fam<-residuals(ar1.becs_fam)  #distribution of residuals
hist(res.becs_fam, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Becarios por familia")
dens.becs_fam<-density(res.becs_fam)
lines(dens.becs_fam,lwd=2,col="blue")

res.fam<-residuals(ar1.fam)  #distribution of residuals
hist(res.fam, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Becarios por familia")
dens.fam<-density(res.fam)
lines(dens.fam,lwd=2,col="blue")

res.fam4<-residuals(ar1.fam4)  #distribution of residuals
hist(res.fam4, freq=FALSE, col="gray",main="Distribución de Residuales",xlab="Becarios por familia")
dens.fam4<-density(res.fam)
lines(dens.fam4,lwd=2,col="blue")

#let's do forecasting
pred.pm<-predict(ar1.pm,10)
pred.ph<-predict(ar1.ph,10)
pred.sm<-predict(ar1.sm,10)
pred.sh<-predict(ar1.sh,10)
pred.em<-predict(ar1.em,10)
pred.eh<-predict(ar1.eh,10)
pred.em4<-predict(ar1.em4,3)
pred.eh4<-predict(ar1.eh4,3)
pred.becs<-predict(ar1.becs,10)
pred.becs4<-predict(ar1.becs4,3)
pred.becs_fam<-predict(ar1.becs_fam,13)
pred.fam<-predict(ar1.fam,13)
pred.fam4<-predict(ar1.fam4,4)

par(mfrow=c(2,3))

plot(forecast(ar1.pm,10),lwd=2,main="Prediccion de 9 Periodos Primaria Mujeres")
for.pm<-forecast(ar1.pm,h=10,level=.95)
for.pm
data.frame(pred.pm$pred) 
for.pm$lower
for.pm$upper

plot(forecast(ar1.ph,10),lwd=2,main="Prediccion de 9 Periodos Primaria Hombres")
for.ph<-forecast(ar1.ph,h=10,level=.95)
for.ph
data.frame(pred.ph$pred) 
for.ph$lower
for.ph$upper

plot(forecast(ar1.sm,10),lwd=2,main="Prediccion de 9 Periodos Secundaria Mujeres")
for.sm<-forecast(ar1.sm,h=10,level=.95)
for.sm
data.frame(pred.sm$pred) 
for.sm$lower
for.sm$upper

plot(forecast(ar1.sh,10),lwd=2,main="Prediccion de 9 Periodos Secundaria Hombres")
for.sh<-forecast(ar1.sh,h=10,level=.95)
for.sh
data.frame(pred.sh$pred) 
for.sh$lower
for.sh$upper

plot(forecast(ar1.em,10),lwd=2,main="Prediccion de 9 Periodos EMS Mujeres")
for.em<-forecast(ar1.em,h=10,level=.95)
for.em
data.frame(pred.em$pred) 
for.em$lower
for.em$upper

plot(forecast(ar1.eh,10),lwd=2,main="Prediccion de 9 Periodos EMS Hombres")
for.eh<-forecast(ar1.eh,h=10,level=.95)
for.eh
data.frame(pred.eh$pred) #new values for series but we need 1rst difference
for.eh$lower
for.eh$upper

par(mfrow=c(1,2))

plot(forecast(ar1.em4,3),lwd=2,main="Prediccion de 9 Periodos EMS Mujeres")
for.em4<-forecast(ar1.em4,h=3,level=.95)
for.em4
data.frame(pred.em4$pred) 
for.em4$lower
for.em4$upper

plot(forecast(ar1.eh4,3),lwd=2,main="Prediccion de 9 Periodos EMS Hombres")
for.eh4<-forecast(ar1.eh4,h=3,level=.95)
for.eh4
data.frame(pred.eh4$pred) #new values for series but we need 1rst difference
for.eh4$lower
for.eh4$upper

par(mfrow=c(1,1))

plot(forecast(ar1.becs,10),lwd=2,main="Prediccion Becarios Total")
for.becs<-forecast(ar1.becs,h=10,level=.95)
for.becs
data.frame(pred.becs$pred) 
for.becs$lower
for.becs$upper

plot(forecast(ar1.becs4,3),lwd=2,main="Prediccion Becarios Total")
for.becs4<-forecast(ar1.becs4,h=3,level=.95)
for.becs4
data.frame(pred.becs4$pred) 
for.becs4$lower
for.becs4$upper

plot(forecast(ar1.becs_fam,13),lwd=2,main="Prediccion Becarios por familia")
for.becs_fam<-forecast(ar1.becs_fam,h=13,level=.95)
for.becs_fam
data.frame(pred.becs_fam$pred) 
for.becs_fam$lower
for.becs_fam$upper

plot(forecast(ar1.fam,13),lwd=2,main="Prediccion familias")
for.fam<-forecast(ar1.fam,h=13,level=.95)
for.fam
data.frame(pred.fam$pred) 
for.fam$lower
for.fam$upper

plot(forecast(ar1.fam4,13),lwd=2,main="Prediccion familias 4")
for.fam4<-forecast(ar1.fam4,h=4,level=.95)
for.fam4
data.frame(pred.fam4$pred) 
for.fam4$lower
for.fam4$upper