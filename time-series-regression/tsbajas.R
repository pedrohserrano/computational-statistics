#se definen los campos a la funcion serie
ts.bajas<-ts(bajas)
ts.bajas
par(mfrow=c(1,1))
plot(ts.bajas,col=c(1,2),main="Bajas")
#differences= wt=vt-v(t-1)
d_bajas<-diff(ts.bajas)
d_bajas
#para parametrizar el retrazo de la serie)
lag(ts.bajas)

q_ts.bajas<-ts.bajas-lag(ts.bajas,-1)
q_ts.bajas

#optimizar el orden del modelo
fracdiff(ts.bajas,nar=2,nma=1)

#ajustar ar y ma de orden 1
ar1<-arima(ts.bajas,order=c(1,0,0))
ma1<-arima(ts.bajas,order=c(0,0,1))
ar1
ma1
arima<-arima(ts.bajas,order=c(1,1,2))
arima1<-arima(ts.bajas,order=c(1,0,0))
arima1
summary(arima)

#ploting autocorrelations, autocorrelogram function, and partial
par(mfrow=c(1,1))
acf(ts.bajas)
pacf(ts.bajas)
stl(ts.bajas)#no es periodica



#predicted values take only regresion objects, (y,#predictions)
predict(arima,9)
predict(arima1,9)
predict(ar1,9)
predict(ma1,9)


dens<-density(ts.bajas)
dens
par(mfrow=c(2,1))
plot(dens)
hist(res)