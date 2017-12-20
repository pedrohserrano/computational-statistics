#####cross sectional regression

#ordinary least squares,lets consider a data frame called byu, containing for age, salary an exper. we want regress various forms of age exper on salary
#a linear (model) regression might be

bajast<-read.csv("bajas.csv",header=T)
bajast
bajas<-bajast$bajas
id<-bajast$id
reg<-lm(bajas~id)

#response to: salary=intercept + beta1(age) + beta2(exper) + error
#we can call all componets
anova(reg)  #anova
s.reg<-summary(reg) #resumen y significancia
s.reg
coef<-reg$coefficients #coeficientes de regresion
coef
res<-reg$resid #vector de residuales
res
linear<-coef[1]+coef[2]*id+res #modelo
linear
par(mfrow=c(1,1))
plot(linear,main="Dispersion del Modelo Ajustado",)  #grafico de dispercion
visreg(reg, main="Regresión Bajas") #grafico de regresion
par(mfrow=c(2,2))
plot(reg,main="Regresion nuevos IVM",col=4) #grafico de normalidad #del QQ plot vemos que es una distribución de colas ligeras, se observa la normalidad de los errores
shapiro.test(res)  #test shapiro para residuales
par(mfrow=c(1,1))
plot(id,res)
plotCI(linear,res,.05,err="y",main="Bandas de Confianza del Error")
durbin.watson(reg)  #prueba durbin watson para probar el supuesto de los residuos no correlacionados
outlier.test(reg,cutoff=Inf,n.mx=Inf) 
hist(res)

#matriz de covarianzas y regresiones
vcov(reg)

#salidas
aic<-AIC(reg)
aic
ssr<-deviance(reg)
ssr
ll <- logLik(reg)
gradosdl<- reg$df
gradosdl
fit <- reg$fitted.values
fit
sigma <- s.reg$sigma
sigma
r.cuadrado<- s.reg$r.squared
r.cuadrado
predict.lm(reg,9)
