###########################
#regresion para bajas naturales, considerando total de familias activas
###########################

b_naturales<-read.csv("b.naturales.csv", header=T)
b_naturales2<-read.csv("b.naturales_outliers.csv", header=T)
names(b_naturales)

#naming & munging variables
b_nat<-b_naturales2$b.naturales
fam_act<-b_naturales2$fam.activas

#fitting model
lm.b_nat <- lm(b_nat~fam_act)

s.lm.b_nat<-summary(lm.b_nat) #summary and significance
coef<-lm.b_nat$coefficients #regression coefficents
compareCoefs(lm.b_nat)
vcov(lm.b_nat) #covariance matrix
par(mfrow=c(2,2))
plot(lm.b_nat,main="Regression",col=1) #weight tails distribution
mmps(lm.b_nat)#marginal model plots

#outliers
outlier.test(lm.b_nat,cutoff=Inf,n.mx=Inf) 
par(mfrow=c(1,1))
qqPlot(lm.b_nat, main="Gráfico QQ", lwd=2)
leveragePlots(lm.b_nat)

#influential observations
par(mfrow=c(1,1))
avPlots(lm.b_nat) #added variable plot
cutoff <- 4/((nrow(b_naturales)-length(coef.b_nat)-2)) 
plot(lm.b_nat, which=4, cook.levels=cutoff, main="Observaciones Influyentes",lwd=2)  #identify values
influencePlot(lm.b_nat,id.method="identify", main="Área de influencia", lwd=2, sub="El tamaño del circulo es proporcional a la distancia del modelo Cook" )

#non normality errors
sres.lm.b_nat<-studres(lm.b_nat)  #distribution of studentized residuals
hist(sres.lm.b_nat, freq=FALSE, main="Distribucion de Residuales Estudentizados",xlab="Residuales",col="gray")
xlm.b_nat<-seq(min(sres.lm.b_nat),max(sres.lm.b_nat)) 
ylm.b_nat<-dnorm(xlm.b_nat )
lines(xlm.b_nat, ylm.b_nat,lwd=2,col="blue")
shapiro.test(sres.lm.b_nat)  #residuals normality

#non constant error variance  Evaluate homoscedasticity
ncvTest(lm.b_nat) # non-constant error variance test
spreadLevelPlot(lm.b_nat,main="Heterocedasticidad (Varianza Constante)") # plot studentized residuals vs. fitted values 

# Evaluate Nonlinearity (ot clear)
crPlots(lm.b_nat) #component + residual plot 

# Test for Autocorrelated Errors for evaluate non independence
durbin.watson(lm.b_nat)

#global results
gvlma(lm.b_nat)
s.lm.b_nat
res<-lm.b_nat$resid #vector of residuals
lin<-coef[1]+coef[2]*fam_act #modelo (univarate)
par(mfrow=c(1,1))
plot(lin,main="Fitted model")  #scatterplot
lines(lin)
visreg(lm.b_nat, main="Modelo de Regresión",xlab="Familias Activas",ylab="Bajas Naturales") 

inverseResponsePlot(lm.b_nat)
cor.test(b_nat,fam_act,type="pearson")
y_nat<-predict(lm.b_nat)
data.frame(y_nat)