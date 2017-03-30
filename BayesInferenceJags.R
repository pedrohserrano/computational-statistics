library(rjags)
library(R2jags)
library(ggplot2)
data(iris)
n<-nrow(iris)
x<-data.frame(iris$Sepal.Width,iris$Petal.Length,iris$Petal.Width)
plot(iris)
table <- iris

#-Defining data-
data <- list('n'=n,'x'=x,'y'=iris$Sepal.Length)

#-Defining inits-
inits<-function(){list(beta0=0,beta=rep(0,3),tau=1,y_hat=rep(0,n))}

#-Selecting parameters to monitor-
parameters<-c('beta0','beta','tau','y_hat')

norm_model <- function(){
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dnorm(mu[i],tau)
    mu[i] <- beta0+beta[1]*x[i,1]+beta[2]*x[i,2]+beta[3]*x[i,3]
  }
  #Priors 
  beta0 ~ dnorm(0,0.001)
  for (i in 1:3) {
    beta[i] ~ dnorm(0,0.001)
  }
  tau ~ dgamma(0.001,0.001)
  #Estimacion y_hat
  for (i in 1:n) { 
    y_hat[i] ~ dnorm(mu[i],tau)
  }
        }


#-Running code-
fit<-jags(data,
          inits,
          parameters,
          model.file=
          norm_model,
          n.chains=1,
          n.iter=10000,
          n.burnin=4000) 


#--------------------Monitoring chain----------------------#
#Traza de la cadena
#traceplot(fit)
#Cadena
out<-fit$BUGSoutput$sims.list
out.DIC<-fit$BUGSoutput$DIC


par(mfrow=c(3,3)) 
# for (i in 1:3) {
#   betas<-out$beta[,i]
#   plot(betas,type="l",col='brown')
#   plot(cumsum(betas)/(1:length(betas)),type="l", ylab = 'Estabilizacion')
#   acf(betas)
# }
# b0<-out$beta0
# dev<-out$deviance
# plot(b0,type="l",col='brown')
# plot(cumsum(b0)/(1:length(b0)),type="l", ylab = 'Estabilizacion')
# acf(b0)
# plot(dev,type="l",col='brown')
# plot(cumsum(dev)/(1:length(dev)),type="l", ylab = 'Estabilizacion') 
# acf(dev)

#Resumen (estimadores)
out.sum<-fit$BUGSoutput$summary
out.sum
print(out.sum[1:3,1:3])


#--------------------Comentarios del modelo----------------------#

#----------opcion normal-identidad    
out.beta<-out.sum[grep("beta",rownames(out.sum)),]
out.DIC<-out.sum[grep("deviance",rownames(out.sum)),]
out.y_hat<-out.sum[grep("y_hat",rownames(out.sum)),]
out.DIC[1]

#ir comparando las DIC y las probabilidades y compararlas en un cuadro
par(mfrow=c(1,1)) 
or.y_hat<-sort(out.y_hat[,1])
or.y<-sort(iris$Sepal.Length)

#estimaciones vs reales
tab_est<-data.frame(or.y,or.y_hat)
colnames(tab_est)<- c("Reales", "Estimados")
plot(tab_est$Reales,tab_est$Estimados)
p2 <-ggplot(tab_est, aes(Reales,Estimados)) + geom_point()
p2 + geom_abline(intercept=0,slope=1,colour='#CC6633') +
  ggtitle("Comparativo de ajuste del modelo")

