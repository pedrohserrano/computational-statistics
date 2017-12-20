library(Rcpp)
library(ggplot2)

#-Defining data-
data(iris)
X <- as.matrix(cbind(1,iris[,2:4]))
Y <- iris$Sepal.Length

#-Defining inits-
nsim <- 1000
init <- c(1,1,1,1,10)

#-Running MH-
mh.y <- MHBayes(nsim, theta0=init, objdens1, proposal1, X, Y)
thetas <- mh.y$theta

#-Monitoring Chain-
par(mfrow=c(2,2)) 
b0<-thetas[,5]
plot(b0,type="l",col='brown')
plot(cumsum(b0)/(1:length(b0)),type="l", ylab = 'Estabilizacion')
hist(b0)
acf(b0)

#-Summary y_hat-
simulacionesY<-SimulacionesYest(thetas,X)
sim<-simulacionesY$Simulaciones
Y_hat<-simulacionesY$Medias_Simulaciones

#-Boundary- real vs est
tab_est<-data.frame(Y,Y_hat)
colnames(tab_est)<- c("Reales", "Estimados")
plot(tab_est$Reales,tab_est$Estimados)
p <-ggplot(tab_est, aes(Reales,Estimados)) + geom_point()
p + geom_abline(intercept=0,slope=1,colour='#CC6633') +
  ggtitle("Comparativo de ajuste del modelo")

