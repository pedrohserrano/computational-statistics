# BAYESIAN APPROACH

# mu ~ NORMAL(mean=6, sd=4) 
# sigma ~ Gamma(rate=5, shape=1)  
library(ggplot2)
prior.mean <- function(x) dnorm(x, 0, .2)
prior.sd <- function(x) dgamma(x, , 0.001)
plot(prior.mean, col="darkblue", lwd="2", main="Prior for Betas", ylab="density")
plot(prior.sd , col="darkred", lwd="2", main="Prior for standard deviation", ylab="density")
#A prioris no informativas

data <- matrix(c(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width),  ncol=4) #esto es solo una conversion por que la funciopn de cpp comem matriz

# WE WILL USE AND MCMC METHOD.
# 1: A objective density: 2) a proposal density
# Recall obj ~~ L(theta|X)prior(X)
# But as we want logarithms, we have log(obj) = log(L) + log(prior)

#a cada parametro se le hace una caminata aleatoria
# 1)

#en el caso de la regresion lineal tenemos la verosimilitud
#L(sigma, beta | Y ) = productoria de f(Y | beta, sigma)
#donde f es la densidad normal
#peeero se puede escibir como 
#log L(sigma, beta | Y ) = suma(-.5(y_i-x_i beta )**2/ sigma**2   -log(sigma))

#hay que cambiar pi objetivo

cppFunction('
            double objdens(NumericMatrix X, NumericVector Y, NumericVector theta){
            int i;
            double lkh, logprior, yhat;
            int m=data.nrow();
            int e=data.ncol();
            NumericVector beta(m-1);
            double sd;
            for (i=0; i<p; i++){
              beta[i] = theta[i];
            }
            sd = theta[p]
            NumericVector aux(m);
            // Compute loglikelihood
            lkh=0;
            for (int i=0; i<m; i++){
            aux = X
            lkh += -.5*pow((Y[i]-X[i]theta[0])/theta[1],2)-log(theta[1]);
            }
            // Compute logprior
            logprior = R::dnorm(theta[0], 3.0,.5, true) +  R::dgamma(theta[1], 5.0, 1.0/40.0, true);
            // Log of target density
            return lkh + logprior;
            }')
objdens(data, c(2,3))
#funcion objetivo

# 2) Proposal: random walk in the same dimension as the number of parameters
cppFunction('
            NumericVector proposal(NumericVector theta, NumericMatrix X){
            int nparam = theta.size();
            double jump = .25/sqrt(m); 
            NumericVector newtheta(nparam);
            for (int i=0; i<nparam; i++){
            newtheta[i] = R::rnorm(theta[i], jump);
            }
            return newtheta;
            }')
proposal(c(1,2))
#prorpuesta se cambia de .1 a .05

#gibs tiene ventajas cuando los parametros estan correlacionados
#MH tiene ventaja con velocidad

# 3) METROPOLIS

source("BayesianMH.cpp")

nsim <- 1000
init <- c(3, 1)
MHBayes(20, init, objdens, proposal, data)
mh.samp <- MHBayes(nsim, init, objdens, proposal, data)
estims <- mh.samp$theta

#  SOME DIAGNOSTIC IMPORTANT STUFF
#  Exploration graph:
library(calibrate)
pts <- seq(1,100,by=5)
plot(estims[pts, ], type="l", xlab="mean", ylab="sd")
textxy(estims[pts,1], estims[pts,2], pts)
cor(estims)
##
### 1) REJECTION RATES
rejections <- mh.samp$rejections[-1]
trials <- rejections + 1
rej.rate <- cumsum(rejections)/cumsum(trials)
plot(rej.rate, type="l", ylim=c(0,1), main="Rejection rate")
plot(trials[-1], type="l", main="Number of trials")
### 2) AUTOCORRELATION
acf(estims[ , 1])
acf(estims[ , 2]) # WARNING HERE!
# burnin and subsampling
burnin <- 100
estim <- estims[-(1:burnin), ]
thinning <- .9 # meaning we'll keep 75% of observations to reduce autocorrelation
# OBS: thinning is rarely usefull!!!! check that nothing changes
sub <- sample.int(nsim-burnin, size=round(thinning*nsim))
estims <- estims[sub, ]
acf(estims[ , 1])
acf(estims[ , 2]) 


# LET'S COMPARE PRIORS AND POSTERIORS AND DO INFERENCE

hist(estims[ ,1], prob=TRUE, xlim=c(2.5,3.5), breaks=20, col="lightgreen",
     main="Histogram and Posterior(blue) vs Prior(red) of the Mean") # posterior distribution of mean
plot(prior.mean, xlim=c(2.5,3.5), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,1]), col="darkblue", lwd="2")

hist(estims[ ,2], prob=TRUE, xlim=c(0,1), breaks=40, col="yellow",
     main="Histogram and Posterior(blue) vs Prior(red) of the s.d.") # posterior distribution of mean
plot(prior.sd, xlim=c(0,1), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,2]), col="darkblue", lwd="2")

mean(estims[ ,1]) # approx. mean-value of the posterior of mean
mean(estims[ ,2]) # approx. mean-value of the posterior of standard deviation

# CERTAINTY INTERVALS
intervals3 <- quantile(estims[ ,1], c(alpha/2, 1-alpha/2))
intervals3
quantile(estims[ ,2], c(alpha/2, 1-alpha/2)) # ALSO FOR SSSDDDD

# COMPARISON OF ALL RESULTS
meanestims <- c(est.mean, params[1], mean(estims[ ,1]))
sdestims <- c(est.sd, params[2], mean(estims[ ,2]))
intmeanlow <- c(intervals[1], intervals2[1], intervals3[1])
intmeanhigh <- c(intervals[2], intervals2[2], intervals3[2])
Comparison <- data.frame(meanestims, sdestims, intmeanlow, intmeanhigh)
row.names(Comparison) <- c("Pivot", "Likelihood", "Bayesian")
Comparison

##hay que usar esto mismo para hacer una regresion lineal
#hay una apriori para cada beta y una para el error
#de la base de datos de iris
#queremos hacer regresion de la primer variable (sepal.lenght)
#y las variables explicativas 
#hay que cambiar el codigo de la funcion objetivo que es el likelihood por priors