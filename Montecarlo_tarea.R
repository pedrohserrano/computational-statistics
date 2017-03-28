f<-function(n){
  x<- runif(n,-2,2)
  q<-4*(exp(-(x^2)/2))/sqrt(2*pi)
  thetaF<-mean(q)
  thetaF
  }
u1<- runif(n,-2,2)
u1
hist(u1)
plot(u1,f)


erf(sqrt(2))-f(10000)
