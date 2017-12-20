fuente<-iris
x<-fuente[[2]]
hist(x)

nsim<-10000
vector_mu<-numeric(nsim)
vector_var<-numeric(nsim)

iniciales<-c(mean(x),1000)

vector_mu[1]<-iniciales[1]
vector_var[1]<-iniciales[2]


for(i in 2:nsim){
  
  vector_mu[i]<-rnorm(1,sum(x)/(1+vector_var[i-1]), 
  sqrt(vector_var[i-1]/(1+vector_var[i-1])))
  suma=0
  for(j in 1:length(x)){
    
    suma=suma+(x[j]-vector_mu[i])*(x[j]-vector_mu[i])
  }
  vector_var[i]<-1/rgamma(1,(length(x)/2)+1,1+suma/2)
}

hist(vector_mu)
hist(vector_var)
