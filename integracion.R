#INTEGRACION NUMERODA
set.seed(456456)
n<-100000
mu1<-0
var1<-1
mu2<-0.8
var2<-1

x<-seq(min(mu1-3*var1,mu2-3*var2),max(mu1+3*var1,mu2+3*var2),length.out = n)
f1<-dnorm(x,mean=mu1, sd=var1)
f2<-dnorm(x,mean=mu2, sd=var2)

ps <- matrix(c(runif(n, min(x), max(x)), runif(n, min=0, max=max(f1,f2)) ), ncol=2) # sample x,y from uniform dist

z1<- ps[,2] <= dnorm(ps[,1], mu1, var1) # dist1
z2<- ps[,2] <= dnorm(ps[,1], mu2, var2) # dist 2
z12 <- z1 | z2 # both dists
z3 <- ps[,2] <= pmin(dnorm(ps[,1], mu1, var1), dnorm(ps[,1], mu2, var2)) # overlap

# plot
plot(ps[!z12, 1], ps[!z12, 2], col='#137072', pch=20, ylim=c(0, max(f1,f2)), xlim=range(x), xlab="", ylab="")
points(ps[z1,1], ps[z1,2], col="#FBFFC0")
points(ps[z2,1], ps[z2,2], col="#56B292")
points(ps[z3, 1], ps[z3,2], col="#BF223D")
lines(x, f1, lwd=2)
lines(x, f2, lty="dotted",lwd=2)


(sum(z3)/sum(z1) + sum(z3)/sum(z2))/2



la integral es = 1/N(la suma de las funciones estimadas valuadas)
el error es proporcional a 1/raiz(n)


el error del tarpezoidal es 1/n^2

d es la dimension y mientras mas grande mejor es montcarlo

##se evelua una funcion que puede calcularse de manera analitica
h=function(x){(cos(50*x)+sin(20*x))^2}
par(mar=c(2,2,2,1),mfrow=c(2,1))
curve(h,xlab="Function",ylab="",lwd=2)
integrate(h,0,1)
##0.9652009 with absolute error < 1.9e-10
x=h(runif(10^4))
estint=cumsum(x)/(1:10^4)
esterr=sqrt(cumsum((x-estint)^2))/(1:10^4)
plot(estint,xlab="Mean and error range",type="l",lwd=2,
        ylim=mean(x)+20*c(-esterr[10^4],esterr[10^4]),ylab="")
lines(estint+2*esterr,col="gold",lwd=2)
lines(estint-2*esterr,col="gold",lwd=2)


##sobre la normal
x=rnorm(10^8)
bound=qnorm(c(.5,.75,.8,.9,.95,.99,.999,.9999))
res=matrix(0,ncol=8,nrow=7)
for (i in 2:8)
  for (j in 1:8)
    res[i-1,j]=mean(x[1:10^i]<bound[j])
matrix(as.numeric(format(res,digi=4)),ncol=8)

