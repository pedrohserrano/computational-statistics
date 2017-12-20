Header 1
=========================
Header 2
-------------------------

### Header 3

#### Header 4

* Item 1
* Item 2
  * Item 2a
  * Item 2b

data(cps71)  
attach(cps71)  
####regresion no parametrica
cps71  
m <- npreg(logwage~age)  
summary(m)  
plot(m,plot.errors.method="asymptotic",
     plot.errors.style="band",
     ylim=c(11,15.2))

points(age,logwage,cex=.25)
predict(m)