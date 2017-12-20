#importance sampling
library(shiny)
library(ggplot2)
library(plyr)

#se define la funcion que estima una phi(x) dada
means_crudo<-function (nsim,m=1,alpha=.05){
  x <- runif (nsim, 0,1) 
  phi <- function (x,m)  m*exp(-m*x)
  theta_crudo <- mean (phi(x,m))
  theta_crudo_var <- var (phi(x,m))
  quant<-qnorm(alpha/2, lower.tail = FALSE)
  int.upper<-theta_crudo+sqrt(theta_crudo_var/nsim)*quant
  int.lower<-theta_crudo-sqrt(theta_crudo_var/nsim)*quant
  return(c(theta_crudo,theta_crudo_var,int.upper,int.lower))
}

#funcion importance sampling
means_importance<-function (nsim,m=1,lambda=2,alpha=.05){
   u <- runif (nsim, 0 ,1)
   x <- -log(1-u*(1-exp(-2*m))) #inversa de exp truncada en 2 #rbeta
   g <- function(x,lambda) lambda*exp(-x*lambda)/(1-exp(-2*lambda)) #funcion elegida como peso de importancia 2dbeta(u/2,a,b)
   phi <- function(x,m) m*exp(-m*x) #funcion a estimar
   importance <- function(x,m) phi(x,m)/g(x,lambda)
   theta_importance <- mean (importance(x,m))
   theta_imp_var <- var(importance(x,m))
   quant<-qnorm(alpha/2, lower.tail = FALSE)
   int.upper<-theta_importance+sqrt(theta_imp_var/nsim)*quant
   int.lower<-theta_importance-sqrt(theta_imp_var/nsim)*quant
   return (c(theta_importance,theta_imp_var,int.upper,int.lower))
 }

#aplicaci??n
shinyServer(function(input, output){
  
  tabla_estimadores_crudo<-reactive({
    secuencia <- seq(from=1, to=input$nsim, by=1)
    vec <- sapply(secuencia, means_crudo, input$m, input$alpha)[, -1]
    table<-data.frame(secuencia[-1],vec[1,],vec[2,],vec[3,],vec[4,])
    colnames(table) <- c("Simulaciones", "theta", "varianza", "lim_inf", "lim_sup")
    table
  })
  tabla_estimadores_importance<-reactive({
    secuencia <- seq(from=1, to=input$nsim, by=1)
    vec <- sapply(secuencia, means_importance, input$m, input$alpha)[, -1]
    table<-data.frame(secuencia[-1],vec[1,],vec[2,],vec[3,],vec[4,])
    colnames(table) <- c("Simulaciones", "theta", "varianza", "lim_inf", "lim_sup")
    table
  })
  output$estimadores_crudo <- renderDataTable({
        data.frame(tabla_estimadores_crudo())
        })
  output$estimadores_importance <- renderDataTable({
        data.frame(tabla_estimadores_importance())
        })
  output$plot_crudo <- renderPlot({
        ggplot(data=tabla_estimadores_crudo(), aes(x=Simulaciones,y=theta)) + geom_line() +
    geom_line(data=tabla_estimadores_crudo(), aes(y=lim_inf), colour='skyblue')+
    geom_line(data=tabla_estimadores_crudo(), aes(y=lim_sup), colour='skyblue')
        })
  output$plot_importance <- renderPlot({
    ggplot(data=tabla_estimadores_importance(), aes(x=Simulaciones,y=theta)) + geom_line() + 
      geom_line(data=tabla_estimadores_importance(), aes(y=lim_inf), colour='skyblue')+
      geom_line(data=tabla_estimadores_importance(), aes(y=lim_sup), colour='skyblue')
    
  })
  
})

##agregar mejora de varianza
