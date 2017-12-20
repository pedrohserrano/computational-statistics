library(shiny)
library(ggplot2)

rnorm <- function(nsim){
  p1<-runif(nsim,0,1)#genera probs entre 0 y 1
  p2<-runif(nsim,0,1)#genera probs entre 0 y 1
  x<-sqrt(-2*log(p1))*cos(1*360*p2)
  x
}

shinyServer(function(input, output){
  
  data_norm <- reactive(rnorm(input$nsim))
  
  output$random_numbers <- renderDataTable(
    data.frame(random = data_norm())
    )
  
  output$random_hist <- renderPlot(
    qplot(data_norm())
    )
})

