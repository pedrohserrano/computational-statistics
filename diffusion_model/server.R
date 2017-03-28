
library(shiny)
library(RWiener)
shinyServer(function(input, output) {
  
  data_weiner <- reactive({
    n <- input$n
    alpha <- as.numeric(input$alpha)
    tau <- as.numeric(input$tau)
    beta <- as.numeric(input$beta)
    delta <- as.numeric(input$delta)
    dat <- rwiener(n,alpha,tau,beta,delta)
    dat
  })

  output$Plot2 <- renderPlot({
    n <- input$n
    delta <- input$delta
    alpha <- as.numeric(input$alpha)
    dis1 <- rnorm(n, delta, 10)
    dis2 <- cumsum(dis1)
    plot(dis2, type= 'l',
         main= 'Acumulacion de Informacion hasta el punto de desicion',
         xlab='Tiempo', ylab='Desplazamiento')
    lines(1:n, rep(alpha,n), col='green')
    })
  output$data_w <- renderTable({
    data.frame(data_weiner())
  })
  output$Plot3 <- renderPlot({
    wiener_plot(data_weiner())
   })

})




