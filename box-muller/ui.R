library(shiny)

shinyUI(function(input, output){
  fluidPage(
    titlePanel('Metodo Box-Muller'),
    sidebarPanel(
      numericInput('nsim', label = 'Numero de simulaciones', value = 5000, min = 1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Histograma', 
                 plotOutput('random_hist'),
                 submitButton("Refresh",icon("bar-chart-o"))),
        tabPanel('Numeros Aleatorios', dataTableOutput('random_numbers'))
      )
    )
  )
})