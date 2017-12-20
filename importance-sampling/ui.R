#importance sampling
library(shiny)

shinyUI(function(input, output){
  fluidPage(
    titlePanel('Importance Sampling'),
    sidebarPanel(
      p("Se desea aproximar integrales via Montecarlo, comparando iteraciones en el modo 
        'crudo' contra el modo aplicando importance sampling"), br(),
      numericInput('nsim', label = 'Numero de simulaciones', value = 1000, min = 1),
      sliderInput('alpha', label = 'alpha de confianza', value = .05, min = .0000001, max=.99),
      sliderInput('m', label = 'Parametro de Phi', value = 10, min = .01, max=100),
      sliderInput('lambda', label = 'Parametro de g(.) importance', value = 2, min = .01, max=100)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Estimador', 
                 plotOutput('plot_crudo'),
                 plotOutput('plot_importance'),
                 submitButton("Refresh",icon("bar-chart-o"))),
        
        tabPanel('Tabla estimadores crudo', dataTableOutput('estimadores_crudo')),
        
        tabPanel('Tabla estimadores importance', dataTableOutput('estimadores_importance'))
      )
    )
  )
})

