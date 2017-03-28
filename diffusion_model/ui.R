
library(shiny)

shinyUI(fluidPage(

  titlePanel("Diffusion Model"),

  sidebarLayout(
    sidebarPanel(
      p("Se desea simular un proceso Wiener para predecir el tiempo de respuesta"), br(),
      sliderInput('n','Numero de Simulaciones', min = 1,max = 1000, value = 100),
      numericInput('alpha','Umbral de desicion (alpha)', min = 0,max = 10, value =2),
      numericInput('tau','Tiempo de retraso (tau)', min = 0,max = 4, value = 0.3),
      numericInput('delta','Drift del proceso (delta)', min = -1 ,max = 1, value = 0.5),
      sliderInput('beta','Sesgo en la desicion (beta)', min = 0,max = 1, value = 0.5)
      
    ),

    mainPanel(
      tabsetPanel(
        tabPanel('Proceso de difusion y distribuciones',
          plotOutput("Plot2"),
          plotOutput("Plot3")
        ),
        tabPanel('Tiempo en que se decide una clase upper o lower',
          tableOutput("data_w")
        )
      )
    )
  )
))

