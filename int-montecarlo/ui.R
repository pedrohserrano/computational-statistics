
library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Aproximaciones de integrales Montecarlo"),
  
  sidebarLayout(
    sidebarPanel(
      p("Se desea aporximar el valor de Pi, mediante el metodo Montecarlo de aproximacion de 
        integrales"), br(),
      p("Funcion 1: (1/2)*sqrt(4-x^2)"), br(),
      p("Funcion 2: 4/(1+x^2)"), br(),
      p("Funcion 3: 6/sqrt(4-x^2)"), br(),
      p("Selecciona el numero de simulaciones deseadas"),
      numericInput("nsim", 
                  "Simulaciones", 
                  value = 500,
                  min = 1, 
                  max = 10000)
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Pi Integral 1", 
                           plotOutput('pi1'),
                           plotOutput("area1"),
                  submitButton("Refresh",icon("bar-chart-o"))),
                  
                  tabPanel("Pi Integral 2", 
                           plotOutput('pi2'),
                           plotOutput("area2"),
                           submitButton("Refresh",icon("bar-chart-o"))),
                  
                  tabPanel("Pi Integral 3", 
                           plotOutput('pi3'),
                           plotOutput("area3"),
                           submitButton("Refresh",icon("bar-chart-o")))
      )
    )
  )
))