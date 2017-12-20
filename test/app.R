
library(shiny)
library(ggplot2)
library(plyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
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
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ########integral 1  
  f1<-function(x) (1/2)*sqrt(4-x^2)
  pi_f1<-function(nsim){
    x<-runif(nsim,0,2)
    q<-4*f1(x)
    pi<-mean(q)
    pi
  }
  tabla_pi1 <- reactive({
    secuencia <- seq(from=1, to=input$nsim, by=1)
    vec <- sapply(secuencia, pi_f1)
    table<-data.frame(secuencia,vec)
    colnames(table) <- c("Simulaciones", "Pi")
    table
  })
  output$pi1 <- renderPlot({
    p <- ggplot(data=tabla_pi1(), aes(x=Simulaciones,y=Pi)) + geom_line()
    p + geom_hline(yintercept = 3.14, colour = "gold")
  })
  output$area1 <- renderPlot({
    sec=seq(0,2,.005)
    cord.x <- c(0,sec,2) 
    cord.y <- c(0,f1(sec),0) 
    curve(f1,ylab="f(x)=(1/2)*sqrt(4-x^2)",xlim=c(-2,2),ylim=c(-2,2),main='Pi Integral 1')
    polygon(cord.x,cord.y,col='gold')
  })
  
  ########integral 2  
  f2<-function(x) 4/(1+x^2)
  pi_f2<-function(nsim){
    x<-runif(nsim,0,1)
    q<-f2(x)
    pi<-mean(q)
    pi
  }
  tabla_pi2 <- reactive({
    secuencia <- seq(from=1, to=input$nsim, by=1)
    vec <- sapply(secuencia, pi_f2)
    table<-data.frame(secuencia,vec)
    colnames(table) <- c("Simulaciones", "Pi")
    table
  })
  output$pi2 <- renderPlot({
    p <- ggplot(data=tabla_pi2(), aes(x=Simulaciones,y=Pi)) + geom_line()
    p + geom_hline(yintercept = 3.14, colour = "gold")
  })
  output$area2 <- renderPlot({
    sec=seq(0,1,.01)
    cord.x <- c(0,sec,1) 
    cord.y <- c(0,f2(sec),0) 
    curve(f2,ylab="f(x)=4/(1+x^2)",xlim=c(-2,3),ylim=c(0,5),main='Pi Integral 2')
    polygon(cord.x,cord.y,col='gold')
  })    
  
  ########integral 3 
  f3<-function(x) 6/sqrt(4-x^2)
  pi_f3<-function(nsim){
    x<-runif(nsim,0,1)
    q<-f3(x)
    pi<-mean(q)
    pi
  }
  tabla_pi3 <- reactive({
    secuencia <- seq(from=1, to=input$nsim, by=1)
    vec <- sapply(secuencia, pi_f3)
    table<-data.frame(secuencia,vec)
    colnames(table) <- c("Simulaciones", "Pi")
    table
  })
  output$pi3 <- renderPlot({
    p <- ggplot(data=tabla_pi3(), aes(x=Simulaciones,y=Pi)) + geom_line()
    p + geom_hline(yintercept = 3.14, colour = "gold")
  })
  output$area3 <- renderPlot({
    sec=seq(0,1,.001)
    cord.x <- c(0,sec,1) 
    cord.y <- c(0,f3(sec),0) 
    curve(f2,ylab="f(x)=6/sqrt(4-x^2)",xlim=c(-2,2),ylim=c(0,30),main='Pi Integral 3')
    polygon(cord.x,cord.y,col='gold')
  })    
  
}

# Run the application 
shinyApp(ui = ui, server = server)

