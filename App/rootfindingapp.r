library(shiny)
library(spuRs)

#define UI for root finding methods app

#1. Select which root finding method
#2. Select function to use
#3. display output/viz

#define example functions

#cos(x)
ftn1 <- function(x){
  return(cos(x))
}

ftn1_1 <- function(x){
  fx <- cos(x)
  dfx <- -sin(x)
  return(c(fx,dfx))
}

#cos(x)-x
ftn2 <- function(x){
  return(cos(x) - x)
}

ftn2_1 <- function(x){
  fx <- cos(x) - x
  dfx <- -sin(x) - 1
  
  return(c(fx,dfx))
}

#ln(x)-e^(-x)
ftn3 <- function(x){
  return(log(x) - exp(-x))
}

ftn3_1 <- function(x){
  fx <- log(x) - exp(-x)
  dfx <- (1/x) + exp(-x)
  
  return(c(fx,dfx))
}

#sin(x) + sin(2x) + cos(3x)
ftn4 <- function(x){
  return((sin(x) + sin(2*x) + cos(3*x))) 
}

ftn4_1 <- function(x){
  fx <- (sin(x) + sin(2*x) + cos(3*x))
  dfx <- cos(x) + cos(2*x)*2 - sin(3*x)*3
  
  return(c(fx,dfx))
}

#define secant method
secant <- function (ftn, x0, x1, tol = 1e-09, max.iter = 100) 
{
  for (i in 1:max.iter) {
    xnew <- x1 - ftn(x1) * (x1-x0) / (ftn(x1) - ftn(x0))
    if (abs(ftn(xnew)) < tol) {
      return (xnew)
    }
    x0 <- x1
    x1 <- xnew
  }
  stop("Maximum iterations reached.")
}


#SHINY
ui <- fluidPage(
  backgroundColor = "gray",
  h2("Root Finding Methods in R"),
  sidebarPanel(
    
    #SELECT METHOD
    radioButtons("method", "Select Root Finding Method:",
                choices = c("Fixed Point" = 1, "Newton-Raphson" = 2, "Secant" = 3, "Bisection" = 4),
                selected = 1),

    
    #FIXED POINT
    conditionalPanel(condition="input.method == '1'",
                     p(em("Applies the fixed point method to find x such that ftn(x) == x.")),
                     radioButtons("func","Select Example Function:", choices = c("f(x) = cos(x)" = 1)
                                                                                  ),
                     sliderInput("fixed_initial", "Initial Guess", min = 0, max = 10, value = 0)),
    
    #NEWTON RAPHSON
    conditionalPanel(condition="input.method == '2'",
                     p(em("Applies the Newton-Raphson algorithm to find x such that ftn(x) == 0.")),
                     radioButtons("func2","Select Example Function:", choices = c("f(x) = cos(x)" = 1,
                                                                                 "f(x) = cos(x) - x" = 2,
                                                                                 "f(x) = ln(x) - e^-x" = 3,
                                                                                 "f(x) = sin(x) + sin(2x) + cos(3x)" = 4)),
                     sliderInput("newton_initial", "Initial Guess", min = 0, max = 10, value = 1)),
    
    #SECANT
    conditionalPanel(condition="input.method == '3'",
                     p(em("Applies the secant algorithm to find x such that ftn(x) == 0")),
                     radioButtons("func3","Select Example Function:", choices = c("f(x) = cos(x)" = 1,
                                                                                  "f(x) = cos(x) - x" = 2,
                                                                                  "f(x) = ln(x) - e^-x" = 3,
                                                                                  "f(x) = sin(x) + sin(2x) + cos(3x)" = 4)),
                     sliderInput("secant_init1", "X0", min = 0, max = 10, value = 1),
                     sliderInput("secant_init2", "X1", min = 0, max = 10, value = 3)), 
  
    #Bisection
    conditionalPanel(condition="input.method == '4'",
                     p(em("Applies the bisection algorithm to find x such that ftn(x) == 0")),
                     radioButtons("func4","Select Example Function:", choices = c("f(x) = cos(x)" = 1,
                                                                                  "f(x) = cos(x) - x" = 2,
                                                                                  "f(x) = ln(x) - e^-x" = 3,
                                                                                  "f(x) = sin(x) + sin(2x) + cos(3x)" = 4)),
                     sliderInput("bisec_init1", "X0", min = 0, max = 10, value = 0),
                     sliderInput("bisec_init2", "X1", min = 0, max = 10, value = 3))), 
  mainPanel(
  plotOutput("plot"),
  h3("Results"),
  verbatimTextOutput("output")
))

server <- function(input, output) {
  #PLOTS
  output$plot <- renderPlot({
    switch(input$method,
           '1'={
             #FIXED POINT
             plot(ftn1, from = 0, to = 5)
             if (is.null(fixedpoint(ftn1,input$fixed_initial)) == 0){points((x = fixedpoint(ftn1,input$fixed_initial)), y = ftn1(fixedpoint(ftn1,input$fixed_initial)),
                     col = "red",
                     pch=19)}
           },
           '2'={
             #NEWTON RAPHSON
             switch(input$func2,
                    '1'={
                      plot(ftn1, from = 0, to = 15)
                      
                      if (class(try(newtonraphson(ftn1_1,input$newton_initial))) != "try-error"){
                      points((x = newtonraphson(ftn1_1,input$newton_initial)),
                              y = ftn1(newtonraphson(ftn1_1,input$newton_initial)),
                              col = "red",
                              pch= 19)
                        }
                    },
                    '2'={
                      plot(ftn2, from = -pi, to = pi)
                      if (class(try(newtonraphson(ftn2_1,input$newton_initial))) != "try-error"){
                      points((x = newtonraphson(ftn2_1,input$newton_initial)),
                             y = ftn2(newtonraphson(ftn2_1,input$newton_initial)),
                             col = "red",
                             pch= 19)
                      }
                    },
                    '3'={
                      plot(ftn3, from = 0, to = 2)
                      if (class(try(newtonraphson(ftn3_1,input$newton_initial))) != "try-error"){
                      points((x = newtonraphson(ftn3_1,input$newton_initial)),
                             y = ftn3(newtonraphson(ftn3_1,input$newton_initial)),
                             col = "red",
                             pch= 19)
                      }
                    },
                    '4'={
                      plot(ftn4, from = -1, to = 2*pi)
                      if (class(try(newtonraphson(ftn4_1,input$newton_initial))) != "try-error"){
                      points((x = newtonraphson(ftn4_1,input$newton_initial)),
                             y = ftn4(newtonraphson(ftn4_1,input$newton_initial)),
                             col = "red",
                             pch= 19)
                      }
                    })
           },
           '3'={
             #SECANT
             switch(input$func3,
                    '1'={
                      plot(ftn1, from = 0, to = 15)
                      if (class(try(secant(ftn1,input$secant_init1,input$secant_init2))) != "try-error"){
                      points((x = secant(ftn1,input$secant_init1,input$secant_init2)),
                             y = ftn1(secant(ftn1,input$secant_init1,input$secant_init2)),
                             col = "red",
                             pch= 19)
                      }
                    },
                    '2'={
                      plot(ftn2, from = -pi, to = pi)
                      if (class(try(secant(ftn2,input$secant_init1,input$secant_init2))) != "try-error"){
                      points((x = secant(ftn2,input$secant_init1,input$secant_init2)),
                             y = ftn2(secant(ftn2,input$secant_init1,input$secant_init2)),
                             col = "red",
                             pch= 19)
                      }
                    },
                    '3'={
                      plot(ftn3, from = 0, to = 2)
                      points((x = secant(ftn3,input$secant_init1,input$secant_init2)),
                             y = ftn3(secant(ftn3,input$secant_init1,input$secant_init2)),
                             col = "red",
                             pch= 19)
                    },
                    '4'={
                      plot(ftn4, from = -1, to = 2*pi)
                      if (class(try(secant(ftn4,input$secant_init1,input$secant_init2))) != "try-error"){
                      points((x = secant(ftn4,input$secant_init1,input$secant_init2)),
                             y = ftn4(secant(ftn4,input$secant_init1,input$secant_init2)),
                             col = "red",
                             pch= 19)
                      }
                    })
           },
           '4'={
             #BISECTION
             switch(input$func4,
                    '1'={
                      plot(ftn1, from = 0, to = 15)
                      if (class(try(bisection(ftn1,input$bisec_init1,input$bisec_init2))) != "try-error"){
                      points((x = bisection(ftn1,input$bisec_init1,input$bisec_init2)),
                             y = ftn1(bisection(ftn1,input$bisec_init1,input$bisec_init2)),
                             col = "red",
                             pch= 19)
                      }
                    },
                    '2'={
                      plot(ftn2, from = -pi, to = pi)
                      if (class(try(bisection(ftn2,input$bisec_init1,input$bisec_init2))) != "try-error"){
                      points((x = bisection(ftn2,input$bisec_init1,input$bisec_init2)),
                             y = ftn2(bisection(ftn2,input$bisec_init1,input$bisec_init2)),
                             col = "red",
                             pch= 19)
                      }
                    },
                    '3'={
                      plot(ftn3, from = 0, to = 2)
                      if (class(try(bisection(ftn3,input$bisec_init1,input$bisec_init2))) != "try-error"){
                      points((x = bisection(ftn3,input$bisec_init1,input$bisec_init2)),
                             y = ftn3(bisection(ftn3,input$bisec_init1,input$bisec_init2)),
                             col = "red",
                             pch= 19)
                      }
                    },
                    '4'={
                      plot(ftn4, from = -1, to = 2*pi)
                      if (class(try(bisection(ftn4,input$bisec_init1,input$bisec_init2))) != "try-error"){
                      points((x = bisection(ftn4,input$bisec_init1,input$bisec_init2)),
                             y = ftn4(bisection(ftn4,input$bisec_init1,input$bisec_init2)),
                             col = "red",
                             pch= 19)
                      }
                    })
           })
  })
  
  #RESULTS BOX
  output$output <- renderText({
    switch(input$method,
           '1'={
             #FIXED POINT
              fixedpoint(ftn1,input$fixed_initial)
           },
           '2'={
             #NEWTON RAPHSON
             switch(input$func2,
                    '1'={
                      newtonraphson(ftn1_1,input$newton_initial)
                    },
                    '2'={
                      newtonraphson(ftn2_1,input$newton_initial)
                    },
                    '3'={
                      newtonraphson(ftn3_1,input$newton_initial)
                    },
                    '4'={
                      newtonraphson(ftn4_1,input$newton_initial)
                    })
           },
           '3'={
             #SECANT
             switch(input$func3,
                    '1'={
                      secant(ftn1,input$secant_init1,input$secant_init2)
                    },
                    '2'={
                      secant(ftn2,input$secant_init1,input$secant_init2)
                    },
                    '3'={
                      secant(ftn3,input$secant_init1,input$secant_init2)
                    },
                    '4'={
                      secant(ftn4,input$secant_init1,input$secant_init2)
                    })
           },
           '4'={
             switch(input$func4,
                    '1'={
                      bisection(ftn1,input$bisec_init1,input$bisec_init2)
                    },
                    '2'={
                      bisection(ftn2,input$bisec_init1,input$bisec_init2)
                    },
                    '3'={
                      bisection(ftn3,input$bisec_init1,input$bisec_init2)
                    },
                    '4'={
                      bisection(ftn4,input$bisec_init1,input$bisec_init2)
                    })
           })
  })
  }


# Run the application 
shinyApp(ui = ui, server = server)