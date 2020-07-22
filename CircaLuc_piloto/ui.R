#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("CircaLuc piloto"),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("subject", "Well:",
                  unique(data.df$Subject)),
      selectInput("method", "Method:",
                  c("Fourier" = "fourier",
                    "Wavelet" = "wavelet",
                    "LS" = "ls",
                    "Mesa" = "mesa",
                    "24 hs" = "24hs",
                    "LS Graph" = "lsgraph")),
      numericInput("min", "Min:", 10, min = 1, max = 100),
      numericInput("smooth", "Smooth:", 10, min = 1, max = 100),
      numericInput("ls", "LD:", 10, min = 1, max = 100),
      numericInput("detrend", "Detrend:", 10, min = 1, max = 100),
      numericInput("corte", "Corte:", 10, min = 1, max = 100),
      numericInput("dd", "DD:", 10, min = 1, max = 100)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
