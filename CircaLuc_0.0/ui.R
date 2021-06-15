#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(viridis)
library(tidyverse)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  useShinyjs(),
  # Application title
  titlePanel("CircaLuc v0.0"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("input_file",
                "Choose input data (.csv)",
                accept = ".csv"),
      selectInput("well", "Well:", 
                  c("All", "Mean", as.character(unique(data.df$well))), width = 100),
      numericInput("sp", "Sampling period (mins):", 30, 
                   min = 1, max = 100, width = 100 ),
      downloadButton("downloadData", "Download processed data"),
      downloadButton("downloadPeriods", "Download periods")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "RAW data",
          plotOutput("rawPlot")
          ),
        tabPanel(
          "Processed data",
          fluidPage(
            fluidRow(
              column(width = 4,
                     numericInput("ZTcorte", "Start of LD section (hours):", 48, 
                                  min = 1, max = 125, width = 150),
                     numericInput("smo","Smoothing width (hours):", 10,
                       min = 1, max = 100, width = 150)
                     ),
              column(width = 4,
                     numericInput("ZTLD", "End of LD section (hours):", 120,
                                  min = 1, max = 250, width = 150),
                     numericInput("det", "Detrend length (hours):", 48,
                                  min = 1, max = 100, width = 150)
                     ),
              column(width = 4,
                     numericInput("ZTDD", "End of DD section (hours):", 168,
                                  min = 1, max = 250, width = 150),
                     selectInput("section_raw", "Section:",
                                 c("LD", "DD"))
                     ),
          ),
          plotOutput("detrendedPlot")
          )
        ),
        tabPanel("Periods",
                 fluidRow(
                   column(width = 4,
                          selectInput("methodLD", "Methodfor LD:",
                                      c("Fourier" = "fourier",
                                        "Wavelet" = "wavelet",
                                        "LS" = "ls",
                                        "Mesa" = "mesa",
                                        "24 hs" = "24hs",
                                        "LS Graph" = "lsgraph"),
                                      width = 150)
                          ),
                   column(width = 4,
                          selectInput("methodDD", "Method for DD:",
                                      c("Fourier" = "fourier",
                                        "Wavelet" = "wavelet",
                                        "LS" = "ls",
                                        "Mesa" = "mesa",
                                        "24 hs" = "24hs",
                                        "LS Graph" = "lsgraph"),
                                      width = 150)
                          ),
                 ),
                 fluidRow(
                   column(width = 12,
                          dataTableOutput('table')
                          )
                   )
                 ),
        tabPanel(
          "Cosinor",
          selectInput("section_cosinor", "Section:",
                      c("LD", "DD")),
          plotOutput("cosinorPlot")
        )
      )
    )
  )
))
