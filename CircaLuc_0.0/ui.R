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
      selectInput("well", "Well(s):", 
                  multiple = TRUE,
                  choices = "",
                  selected = "",
                  width = 180),
      numericInput("sp", "Sampling period (mins):", 30, 
                   min = 1, max = 100, width = 180 ),
      downloadButton("downloadData", "Download processed data"),
      downloadButton("downloadPeriods", "Download periods")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "RAW data",
          fluidPage(
            fluidRow(
              selectInput("raw_y_scale", "Luminosity scale:",
                          c("linear", "log2", "log10")),
              plotOutput("rawPlot")),
          )
        ),
        tabPanel(
          "Processed data",
          fluidPage(
            fluidRow(
              h3("Preprocessing parameters:"),
              column(width = 4,
                     numericInput("ZTcorte", "Start of LD section (hs):", 48, 
                                  min = 1, 
                                  max = 125, 
                                  width = 160),
                     numericInput("smo","Smoothing width (s):", 10,
                                  min = 1, 
                                  max = 100, 
                                  width = 160)
                     ),
              column(width = 4,
                     numericInput("ZTLD", "End of LD section (s):", 120,
                                  min = 1, 
                                  max = 250, 
                                  width = 160),
                     numericInput("det", "Detrend length (s):", 48,
                                  min = 1, 
                                  max = 100, 
                                  width = 160)
                     ),
              column(width = 4,
                     numericInput("ZTDD", "End of DD section (hs):", 168,
                                  min = 1, 
                                  max = 250, 
                                  width = 160),
                     selectInput("section_preprocessed", "Section:",
                                 multiple = TRUE,
                                 c("LD", "DD"),
                                 selected = "DD")
                     ),
          ),
          plotOutput("detrendedPlot")
          )
        ),
        tabPanel("Periods",
                 fluidRow(
                   h3("Methods for fitting:"),
                   column(width = 4,
                          selectInput("methodLD", "Methodfor LD:",
                                      c("Fourier" = "fourier",
                                        "Wavelet" = "wavelet",
                                        "LS" = "ls",
                                        "Mesa" = "mesa",
                                        "24 hs" = "twentyfour",
                                        "LS Graph" = "lsgraph"),
                                      selected = "ls",
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
                                      selected = "ls",
                                      width = 150)
                          ),
                 ),
                 fluidRow(
                   h3("Period fit parameters:"),
                          ),
                 fluidRow(
                   column(width = 4,
                          numericInput("min_period", "Minimum period (hs):", 20,
                                       min = 1, 
                                       max = 20, 
                                       width = 150),
                   ),
                   column(width = 4,
                          numericInput("max_period", "Maximum period (hs):", 28.5,
                                       min = 20, 
                                       max = 40, 
                                       width = 150),
                   ),
                   column(width = 4,
                          numericInput("oversampling", "Oversampling (for LS):", 30,
                                       min = 1, 
                                       max = 60, 
                                       width = 150),
                   ),
                 ),
                 plotOutput("periodsPlot"),
                 fluidRow(
                   column(width = 12,
                          dataTableOutput('table_periods')
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
