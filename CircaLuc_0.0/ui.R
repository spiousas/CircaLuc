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
library(emo)

shinyUI(fluidPage(
  useShinyjs(),
  # Application title ####
  titlePanel(paste0("CircaLuc v0.1 ", emo::ji("clock"))),
  
  # Sidebar ####
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
      downloadButton("downloadPeriods", "Download periods"),
      downloadButton("downloadParameters", "Download parameters")
    ),
    
    # Show a plot of raw data
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          # Raw data ####
          "RAW data",
          fluidPage(
            fluidRow(
              column(width = 6,
              selectInput("raw_y_scale", "Luminosity scale:",
                          c("linear", "log2", "log10")),
              plotOutput("rawPlot"))
            )
          )
        ),
        # Processed data ####
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
                     numericInput("smo","Smoothing width (s):", 12,
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
                                 selected = c("LD", "DD"))
                     )
          ),
          plotOutput("detrendedPlot")
          )
        ),
        # Periods ####
        tabPanel("Periods",
                 fluidRow(
                   h3("Methods for fitting:"),
                   column(width = 4,
                          selectInput("methodLD", "Methodfor LD:",
                                      c("Fourier" = "fourier",
                                        "LS" = "ls",
                                        "24 hs" = "twentyfour"),
                                      selected = "twentyfour",
                                      width = 150)
                          ),
                   column(width = 4,
                          selectInput("methodDD", "Method for DD:",
                                      c("Fourier" = "fourier",
                                        "LS" = "ls",
                                        "24 hs" = "24hs"),
                                      selected = "ls",
                                      width = 150)
                          )
                 ),
                 fluidRow(
                   h3("Period fit parameters:")
                          ),
                 fluidRow(
                   column(width = 4,
                          numericInput("min_period", "Minimum period (hs):", 20,
                                       min = 1, 
                                       max = 20, 
                                       width = 150)
                   ),
                   column(width = 4,
                          numericInput("max_period", "Maximum period (hs):", 28.5,
                                       min = 20, 
                                       max = 40, 
                                       width = 150)
                   ),
                   column(width = 4,
                          numericInput("oversampling", "Oversampling (for LS):", 30,
                                       min = 1, 
                                       max = 60, 
                                       width = 150)
                   )
                 ),
                 fluidRow(
                   column(width = 12,
                          dataTableOutput('table_periods')
                          )
                   )
                 ),
        # Cosinor ####
        tabPanel("Cosinor",
          fluidRow(
            column(width = 4,
                   selectInput("section_cosinor", 
                               multiple = TRUE,
                               "Section to plot:", 
                               c("LD", "DD"),
                               selected = c("LD", "DD"))
            ),
            column(width = 8,
                   sliderInput("Rpass", "Threshold R:",
                               min = 0, max = 1,
                               value = 0.5),
            )
          ),
          fluidRow(
            column(width = 12,
                   plotOutput("cosinorPlot")
            )
          ),
          fluidRow(
            column(width = 12,
                   dataTableOutput('table_cosinor')
            )
          )
        ),
        # Figures ####
        tabPanel("Figures",
                 fluidRow(
                   column(width = 6,
                          selectInput("filter_figures", 
                                      multiple = FALSE,
                                      "What to plot:", 
                                      c("All the wells" = "all",
                                        "Only the synchronized in LD" = "only_synch"),
                                      selected = c("All the wells"))
                   )
                 ),
           fluidRow(
             h2("Cosinor fit results"),
             column(width = 4,
                    plotOutput("periodsPlot")
             ),
             column(width = 4,
                    plotOutput("ampsPlot")
             ),
             column(width = 4,
                    plotOutput("acrosPlot")
             )
           ),
          fluidRow(
            h2("Polar plots of acrophase"),
            column(width = 4,
                          plotOutput("acrospolarPlot")
            ),
            column(width = 8,
                   dataTableOutput('table_rayleigh')
            )
          )
          )
        )
      )
    )
  )
)
