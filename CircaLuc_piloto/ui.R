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
  
  # Application title
  titlePanel("CircaLuc pilot"),

  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("subject", "Well:",
                  c("All", "Mean", as.character(unique(data.df$Subject)))),
      selectInput("method", "Method:",
                  c("Fourier" = "fourier",
                    "Wavelet" = "wavelet",
                    "LS" = "ls",
                    "Mesa" = "mesa",
                    "24 hs" = "24hs",
                    "LS Graph" = "lsgraph")),
      numericInput("sp", "Sampling period (mins):", 30, min = 1, max = 100),
      numericInput("smo", "Smoothing width (hours):", 10, min = 1, max = 100),
      numericInput("det", "Detrend length (hours):", 48, min = 1, max = 100),
      numericInput("ZTcorte", "Start of LD section (hours):", 48, min = 1, max = 125),
      numericInput("ZTLD", "End of LD section (hours):", 120, min = 1, max = 250),
      numericInput("ZTDD", "End of DD section (hours)::", 168, min = 1, max = 250)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("rawPlot"),
      selectInput("section", "Section:",
                   c("LD", "DD")),
      plotOutput("detrendedPlot")
       
    )
  )
))
