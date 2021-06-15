#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(viridis)
library(tidyverse)
library(zoo)
library(forecast)
library(pracma)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Set workign directory
  setwd("~/Dropbox/Estadistica/CircaLuc")
  
  # Get and reshape the data
  data.df <- read_csv("./data/data.csv") %>%
    gather(key = "Subject", 
           value = "Lumin", 
           -ZTTime , 
           factor_key=TRUE) %>%
    group_by(Subject) %>%
    summarise(ZTTime = ZTTime[1:length(na.trim(Lumin))], 
              Lumin=na.trim(Lumin)) %>% # Clean trailing NAs in Lumin
    mutate(ZTTime = na.approx(ZTTime), # Linear approximation of missing points the time vector
           Lumin = na.approx(Lumin), # Linear approximation of missing luminosity
           Z_Lumin = (Lumin-mean(Lumin, na.rm=TRUE))/sd(Lumin, na.rm=TRUE)) # Z-scored lumin
  
  # Detrending and smoothing #Separar
  smoothed.df <- reactive({data.df %>%
      group_by(Subject) %>% 
      mutate(Ys = convolve(abs(Lumin), 
                           rep(1, input$det * 60/as.numeric(input$sp)), 
                           type = "open")[1:length(Lumin)],
             Ys = c(rep(Ys[input$det * 60/as.numeric(input$sp)/2+1], input$det * 60/as.numeric(input$sp)/2), 
                    Ys[(input$det * 60/as.numeric(input$sp)/2+1):(length(Ys)-input$det * 60/as.numeric(input$sp)/2)], 
                    rep(Ys[length(Ys)-input$det * 60/as.numeric(input$sp)/2], input$det * 60/as.numeric(input$sp)/2)),
             Lumin_weighted = Lumin/Ys,
             Section = ifelse(ZTTime>as.numeric(input$ZTcorte) & ZTTime<=as.numeric(input$ZTLD), "LD",
                              ifelse(ZTTime>as.numeric(input$ZTLD) & ZTTime<=as.numeric(input$ZTDD), "DD", NA))) %>%
      ungroup() %>%
      group_by(Subject, Section) %>%
      mutate(Lumin_detrended = detrend(Lumin_weighted, "linear"),
             Lumin_smoothed = as.numeric(ma(Lumin_detrended, input$smo)))
  })
  
    output$rawPlot <- renderPlot({
    
    if (input$subject=="All") {
      data.df.plot <- data.df
      plot.title <- "Raw luminosity of all the wells"
    } else if (input$subject=="Mean") {
      data.df.plot <- data.df %>% 
        group_by(ZTTime) %>% 
        summarise(Lumin = mean(Lumin)) 
      plot.title <- "Raw luminosity of the mean"
    } else {
      data.df.plot <- data.df %>% filter(Subject == input$subject)
      plot.title <- paste0("Raw luminosity of well ",input$subject)
    }

    data.df.plot %>%
      ggplot(aes(x = ZTTime, y = Lumin, colour = Subject)) +
      geom_line() +
      ggtitle(plot.title) +
      scale_colour_viridis(discrete=TRUE) +
      theme_classic() +
      theme(legend.position = "none")
    
  })
  
  output$detrendedPlot <- renderPlot({
    
    if (input$subject=="All") {
      smoothed.df.plot <- smoothed.df()
      plot.title <- "Smoothed luminosity of all the wells"
    } else if (input$subject=="Mean") {
      smoothed.df.plot <- smoothed.df() %>% 
        group_by(ZTTime) %>% 
        summarise(Lumin = mean(Lumin)) 
      plot.title <- "Smoothed luminosity of the mean"
    } else {
      smoothed.df.plot <- smoothed.df() %>% 
        filter(Subject == input$subject)
      plot.title <- paste0("Smoothed luminosity of well ",input$subject)
    }
    
    smoothed.df.plot %>%
      filter((Section == as.character(input$section))) %>%
      drop_na() %>%
      ggplot(aes(x = ZTTime, y = Lumin_smoothed, colour = Subject)) + 
      geom_line() +
      ggtitle(paste0(plot.title, " in ", as.character(input$section))) +
      scale_colour_viridis(discrete=TRUE) +
      theme_classic() +
      theme(legend.position = "none")
    
  })
  
  # output$mytable = renderDataTable({
  #   smoothed.df()
  # })
  
})
