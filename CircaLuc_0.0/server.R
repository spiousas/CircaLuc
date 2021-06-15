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
#library(forecast)
#library(pracma)
#library(DT)
library(gsignal)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Set workign directory
  setwd("~/Dropbox/Estadistica/CircaLuc")
  source("period_estim_funs.R")
  
  # Get and reshape the data
  data.df <- read_csv("./data/data.csv") %>%
    pivot_longer(!ZTTime,
                 names_to = "well",
                 values_to = "lumin") %>%
    group_by(well) %>%
    summarise(ZTTime = ZTTime[1:length(na.trim(lumin))],
              lumin = na.trim(lumin)) %>% # Clean trailing NAs in Lumin
    mutate(
      ZTTime = na.approx(ZTTime),
      # Linear approximation of missing points the time vector
      lumin = na.approx(lumin),
      # Linear approximation of missing luminosity
      Z_lumin = (lumin - mean(lumin, na.rm = TRUE)) / sd(lumin, na.rm =
                                                           TRUE) # Z-scored lumin
    ) 
  
  # Detrending and smoothing
  smoothed.df <- reactive({
    data.df %>%
      group_by(well) %>%
      mutate(
        Ys = gsignal::conv(abs(lumin),
                           rep(1, 60 * input$det / input$sp),
                           shape = "same"),
        lumin_weighted = lumin / Ys,
        section = ifelse(
          ZTTime >= ZTcorte & ZTTime <= ZTLD,
          "LD",
          ifelse(ZTTime > ZTLD &
                   ZTTime <= ZTDD, "DD", "NonU")
        )
      ) %>%
      ungroup() %>%
      group_by(well, section) %>%
      mutate(
        lumin_detrended = detrend(lumin_weighted, "linear")[, 1],
        lumin_smoothed = gsignal::conv(lumin_detrended,
                                       rep(1, 60 * input$smo / input$sp),
                                       shape = "same")
      ) %>%
      select(-c(Ys, lumin_weighted)) %>%
      ungroup()
  })
  
  periods.df <- reactive({
    switch(input$method,
           ls = {
             periods.df <- smoothed.df() %>%
               dplyr::filter(section %in% c("LD", "DD")) %>%
               group_by(well, section) %>%
               summarise(
                 LS_period(
                   signal = lumin_smoothed,
                   time = ZTTime,
                   from_freq = .035,
                   to_freq = .05,
                   oversampling_freq = 30
                 )
               )
           }, {
           tibble(data = input$method)
           }
    )
  })
  
  output$rawPlot <- renderPlot({
    
    if (input$well == "All") {
      data.df.plot <- data.df
      plot.title <- "Raw luminosity of all the wells"
    } else if (input$well == "Mean") {
      data.df.plot <- data.df %>%
        group_by(ZTTime) %>%
        summarise(lumin = mean(lumin)) %>%
        mutate(well = "mean")
      plot.title <- "Raw luminosity of the mean"
    } else {
      data.df.plot <- data.df %>% dplyr::filter(well == input$well)
      plot.title <- paste0("Raw luminosity of well ", input$well)
    }
    
    data.df.plot %>%
      ggplot(aes(x = ZTTime, y = lumin, colour = well)) +
      geom_line() +
      ggtitle(plot.title) +
      scale_colour_viridis(discrete = TRUE) +
      theme_classic() +
      theme(legend.position = "none")
    
  })
  
  output$detrendedPlot <- renderPlot({
    
    if (input$well == "All") {
      smoothed.df.plot <- smoothed.df()
      plot.title <- "Smoothed luminosity of all the wells"
    } else if (input$well == "Mean") {
      smoothed.df.plot <- smoothed.df() %>%
        group_by(ZTTime, section) %>%
        summarise(lumin_smoothed = mean(lumin_smoothed)) %>%
        mutate(well = "mean")
      plot.title <- "Smoothed luminosity of the mean"
    } else {
      smoothed.df.plot <- smoothed.df() %>%
        dplyr::filter(well == input$well)
      plot.title <-
        paste0("Smoothed luminosity of well ", input$well)
    }
    
    smoothed.df.plot %>%
      dplyr::filter((section == as.character(input$section_raw))) %>%
      drop_na() %>%
      ggplot(aes(x = ZTTime, y = lumin_smoothed, colour = well)) +
      geom_line() +
      ggtitle(paste0(plot.title, " in ", as.character(input$section))) +
      scale_colour_viridis(discrete = TRUE) +
      theme_classic() +
      theme(legend.position = "none")
    
  })
  
  output$cosinorPlot <- renderPlot({
    
    if (input$well == "All") {
      smoothed.df.plot <- smoothed.df()
      plot.title <- "Smoothed luminosity of all the wells"
    } else if (input$well == "Mean") {
      smoothed.df.plot <- smoothed.df() %>%
        group_by(ZTTime, section) %>%
        summarise(lumin_smoothed = mean(lumin_smoothed)) %>%
        mutate(well = "mean")
      plot.title <- "Smoothed luminosity of the mean"
    } else {
      smoothed.df.plot <- smoothed.df() %>%
        dplyr::filter(well == input$well)
      plot.title <-
        paste0("Smoothed luminosity of well ", input$well)
    }
    
    smoothed.df.plot %>%
      dplyr::filter((section == as.character(input$section_cosinor))) %>%
      drop_na() %>%
      ggplot(aes(x = ZTTime, y = lumin_smoothed, colour = well)) +
      geom_line() +
      ggtitle(paste0(plot.title, " in ", as.character(input$section))) +
      scale_colour_viridis(discrete = TRUE) +
      theme_classic() +
      theme(legend.position = "none")
    
  })
  
  output$table <- renderDataTable(
    periods.df()
    )
  
  # Downloadable csv of periods dataset
  output$downloadPeriods <- downloadHandler(
    filename = function() {
      "periods.csv"
    },
    content = function(file) {
      write.csv(periods.df(), 
                file, 
                row.names = FALSE)
    }
  )
  
  # Downloadable csv of smoothed dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      "smoothed_data.csv"
    },
    content = function(file) {
      write.csv(smoothed.df() %>% 
                  select(-c(lumin, Z_lumin, lumin_detrended)) %>% 
                  pivot_wider(names_from = well, 
                              values_from = lumin_smoothed), 
                file, row.names = FALSE)
    }
  )
  
  
})
