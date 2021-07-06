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
library(shinyjs)
library(gghalves)
library(systemfonts)
library(scales)
library(gsignal)
library(here)

# Define server logic 
shinyServer(function(session, input, output) {
  
  # Setup ####
  
  ## ggplot2 theme
  theme_set(
    theme_minimal(
      ## increase size of all text elements
      base_size = 14, 
      ## set custom font family for all text elements
      base_family = "Helvetica")
  )
  
  ## overwrite other defaults of theme_minimal()
  theme_update(
    ## remove major horizontal grid lines
    panel.grid.major.x = element_blank(),
    ## remove all minor grid lines
    panel.grid.minor = element_blank(),
  )
  
  # Set working directory
  setwd(here())
  source("period_estim_funs.R")
  
  # Raw data ####
  
  # Get and reshape the data
  data.df <- reactive({
    file <- input$input_file
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read_csv(file$datapath) %>%
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
  })
  
  observe({
    updateSelectInput(
      session,
      "well",
      "Well(s):",
      choices = list(
        `All the wells` = "all",
        `Mean` = "mean",
        `Individual wells` = as.character(unique(data.df()$well))
      ),
      selected = "all"
    )
  })
  
  
  output$rawPlot <- renderPlot({
    
    data.df.plot <- data.df() %>%
      group_by(ZTTime) %>%
      summarise(lumin = mean(lumin)) %>%
      mutate(well = "mean") %>%
      rbind(data.df() %>% select(well, ZTTime, lumin))
    
    plot.title <- "Raw luminosity"
    
    if (!("all" %in% input$well)) {
      data.df.plot <- data.df.plot %>% dplyr::filter(well %in% input$well)
    }
    
    data.df.plot %>%
      ggplot(aes(x = ZTTime, y = lumin, colour = well)) +
      geom_line() +
      labs(x = "Time [hs]",
           y = "Luminosity",
           color = "Well",
           title = plot.title) +
      scale_colour_viridis(discrete = TRUE) +
      scale_y_continuous(labels = scientific,
                         trans = if_else(input$raw_y_scale == "linear", 
                                         "identity",
                                         input$raw_y_scale)) +
      theme(legend.position = "bottom") 
    
  }, 
  height = 600, 
  width = 600 )
  
  # Processed data ####
  # Detrending and smoothing
  smoothed.df <- reactive({
    data.df() %>%
      group_by(well) %>%
      mutate(
        Ys = gsignal::conv(abs(lumin),
                           rep(1, 60 * input$det / input$sp),
                           shape = "same"),
        lumin_weighted = lumin / Ys,
        section = ifelse(
          ZTTime >= input$ZTcorte & ZTTime <= input$ZTLD,
          "LD",
          ifelse(ZTTime > input$ZTLD &
                   ZTTime <= input$ZTDD, "DD", "NonU")
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
  
  output$detrendedPlot <- renderPlot({
    
    smoothed.df.plot <- smoothed.df() %>%
      group_by(ZTTime, section) %>%
      summarise(lumin_smoothed = mean(lumin_smoothed)) %>%
      mutate(well = "mean") %>%
      rbind(smoothed.df() %>% select(well, ZTTime, lumin_smoothed, section))
    
    plot.title <- "Preprocessed luminosity"
    
    if (!("all" %in% input$well)) {
      smoothed.df.plot <- smoothed.df.plot %>% dplyr::filter(well %in% input$well)
    }
    
    smoothed.df.plot %>%
      dplyr::filter((section %in% input$section_preprocessed)) %>%
      drop_na() %>%
      ggplot(aes(x = ZTTime, y = lumin_smoothed, colour = well)) +
      geom_line() +
      labs(x = "Time [hs]",
           y = "Luminosity",
           title = paste0(plot.title, " in ", as.character(input$section_raw))) +
      scale_colour_viridis(discrete = TRUE) +
      theme(legend.position = "bottom")
    
  }, 
  height = 600, 
  width = 600 )
  
  # Periods ####
  
  periods.df <- reactive({
    switch(input$methodLD,
           ls = {
             periods.df <- smoothed.df() %>%
               dplyr::filter(section %in% c("LD", "DD")) %>%
               group_by(well, section) %>%
               summarise(
                 LS_period(
                   signal = lumin_smoothed,
                   time = ZTTime,
                   from_freq = 1/input$max_period,
                   to_freq = 1/input$min_period,
                   oversampling_freq = input$oversampling
                 )
               )
           }, 
           twentyfour = {
             periods.df <- smoothed.df() %>%
               dplyr::filter(section %in% c("LD", "DD")) %>%
               group_by(well, section) %>%
               summarise(
                 twentyfour_period()
               )
           },
           {
             tibble(data = input$method)
           }
    )
  })
  
  output$periodsPlot <- renderPlot({
    
    periods.df.plot <- periods.df()
    
    periods.df.plot %>%
      ggplot(aes(x = section, y = period, colour = section, fill = section)) +
      ggdist::stat_halfeye(
        ## custom bandwidth
        adjust = 1, 
        width = .6, 
        justification = -.2, 
        .width = 0, 
        alpha = 0.4,
        point_colour = NA
      ) + 
      geom_boxplot(
        width = .12, 
        alpha = 0.4,
        outlier.color = NA # Remove outliers
      ) +
      ## add dot plots from {ggdist} package
      gghalves::geom_half_point(
        side = "l", 
        range_scale = .4,
        alpha = .3
      )  + 
      coord_cartesian(xlim = c(1.2, NA)) +
      labs(x = "Section",
           y = "Period [hs]",
           title = "Estimated periods") +
      theme(legend.position = "none")
    
  })
  
  output$table_periods <- renderDataTable(
    periods.df()
    )

  # Cosinor ####
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
      theme(legend.position = "none")
    
  })
  
  # Download ####
  
  # Hide download action buttons when there is no file loaded
  observe({
    if (is.null(input$input_file)) {
      shinyjs::disable("downloadData")
      shinyjs::disable("downloadPeriods")
    } else {
      shinyjs::enable("downloadData")
      shinyjs::enable("downloadPeriods")
    }
  })
  
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
  
  # Downloadable csv of pre-processed data
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
