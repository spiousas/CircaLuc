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
    
    if (!("all" %in% input$well)) {
      data.df.plot <- data.df.plot %>% dplyr::filter(well %in% input$well)
    }
    
    data.df.plot %>%
      ggplot(aes(x = ZTTime, y = lumin, colour = well)) +
      geom_line(size = 1) +
      annotate("rect", 
               xmin = seq(input$ZTcorte + 12,input$ZTLD-1,24), 
               xmax = seq(input$ZTcorte + 24,input$ZTLD,24), 
               ymin = min(input$lumin_smoothed), 
               ymax = max(input$lumin_smoothed),
               alpha = .2) +
      annotate("rect",
               xmin = input$ZTLD, 
               xmax = input$ZTDD, 
               ymin = min(input$lumin_smoothed), 
               ymax = max(input$lumin_smoothed),
               alpha = .2) +
      geom_vline(xintercept = c(input$ZTLD, input$ZTcorte),
                 linetype = "dashed",
                 size = 1) +
      labs(x = "Time [hs]",
           y = "Luminescence (RLU/min)",
           color = "Well",
           title = "Luminescence") +
      scale_colour_viridis(discrete = TRUE) +
      scale_y_continuous(labels = scientific,
                         trans = if_else(input$raw_y_scale == "linear", 
                                         "identity",
                                         input$raw_y_scale)) +
      theme(legend.position = "none") 
    
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
        lumin_detrended = pracma::detrend(lumin_weighted, "linear")[, 1],
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
    
    
    if (!("all" %in% input$well)) {
      smoothed.df.plot <- smoothed.df.plot %>% dplyr::filter(well %in% input$well)
    }
    
    smoothed.df.plot %>%
      dplyr::filter((section %in% input$section_preprocessed)) %>%
      drop_na() %>%
      ggplot(aes(x = ZTTime-input$ZTcorte, y = lumin_smoothed, colour = well)) +
      geom_line(size = 1) +
      annotate("rect", 
               xmin = seq(12,input$ZTLD-input$ZTcorte-1,24), 
               xmax = seq(24,input$ZTLD-input$ZTcorte,24), 
               ymin = min(input$lumin_smoothed), 
               ymax = max(input$lumin_smoothed),
               alpha = .2) +
      annotate("rect",
               xmin = input$ZTLD-input$ZTcorte, 
               xmax = input$ZTDD-input$ZTcorte, 
               ymin = min(input$lumin_smoothed), 
               ymax = max(input$lumin_smoothed),
               alpha = .2) +
      geom_vline(xintercept = input$ZTLD-input$ZTcorte,
                 linetype = "dashed",
                 size = 1) +
      labs(x = "Time [hs]",
           y = "Detrended luminescence",
           title = "Detrended luminescence") +
      scale_colour_viridis(discrete = TRUE) +
      scale_x_continuous(breaks = seq(0,input$ZTDD-input$ZTcorte,12),
                         limits = c(0, input$ZTDD-input$ZTcorte)) +
      theme(legend.position = "none")
    
  }, 
  height = 400, 
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
  cosinor.df <- reactive({
    smoothed.df() %>% 
      dplyr::filter(section %in% c("LD", "DD")) %>% 
      group_by(well, section) %>% 
      nest() %>% 
      left_join(periods.df(), by = c("well", "section")) %>%
      mutate(
        fit = map(data, ~nls(lumin_smoothed ~ alpha + amp * cos(2*pi*ZTTime/period - acro),
                             data = .x,
                             start = list(alpha = 0, amp = 1, acro = 1))),
        tidied = map(fit, broom::tidy),
        R = map2(fit, data, modelr::rsquare)
      ) %>% 
      unnest(tidied)  %>% 
      unnest(R) %>%
      dplyr::filter(term != "alpha") %>%
      select(all_of(c("well", "section", "period", "term", "estimate", "R"))) %>%
      pivot_wider(names_from = "term",
                  values_from = "estimate") 
  })
  
  output$table_cosinor <- renderDataTable(
    cosinor.df() %>%
      dplyr::filter((section %in% input$section_cosinor))
  )
  
  output$cosinorPlot <- renderPlot({
    
    cosinor.df.plot <- cosinor.df()
    
    cosinor.df.plot %>%
      ggplot(aes(x = section, y = acro, colour = section, fill = section)) +
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
      scale_y_continuous(limits = c(0, 3)) +
      labs(x = "Section",
           y = "Acrophase [hs]",
           title = "Estimated acrophases") +
      theme(legend.position = "none")
    
    # cosinor.df() %>%
    #   ggplot(aes(x = section, y = acro, colour = well)) +
    #   geom_jitter() +
    #   #ggtitle(paste0(plot.title, " in ", as.character(input$section))) +
    #   scale_colour_viridis(discrete = TRUE) +
    #   scale_y_continuous(limits = c(0, 5)) +
    #   theme(legend.position = "none")
    
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
