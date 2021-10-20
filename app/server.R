#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


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
  source(here("period_estim_funs.R"))
  
  # Raw data ####
  
  ## Get and reshape the data ####
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

  # Raw data plot ####
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
      annotate("rect", 
               xmin = seq(input$ZTcorte + 12,input$ZTLD-1,24), 
               xmax = seq(input$ZTcorte + 24,input$ZTLD,24), 
               ymin = min(data.df.plot$lumin[data.df.plot$well %in% input$well]), 
               ymax = max(data.df.plot$lumin[data.df.plot$well %in% input$well]),
               color = "yellow", fill = "yellow",
               alpha = .2) +
      annotate("rect",
               xmin = input$ZTLD, 
               xmax = input$ZTDD, 
               ymin = min(data.df.plot$lumin[data.df.plot$well %in% input$well]), 
               ymax = max(data.df.plot$lumin[data.df.plot$well %in% input$well]),
               color = "yellow", fill = "yellow",
               alpha = .2) +
      geom_line(size = 1) +
      geom_vline(xintercept = c(input$ZTLD, input$ZTcorte),
                 linetype = "dashed",
                 size = 0.5) +
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
  height = 400, 
  width = 600 )
  
  # Processed data ####
  ## Detrending and smoothing ####
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
      group_by(well) %>%
      mutate(
        lumin_detrended = pracma::detrend(lumin_weighted, "linear")[, 1],
        lumin_smoothed = gsignal::conv(lumin_detrended,
                                       rep(1, 60 * input$smo / input$sp),
                                       shape = "same")
      ) %>%
      select(-c(Ys, lumin_weighted)) %>%
      ungroup()
  })
  
  ## Detrended plot ####
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
      annotate("rect", 
               xmin = seq(12,input$ZTLD-input$ZTcorte-1,24), 
               xmax = seq(24,input$ZTLD-input$ZTcorte,24), 
               ymin = min(smoothed.df.plot$lumin_smoothed[smoothed.df.plot$well %in% input$well & smoothed.df.plot$section %in% input$section_preprocessed]),
               ymax = max(smoothed.df.plot$lumin_smoothed[smoothed.df.plot$well %in% input$well & smoothed.df.plot$section %in% input$section_preprocessed]),
               color = "yellow", fill = "yellow",
               alpha = .2) +
      annotate("rect",
               xmin = input$ZTLD-input$ZTcorte, 
               xmax = input$ZTDD-input$ZTcorte, 
               ymin = min(smoothed.df.plot$lumin_smoothed[smoothed.df.plot$well %in% input$well & smoothed.df.plot$section %in% input$section_preprocessed]),
               ymax = max(smoothed.df.plot$lumin_smoothed[smoothed.df.plot$well %in% input$well & smoothed.df.plot$section %in% input$section_preprocessed]),
               color = "yellow", fill = "yellow",
               alpha = .2) +
      geom_line(size = 1) +  
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
  ## Period estimation ####
  periods.df <- reactive({
    smoothed.df() %>%
      dplyr::filter(section %in% c("LD", "DD")) %>%
      mutate(method = if_else(section == "LD", 
                              input$methodLD, 
                              input$methodDD)) %>%
      group_by(well, section, method) %>%
      nest() %>%
      mutate(period = map(data, ~period_estimation(signal = .x$lumin_smoothed,
                                                   time = .x$ZTTime,
                                                   from_freq = 1/input$max_period,
                                                   to_freq = 1/input$min_period,
                                                   oversampling_freq = input$oversampling,
                                                   method = method))) %>%
      select(-data) %>%
      unnest(c(period))
  })
  
  ## Periods plot ####
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
  ## Cosinor fitting ####
  cosinor.df <- reactive({
    smoothed.df() %>% 
      dplyr::filter(section %in% c("LD", "DD")) %>% 
      group_by(well, section) %>% 
      mutate(ZTTime = if_else(section == "LD", 
                              ZTTime - input$ZTcorte,
                              ZTTime - input$ZTLD)) %>%
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
      select(all_of(c("well", "section", "period", "term", "estimate", "R"))) %>%
      pivot_wider(names_from = "term",
                  values_from = "estimate") %>%
      ungroup() %>%
      mutate(
        acro = if_else(amp<0, pi+acro, acro),
        acro = if_else(acro<0, 2*pi-acro, acro),
        acro = if_else(abs(acro)>2*pi, acro%%(2*pi), acro),
        amp = if_else(amp<0, -amp, amp),
        acro_24 = acro*12/pi,
        synch = if_else(R>input$Rpass, "yes!", "no"),
      ) %>%
      group_by(well) %>%
      mutate(rhythm = if_else((R[section=="LD"]>input$Rpass) & (R[section=="DD"]>input$Rpass), 
                              "yes!", "no"),
             entr = if_else((rhythm=="yes!") & (between(acro_24[section=="DD"]-acro_24[section=="LD"], input$phasepass[1], input$phasepass[2])), 
                                                "yes!", "no")
      ) %>%
      ungroup()
  })
  
  ## Circular means ####
  circ.means <- reactive({
    
    if (input$filter_figures == "only_synch") {
      cosinor.df_circmeans <- cosinor.df() %>% dplyr::filter(synchronized == "yes!")  
    } else {
      cosinor.df_circmeans <- cosinor.df()
    }
    
    cosinor.df_circmeans %>% 
      group_by(section) %>% 
      nest() %>%
      mutate(circ_acro = map(data, ~circular(.x$acro_24, 
                                             units = "hours", 
                                             template = "clock24", 
                                             modulo = "2pi")),
             mean_acro_24 = map(circ_acro, ~mean(.x)),
             sd_acro_24 = map(circ_acro, ~sd(.x)),
             rayleigh = map(circ_acro, ~rayleigh.test(.x)),
             p.value = rayleigh[[1]]$p.value) %>%
      select(-c(data, circ_acro, rayleigh)) %>%
      unnest(mean_acro_24) %>%
      unnest(sd_acro_24) %>%
      mutate(r = exp(-sd_acro_24^2/2))
  })

  ## Cosinor table ####
  output$table_cosinor <- renderDataTable(
    cosinor.df()
  )
  
  ## Predicted values ####
  smoothed.df.predicted <- reactive({
    smoothed.df() %>%
      dplyr::filter(section %in% c("LD", "DD")) %>% 
      mutate(ZTTime_fit = if_else(section == "LD", 
                                  ZTTime - input$ZTcorte,
                                  ZTTime - input$ZTLD)) %>%
      left_join(cosinor.df() %>% select(-c(R, acro_24, synchr)),
                by = c("well", "section")) %>%
      mutate(lumin_predicted = alpha + amp * cos(2*pi*ZTTime_fit/period - acro)) %>%
      select(-c(period, alpha, amp, acro, ZTTime_fit))
  })
  
  ## Cosinor plot ####
  output$cosinorPlot <- renderPlot({
    
    if (length(input$well == 1) & input$well %in% as.character(unique(data.df()$well)) ){
    smoothed.df.predicted() %>%
      dplyr::filter(well %in% input$well) %>%
      dplyr::filter(section %in% input$section_cosinor) %>%
      drop_na() %>%
      ggplot(aes(x = ZTTime-input$ZTcorte, color = section)) +
        annotate("rect",
                 xmin = seq(12,input$ZTLD-input$ZTcorte-1,24),
                 xmax = seq(24,input$ZTLD-input$ZTcorte,24),
                 ymin = min(smoothed.df.predicted()$lumin_smoothed[smoothed.df.predicted()$well %in% input$well & smoothed.df.predicted()$section %in% input$section_cosinor]),
                 ymax = max(smoothed.df.predicted()$lumin_smoothed[smoothed.df.predicted()$well %in% input$well & smoothed.df.predicted()$section %in% input$section_cosinor]),
                 color = "yellow", fill = "yellow",
                 alpha = .2) +
        annotate("rect",
                 xmin = input$ZTLD-input$ZTcorte, 
                 xmax = input$ZTDD-input$ZTcorte, 
                 ymin = min(smoothed.df.predicted()$lumin_smoothed[smoothed.df.predicted()$well %in% input$well & smoothed.df.predicted()$section %in% input$section_cosinor]),
                 ymax = max(smoothed.df.predicted()$lumin_smoothed[smoothed.df.predicted()$well %in% input$well & smoothed.df.predicted()$section %in% input$section_cosinor]),
                 color = "yellow", fill = "yellow",
                 alpha = .2) +
        geom_line(aes(y = lumin_predicted), size = 1, color = "grey50") +
        geom_line(aes(y = lumin_smoothed), size = 1) +
        geom_vline(xintercept = input$ZTLD-input$ZTcorte,
                   linetype = "dashed",
                   size = 1) +
        labs(x = "Time [hs]",
             y = "Detrended luminescence",
             title = "Cosinor fit") +
        scale_colour_manual(values = c("#7373FF", "#FF7272")) +
        scale_x_continuous(breaks = seq(0,input$ZTDD-input$ZTcorte,12),
                           limits = c(0, input$ZTDD-input$ZTcorte)) +
        theme(legend.position = "none")
    } else {
      ggplot()
    }
  }, 
  height = 400, 
  width = 600 )
  
  # Figures ####
  
  cosinor.df.plot <- reactive({
    if (input$filter_figures == "only_synch") {
      cosinor.df() %>% dplyr::filter(synch == "yes!")  
    } else {
      cosinor.df()
    }
  })
  
  circ.means.plot <- reactive({
    if (input$filter_figures == "only_synch") {
      circ.means() %>% dplyr::filter(synch == "yes!")  
    } else {
      circ.means()
    }
  })
  
  ## Period ####
  output$periodsPlot <- renderPlot({

    cosinor.df.plot() %>% ggplot(aes(x = section,
                                   y = period,
                                   color = section,
                                   fill = section)) +
      geom_jitter(width = .2,
                  alpha = .2) + 
      stat_summary(fun.data = mean_se,
                   geom = "bar", 
                   alpha = .2) +
      stat_summary(fun.data = mean_se,
                   geom = "errorbar", 
                   alpha = 1,
                   width = .2) +
      geom_hline(yintercept = 24, color = "black", linetype = "dashed") +
      labs(x = NULL,
           y = "Period (hs)",
           caption = paste0("n=", nrow(cosinor.df.plot())/2)) +
      scale_x_discrete(limits = c("LD", "DD")) +
      scale_y_continuous(breaks = seq(0, 48, 6)) +
      scale_colour_manual(values = c("#7373FF", "#FF7272")) +
      scale_fill_manual(values = c("#7373FF", "#FF7272")) +
      theme(legend.position = "none")
  }, 
  height = 300, 
  width = 200 )
  
  ## Amplitude ####
  output$ampsPlot <- renderPlot({
    
    cosinor.df.plot() %>% ggplot(aes(x = section,
                                y = amp,
                                color = section,
                                fill = section)) +
      geom_jitter(width = .2,
                  alpha = .2) + 
      stat_summary(fun.data = mean_se,
                   geom = "bar", 
                   alpha = .2) +
      stat_summary(fun.data = mean_se,
                   geom = "errorbar", 
                   alpha = 1,
                   width = .2) +
      labs(x = NULL,
           y = "Fitted amplitude",
           caption = paste0("n=", nrow(cosinor.df.plot())/2)) +
      scale_x_discrete(limits=c("LD", "DD")) +
      scale_colour_manual(values = c("#7373FF", "#FF7272")) +
      scale_fill_manual(values = c("#7373FF", "#FF7272")) +
      theme(legend.position = "none")
  }, 
  height = 300, 
  width = 200 )
  
  ## Acrophase ####
  output$acrosPlot <- renderPlot({
    
    cosinor.df.plot() %>% ggplot(aes(x = section,
                                y = acro_24,
                                color = section,
                                fill = section)) +
      geom_jitter(width = .2,
                  alpha = .2) + 
      stat_summary(fun.data = mean_se,
                   geom = "bar", 
                   alpha = .2) +
      stat_summary(fun.data = mean_se,
                   geom = "errorbar", 
                   alpha = 1,
                   width = .2,
                   position=position_nudge(-.2)) +
      geom_point(data = circ.means(), 
                 aes(y = mean_acro_24), 
                 color = "black",
                 position=position_nudge(.2)) +
      geom_errorbar(data = circ.means(), 
                    aes(y = mean_acro_24, ymin = mean_acro_24-sd_acro_24, ymax = mean_acro_24+sd_acro_24), 
                    width = .2,
                    color = "black",
                    position=position_nudge(.2)) +
      geom_hline(yintercept = 24, color = "black", linetype = "dashed") +
      labs(x = NULL,
           y = "Fitted acrophase (24 hs)",
           caption = paste0("n=", nrow(cosinor.df.plot())/2)) +
      scale_x_discrete(limits = c("LD", "DD")) +
      scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 6)) +
      scale_colour_manual(values = c("#7373FF", "#FF7272")) +
      scale_fill_manual(values = c("#7373FF", "#FF7272")) +
      theme(legend.position = "none")
    }, 
    height = 300, 
    width = 200 )
  
  ## Polar acrophase ####
  output$acrospolarPlot <- renderPlot({
    
    cosinor.df.plot() %>% ggplot(aes(x = acro_24,
                                   y = 1,
                                   color = section)) +
      geom_hline(yintercept = .5, color = "grey90") +
      geom_hline(yintercept = 1, color = "grey90") +
      geom_point(size = 3, alpha = .5) +
      geom_segment(
        data = circ.means(),
        aes(x = mean_acro_24, 
            xend = mean_acro_24,
            yend = .9*r),
        y = 0,
        size = 1,
        arrow = arrow(length = unit(0.05, "npc"))
      ) +
      labs(x = NULL,
           y = NULL,
           color = "Section",
           caption = paste0("n=", nrow(cosinor.df.plot())/2)) +
      scale_x_continuous(breaks = c(0,6,12,18,24),
                         limits = c(0, 24)) +
      scale_y_continuous(breaks = 1,
                         limits = c(0, 1)) +
      theme_void() +
      coord_polar() +
      scale_colour_manual(values = c("#7373FF", "#FF7272")) +
      theme(legend.position = "top",
            axis.text.x = element_text(size = 12))
  }, 
  height = 300, 
  width = 300 )
  
  ## Rayleigh table ####
  output$table_rayleigh <- renderDataTable(
    circ.means()
  )
  
  # Download ####
  # Hide download action buttons when there is no file loaded
  observe({
    if (is.null(input$input_file)) {
      shinyjs::disable("downloadData")
      shinyjs::disable("downloadPeriods")
      shinyjs::disable("downloadParameters")
    } else {
      shinyjs::enable("downloadData")
      shinyjs::enable("downloadPeriods")
      shinyjs::enable("downloadParameters")
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
  
  # Downloadable csv of parameters dataset
  output$downloadParameters <- downloadHandler(
    filename = function() {
      "parameters.csv"
    },
    content = function(file) {
      write.csv(cosinor.df(), 
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
