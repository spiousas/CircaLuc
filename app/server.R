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
      base_family = "Arial")
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
    validate(need(ext == c("csv", "tsv"), "Please upload a csv file"))
    
    read_delim(file$datapath, delim = input$sep) %>%
    pivot_longer(!ZTTime,
                 names_to = "well",
                 values_to = "lumin") %>%
    mutate(well = str_trim(well)) %>%
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
               xmin = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period/2,0),
                          input$ZTLD-1,input$LD_period), 
               xmax = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period,input$LD_period/2),
                          input$ZTLD,input$LD_period), 
               ymin = min(data.df.plot$lumin[data.df.plot$well %in% input$well]), 
               ymax = max(data.df.plot$lumin[data.df.plot$well %in% input$well]),
               fill = "grey40",
               alpha = .2) +
      annotate("rect",
               xmin = input$ZTLD, 
               xmax = input$ZTDD, 
               ymin = min(data.df.plot$lumin[data.df.plot$well %in% input$well]), 
               ymax = max(data.df.plot$lumin[data.df.plot$well %in% input$well]),
               fill = "grey40",
               alpha = .2) +
      geom_vline(xintercept = seq(input$ZTcorte+input$LD_period/2, input$ZTDD, input$LD_period/2),
                 size = .5, 
                 color = "gray70") +
      geom_line(size = 1) +
      geom_vline(xintercept = c(input$ZTLD, input$ZTcorte),
                 linetype = "dashed",
                 size = 1) +
      labs(x = "Time (h)",
           y = "Luminescence (RLU/min)",
           color = "Well",
           title = "Luminescence") +
      scale_colour_viridis(discrete = TRUE) +
      scale_y_continuous(labels = scientific,
                         trans = if_else(input$raw_y_scale == "linear", 
                                         "identity",
                                         input$raw_y_scale)) +
      scale_x_continuous(breaks = seq(min(data.df.plot$ZTTime), input$ZTDD, 12),
                         limits = c(min(data.df.plot$ZTTime)-1, input$ZTDD+1)) +
      theme(legend.position = "none") 
    
  }, 
  height = 400, 
  width = 600 )
  
  # Processed data ####
  ## Weighting and  cutting in sections ####
  weighted.df <- reactive({
    data.df() %>%
      group_by(well) %>%
      mutate(
        Ys = gsignal::conv(abs(lumin),
                           rep(1, 60 * input$det / input$sp),
                           shape = "same"),
        Ys = if_else(row_number() <= 60 * input$det / (2 * input$sp),
                     Ys[min(which(row_number() >= 60 * input$det / (2 * input$sp)))],
                     Ys),
        lumin_weighted = lumin / Ys,
        section = ifelse(
          ZTTime >= input$ZTcorte & ZTTime <= input$ZTLD,
          "LD",
          ifelse(ZTTime > input$ZTLD &
                   ZTTime <= input$ZTDD, "DD", "NonU")
        )
      ) %>%
      dplyr::filter(section != "NonU") %>%
      ungroup() 
  })
  
  ## Detrended and smoothed data for fits and calculations ####
  smoothed.df <- reactive({
    weighted.df() %>%
      group_by(well, section) %>% # Grouping by well and section for detrending and smoothing
      mutate(
        lumin_detrended = pracma::detrend(lumin_weighted, "linear")[, 1],
        lumin_smoothed = gsignal::conv(lumin_detrended,
                                       rep(1, 60 * input$smo / input$sp),
                                       shape = "same")
      ) %>%
      select(-c(Ys, lumin_weighted)) %>%
      ungroup()
  })

  ## Detrended data for plotting ####
  smoothed.df.plot <- reactive({ 
    
    smoothed.df.continuous <- weighted.df() %>% 
      group_by(well) %>% # Only grouping by well for detrending and smoothing
      mutate(
        lumin_detrended = pracma::detrend(lumin_weighted, "linear")[, 1],
        lumin_smoothed = gsignal::conv(lumin_detrended,
                                       rep(1, 60 * input$smo / input$sp),
                                       shape = "same")
      ) %>%
      select(-c(Ys, lumin_weighted)) %>%
      ungroup() 
    
    smoothed.df.continuous %>%
      group_by(ZTTime, section) %>%
      summarise(lumin_se = sd(lumin_smoothed)/sqrt(length(unique(smoothed.df()$well))),
                lumin_smoothed = mean(lumin_smoothed)) %>%
      mutate(well = "mean") %>%
      bind_rows(smoothed.df.continuous %>% select(well, ZTTime, lumin_smoothed, section))
  })
  
  ## Detrended plot ####
  output$detrendedPlot <- renderPlot({
    
    smoothed.df.plot <- smoothed.df.plot()
    
    # Filtering and gray rectangles limits
    if (!("all" %in% input$well)) {
      smoothed.df.plot <- smoothed.df.plot %>% dplyr::filter(well %in% input$well)
      min_lumin <- min(smoothed.df.plot$lumin_smoothed[smoothed.df.plot$well %in% input$well & smoothed.df.plot$section %in% input$section_preprocessed])
      max_lumin <- max(smoothed.df.plot$lumin_smoothed[smoothed.df.plot$well %in% input$well & smoothed.df.plot$section %in% input$section_preprocessed])
    } else {
      min_lumin <- min(smoothed.df.plot$lumin_smoothed[smoothed.df.plot$section %in% input$section_preprocessed])
      max_lumin <- max(smoothed.df.plot$lumin_smoothed[smoothed.df.plot$section %in% input$section_preprocessed])
    }
    
    ymin_rect <- min_lumin - abs(min_lumin-max_lumin)*.1
    ymax_rect <- max_lumin + abs(min_lumin-max_lumin)*.1
    ymax_label <-min_lumin - abs(min_lumin-max_lumin)*.05
    
    # Creates geom_ribbon() if the selected well is "mean"
    if (input$well == "mean") {
      extra_plots <- list(geom_ribbon(aes(ymin = lumin_smoothed - lumin_se,
                                          ymax = lumin_smoothed + lumin_se),
                                      alpha = .3,
                                      color = NA))
    } else {
      extra_plots <- list()
    }
    
    # Plot
    smoothed.df.plot %>%
      dplyr::filter((section %in% input$section_preprocessed)) %>%
      drop_na(lumin_smoothed) %>%
      ggplot(aes(x = ZTTime, y = lumin_smoothed, colour = well)) +
      annotate("rect", 
               xmin = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period/2,0),
                          input$ZTLD-1,input$LD_period), 
               xmax = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period,input$LD_period/2),
                          input$ZTLD,input$LD_period), 
               ymin = ymin_rect,
               ymax = ymax_rect,
               fill = "grey40",
               alpha = .2) +
      annotate("rect", 
               xmin = seq(input$ZTcorte,input$ZTLD-1,24), 
               xmax = seq(input$ZTcorte+12,input$ZTLD,24), 
               ymin = ymin_rect,
               ymax = ymax_label,
               fill = "red",
               alpha = .7) +
      annotate("rect", 
               xmin = seq(input$ZTcorte+12,input$ZTLD-1,24), 
               xmax = seq(input$ZTcorte+24,input$ZTLD,24), 
               ymin = ymin_rect,
               ymax = ymax_label,
               fill = "blue",
               alpha = 1) +
      annotate("rect",
               xmin = input$ZTLD, 
               xmax = input$ZTDD, 
               ymin = ymin_rect,
               ymax = ymax_rect,
               fill = "grey40",
               alpha = .2) +
      annotate("rect",
               xmin = input$ZTLD, 
               xmax = input$ZTDD, 
               ymin = ymin_rect,
               ymax = ymax_label,
               fill = "red",
               alpha = 1) +
      extra_plots +
      geom_vline(xintercept = seq(input$ZTcorte+input$LD_period/2, input$ZTDD, input$LD_period/2),
                 size = .5, 
                 color = "gray70") +
      geom_line(size = 1) +
      geom_vline(xintercept = input$ZTLD,
                 linetype = "dashed",
                 size = 1) +
      labs(x = "Time (h)",
           y = "Detrended luminescence",
           title = "Detrended luminescence") +
      scale_colour_viridis(discrete = TRUE) +
      scale_x_continuous(breaks = seq(input$ZTcorte, input$ZTDD, 12),
                         limits = c(input$ZTcorte-1, input$ZTDD+1)) +
      scale_y_continuous(limits = c(ymin_rect, ymax_rect)) +
      coord_cartesian(expand = FALSE,
                      clip = 'off') +
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
                                                   method = method,
                                                   period24 = input$LD_period))) %>%
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
           y = "Period (h)",
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
      dplyr::filter((section %in% c("LD", "DD")) & 
                      (ZTTime < if_else(input$fit_length_cosinor==0, 
                                        input$ZTDD, 
                                        input$ZTLD + input$fit_length_cosinor))) %>% # Only fits the first fit_length_cosinor hs of DD
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
        acro_24 = acro*12/pi
      ) %>%
      group_by(well) %>%
      mutate(synch =  if_else(R[section=="LD"]>input$RpassLD, 
                              "yes!", "no"),
             rhythm = if_else((synch=="yes!") & (R[section=="DD"]>input$RpassDD), 
                              "yes!", "no"),
             entrained = if_else((rhythm=="yes!") & (abs(acro_24[section=="DD"]-acro_24[section=="LD"]) < input$phasepass), 
                                                "yes!", "no") 
      ) %>%
      ungroup()
  })
  
  ## Circular means ####
  circ.means <- reactive({
    
    if (input$filter_figures == "only_synch") {
      cosinor.df_circmeans <- cosinor.df() %>% dplyr::filter(synch == "yes!")  
    } else if (input$filter_figures == "only_rhythm") {
      cosinor.df_circmeans <- cosinor.df() %>% dplyr::filter(rhythm == "yes!")  
    } else if (input$filter_figures == "only_entrained") {
      cosinor.df_circmeans <- cosinor.df() %>% dplyr::filter(entrained == "yes!")  
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
             mean_period = map(data, ~mean(.x$period)),
             sd_period = map(data, ~sd(.x$period)),
             mean_acro_24 = map(circ_acro, ~mean(.x)),
             sd_acro_24 = map(circ_acro, ~sd(.x)),
             rayleigh = map(circ_acro, ~rayleigh.test(.x)),
             p.value = rayleigh[[1]]$p.value) %>%
      select(-c(data, circ_acro, rayleigh)) %>%
      unnest(c(mean_period, sd_period, mean_acro_24, sd_acro_24)) %>%
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
      left_join(cosinor.df() %>% select(-c(R, acro_24, synch)),
                by = c("well", "section")) %>%
      mutate(lumin_predicted = alpha + amp * cos(2*pi*ZTTime_fit/period - acro)) %>%
      select(-c(period, alpha, amp, acro, ZTTime_fit))
  })
  
  ## Cosinor plot ####
  output$cosinorPlot <- renderPlot({
    
    #  Gray rectangles limits
    min_lumin <- min(smoothed.df.predicted()$lumin_smoothed[smoothed.df.predicted()$well %in% input$well & smoothed.df.predicted()$section %in% input$section_cosinor])
    max_lumin <- max(smoothed.df.predicted()$lumin_smoothed[smoothed.df.predicted()$well %in% input$well & smoothed.df.predicted()$section %in% input$section_cosinor])

    ymin_rect <- min_lumin - abs(min_lumin-max_lumin)*.1
    ymax_rect <- max_lumin + abs(min_lumin-max_lumin)*.1
    ymax_label <-min_lumin - abs(min_lumin-max_lumin)*.05
    
    # The cosinor plot
    if ((length(input$well) == 1) & input$well %in% as.character(unique(data.df()$well)) ){
    smoothed.df.predicted() %>%
      dplyr::filter(well %in% input$well) %>%
      dplyr::filter(section %in% input$section_cosinor) %>%
      drop_na() %>%
      ggplot(aes(x = ZTTime, color = section)) +
        annotate("rect", 
                 xmin = seq(12,input$ZTLD-1,24), 
                 xmax = seq(24,input$ZTLD,24), 
                 ymin = ymin_rect,
                 ymax = ymax_rect,
                 fill = "grey40",
                 alpha = .2) +
        annotate("rect", 
                 xmin = seq(12,input$ZTLD-1,24), 
                 xmax = seq(24,input$ZTLD,24), 
                 ymin = ymin_rect,
                 ymax = ymax_label,
                 fill = "red",
                 alpha = .7) +
        annotate("rect", 
                 xmin = seq(0,input$ZTLD-1,24), 
                 xmax = seq(12,input$ZTLD,24), 
                 ymin = ymin_rect,
                 ymax = ymax_label,
                 fill = "blue",
                 alpha = 1) +
        annotate("rect",
                 xmin = input$ZTLD, 
                 xmax = input$ZTDD, 
                 ymin = ymin_rect,
                 ymax = ymax_rect,
                 fill = "grey40",
                 alpha = .2) +
        annotate("rect",
                 xmin = input$ZTLD, 
                 xmax = input$ZTDD, 
                 ymin = ymin_rect,
                 ymax = ymax_label,
                 fill = "red",
                 alpha = 1) +
        geom_line(aes(y = lumin_predicted), size = 1, color = "grey50") +
        geom_line(aes(y = lumin_smoothed), size = 1) +
        geom_vline(xintercept = input$ZTLD,
                   linetype = "dashed",
                   size = 1) +
        geom_vline(xintercept = seq(input$ZTcorte+12, input$ZTDD, 12),
                   size = .5,
                   color = "gray70") +
        labs(x = "Time (h)",
             y = "Detrended luminescence",
             title = "Cosinor fit") +
        scale_colour_manual(values = c("#7373FF", "#FF7272")) +
        scale_x_continuous(breaks = seq(input$ZTcorte, input$ZTDD, 12),
                           limits = c(input$ZTcorte-1, input$ZTDD+1)) +
        scale_y_continuous(limits = c(ymin_rect, ymax_rect)) +
        coord_cartesian(expand = FALSE,
                        clip = 'off') +
        theme(legend.position = "none")
    } else {
      ggplot() +
        labs(title = "Please choose a unique well to plot") +
        theme(plot.title = element_text(size = 20, color = "red", hjust = 0.5))
    }
  }, 
  height = 400, 
  width = 600 )
  
  # Figures ####
  
  cosinor.df.plot <- reactive({
    if (input$filter_figures == "only_synch") {
      cosinor.df() %>% dplyr::filter(synch == "yes!")  
    } else if (input$filter_figures == "only_rhythm") {
        cosinor.df() %>% dplyr::filter(rhythm == "yes!")  
    } else if (input$filter_figures == "only_entrained") {
      cosinor.df() %>% dplyr::filter(entrained == "yes!")  
    } else {
      cosinor.df()
    }
  })
  
  circ.means.plot <- reactive({
    if (input$filter_figures == "only_synch") {
      cosinor.df() %>% dplyr::filter(synch == "yes!")  
    } else if (input$filter_figures == "only_rhythm") {
      cosinor.df() %>% dplyr::filter(rhythm == "yes!")  
    } else if (input$filter_figures == "only_entrained") {
      cosinor.df() %>% dplyr::filter(entrained == "yes!")  
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
           y = "Period (h)",
           caption = paste0("n=", nrow(cosinor.df.plot())/2)) +
      scale_x_discrete(limits = c("LD", "DD")) +
      scale_y_continuous(breaks = seq(0, 48, 6)) +
      scale_colour_manual(values = c("#7373FF", "#FF7272")) +
      scale_fill_manual(values = c("#7373FF", "#FF7272")) +
      theme(legend.position = "none")
  }, 
  height = 300, 
  width = 180 )
  
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
  width = 180 )
  
  ## Polar acrophase ####
  output$acrospolarPlot <- renderPlot({
    
    cosinor.df.plot() %>% ggplot(aes(x = acro_24,
                                   y = R,
                                   color = section)) +
      geom_hline(yintercept = 1, color = "black", size = .25) +
      geom_vline(xintercept = seq(0,24,3), color = "black", size = .25) +
      geom_segment(
        x = 12, xend = 24, 
        y = 1, yend = 1,
        size = 2,
        color = "black"
      ) +
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
      scale_x_continuous(breaks = seq(0,24,3),
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
    } else {
      shinyjs::enable("downloadData")
    }
  })
  
  # Downloadable xslx
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0('data-', Sys.Date(), '.xlsx')
      },
    content = function(file) {
      list_of_sheets <- list("Periods_LD" = periods.df() %>% 
                               dplyr::filter(section == "LD"),
                             "Periods_DD" = periods.df() %>% 
                               dplyr::filter(section == "DD"),
                             "Cosinor_LD" = cosinor.df() %>% 
                               dplyr::filter(section == "LD"),
                             "Cosinor_DD" = cosinor.df() %>% 
                               dplyr::filter(section == "DD"),
                             "Circular_means" = circ.means(),
                             "Smoothed_data" = smoothed.df.plot() %>%
                               select(-c(lumin_se, section)) %>% 
                               pivot_wider(names_from = well, 
                                           values_from = lumin_smoothed))
      
      write_xlsx(list_of_sheets, path = file)
    }
  )
  
})
