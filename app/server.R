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
  # Disable action buttons
  shinyjs::disable('plot_raws')
  shinyjs::disable('run_prepro')
  shinyjs::disable('plot_prepro1')
  shinyjs::disable('plot_prepro2')
  
  # Turn summarise notifications off
  options(dplyr.summarise.inform = FALSE)
  
  # Info on the conditions
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "What does this means",
      text = paste("(*)Synchronized: R>Rtr in LD. Populations where the period and acrophase are set by the LD/CW zeitgebers.", 
                   "Rhythmic: R>Rtr in LD and R>Rtr in DD. Circadian under constant conditions populations.", 
                   "Entrained: R>Rtr in LD and phase difference of +/- phase_tr. Populations that retained their circadian acrophase when placed under constant conditions"),
      type = "info"
    )
  })
  
  # ggplot2 theme
  theme_set(
    theme_minimal(
      ## increase size of all text elements
      base_size = 14,
      ## set custom font family for all text elements
      base_family = "Arial")
  )

  # overwrite other defaults of theme_minimal()
  theme_update(
    ## remove major horizontal grid lines
    panel.grid.major.x = element_blank(),
    ## remove all minor grid lines
    panel.grid.minor = element_blank(),
  )
    
  # Set working directory
  #source(here("R/aux_funs.R"))
  
  # Raw data ####
  
  ## ├Get and reshape the data ####
  data.df <- reactive({
    file <- input$input_file
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == c("csv", "tsv"), "Please upload a csv file"))
    
    read_delim(file$datapath, delim = input$sep, show_col_types = FALSE) %>%
    pivot_longer(!ZTTime,
                 names_to = "well",
                 values_to = "lumin") %>%
    mutate(well = str_trim(well)) %>%
    mutate(well = if_else(str_detect(well, "-"), well, paste0("GroupA-", well))) %>%
    separate(well, c("group", "well"), sep = "-") %>%
    group_by(group, well) %>%
    summarise(ZTTime = ZTTime[1:length(na.trim(lumin))],
              lumin = na.trim(lumin)) %>% # Clean trailing NAs in Lumin
    mutate(
      ZTTime = na.approx(ZTTime),
      # Linear approximation of missing points the time vector
      lumin = na.approx(lumin),
      # Linear approximation of missing luminosity
      Z_lumin = (lumin - mean(lumin, na.rm = TRUE)) / sd(lumin, na.rm = TRUE) # Z-scored lumin
    )
  })
  
  # Update of the plot rawand preprocessing action buttons
  observeEvent(!is_empty(data.df()), {
    message("Data loaded!")
    shinyjs::enable('plot_raws')
    shinyjs::enable('run_prepro')
    })
  
  ## ├Filter the data ####
  observe({
    updateSelectInput(
      session,
      "filter_raw_data",
      "Well(s) to keep:", 
      choices = as.character(unique(data.df()$well)),
      selected = as.character(unique(data.df()$well))
    )
  })
  
  data.df_filtered <- reactive({
    data.df() %>% 
      dplyr::filter(well %in% input$filter_raw_data)
  })
  
  ## ├Set up all the input options (wells and groups) ####
  observe({
    updateSelectInput(
      session,
      "raw_group",
      "Group:",
      choices = as.character(unique(data.df_filtered()$group))
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "preprocessed_grouped_group",
      "Group:",
      choices = as.character(unique(data.df_filtered()$group)),
      selected = as.character(unique(data.df_filtered()$group))
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "preprocessed_indiv_group",
      "Group:",
      choices = as.character(unique(data.df_filtered()$group)),
      selected = as.character(unique(data.df_filtered()$group)[1])
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "raw_well",
      "Well(s):",
      choices = list(
        `All in the group` = "all",
        `Individual wells` = as.character(unique(data.df_filtered() %>% 
                                                   dplyr::filter(group==input$raw_group) %>% 
                                                   pull(well)))
      ),
      selected = "all"
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "well_processed",
      "What to plot:",
      choices = list(
        `All the wells` = "all",
        `Means` = c("Mean" = "all",
                   "Only synchronized" = "only_synch",
                   "Only rhythmic" = "only_rhythm",
                   "Only entrained" = "only_entrained"),
        `Individual wells` = as.character(unique(data.df_filtered()$well))
      ),
      selected = "all"
    )
  })
  
  observe({
    updateSelectInput(
      session,
      "well_cosinor",
      "Well to plot:",
      choices = list(
        `Individual wells` = as.character(unique(data.df_filtered()$well))
      ),
      selected = as.character(unique(data.df_filtered()$well))[1]
    )
  })
  
  # Raw data plot ####
  data_rawPlot <- eventReactive(input$plot_raws, {
    
    # Filter what group to plot
    data.df.plot <- data.df_filtered() %>% 
      dplyr::filter(group %in% input$raw_group)
    
    # Filter what wells to plot
    if (!("all" %in% input$raw_well)) {
      data.df.plot <- data.df.plot %>% 
        dplyr::filter(well %in% input$raw_well) %>%
        drop_na(lumin)
    }
    
    # Min and max of the gray rectangles in the plot
    min_rect <- min(data.df.plot$lumin, na.rm = TRUE) 
    max_rect <- max(data.df.plot$lumin, na.rm = TRUE)
    
    # Build the figure
    data.df.plot %>%
      ggplot(aes(x = ZTTime, y = lumin)) +
      annotate("rect", 
               xmin = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period/2,0),
                          input$ZTLD-1,input$LD_period), 
               xmax = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period,input$LD_period/2),
                          input$ZTLD,input$LD_period),
               ymin = min_rect,
               ymax = max_rect,
               fill = input$DarkShadeColor,
               alpha = .2) +
      annotate("rect",
               xmin = input$ZTLD, 
               xmax = input$ZTDD,
               ymin = min_rect,
               ymax = max_rect,
               fill = input$DarkShadeColor,
               alpha = .2) +
      geom_vline(xintercept = seq(input$ZTcorte+input$LD_period/2, input$ZTDD, input$LD_period/2),
                 linewidth = .5, 
                 color = "gray70") +
      geom_vline(xintercept = c(input$ZTLD, input$ZTcorte),
                 linetype = "dashed",
                 linewidth = 1) +
      geom_line(aes(group = well),
                linewidth = .5,
                color = input$IndividualRawColor,
                alpha = .5) +
      stat_summary(linewidth = 1,
                   color = input$MeanRawColor,
                   geom = "line",
                   fun.data = "mean_se") +
      labs(x = "Time (h)",
           y = "Luminescence (RLU/min)") +
      scale_y_continuous(labels = scientific,
                         trans = if_else(input$raw_y_scale == "linear", 
                                         "identity",
                                         input$raw_y_scale)) +
      scale_x_continuous(breaks = seq(min(data.df.plot$ZTTime)-1, input$ZTDD, 12),
                         limits = c(min(data.df.plot$ZTTime)-1, input$ZTDD+1),
                         oob = scales::squish) +
      theme(legend.position = "none",
            plot.title.position = "plot") 
    
  })
  
  # Render raw plot
  output$rawPlot <- renderPlot({data_rawPlot()}, 
                               height = 400, 
                               width = 600 )
  
  # Processed data ####
  ## ├Weighting and  cutting in sections ####
  
  observeEvent(input$run_prepro,
               shinyWidgets::sendSweetAlert(
                 title = "Running!!",
                 text = "The data is being preprocessed",
                 type = "info",
                 showCloseButton = FALSE,
                 closeOnClickOutside = FALSE,
               ))
  
  weighted.df <- eventReactive(input$run_prepro, {
    data.df_filtered() %>%
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
          ZTTime >= input$ZTcorte & ZTTime < input$ZTLD,
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
      group_by(group, well, section) %>% # Grouping by well for detrending and smoothing
      mutate(
        lumin_detrended = circaluc_detrending(lumin_weighted, input$method_detrending),
        lumin_smoothed = gsignal::conv(lumin_detrended,
                                       rep(1, 60 * input$smo / input$sp),
                                       shape = "same")
      ) %>%
      select(-c(Ys, lumin_weighted)) %>%
      ungroup()
  })

  ## ├Detrended data for plotting ####
  smoothed.df.continuous <- reactive({ 
    
    weighted.df() %>% 
      group_by(well) %>% # Only grouping by well for detrending and smoothing
      mutate(
        lumin_detrended = pracma::detrend(lumin_weighted, "linear")[, 1],
        lumin_smoothed = gsignal::conv(lumin_detrended,
                                       rep(1, 60 * input$smo / input$sp),
                                       shape = "same")
      ) %>%
      select(-c(Ys, lumin_weighted)) %>%
      ungroup() %>%
      left_join(cosinor.df() %>% select(c("well", "section", "synch", "rhythm","entrained")), 
                by = c("well", "section"))
    
  })
    
  smoothed.df.download <- reactive({ 
    smoothed.df.continuous() %>%
      group_by(ZTTime, section, group) %>%
      summarise(lumin_smoothed = mean(lumin_smoothed)) %>%
      mutate(well = paste0("mean_", group)) %>%
      select(-group) %>%
      bind_rows(smoothed.df.continuous() %>% select(well, ZTTime, lumin_smoothed, section))
  })
  
    smoothed.df.plot <- reactive({ 
    smoothed.df.pre_plot <- if (input$preprocessed_grouped_well == "only_synch") {
         smoothed.df.continuous() %>% dplyr::filter(synch == "yes!")
       } else if (input$preprocessed_grouped_well == "only_rhythm") {
         smoothed.df.continuous() %>% dplyr::filter(rhythm == "yes!")
       } else if (input$preprocessed_grouped_well == "only_entrained") {
         smoothed.df.continuous() %>% dplyr::filter(entrained == "yes!")
       } else {
         smoothed.df.continuous()
       }
    
    smoothed.df.pre_plot %>%
      group_by(ZTTime, section, group) %>%
      summarise(lumin_sd = sd(lumin_smoothed),
                lumin_smoothed = mean(lumin_smoothed)) %>%
      mutate(well = "mean") %>%
      bind_rows(smoothed.df.continuous() %>% select(well, ZTTime, lumin_smoothed, section, group))
  })
  
    # Update of the action buttons for plotting preprocessed data
    observeEvent(!is_empty(smoothed.df.plot()), {
      shinyjs::enable('plot_prepro1')
      shinyjs::enable('plot_prepro2')
      show_alert(
        title = "Data ready!!",
        text = "The preprocessing ran succesfully",
        type = "success"
      )
    })
    
  ## ├Detrended group plot ####
    
  observe({
    smoothed_data <- smoothed.df.plot() %>%
      dplyr::filter((section %in% input$section_grouped_preprocessed)) %>%
      dplyr::filter(group %in% input$preprocessed_grouped_group) %>%
      dplyr::filter(well == "mean")
    
    range_lumin <- range(smoothed_data %>% pull(lumin_smoothed))
    min_lumin_sd <- smoothed_data$lumin_sd[which.min(smoothed_data$lumin_smoothed)]
    max_lumin_sd <- smoothed_data$lumin_sd[which.max(smoothed_data$lumin_smoothed)]
    
    # Update the limits 
      updateSliderInput(
        session,
        "preprocessed_y_limits", "Plot SD band:",
        min = -0.1, max = 0.1,
        value = c(range_lumin[1] - 1.1 * max_lumin_sd, range_lumin[2] + 1.1 * max_lumin_sd)
      )
  })
    
  detrended_grouped_Plot <- eventReactive(input$plot_prepro1, {
    
    smoothed_data <- smoothed.df.plot() %>%
      dplyr::filter((section %in% input$section_grouped_preprocessed)) %>%
      dplyr::filter(group %in% input$preprocessed_grouped_group) %>%
      dplyr::filter(well == "mean")
    
    ymin_rect  <- input$preprocessed_y_limits[1] * 1.1
    ymax_rect  <- input$preprocessed_y_limits[2]
    ymax_label <- input$preprocessed_y_limits[1]
    
    # Grouped plot
    # Creates geom_ribbon() if "yes" is selected in "Plot SD band:"
    if (input$preprocessed_sd == "yes") {
      extra_plots <- list(geom_ribbon(aes(ymin = lumin_smoothed - lumin_sd,
                                          ymax = lumin_smoothed + lumin_sd),
                                      color = NA,
                                      alpha = .3))
    } else {
      extra_plots <- list()
    }
    
    smoothed_data %>%
      drop_na(lumin_smoothed) %>%
      ggplot(aes(x = ZTTime, y = lumin_smoothed, color = group, fill = group)) +
      annotate("rect", 
               xmin = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period/2,0),
                          input$ZTLD-1,input$LD_period), 
               xmax = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period,input$LD_period/2),
                          input$ZTLD,input$LD_period), 
               ymin = ymin_rect,
               ymax = ymax_rect,
               fill = input$DarkShadeColor,
               alpha = .2) +
      annotate("rect", 
               xmin = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period/2,0),
                          input$ZTLD-1,input$LD_period), 
               xmax = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period,input$LD_period/2),
                          input$ZTLD,input$LD_period), 
               ymin = ymin_rect,
               ymax = ymax_label,
               fill = input$DarkColor) +
      annotate("rect", 
               xmin = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               0, input$LD_period/2),
                          input$ZTLD-1,input$LD_period), 
               xmax = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period/2, input$LD_period),
                          input$ZTLD,input$LD_period), 
               ymin = ymin_rect,
               ymax = ymax_label,
               fill = input$LightColor) +
      annotate("rect",
               xmin = input$ZTLD, 
               xmax = input$ZTDD, 
               ymin = ymin_rect,
               ymax = ymax_rect,
               fill = input$DarkShadeColor,
               alpha = .2) +
      annotate("rect",
               xmin = input$ZTLD, 
               xmax = input$ZTDD, 
               ymin = ymin_rect,
               ymax = ymax_label,
               fill = input$DarkColor) +
      geom_vline(xintercept = seq(input$ZTcorte+input$LD_period/2, input$ZTDD, input$LD_period/2),
                 linewidth = .5, 
                 color = "gray70") +
      extra_plots +
      geom_line(linewidth = 1,
                alpha = .8) +
      geom_vline(xintercept = input$ZTLD,
                 linetype = "dashed",
                 linewidth = 1) +
      labs(x = "Time (h)",
           y = "Detrended luminescence",
           color = NULL) +
      groupcolors_list() +
      guides(fill = "none") +
      scale_x_continuous(breaks = seq(input$ZTcorte, input$ZTDD, 12),
                         limits = c(input$ZTcorte-1, input$ZTDD+1)) +
      scale_y_continuous(limits = c(ymin_rect, ymax_rect)) +
      coord_cartesian(expand = FALSE,
                      clip = 'off') +
      theme(legend.position = "bottom",
            plot.title.position = "plot") 
    })
  
    output$detrended_group_Plot <- renderPlot({detrended_grouped_Plot()}, 
                                            height = 400, 
                                            width = 600)
  
    ## ├Detrended indiv plot ####
    detrended_indiv_Plot <- eventReactive(input$plot_prepro2, {
      
      smoothed_data <- smoothed.df.plot() %>%
        dplyr::filter((section %in% input$section_indiv_preprocessed)) %>%
        dplyr::filter(group %in% input$preprocessed_indiv_group) %>%
        dplyr::filter(well != "mean")
      
      range_lumin <- range(smoothed_data %>% pull(lumin_smoothed))
      min_lumin_sd <- smoothed_data$lumin_sd[which.min(smoothed_data$lumin_smoothed)]
      max_lumin_sd <- smoothed_data$lumin_sd[which.max(smoothed_data$lumin_smoothed)]
      
      ymin_rect <- range_lumin[1] - 1.3 * min_lumin_sd
      ymax_rect <- range_lumin[2] + 1.1 * max_lumin_sd
      ymax_label <-range_lumin[1] - 1.1 * min_lumin_sd
      
      
      # Individual plot

    smoothed_data %>%
      dplyr::filter((section %in% input$section_indiv_preprocessed)) %>%
      drop_na(lumin_smoothed) %>%
      ggplot(aes(x = ZTTime, y = lumin_smoothed, color = group, fill = group)) +
      annotate("rect", 
               xmin = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period/2,0),
                          input$ZTLD-1,input$LD_period), 
               xmax = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period,input$LD_period/2),
                          input$ZTLD,input$LD_period), 
               ymin = ymin_rect,
               ymax = ymax_rect,
               fill = input$DarkShadeColor,
               alpha = .2) +
      annotate("rect", 
               xmin = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period/2,0),
                          input$ZTLD-1,input$LD_period), 
               xmax = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period,input$LD_period/2),
                          input$ZTLD,input$LD_period), 
               ymin = ymin_rect,
               ymax = ymax_label,
               fill = input$DarkColor) +
      annotate("rect", 
               xmin = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               0, input$LD_period/2),
                          input$ZTLD-1,input$LD_period), 
               xmax = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period/2, input$LD_period),
                          input$ZTLD,input$LD_period), 
               ymin = ymin_rect,
               ymax = ymax_label,
               fill = input$LightColor) +
      annotate("rect",
               xmin = input$ZTLD, 
               xmax = input$ZTDD, 
               ymin = ymin_rect,
               ymax = ymax_rect,
               fill = input$DarkShadeColor,
               alpha = .2) +
      annotate("rect",
               xmin = input$ZTLD, 
               xmax = input$ZTDD, 
               ymin = ymin_rect,
               ymax = ymax_label,
               fill = input$DarkColor) +
      geom_vline(xintercept = seq(input$ZTcorte+input$LD_period/2, input$ZTDD, input$LD_period/2),
                 linewidth = .5, 
                 color = "gray70") +
      geom_line(aes(group = well),
                linewidth = 1,
                alpha = .5, 
                color = "gray50") +
      stat_summary(size = 1,
                color = input$MeanProcessedColor,
                geom = "line",
                fun.data = mean_se) +
      geom_vline(xintercept = input$ZTLD,
                 linetype = "dashed",
                 linewidth = 1) +
      labs(x = "Time (h)",
           y = "Detrended luminescence",
           color = NULL) +
      scale_x_continuous(breaks = seq(input$ZTcorte, input$ZTDD, 12),
                         limits = c(input$ZTcorte-1, input$ZTDD+1)) +
      scale_y_continuous(limits = c(ymin_rect, ymax_rect)) +
      coord_cartesian(expand = FALSE,
                      clip = 'off') +
      theme(legend.position = "bottom",
            plot.title.position = "plot") 
  })

  output$detrended_indiv_Plot <- renderPlot({detrended_indiv_Plot()}, 
                                            height = 400, 
                                            width = 600)
  
  # Periods ####
  ## ├Period estimation ####
  periods.df <- reactive({
    smoothed.df() %>%
      dplyr::filter(section %in% c("LD", "DD")) %>%
      mutate(method = if_else(section == "LD", 
                              input$methodLD, 
                              input$methodDD)) %>%
      group_by(group, well, section, method) %>%
      nest() %>%
      mutate(period = map(data, ~period_estimation(signal = .x$lumin_smoothed,
                                                   time = .x$ZTTime,
                                                   from_freq = 1/input$max_period,
                                                   to_freq = 1/input$min_period,
                                                   oversampling_freq = input$oversampling,
                                                   method = method,
                                                   period24 = if_else(section=="LD", input$fixed_period_LD, input$fixed_period_DD)))) %>%
      select(-data) %>%
      unnest(c(period))
  })
  
  ## ├Periods plot ####
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
  ## ├Cosinor fitting ####
  cosinor.df <- reactive({
    smoothed.df() %>% 
      dplyr::filter((section %in% c("LD", "DD")) & 
                      (ZTTime < if_else(input$fit_length_cosinor==0, 
                                        input$ZTDD, 
                                        input$ZTLD + input$fit_length_cosinor))) %>% # Only fits the first fit_length_cosinor hs of DD
      group_by(well, section, group) %>% 
      mutate(ZTTime = if_else(section == "LD", 
                              ZTTime - input$ZTcorte,
                              ZTTime - input$ZTLD)) %>%
      nest() %>% 
      left_join(periods.df(), by = c("well", "section", "group")) %>%
      mutate(
        fit = map(data, ~nls(lumin_smoothed ~ alpha + amp * cos(2*pi*ZTTime/period - acro),
                             data = .x,
                             start = list(alpha = 0, amp = 1, acro = 1))),
        tidied = map(fit, broom::tidy),
        R = map2(fit, data, modelr::rsquare)
      ) %>% 
      unnest(tidied)  %>% 
      unnest(R) %>%
      select(all_of(c("well", "section", "group", "period", "term", "estimate", "R"))) %>%
      pivot_wider(names_from = "term",
                  values_from = "estimate") %>%
      ungroup() %>%
      mutate(
        acro = if_else(amp<0, pi+acro, acro),
        acro = if_else(acro<0, acro + (abs(acro)%/%(2*pi)+1) * 2*pi, acro) +
               if_else((input$LD_starts_with=="Darkness") & (section == "LD"), pi, 0),
        acro = if_else(abs(acro)>2*pi, acro%%(2*pi), acro),
        amp = if_else(amp<0, -amp, amp),
        acro_24 = acro*12/pi
      ) %>%
      group_by(group, well) %>%
      mutate(synch =  if_else(R[section=="LD"]>input$RpassLD, 
                              "yes!", "no"),
             rhythm = if_else((synch=="yes!") & (R[section=="DD"]>input$RpassDD), 
                              "yes!", "no"),
             entrained = if_else((rhythm=="yes!") & (abs(acro_24[section=="DD"]-acro_24[section=="LD"]) < input$phasepass), 
                              "yes!", "no") 
      ) %>%
      ungroup()
  })
  
  ## ├Circular means ####
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
      group_by(section, group) %>% 
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

  ## ├Cosinor table ####
  output$table_cosinor <- renderDataTable(
    cosinor.df()
  )
  
  ## ├Predicted values ####
  smoothed.df.predicted <- reactive({
    smoothed.df() %>%
      dplyr::filter(section %in% c("LD", "DD")) %>% 
      mutate(ZTTime_fit = if_else(section == "LD", 
                                  ZTTime - input$ZTcorte,
                                  ZTTime - input$ZTLD)) %>%
      left_join(cosinor.df() %>% select(-c(R, acro_24, synch)),
                by = c("well", "section")) %>%
      mutate(lumin_predicted = alpha + amp * cos(2*pi*ZTTime_fit/period - 
                                                   acro - if_else((input$LD_starts_with=="Darkness") & (section == "LD"), pi, 0))) %>%
      select(-c(period, alpha, amp, acro, ZTTime_fit))
  })
  
  ## ├Cosinor plot ####
  data_cosinorPlot <- reactive({
    
    #  Gray rectangles limits
    min_lumin <- min(smoothed.df.predicted()$lumin_smoothed[smoothed.df.predicted()$well %in% input$well_cosinor & smoothed.df.predicted()$section %in% input$section_cosinor])
    max_lumin <- max(smoothed.df.predicted()$lumin_smoothed[smoothed.df.predicted()$well %in% input$well_cosinor & smoothed.df.predicted()$section %in% input$section_cosinor])

    ymin_rect <- min_lumin - abs(min_lumin-max_lumin)*.1
    ymax_rect <- max_lumin + abs(min_lumin-max_lumin)*.1
    ymax_label <-min_lumin - abs(min_lumin-max_lumin)*.05
    
    # The cosinor plot
    smoothed.df.predicted() %>%
      dplyr::filter(well %in% input$well_cosinor) %>%
      dplyr::filter(section %in% input$section_cosinor) %>%
      drop_na() %>%
      ggplot(aes(x = ZTTime, color = section)) +
        annotate("rect", 
               xmin = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period/2,0),
                          input$ZTLD-1,input$LD_period), 
               xmax = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period,input$LD_period/2),
                          input$ZTLD,input$LD_period), 
               ymin = ymin_rect,
               ymax = ymax_rect,
               fill = input$DarkShadeColor,
               alpha = .2) +
      annotate("rect", 
               xmin = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period/2,0),
                          input$ZTLD-1,input$LD_period), 
               xmax = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period,input$LD_period/2),
                          input$ZTLD,input$LD_period), 
               ymin = ymin_rect,
               ymax = ymax_label,
               fill = input$DarkColor) +
      annotate("rect", 
               xmin = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               0, input$LD_period/2),
                          input$ZTLD-1,input$LD_period), 
               xmax = seq(input$ZTcorte+ifelse(input$LD_starts_with=="Light",
                                               input$LD_period/2, input$LD_period),
                          input$ZTLD,input$LD_period), 
               ymin = ymin_rect,
               ymax = ymax_label,
               fill = input$LightColor) +
        annotate("rect",
                 xmin = input$ZTLD, 
                 xmax = input$ZTDD, 
                 ymin = ymin_rect,
                 ymax = ymax_rect,
                 fill = input$DarkShadeColor,
                 alpha = .2) +
        annotate("rect",
                 xmin = input$ZTLD, 
                 xmax = input$ZTDD, 
                 ymin = ymin_rect,
                 ymax = ymax_label,
                 fill = input$DarkColor,
                 alpha = 1) +
        geom_line(aes(y = lumin_predicted), linewidth = 1, color = input$FitLineColor) +
        geom_line(aes(y = lumin_smoothed), linewidth = 1) +
        geom_vline(xintercept = input$ZTLD,
                   linetype = "dashed",
                   size = 1) +
        geom_vline(xintercept = seq(input$ZTcorte+12, input$ZTDD, 12),
                   size = .5,
                   color = "gray70") +
        labs(x = "Time (h)",
             y = "Detrended luminescence",
             title = paste0("Cosinor fit for well ", input$well_cosinor)) +
        scale_colour_manual(values = c(input$LineLDColor, input$LineDDColor)) +
        scale_x_continuous(breaks = seq(input$ZTcorte, input$ZTDD, 12),
                           limits = c(input$ZTcorte-1, input$ZTDD+1)) +
        scale_y_continuous(limits = c(ymin_rect, ymax_rect)) +
        coord_cartesian(expand = FALSE,
                        clip = 'off') +
        theme(legend.position = "none")
  })
  
  output$cosinorPlot <- renderPlot({data_cosinorPlot()}, 
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
  
  ## ├Figure dimensions ####
  fig_height <- reactive({300*((length(unique(cosinor.df()$group))-1)%/%4+1)})
  fig_width <- reactive({
    if (length(unique(cosinor.df()$group))<4) {
      600*length(unique(cosinor.df()$group)) / 4
    } else {
      600
    }
    })
  
  groupcolors_list <- reactive({ case_when(
    input$GroupColors == "Lancet" ~ list(scale_color_lancet(), scale_fill_lancet()),
    input$GroupColors == "Nature" ~ list(scale_color_npg(), scale_fill_npg()),
    input$GroupColors == "NEJM" ~ list(scale_color_nejm(), scale_fill_nejm()),
   ) })
  
  ## ├Period ####
  data_periodsPlot <- reactive({

    cosinor.df.plot() %>% ggplot(aes(x = group,
                                   y = period,
                                   color = group,
                                   fill = group)) +
      geom_jitter(width = .2,
                  alpha = .2) + 
      stat_summary(fun.data = mean_se,
                   geom = "bar", 
                   alpha = .2) +
      stat_summary(fun.data = mean_se,
                   geom = "errorbar", 
                   alpha = 1,
                   width = .2) +
      geom_hline(yintercept = input$fixed_period_LD, color = "black", linetype = "dashed") +
      facet_grid(factor(section, levels=c('LD', 'DD')) ~ .) +
      labs(x = NULL,
           y = "Period (h)",
           caption = paste0("n=", nrow(cosinor.df.plot())/2)) +
      #scale_x_discrete(limits = c("LD", "DD")) +
      scale_y_continuous(breaks = seq(0, 48, 6)) +
      # scale_colour_manual(values = c(input$FigsLDColor, input$FigsDDColor)) +
      # scale_fill_manual(values = c(input$FigsLDColor, input$FigsDDColor)) +
      groupcolors_list() +
      theme(legend.position = "none",
            panel.grid.major.x = element_line(color = "gray90"),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$periodsPlot <- renderPlot({ data_periodsPlot() },
                                   height = function(){fig_height()}, 
                                   width = function(){fig_width()}  )
  
  ## ├Amplitude ####
  data_ampsPlot <- reactive({
    
    cosinor.df.plot() %>% ggplot(aes(x = group,
                                y = amp,
                                color = group,
                                fill = group)) +
      geom_jitter(width = .2,
                  alpha = .2) + 
      stat_summary(fun.data = mean_se,
                   geom = "bar", 
                   alpha = .2) +
      stat_summary(fun.data = mean_se,
                   geom = "errorbar", 
                   alpha = 1,
                   width = .2) +
      facet_grid(factor(section, levels=c('LD', 'DD')) ~ .) +
      labs(x = NULL,
           y = "Fitted amplitude",
           caption = paste0("n=", nrow(cosinor.df.plot())/2)) +
      #scale_x_discrete(limits=c("LD", "DD")) +
      groupcolors_list() +
      #scale_colour_manual(values = c(input$FigsLDColor, input$FigsDDColor)) +
      #scale_fill_manual(values = c(input$FigsLDColor, input$FigsDDColor)) +
      theme(legend.position = "none",
            panel.grid.major.x = element_line(color = "gray90"),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$ampsPlot <- renderPlot({ data_ampsPlot() },
                                   height = function(){fig_height()}, 
                                   width = function(){fig_width()} )
  
  ## ├Polar acrophase ####
  data_acrospolarPlot <- reactive({
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
      facet_wrap(.~group, ncol = 3) +
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
      scale_colour_manual(values = c(input$FigsLDColor, input$FigsDDColor)) +
      theme(legend.position = "top",
            axis.text.x = element_text(size = 12))
  })
  
  output$acrosPlot <- renderPlot({ data_acrospolarPlot() },
                                height = function(){fig_height()}, 
                                width = function(){fig_width()})
  
  ## ├Rayleigh table ####
  output$table_rayleigh <- renderDataTable(
    circ.means()
  )
  
  # Stats ####
  ## ├Period comp DD ####
  stat_periods_DD <- reactive({
    m_periods_DD <- periods.df() %>%
      dplyr::filter(section == "DD") %>%
      lm(data = ., period ~ group)
    
    tidy(aov(m_periods_DD))
  })
  
  output$stat_periods_DD <- renderDataTable(
    stat_periods_DD()
  )
  
  # ## ├Mult-comp period DD ####
  # pairwise_periods_DD <- reactive({
  #   tidy(pairwise.t.test(
  #     x = periods.df() %>%
  #       dplyr::filter(section == "DD") %>%
  #       pull(period),
  #     g = periods.df() %>%
  #       dplyr::filter(section == "DD") %>%
  #       pull(group),
  #     p.adjust.method = input$mult_comp_corr_period
  #   )
  #   )
  # })
  
  output$pairwise_periods_DD <- renderDataTable(
    pairwise_periods_DD(), options = list(
      paging = TRUE,
      pageLength =  5)
  )
  
  ## ├Amplitude comp DD ####
  stat_amps_DD <- reactive({
    m_amps_DD <- cosinor.df() %>%
      dplyr::filter(section == "DD") %>%
      lm(data = ., amp ~ group)
    
    tidy(aov(m_amps_DD))
  })
  
  output$stat_amps_DD <- renderDataTable(
    stat_amps_DD()
  )
  
  # ## ├Mult-comp amplitude DD ####
  # pairwise_amps_DD <- reactive({
  #   tidy(pairwise.t.test(
  #     x = cosinor.df() %>%
  #       dplyr::filter(section == "DD") %>%
  #       pull(amp),
  #     g = cosinor.df() %>%
  #       dplyr::filter(section == "DD") %>%
  #       pull(group),
  #     p.adjust.method = input$mult_comp_corr_period
  #   )
  #   )
  # })
  
  output$pairwise_amps_DD <- renderDataTable(
    pairwise_amps_DD(), options = list(
      paging = TRUE,
      pageLength =  5)
  )
  
  # Download ####
  ## ├Files ####
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
                             "Smoothed_data" = smoothed.df.download() %>%
                               ungroup() %>%
                               select(-c(section)) %>% 
                               pivot_wider(names_from = well, 
                                           values_from = lumin_smoothed))
      write_xlsx(list_of_sheets, path = file)
    }
  )
  
  ## ├Plots ####
  # Raw plot
  output$rawDownloadPlot <- downloadHandler(
    filename = function(){
      here(paste0("rawPlot-", Sys.Date(), ".png"))
    },
    content = function(file){
      ggsave(file, 
             width = input$rawPlot_W,
             height = input$rawPlot_H,
             dpi = input$rawPlot_DPI,
             plot = data_rawPlot())
    }
  )
  
  # Detrended plot
  output$detrendedDownloadPlot <- downloadHandler(
    filename = function(){
      if (input$detrendedPlot_device == "png") {
        here(paste0("detrendedGroupedPlot-", Sys.Date(), ".png"))
      } else {
        here(paste0("detrendedGroupedPlot-", Sys.Date(), ".svg"))  
      }
    },
    content = function(file){
      ggsave(file, 
             width = input$detrendedPlot_W,
             height = input$detrendedPlot_H,
             dpi = input$detrendedPlot_DPI,
             plot = detrended_grouped_Plot(),
             device = input$detrendedPlot_device)
    }
  )
  
  # Detrended plot
  output$detrendedDownload_indiv_Plot <- downloadHandler(
    filename = function(){
      here(paste0("detrended_indiv_Plot-", Sys.Date(), ".png"))
    },
    content = function(file){
      ggsave(file, 
             width = input$detrendedIndivPlot_W,
             height = input$detrendedIndivPlot_H,
             dpi = input$detrendedIndivPlot_DPI,
             plot = detrended_indiv_Plot())
    }
  )
  
  # Cosinor plot
  output$cosinorDownloadPlot <- downloadHandler(
    filename = function(){
      here(paste0("cosinorPlot-", Sys.Date(), ".png"))
    },
    content = function(file){
      ggsave(file, 
             width = input$cosinorPlot_W,
             height = input$cosinorPlot_H,
             dpi = input$cosinorPlot_DPI,
             plot = data_cosinorPlot())
    }
  )
  
  # Periods plots
  output$periodsPlotDownloadPlot <- downloadHandler(
    filename = function(){
      here(paste0("periodsPlots-", Sys.Date(), ".png"))
    },
    content = function(file){
      ggsave(file, 
             width = input$periodsPlot_W,
             height = input$periodsPlot_H,
             dpi = input$periodsPlot_DPI,
             plot = data_periodsPlot())
    }
  )
  
  # Amplitude plots
  output$ampsPlotDownloadPlot <- downloadHandler(
    filename = function(){
      here(paste0("amplitudesPlots-", Sys.Date(), ".png"))
    },
    content = function(file){
      ggsave(file, 
             width = input$ampsPlot_W,
             height = input$ampsPlot_H,
             dpi = input$ampsPlot_DPI,
             plot = data_ampsPlot())
    }
  )
  
  # Acrophase plots
  output$acrosPlotDownloadPlot <- downloadHandler(
    filename = function(){
      here(paste0("acophasesPlots-", Sys.Date(), ".png"))
    },
    content = function(file){
      ggsave(file, 
             width = input$acrosPlot_W,
             height = input$acrosPlot_H,
             dpi = input$acrosPlot_DPI,
             plot = data_acrospolarPlot())
    }
  )
  
})

# Period estimation functions ####
pacman::p_load(lomb)
period_estimation <- function(signal, time, from_freq, to_freq, oversampling_freq, method, period24) {
  
  if (method == "ls") {
    
    ls <- lsp(
      signal,
      times = time,
      from = from_freq,
      to = to_freq,
      ofac = oversampling_freq,
      plot = FALSE
    )
    
    period <- as.numeric(summary.lsp(ls)$Value[11])
    pvalue <- as.numeric(summary.lsp(ls)$Value[12])
    
  } else if (method == "twentyfour") {
    period <- period24
    pvalue <- NA
    
  } else {
    stop("Wrong period estimation method!")
    
  }
  
  return(tibble(period = period, 
                pvalue = pvalue))
  
}

# Detrending functions ####
pacman::p_load(pracma)
circaluc_detrending <- function(signal, method) {
  
  if (method == "linear") {
    signal <- pracma::detrend(signal, "linear")[, 1]
    
  } else if (method == "exponential") {
    signal <- exp(pracma::detrend(log(signal), "linear")[, 1])
    
  } else {
    stop("Wrong detrending estimation method!")
    
  }
  
  return(signal)
  
}