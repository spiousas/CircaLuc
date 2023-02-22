#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, viridis, tidyverse, zoo, shinyjs, scales, gsignal, 
               here, circular, gghalves, writexl, shinyWidgets, scales,
               patchwork, ggsci, tidymodels, shinyWidgets, pairwiseComparisons)
pacman::p_load_gh("emo")

useShinyjs()
useSweetAlert()

color_choices = list(
  list(
    'black',
    'white',
    'red',
    'blue',
    'forestgreen',
    '#666666',
    '#7f7f7f',
    "#7373FF", 
    "#FF7272"
  ),
  as.list(c("#000000" "#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7")),
  as.list(brewer_pal(palette = "Blues")(9)),
  as.list(brewer_pal(palette = "Greens")(9)),
  as.list(brewer_pal(palette = "Spectral")(11)),
  as.list(brewer_pal(palette = "Dark2")(8))
)

shinyUI(fluidPage(
  #theme = bslib::bs_theme(bootswatch = "flatly"),
  
  # Application title ####
  titlePanel(paste0("CircaLuc v0.6 ", emo::ji("clock"))),
  
  # Sidebar ####
  sidebarLayout(
    sidebarPanel(
      radioButtons('sep', 'Separator for the file',
                   c("Comma (,)" = ',', "Semicolon (;)" = ';',
                     "Tab ( )" = '\t', "Pipe (|)" = '|'),
                   ','),
      fileInput("input_file",
                "Choose input data (.csv or .tsv)",
                accept = c(".csv", ".tsv")),
      numericInput("sp", "Sampling interval (min):", 30, 
                   min = 1, max = 100, width = 180 ),
      downloadButton("downloadData", "Download data")
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
              br(),
              "<- Please start the analysis by loading data on the lefside panel",
              br()),
            fluidRow(
              h3("Filter unwanted wells:"),
              column(width = 12,
                     selectInput("filter_raw_data", "Well(s) to keep (only selected wells will be further processed):", 
                                 multiple = TRUE,
                                 choices = "",
                                 selected = "",
                                 width = 540))),             
            fluidRow(
              h3("Raw data plot"),
              h4("Luminescence (individual wells + mean)"),
              column(width = 3,
                     shinyWidgets::dropdownButton(
                       h4("List of settings"),
                       selectInput("raw_y_scale", "Luminosity scale:",
                                   c("linear", "log2", "log10"),
                                   width = 180),
                       selectInput("raw_group", "Group:", 
                                   multiple = FALSE,
                                   choices = "",
                                   selected = "",
                                   width = 180),
                       selectInput("raw_well", "Well(s) to plot:", 
                                   multiple = FALSE,
                                   choices = "",
                                   selected = "",
                                   width = 180),
                       circle = FALSE, status = "primary", icon = icon("gear"), width = "300px",
                       style = "material-flat", label = "Figure settings",
                       tooltip = tooltipOptions(title = "Click to see figure settings!")
                     )), 
              column(width = 4,
                     shinyWidgets::actionBttn(
                       "plot_raws",
                       label = "Plot raw data!",
                       color = "success",
                       size = "sm",
                       style = "material-flat",
                       icon = icon("pencil"),
                       block = TRUE
                     ))
              ),
            fluidRow(
              plotOutput("rawPlot", height = "auto"),
              br(),
              column(width = 4,
                     dropdownButton(
                       numericInput("rawPlot_W","Width (cm):", 20,
                                    min = 1, 
                                    max = 100),
                       numericInput("rawPlot_H","Height (cm):", 20,
                                    min = 1, 
                                    max = 100),
                       numericInput("rawPlot_DPI","DPI:", 300,
                                    min = 1, 
                                    max = 3000),
                       downloadButton("rawDownloadPlot", HTML("Download<br/>Plot")),
                       circle = FALSE, status = "success", icon = icon("file"), width = "300px",
                       label = "Download figure",
                       tooltip = tooltipOptions(title = "Click to see download options!")
                     ))))
        ),
        # Processed data ####
        tabPanel(
          "Preprocessed data",
          fluidPage(
            fluidRow(
              h3("Preprocessing parameters:"),
              column(width = 4,
                     numericInput("ZTcorte", "Start of LD section (hs):", 24, 
                                  min = 1, 
                                  max = 125),
                     numericInput("LD_period", "LD period (hs):", 24, 
                                  min = 1, 
                                  max = 125),
                     selectInput("method_detrending", 
                                 multiple = FALSE,
                                 "Detrending method:", 
                                 c("Linear" = "linear",
                                   "Exponential" = "exponential"))
                     ),
              column(width = 4,
                     numericInput("ZTLD", "End of LD section (hs):", 96,
                                  min = 1, 
                                  max = 250),
                     numericInput("ZTDD", "End of DD section (hs):", 168,
                                  min = 1, 
                                  max = 250)
                     ),
              column(width = 4,
                     numericInput("smo","Smoothing width (hs):", 12,
                                  min = 1, 
                                  max = 100),
                     numericInput("det", "Detrending length (hs):", 24,
                                  min = 1, 
                                  max = 100)
                     )), 
          fluidRow(
            h3("Light-Darkness cycle in LD:"),
            column(width = 4,
                   numericInput("LD_duration", "Light-phase lenght (h):", 12, 
                                min = 1, 
                                max = 125)
            ),
            column(width = 4,
                   selectInput("LD_starts_with", "Starts with:",
                               multiple = FALSE,
                               c("Light", "Darkness"),
                               selected = "Light")
            )),
          fluidRow(column(width = 4),
                   column(width = 4,
                          shinyWidgets::actionBttn(
                            inputId = "run_prepro",
                            label = "Preprocess the data!",
                            color = "primary",
                            size = "sm",
                            style = "material-flat",
                            icon = icon("refresh"),
                            block = TRUE
                          ))
          ),
          fluidRow(
            h3("Plotting group means:"),
            h4("Detrended luminescence (group means)"),
            column(width = 3,
                   shinyWidgets::dropdownButton(
                     h4("List of settings"),
                     selectInput("section_grouped_preprocessed", "Plot section:",
                                 multiple = TRUE,
                                 c("LD", "DD"),
                                 selected = c("LD", "DD")),
                     selectInput("preprocessed_grouped_group", "Group:",
                                 multiple = TRUE,
                                 choices = "",
                                 selected = "",
                                 width = 400),
                     selectInput("preprocessed_grouped_well",
                                 multiple = FALSE,
                                 "Select what to plot*:",
                                 c("All the wells" = "all",
                                   "Only synchronized" = "only_synch",
                                   "Only rhythmic" = "only_rhythm",
                                   "Only entrained" = "only_entrained"),
                                 selected = c("All the wells")),
                     actionButton(inputId = "info", label = "(*) Info", width = "100%", class = "btn-info", style = "color: #FFF"),
                     br(),
                     selectInput("preprocessed_sd", "Plot SD band:",
                                 multiple = FALSE,
                                 choices = c("yes", "no"),
                                 selected = "yes",
                                 width = 180),
                     selectInput("GroupColors",
                                 multiple = FALSE,
                                 "Group colors:",
                                 c("Lancet", "Nature", "NEJM"),
                                 selected = "Lancet"),
                     sliderInput("preprocessed_y_limits", "Plot SD band:",
                                 min = -0.1, max = 0.1,
                                 value = c(-0.1, 0.1)),
                     circle = FALSE, status = "primary", icon = icon("gear"), width = "300px",
                     label = "Figure options and plotting",
                     tooltip = tooltipOptions(title = "Click to see figure settings!")
                   )),
            column(width = 3, 
                   shinyWidgets::actionBttn(
                     inputId = "plot_prepro1",
                     label = "Plot!",
                     color = "success",
                     size = "sm",
                     style = "material-flat",
                     icon = icon("pencil"),
                     block = TRUE
                   ))),
          fluidRow(
            plotOutput("detrended_group_Plot", height = "auto"),
            br(),
            column(width = 3,
                   shinyWidgets::dropdownButton(
                     numericInput("detrendedPlot_W","Width (cm):", 8,
                                  min = 1, 
                                  max = 100),
                     numericInput("detrendedPlot_H","Height (cm):", 8,
                                  min = 1, 
                                  max = 100),
                     numericInput("detrendedPlot_DPI","DPI:", 300,
                                  min = 1, 
                                  max = 3000),
                     shinyWidgets::radioGroupButtons(
                       inputId = "detrendedPlot_device",
                       label = "Format", 
                       choices = c("png", "svg"),
                       selected = "png",
                       status = "primary"
                     ),
                     downloadButton("detrendedDownloadPlot", HTML("Download<br/>Plot")),
                     circle = FALSE, status = "success", icon = icon("file"), width = "100px",
                     label = "Download figure",
                     tooltip = tooltipOptions(title = "Click to see download options!")
            ))
          ),
          fluidRow(
            h3("Plotting individual wells for a given group:"),
            h4("Detrended luminescence for a given group (individual wells + group mean)"),
            column(width = 6,
                   shinyWidgets::dropdownButton(
                     h4("List of settings"),
                     selectInput("section_indiv_preprocessed", "Plot section:",
                                 multiple = TRUE,
                                 c("LD", "DD"),
                                 selected = c("LD", "DD")),
                     selectInput("preprocessed_indiv_group", "Group:", 
                                 multiple = FALSE,
                                 choices = "",
                                 selected = "",
                                 width = 400),
                     shinyWidgets::actionBttn(
                       inputId = "plot_prepro2",
                       label = "Plot!",
                       color = "success",
                       size = "sm",
                       style = "material-flat",
                       icon = icon("pencil"),
                       block = TRUE
                     ),
                     circle = FALSE, status = "primary", icon = icon("gear"), width = "300px",
                     label = "Figure options and plotting",
                     tooltip = tooltipOptions(title = "Click to see figure settings!")
                   ))),
          fluidRow(
            plotOutput("detrended_indiv_Plot", height = "auto"),
            br(),
            column(width = 4,
                   shinyWidgets::dropdownButton(
                     numericInput("detrendedIndivPlot_W","Width (cm):", 20,
                                  min = 1, 
                                  max = 100),
                     numericInput("detrendedIndivPlot_H","Height (cm):", 20,
                                  min = 1, 
                                  max = 100),
                     numericInput("detrendedIndivPlot_DPI","DPI:", 300,
                                  min = 1, 
                                  max = 3000),
                     downloadButton("detrendedIndivDownloadPlot", HTML("Download<br/>Plot")),
                     circle = FALSE, status = "success", icon = icon("file"), width = "300px",
                     label = "Download figure",
                     tooltip = tooltipOptions(title = "Click to see download options!")
                   ))))
        ),
        # Periods ####
        tabPanel("Periods",
                 fluidRow(
                   h3("Fitting methods:"),
                   column(width = 4,
                          selectInput("methodLD", "LD:",
                                      c("Fourier" = "fourier",
                                        "LS" = "ls",
                                        "24 hs" = "twentyfour"),
                                      selected = "twentyfour",
                                      width = 150)
                          ),
                   column(width = 4,
                          selectInput("methodDD", "DD:",
                                      c("Fourier" = "fourier",
                                        "LS" = "ls",
                                        "24 hs" = "twentyfour"),
                                      selected = "ls",
                                      width = 150)
                          )
                 ),
                 fluidRow(
                   h3("Fitting parameters")),
                 fluidRow(
                   h4("24 hs: In case you want to set a fixed period different than 24."),
                   column(width = 4,
                          numericInput("fixed_period_LD", "Fixed period for LD (hs):", 24,
                                       min = 1, 
                                       max = 200, 
                                       width = 150)
                   ),
                   column(width = 4,
                          numericInput("fixed_period_DD", "Fixed period for DD (hs):", 24,
                                       min = 1, 
                                       max = 200, 
                                       width = 150)
                   )),
                 fluidRow(
                   h4("LS: Limits and oversampling:"),
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
                               selected = c("LD", "DD")),
                   selectInput("well_cosinor", 
                               multiple = FALSE,
                               "Well to plot:", 
                               c("LD", "DD"),
                               selected = c("LD", "DD")),
                   numericInput("fit_length_cosinor", "DD section length for fitting (h)*:", 0, 
                                min = 4, 
                                max = 125),
                   "* Set as zero to fit the cosinor to the complete DD section. This number should be larger than 2 hs."
            ),
            column(width = 8,
                   sliderInput("RpassLD", HTML("R threshold for LD (R<sub>tr-LD</sub>):"),
                               min = 0, max = 1,
                               value = 0.5),
                   sliderInput("RpassDD", HTML("R threshold for DD (R<sub>tr-DD</sub>):"),
                               min = 0, max = 1,
                               value = 0.5),
                   sliderInput("phasepass", HTML("Threshold phase difference (phase<sub>tr</sub>):"),
                                                 min = 0, max = 12, post = " hs",
                                                 value = 4.0)
            ),
          ),
          fluidRow(
            br(),
            plotOutput("cosinorPlot", height = "auto"),
            br(),
            column(width = 3,
                  downloadButton("cosinorDownloadPlot", HTML("Download<br/>Plot"))
            ),
            column(width = 2,
                   numericInput("cosinorPlot_W","Width (cm):", 20,
                                min = 1, 
                                max = 100)
            ),
            column(width = 2,
                   numericInput("cosinorPlot_H","Height (cm):", 10,
                                min = 1, 
                                max = 100)
            ),
            column(width = 2,
                   numericInput("cosinorPlot_DPI","DPI:", 300,
                                min = 1, 
                                max = 3000)
            )
          ),
          fluidRow(
            h3("Cosinor data"),
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
                                      "Select what to plot:", 
                                      c("All the wells" = "all",
                                        "Only synchronized" = "only_synch",
                                        "Only rhythmic" = "only_rhythm",
                                        "Only entrained" = "only_entrained"),
                                      selected = c("All the wells"))
                   )
                 ),
           fluidRow(HTML(paste("<b>Synchronized</b>: R>R<sub>tr</sub> in LD.<br>&nbsp&nbsp&nbsp&nbsp&nbsp Populations where the period and acrophase are set by the LD/CW zeitgebers.<br>", 
                               "<b>Rhythmic</b>: R>R<sub>tr</sub> in LD and R>R<sub>tr</sub> in DD.<br>&nbsp&nbsp&nbsp&nbsp&nbsp Circadian under constant conditions populations.<br>", 
                               "<b>Entrained</b>: R>R<sub>tr</sub> in LD and phase difference of +/- phase<sub>tr</sub> h.<br>&nbsp&nbsp&nbsp&nbsp&nbsp Populations that retained their circadian acrophase when placed under constant conditions<br>", 
                               sep="<br/>"))),
           fluidRow(
             h2("Cosinor fit results"),
             h3("Periods"),
             plotOutput("periodsPlot", height = "auto"),
             column(width = 3,
                    downloadButton("periodsPlotDownloadPlot", HTML("Download<br/>Plot"))
             ),
             column(width = 2,
                    numericInput("periodsPlot_W","Width (cm):", 20,
                                 min = 1, 
                                 max = 100)
             ),
             column(width = 2,
                    numericInput("periodsPlot_H","Height (cm):", 10,
                                 min = 1, 
                                 max = 100)
             ),
             column(width = 2,
                    numericInput("periodsPlot_DPI","DPI:", 300,
                                 min = 1, 
                                 max = 3000)
             )
           ),
           fluidRow(
             h3("Amplitude"),
             plotOutput("ampsPlot",height = "auto"),
             column(width = 3,
                    downloadButton("ampsPlotDownloadPlot", HTML("Download<br/>Plot"))
             ),
             column(width = 2,
                    numericInput("ampsPlot_W","Width (cm):", 20,
                                 min = 1, 
                                 max = 100)
             ),
             column(width = 2,
                    numericInput("ampsPlot_H","Height (cm):", 10,
                                 min = 1, 
                                 max = 100)
             ),
             column(width = 2,
                    numericInput("ampsPlot_DPI","DPI:", 300,
                                 min = 1, 
                                 max = 3000)
             )
           ),
           fluidRow(
             h3("Acrophase"),
             plotOutput("acrosPlot",height = "auto"),
             column(width = 3,
                    downloadButton("acrosPlotDownloadPlot", HTML("Download<br/>Plot"))
             ),
             column(width = 2,
                    numericInput("acrosPlot_W","Width (cm):", 20,
                                 min = 1, 
                                 max = 100)
             ),
             column(width = 2,
                    numericInput("acrosPlot_H","Height (cm):", 10,
                                 min = 1, 
                                 max = 100)
             ),
             column(width = 2,
                    numericInput("acrosPlot_DPI","DPI:", 300,
                                 min = 1, 
                                 max = 3000)
             )
           ),
          fluidRow(
            h3("Circular data"),
            column(width = 7,
                   dataTableOutput('table_rayleigh')
            )
          )
        ),
        # Stats ####
        tabPanel("Stats",
                 fluidRow(
                   column(width = 6,
                          selectInput("select_stats", 
                                      multiple = FALSE,
                                      "Select what stats to use:", 
                                      c("Parametric" = "parametric",
                                        "Non parametric" = "nonparametric"),
                                      selected = c("Parametric"))
                   ),
                   column(width = 6,
                          selectInput("stat_filter", 
                                      multiple = FALSE,
                                      "Select how to filter the data before analysis:", 
                                      c("All the wells" = "all",
                                        "Only synchronized" = "only_synch",
                                        "Only rhythmic" = "only_rhythm",
                                        "Only entrained" = "only_entrained"),
                                      selected = c("All the wells"))
                   )
                 ),
                 fluidRow(column(width = 4),
                   column(width = 4,
                          actionBttn(
                            inputId = "start_stat",
                            label = "Run the analysis!",
                            color = "primary",
                            size = "sm",
                            style = "unite",
                            icon = icon("refresh"),
                            block = TRUE
                          )),
                          column(width = 4)
                 ),
                 ## Periods ####
                 fluidRow(
                  h3("Period comparison DD"),
                  br(),
                  "First let's test the dependence of the period with the group. This is done by means of a linear model
                  implemented using the lm function of base R using the period ~ group formula:",
                  br(),
                  column(width = 12,
                        dataTableOutput('stat_periods_DD'))
                 ),
                 fluidRow(
                   h4("Multiple comparisons"),
                   selectInput("mult_comp_corr_period", 
                               multiple = FALSE,
                               "How to correct for multiple comparisons:", 
                               c("Bonferroni" = "bonferroni",
                                 "None" = "none",
                                 "Holm-Bonferroni" = "holm"),
                               selected = c("Bonferroni")),
                   column(width = 12,
                        dataTableOutput('pairwise_periods_DD'))
                 ),
                 ## Amplitudes ####
                 fluidRow(
                   h3("Amplitude comparison DD"),
                   br(),
                   "First let's test the dependence of the amplitude with the group. This is done by means of a linear model
                  implemented using the lm function of base R using the amplitude ~ group formula:",
                  br(),
                  column(width = 12,
                         dataTableOutput('stat_amps_DD'))
                 ),
                 fluidRow(
                   h4("Multiple comparisons"),
                   selectInput("mult_comp_corr_amp", 
                               multiple = FALSE,
                               "How to correct for multiple comparisons:", 
                               c("Bonferroni" = "bonferroni",
                                 "None" = "none",
                                 "Holm-Bonferroni" = "holm"),
                               selected = c("Bonferroni")),
                   column(width = 12,
                          dataTableOutput('pairwise_amps_DD'))
                 )
        ),
        # Settings ####
        tabPanel("Settings",
               fluidRow(
                 h2("Color picker"),
                 br(),
                 column(width = 2,
                        h3("General:"),
                        spectrumInput(
                          inputId = "DarkColor",
                          label = "Darkness box color:",
                          choices = color_choices,
                          selected = "red",
                          options = list(`toggle-palette-more-text` = "Show more")
                        ),
                        spectrumInput(
                          inputId = "LightColor",
                          label = "Light box color:",
                          choices = color_choices,
                          selected = "blue",
                          options = list(`toggle-palette-more-text` = "Show more")
                        ),
                        spectrumInput(
                          inputId = "DarkShadeColor",
                          label = "Darkness shading:",
                          choices = color_choices,
                          selected = "#666666",
                          options = list(`toggle-palette-more-text` = "Show more")
                        ),
                        numericInput("FontSize","Font size (pt):", 32,
                                     min = 2, 
                                     max = 36)),
                 column(width = 2,
                        h3("Raw data:"),
                        spectrumInput(
                          inputId = "MeanRawColor",
                          label = "Mean raw signal:",
                          choices = color_choices,
                          selected = 'red',
                          options = list(`toggle-palette-more-text` = "Show more")
                        ),
                        spectrumInput(
                          inputId = "IndividualRawColor",
                          label = "Individual raw signals:",
                          choices = color_choices,
                          selected = '#666666',
                          options = list(`toggle-palette-more-text` = "Show more")
                        )),
                 column(width = 2,
                        h3("Proc data:"),
                        spectrumInput(
                          inputId = "MeanProcessedColor",
                          label = "Mean processed signal:",
                          choices = color_choices,
                          selected = 'red',
                          options = list(`toggle-palette-more-text` = "Show more")
                        ),
                        spectrumInput(
                          inputId = "IndividualProcessedColor",
                          label = "Individual processed signals:",
                          choices = color_choices,
                          selected = '#666666',
                          options = list(`toggle-palette-more-text` = "Show more")
                        )),
                 column(width = 2,
                        h3("Cosinor:"),
                        spectrumInput(
                          inputId = "LineLDColor",
                          label = "Signal LD:",
                          choices = color_choices,
                          selected = '#7373FF',
                          options = list(`toggle-palette-more-text` = "Show more")
                        ),
                        spectrumInput(
                          inputId = "LineDDColor",
                          label = "Signal DD:",
                          choices = color_choices,
                          selected = '#FF7272',
                          options = list(`toggle-palette-more-text` = "Show more")
                        ),
                        spectrumInput(
                          inputId = "FitLineColor",
                          label = "Fit line color:",
                          choices = color_choices,
                          selected = '#7f7f7f',
                          options = list(`toggle-palette-more-text` = "Show more")
                        )),
                 column(width = 2,
                        h3("Figs:"),
                        spectrumInput(
                          inputId = "FigsLDColor",
                          label = "LD:",
                          choices = color_choices,
                          selected = '#7373FF',
                          options = list(`toggle-palette-more-text` = "Show more")
                        ),
                        spectrumInput(
                          inputId = "FigsDDColor",
                          label = "DD:",
                          choices = color_choices,
                          selected = '#FF7272',
                          options = list(`toggle-palette-more-text` = "Show more")
                        )
                )
          )
        )
      )
      )
    )
  )
)
