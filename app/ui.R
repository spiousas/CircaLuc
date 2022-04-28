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
               ggthemes, patchwork)
pacman::p_load_gh("emo")

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
  as.list(colorblind_pal()(8)),
  as.list(brewer_pal(palette = "Blues")(9)),
  as.list(brewer_pal(palette = "Greens")(9)),
  as.list(brewer_pal(palette = "Spectral")(11)),
  as.list(brewer_pal(palette = "Dark2")(8))
)

shinyUI(fluidPage(
  useShinyjs(),
  # Application title ####
  titlePanel(paste0("CircaLuc v0.1 ", emo::ji("clock"))),
  
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
              column(width = 4,
              selectInput("raw_y_scale", "Luminosity scale:",
                          c("linear", "log2", "log10"),
                          width = 180)),
              column(width = 4,
                     selectInput("well", "Well(s):", 
                                 multiple = TRUE,
                                 choices = "",
                                 selected = "",
                                 width = 180))
            ), 
            fluidRow(
              br(),
              plotOutput("rawPlot"),
              br(),
              column(width = 3,
                     downloadButton("rawDownloadPlot", HTML("Download<br/>Plot"))
              ),
              column(width = 2,
                     numericInput("rawPlot_W","Width (cm):", 20,
                                  min = 1, 
                                  max = 100)
              ),
              column(width = 2,
                     numericInput("rawPlot_H","Height (cm):", 10,
                                  min = 1, 
                                  max = 100)
              ),
              column(width = 2,
                     numericInput("rawPlot_DPI","DPI:", 300,
                                  min = 1, 
                                  max = 3000)
              )
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
                     numericInput("ZTcorte", "Start of LD section (hs):", 24, 
                                  min = 1, 
                                  max = 125),
                     numericInput("LD_period", "LD period (hs):", 24, 
                                  min = 1, 
                                  max = 125)
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
          fluidRow(
            h3("Plotting preferences:"),
            column(width = 4,
                 selectInput("section_preprocessed", "Plot section:",
                             multiple = TRUE,
                             c("LD", "DD"),
                             selected = c("LD", "DD"))
                 ),
            column(width = 4,
                 selectInput("filter_signal", 
                             multiple = FALSE,
                             "Select what to plot*:", 
                             c("All the wells" = "all",
                               "Only synchronized" = "only_synch",
                               "Only rhythmic" = "only_rhythm",
                               "Only entrained" = "only_entrained"),
                             selected = c("All the wells"))
          )),
          fluidRow(
            br(),
            plotOutput("detrendedPlot"),
            br(),
            column(width = 3,
                   downloadButton("detrendedDownloadPlot", HTML("Download<br/>Plot"))
                   ),
            column(width = 2,
                   numericInput("detrendedPlot_W","Width (cm):", 20,
                                min = 1, 
                                max = 100)
                   ),
            column(width = 2,
                   numericInput("detrendedPlot_H","Height (cm):", 10,
                                min = 1, 
                                max = 100)
            ),
            column(width = 2,
                   numericInput("detrendedPlot_DPI","DPI:", 300,
                                min = 1, 
                                max = 3000)
            )
          ),
          fluidRow(
            HTML(paste("<b>(*)<br>Synchronized</b>: R>R<sub>tr</sub> in LD.<br>&nbsp&nbsp&nbsp&nbsp&nbsp Populations where the period and acrophase are set by the LD/CW zeitgebers.<br>", 
                       "<b>Rhythmic</b>: R>R<sub>tr</sub> in LD and R>R<sub>tr</sub> in DD.<br>&nbsp&nbsp&nbsp&nbsp&nbsp Circadian under constant conditions populations.<br>", 
                       "<b>Entrained</b>: R>R<sub>tr</sub> in LD and phase difference of +/- phase<sub>tr</sub> h.<br>&nbsp&nbsp&nbsp&nbsp&nbsp Populations that retained their circadian acrophase when placed under constant conditions<br>", 
                       sep="<br/>"))
          ) 
        )
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
                   h3("Fitting parameters (LS):")
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
            plotOutput("cosinorPlot"),
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
             plotOutput("statPlot"),
             column(width = 3,
                    downloadButton("statDownloadPlot", HTML("Download<br/>Plot"))
             ),
             column(width = 2,
                    numericInput("statPlot_W","Width (cm):", 20,
                                 min = 1, 
                                 max = 100)
             ),
             column(width = 2,
                    numericInput("statPlot_H","Height (cm):", 10,
                                 min = 1, 
                                 max = 100)
             ),
             column(width = 2,
                    numericInput("statPlot_DPI","DPI:", 300,
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
