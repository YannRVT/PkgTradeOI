tabItem(tabName = "table_analysis",
        fluidRow(
          tabsetPanel(id = "tabPanel",

                      tabPanel(h4("Report"),
                               titlePanel("Report"),
                               fluidRow(
                                 wellPanel(
                                   tags$b("Processing Report"),
                                   hr(),
                                   downloadButton('downloadPR', 'Download'),
                                   hr(),
                                   DT::DTOutput("proc_report") %>% withSpinner()
                                 ),
                                 wellPanel(
                                   tags$b("Cover Rate"),
                                   hr(),
                                   downloadButton('downloadCR', 'Download'),
                                   hr(),
                                   DT::DTOutput("cover_rate") %>% withSpinner()
                                 )
                               )
                      ),
                      tabPanel(h4("Outliers"),
                               titlePanel("Detected Outliers"),
                               downloadButton('downloadOL', 'Download'),
                               hr(),
                               DT::DTOutput("detected_outliers") %>% withSpinner()
                               ),

                      tabPanel(h4("Heterogeneous Series"),
                               titlePanel("Heterogeneous Series"),
                               downloadButton('downloadHS', 'Download'),
                               useShinyalert(),
                               div(style = "position:absolute;right:1em;",
                               actionButton("RIQ", "RIQ?"),
                               actionButton("RSD", "RSD?")),
                               hr(),
                               DT::DTOutput("series_hetero") %>% withSpinner()
                      ),
                      tabPanel(h4("Indices Table"),
                               titlePanel("Indices Table"),
                               downloadButton('downloadIT', 'Download'),
                               hr(),
                               DT::DTOutput("indices_table") %>% withSpinner()
                      )

          )))
