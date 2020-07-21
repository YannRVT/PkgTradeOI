tabItem(tabName = "analysis",
        fluidRow(
          tabsetPanel(id = "tabPanel",
                      
                      tabPanel(h4("Report"),
                               titlePanel("Report"),
                               fluidRow(
                                 box(
                                   title = h4(strong("Processing Report")),
                                   downloadButton('downloadPR', 'Download'),
                                   hr(),
                                   withSpinner(dataTableOutput("proc_report"))
                                 ),
                                 box(
                                   title = h4(strong("Cover Rate")),
                                   downloadButton('downloadCR', 'Download'),
                                   hr(),
                                   withSpinner(dataTableOutput("cover_rate"))
                                 )
                               )
                      ),
                      tabPanel(h4("Outliers"),
                               titlePanel("Detected Outliers"),
                               downloadButton('downloadOL', 'Download'),
                               hr(),
                               withSpinner(dataTableOutput("detected_outliers"))
                               ),
                      tabPanel(h4("Heterogeneous Series"),
                               titlePanel("Heterogeneous Series"),
                               downloadButton('downloadHS', 'Download'),
                               useShinyalert(),
                               div(style = "position:absolute;right:1em;",
                               actionButton("RIQ", "RIQ?"),
                               actionButton("RSD", "RSD?")),
                               hr(),
                               withSpinner(dataTableOutput("series_hetero"))
                      ),
                      tabPanel(h4("Indices Table"),
                               titlePanel("Indices Table"),
                               downloadButton('downloadIT', 'Download'),
                               hr(),
                               withSpinner(dataTableOutput("indices_table"))
                      )
                    
          )))