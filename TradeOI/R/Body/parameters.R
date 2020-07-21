tabItem(tabName = "parameters",
        fluidPage(
          box(
            title = h4("Choose your parameters", style = "color:#0999DC"),
            height = NULL,
            width = 6,
            
            tabPanel(
              "what",
              
              sliderInput(
                "pct_missdata",
                label = ("Please select the maximum percentage of missing data by series"),
                min = 0,
                max = 100,
                value = 20
              ),
              
              radioButtons(
                "time_break",
                label = ("Please select the time breakdown for indices"),
                choices = list(
                  "Yearly" = "year",
                  "Quarterly" = "quarter",
                  "Monthly" = "month"
                ),
                selected = "year"
              ),
              
              dateRangeInput(
                'dateRange2',
                format = "yyyy/mm",
                label = 'Please select the date range to analyse',
                start = "2008-01-01",
                end = "2015-12-31",
                separator = "to",
                startview = 'year',
                language = 'en',
                weekstart = 1
              ),
              
              em(
                "WARNING: Please make sure the date range parameter follows the time breakdown logic.
       (e.g: if time breakdown is quaterly, date should start with the first month of a quarter)"
              ),
              
              hr(),
              
              radioButtons(
                "indices_bc",
                label = ("Please select the type of indices you want to compute"),
                choices = list("Base" = "base", "Chained" = "chained"),
                selected = "base"
              ),
              
              checkboxInput("date_to_date", label = "Year to date", value = FALSE),
              
              selectInput(
                "product_level",
                label = ("Please select the breakdown level of the product variable"),
                choices = list(
                  "Tariff Line" = 12,
                  "HS6" = 6,
                  "HS4" = 4,
                  "HS2" = 2,
                  "All Aggregated" = 0
                ),
                selected = 1
              ),
              checkboxInput("agr_reporter", label = "Agregated on Reporter variable", value = TRUE),
              checkboxInput("agr_partner", label = "Agregated on Partner variable", value = TRUE),
              em("WARNING: Make sure world partner is not counted twice."),
              hr(),
              actionButton("goButton", "Run analysis")
            )
          ),
          ##################################################################################################################################
          box(
            title = h4("Which do you want to perform?", style = "color:#0999DC"),
            height = NULL,
            width = 3,
            checkboxInput("checkbox1", label = "Unstable HS revision", value = TRUE),
            checkboxInput("checkbox2", label = "Delete Heterogenious time series", value = TRUE),
            checkboxInput("checkbox3", label = "Compute Indices", value = TRUE)
          ),
          ##################################################################################################################################
          box(
            title = h4("Which do you want to perform for outliers?", style = "color:#0999DC"),
            height = NULL,
            width = 3,
            radioButtons(
              "radio_outliers",
              label = NULL,
              choices = list(
                "Detect and delete" = 1,
                "Detect but compute indices with them" = 2,
                "Detect and replace by mean of nearest neighbours" = 3,
                "Detect and replace by robust regression" = 4,
                "No detection" = 5
              ),
              selected = 1
            )
          ),
          ##################################################################################################################################
          box(
            title = h4("Which product?", style = "color:#0999DC"),
            height = NULL,
            width = 6,
            
            em(
              "Info: To select one or multiple product on your trade dataset (by default TradeOI
            compute on all). If your computer is not powerfull it is better to choose this option."
            ),
            
            uiOutput("productUI"),
            uiOutput("selectDeselectUI2")
          ),
          ##################################################################################################################################
          box(
            title = h4("Which country partner?", style = "color:#0999DC"),
            height = NULL,
            width = 6,
            
            em(
              "Info: To select one or multiple partner on your trade dataset (you must uncheck the option: agregated on Partner variable). If your computer
               is not powerfull it is better to choose this option."
            ),
            
            uiOutput("partnerUI"),
            uiOutput("selectDeselectUI3")
          ),
          
          tags$style(
            type = "text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
          )
        ))