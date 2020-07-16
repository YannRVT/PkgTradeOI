#' Function ui to run TradeOi app
#'
#' @import filehash
#' @import sqldf
#' @import stringr
#' @import data.table
#' @import robustbase
#' @import shiny
#' @import shinydashboard
#' @import doParallel
#' @import gsubfn
#' @import proto
#' @import RSQLite
#' @import iterators
#' @import shinycssloaders
#' @import shinyjs
#' @import dplyr
#' @import optiRum
#' @import compiler
#' @import shinyalert
#' @import DT
#' @import stringi
#' @import highcharter
#'
#'
#' @author Yann Rivallant <yrivallant@intracen.org>
#' @export


# Define UI
app_ui <- function() {
  shinyUI(
    dashboardPage(
      skin = "black",
      dashboardHeader(
        title = "Detecting Outliers and Computing Trade Indices",
        titleWidth = 450,
        tags$li(
          a(
            href = 'http://www.intracen.org/',
            img(
              src = 'logo.png',
              title = "ITC Home",
              height = "30px"
            ),
            style = "padding-top:10px; padding-bottom:10px;"
          ),
          class = "dropdown"
        ),

        tags$li(
          a(
            href = 'https://www.trademap.org/Index.aspx',
            img(
              src = 'trademap.png',
              title = "Trade Map",
              height = "30px"
            ),
            style = "padding-top:10px; padding-bottom:10px;"
          ),
          class = "dropdown"
        ),

        tags$li(
          a(
            href = 'mailto:marketanalysis@intracen.org',
            img(
              src = 'mail.png',
              title = "Send us an email",
              height = "30px"
            ),
            style = "padding-top:10px; padding-bottom:10px;"
          ),
          class = "dropdown"
        )
      ),

      #content of the sideBar
      dashboardSidebar(
        sidebarMenu(
          id = "tabs",

          #Tab for introduction about the app
          menuItem(
            "About",
            tabName = "about",
            icon = icon("address-card")
          ),

          #Tab for data process
          menuItem(
            "Data Preparation",
            tabName = "datapreparation",
            icon = icon("wrench")
          ),

          #View your data
          menuItem(
            "View your data",
            tabName = "",
            icon = icon("binoculars"),
            menuSubItem(
              "Trade dataset",
              tabName = "data_trade",
              icon = icon("globe")
            ),
            menuSubItem(
              "Revision table",
              tabName = "data_revision",
              icon = icon("calendar-alt")
            ),
            menuSubItem(
              "Product table",
              tabName = "data_product",
              icon = icon("shopping-cart")
            )
          ),

          #Choose the parameters
          menuItem(
            "Parameters",
            tabName = "parameters",
            icon = icon("filter")
          ),

          #Tab for results and analysis
          menuItem(
            "Analysis",
            tabName = "",
            icon = icon("cogs"),
            menuSubItem("Table", tabName = "table_analysis", icon = icon("table")),
            menuSubItem("Graph", tabName = "graph_analysis", icon = icon("chart-line"))
          ),

          #Explanation for the app
          menuItem("Help", tabName = "help", icon = icon("question"))
        )
      ),

      #content of the body
      dashboardBody(
        tabItems(
          ######################################
          # Tab  Summary how to use contents
          tabItem(tabName = "about",
                  fluidPage(
                    box(
                      width = 12,
                      status = "primary",
                      shiny::includeMarkdown("external/Text/homepage.Rmd")
                    )
                  )),
          ######################################

          ######################################
          # Tab  Data Preparation Tab Contents
          tabItem(tabName = "datapreparation",
                  fluidPage(
                    box(
                      title = h4("Import your data", style = "color:#0999DC"),
                      height = NULL,

                      fileInput(
                        "myData",
                        label = ("Input trade dataset"),
                        accept = c(
                          'text/csv',
                          'text/comma-separated-values,text/plain',
                          '.csv'
                        )
                      ),
                      radioButtons(
                        'sepUI1',
                        'Separator',
                        c(
                          Comma = ',',
                          Semicolon = ';',
                          Tab = '\t',
                          Space = " "
                        ),
                        selected = ',',
                        inline = TRUE
                      ),
                      checkboxInput('headerUI1', 'Header', TRUE),
                      hr(),

                      fileInput(
                        "product_table",
                        label = ("Product Table"),
                        accept = c(
                          'text/csv',
                          'text/comma-separated-values,text/plain',
                          '.csv'
                        )
                      ),
                      radioButtons(
                        'sepUI2',
                        'Separator',
                        c(
                          Comma = ',',
                          Semicolon = ';',
                          Tab = '\t',
                          Space = " "
                        ),
                        selected = ',',
                        inline = TRUE
                      ),
                      checkboxInput('headerUI2', 'Header', TRUE),
                      hr(),

                      fileInput(
                        "revision_table",
                        label = ("Revision Table"),
                        accept = c(
                          'text/csv',
                          'text/comma-separated-values,text/plain',
                          '.csv'
                        )
                      ),
                      radioButtons(
                        'sepUI3',
                        'Separator',
                        c(
                          Comma = ',',
                          Semicolon = ';',
                          Tab = '\t',
                          Space = " "
                        ),
                        selected = ',',
                        inline = TRUE
                      ),
                      checkboxInput('headerUI3', 'Header', TRUE)

                    ),

                    box(
                      title = h4("Variable's names", style = "color:#0999DC"),
                      height = NULL,

                      uiOutput("responseUI1"),
                      uiOutput("responseUI2"),
                      uiOutput("responseUI3"),
                      uiOutput("responseUI4"),
                      uiOutput("responseUI5"),
                      uiOutput("responseUI6"),
                      uiOutput("responseUI7"),
                      hr(),
                      uiOutput("responseUI8"),
                      uiOutput("responseUI9"),
                      hr(),
                      uiOutput("explanatoryUI"),
                      uiOutput("selectDeselectUI")
                    )
                  )),
          ######################################

          ######################################
          ##Data table produit/trade/quantity/couuntry

          #Trade
          tabItem(tabName = "data_trade",
                  fluidRow(
                    tabsetPanel(id = "data_trade_table",
                                tabPanel(
                                  h4("Trade table", style = "color:#0999DC"),
                                  dataTableOutput("input_trade_table")
                                ))
                  )),
          ######################################

          ######################################
          #Product
          tabItem(tabName = "data_product",
                  fluidRow(
                    tabsetPanel(
                      id = "data_product_table",
                      tabPanel(
                        h4("Product table", style = "color:#0999DC"),
                        useShinyalert(),
                        div(style = "position:absolute;right:1em;",
                            actionButton("Revision", "Revision?")),
                        hr(),
                        hr(),
                        dataTableOutput("input_product_table")
                      )
                    )
                  )),
          ######################################

          ######################################
          #Revision
          tabItem(tabName = "data_revision",
                  fluidRow(
                    tabsetPanel(
                      id = "data_revision_table",
                      tabPanel(
                        h4("Revision table", style = "color:#0999DC"),
                        useShinyalert(),
                        div(style = "position:absolute;right:1em;",
                            actionButton("Revision", "Revision?")),
                        hr(),
                        hr(),
                        dataTableOutput("input_revision_table")
                      )
                    )
                  )),
          ######################################


          # #####################################
          # Modeling Tab Contents
          # #####################################

          ##################################################################################
          ####  Tab Cross-Validtion Training/Splitting Tab Set Contents
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
                  )),
          ##################################################################################

          ##################################################################################
          ####   Results of Analysis Technique
          tabItem(tabName = "table_analysis",
                  fluidRow(
                    tabsetPanel(
                      id = "tabPanel",

                      tabPanel(
                        h4("Report"),
                        titlePanel("Report"),
                        fluidRow(
                          wellPanel(
                            tags$b("Processing Report"),
                            hr(),
                            downloadButton('downloadPR', 'Download'),
                            hr(),
                            dataTableOutput("proc_report") %>% withSpinner()
                          ),
                          wellPanel(
                            tags$b("Cover Rate"),
                            hr(),
                            downloadButton('downloadCR', 'Download'),
                            hr(),
                            dataTableOutput("cover_rate") %>% withSpinner()
                          )
                        )
                      ),
                      tabPanel(
                        h4("Outliers"),
                        titlePanel("Detected Outliers"),
                        downloadButton('downloadOL', 'Download'),
                        hr(),
                        dataTableOutput("detected_outliers") %>% withSpinner()
                      ),

                      tabPanel(
                        h4("Heterogeneous Series"),
                        titlePanel("Heterogeneous Series"),
                        downloadButton('downloadHS', 'Download'),
                        useShinyalert(),
                        div(
                          style = "position:absolute;right:1em;",
                          actionButton("RIQ", "RIQ?"),
                          actionButton("RSD", "RSD?")
                        ),
                        hr(),
                        dataTableOutput("series_hetero") %>% withSpinner()
                      ),
                      tabPanel(
                        h4("Indices Table"),
                        titlePanel("Indices Table"),
                        downloadButton('downloadIT', 'Download'),
                        hr(),
                        dataTableOutput("indices_table") %>% withSpinner()
                      )

                    )
                  )),
          ##################################################################################

          ##################################################################################
          ####   Results of Analysis Technique
          tabItem(
            tabName = "graph_analysis",
            h2("Evolution of your product"),
            fluidRow(
              column(
                4,
                title = "Product code on indices table",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                uiOutput("indicesUIproduct")
              ),
              column(
                4,
                title = "Partner code on indices table",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                uiOutput("indicesUIpartner")
              ),
              column(
                4,
                title = "Reporter code on indices table",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                uiOutput("indicesUIreporter")
              ),

              box(
                title = "Product evolution",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                highchartOutput("plot_product"),
                width = 12
              ),

              box(
                title = "All the trade of the product",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                DT::dataTableOutput("table_product"),
                width = 12,
                style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
              )
            )
          ),
          ##################################################################################

          ##################################################################################
          ####   About this App
          tabItem(tabName = "help",
                  fluidPage(
                    box(
                      width = 12,
                      status = "primary",
                      shiny::includeMarkdown("external/Text/help.Rmd")
                    )
                  ))
          ##################################################################################

        )
      ),
      useShinyalert()
    )
  )
}
