#Product
tabItem(tabName = "data_product",
        fluidRow(
          tabsetPanel(id = "data_product_table",
                      tabPanel(h4("Product table", style = "color:#0999DC"),
                               useShinyalert(),
                               div(style = "position:absolute;right:1em;",
                                   actionButton("Revision", "Revision?")),
                               hr(),
                               hr(),
                               dataTableOutput("input_product_table"))
          )
        )
)