tabItem(tabName = "graph_analysis",
        h2("Evolution of your product"),
        fluidRow(
          column(4, title = "Product code on indices table", status = "primary", solidHeader = TRUE,
                 collapsible = TRUE,
                 uiOutput("indicesUIproduct")
                 ),
          column(4, title = "Partner code on indices table", status = "primary", solidHeader = TRUE,
                 collapsible = TRUE,
                 uiOutput("indicesUIpartner")
          ),
          column(4, title = "Reporter code on indices table", status = "primary", solidHeader = TRUE,
                 collapsible = TRUE,
                 uiOutput("indicesUIreporter")
          ),
          
          box(
            title = "Product evolution", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, highchartOutput("plot_product"), width = 12),
          
          box(
            title = "All the trade of the product", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, DT::dataTableOutput("table_product"), width = 12, 
            style = "height:500px; overflow-y: scroll;overflow-x: scroll;")
        )
    ) 