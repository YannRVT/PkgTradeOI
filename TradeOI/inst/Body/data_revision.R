#Revision
tabItem(tabName = "data_revision",
        fluidRow(
          tabsetPanel(id = "data_revision_table",
                      tabPanel(h4("Revision table", style = "color:#0999DC"),
                               useShinyalert(),
                               div(style = "position:absolute;right:1em;",
                                   actionButton("Revision", "Revision?")),
                               hr(),
                               hr(),
                               DT::DTOutput("input_revision_table"))
          )
        )
)
