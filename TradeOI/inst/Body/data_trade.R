##Data table produit/trade/quantity/couuntry

#Trade
tabItem(tabName = "data_trade",
        fluidRow(
          tabsetPanel(id = "data_trade_table",
            tabPanel(h4("Trade table", style = "color:#0999DC"),
                     DT::DTOutput("input_trade_table"))
        ))
)
