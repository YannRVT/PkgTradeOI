output$input_trade_table <- renderDataTable(
  rawInputData(),
  
  options = list(
    pageLength=10, scrollX='400px'), filter = 'top'
  )

output$input_product_table <- renderDataTable(
  rawInputData2(),
  
  options = list(
    pageLength=10, scrollX='400px'), filter = 'top'
  )

output$input_revision_table <- renderDataTable(
  rawInputData3(),
  
  options = list(
    pageLength=10, scrollX='400px'), filter = 'top'
)