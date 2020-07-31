output$input_trade_table <- DT::renderDT(datatable(rawInputData()))

output$input_product_table <- DT::renderDT(datatable(rawInputData2()))

output$input_revision_table <- DT::renderDT(datatable(rawInputData3()))
