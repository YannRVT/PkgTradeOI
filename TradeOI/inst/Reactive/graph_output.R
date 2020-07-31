output$plot_product <- renderHighchart({
  data = indices_table()

  if(!is.null(data) & input$time_break == "year"){

    data = filter(data, Product == input$var_indices_product, Partner == input$var_indices_partner, Reporter == input$var_indices_reporter)
    
    data$Month = "12"
    data$Day = "15"
    
    data$Date <-paste(data$Year,data$Month, data$Day,sep="-")
    data$Date <- as.Date(data$Date, format="%Y-%m-%d")
    
    data = select(data, -c(Year, Month, Day))
    
    dataLP = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
    dataLP$Indices = "Laspeyres price"
    dataLP = rename(dataLP, Value = "Laspeyres price")
    
    dataLQ = select(data, -c("Laspeyres price", "Paasche price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
    dataLQ$Indices = "Laspeyres quantity"
    dataLQ = rename(dataLQ, Value = "Laspeyres quantity")
    
    dataPP = select(data, -c("Laspeyres quantity", "Laspeyres price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
    dataPP$Indices = "Paasche price"
    dataPP = rename(dataPP, Value = "Paasche price")
    
    dataPQ = select(data, -c("Laspeyres price", "Paasche price", "Laspeyres quantity", "Fisher quantity", "Fisher price", "Index value"))
    dataPQ$Indices = "Paasche quantity"
    dataPQ = rename(dataPQ, Value = "Paasche quantity")
    
    dataFP = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Fisher quantity", "Laspeyres price", "Index value"))
    dataFP$Indices = "Fisher price"
    dataFP = rename(dataFP, Value = "Fisher price")
    
    dataFQ = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Laspeyres price", "Fisher price", "Index value"))
    dataFQ$Indices = "Fisher quantity"
    dataFQ = rename(dataFQ, Value = "Fisher quantity")
    
    dataIV = select(data, -c("Laspeyres price", "Paasche price", "Paasche quantity", "Laspeyres quantity", "Fisher price", "Fisher quantity"))
    dataIV$Indices = "Index value"
    dataIV = rename(dataIV, Value = "Index value")
    
    data = rbind(dataFP, dataFQ, dataPP, dataPQ, dataLP, dataLQ, dataIV)
    
    hchart(data, "line", hcaes(x = Date, y = c(Value), group = Indices)) %>%
    hc_exporting(
      enabled = TRUE
    )
    
  } else if(!is.null(data) & input$time_break == "quarter"){
    
    data = filter(data, Product == input$var_indices_product, Partner == input$var_indices_partner, Reporter == input$var_indices_reporter)
    
    data$Quarter = as.character(data$Quarter)
    data$Quarter =ifelse(data$Quarter == "1", "03", data$Quarter)
    data$Quarter =ifelse(data$Quarter == "2", "06", data$Quarter)
    data$Quarter =ifelse(data$Quarter == "3", "09", data$Quarter)
    data$Quarter =ifelse(data$Quarter == "4", "12", data$Quarter)
    data$Day = "15"
    
    data$Date <-paste(data$Year,data$Quarter, data$Day,sep="-")
    data$Date <- as.Date(data$Date, format="%Y-%m-%d")
    
    data = select(data, -c(Year, Quarter, Day))
    
    dataLP = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
    dataLP$Indices = "Laspeyres price"
    dataLP = rename(dataLP, Value = "Laspeyres price")
    
    dataLQ = select(data, -c("Laspeyres price", "Paasche price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
    dataLQ$Indices = "Laspeyres quantity"
    dataLQ = rename(dataLQ, Value = "Laspeyres quantity")
    
    dataPP = select(data, -c("Laspeyres quantity", "Laspeyres price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
    dataPP$Indices = "Paasche price"
    dataPP = rename(dataPP, Value = "Paasche price")
    
    dataPQ = select(data, -c("Laspeyres price", "Paasche price", "Laspeyres quantity", "Fisher quantity", "Fisher price", "Index value"))
    dataPQ$Indices = "Paasche quantity"
    dataPQ = rename(dataPQ, Value = "Paasche quantity")
    
    dataFP = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Fisher quantity", "Laspeyres price", "Index value"))
    dataFP$Indices = "Fisher price"
    dataFP = rename(dataFP, Value = "Fisher price")
    
    dataFQ = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Laspeyres price", "Fisher price", "Index value"))
    dataFQ$Indices = "Fisher quantity"
    dataFQ = rename(dataFQ, Value = "Fisher quantity")
    
    dataIV = select(data, -c("Laspeyres price", "Paasche price", "Paasche quantity", "Laspeyres quantity", "Fisher price", "Fisher quantity"))
    dataIV$Indices = "Index value"
    dataIV = rename(dataIV, Value = "Index value")
    
    data = rbind(dataFP, dataFQ, dataPP, dataPQ, dataLP, dataLQ, dataIV)
    
    hchart(data, "line", hcaes(x = Date, y = c(Value), group = Indices))%>%
      hc_exporting(
        enabled = TRUE
      )
    
  } else if(!is.null(data) & input$time_break == "month"){
    
    data = filter(data, Product == input$var_indices_product, Partner == input$var_indices_partner, Reporter == input$var_indices_reporter)
    
    data$Month = as.character(data$Month)
    data$Reporter = as.character(data$Reporter)
    data$Partner = as.character(data$Partner)
    data$Product = as.character(data$Product)
    
    data$Month =ifelse(data$Month == "1", "01", data$Month)
    data$Month =ifelse(data$Month == "2", "02", data$Month)
    data$Month =ifelse(data$Month == "3", "03", data$Month)
    data$Month =ifelse(data$Month == "4", "04", data$Month)
    data$Month =ifelse(data$Month == "5", "05", data$Month)
    data$Month =ifelse(data$Month == "6", "06", data$Month)
    data$Month =ifelse(data$Month == "7", "07", data$Month)
    data$Month =ifelse(data$Month == "8", "08", data$Month)
    data$Month =ifelse(data$Month == "9", "09", data$Month)
    data$Day = "15"
    
    data$Date <-paste(data$Year,data$Month, data$Day,sep="-")
    data$Date <- as.Date(data$Date, format="%Y-%m-%d")
    
    data = select(data, -c(Year, Month, Day))
    
    dataLP = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
    dataLP$Indices = "Laspeyres price"
    dataLP = rename(dataLP, Value = "Laspeyres price")
    
    dataLQ = select(data, -c("Laspeyres price", "Paasche price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
    dataLQ$Indices = "Laspeyres quantity"
    dataLQ = rename(dataLQ, Value = "Laspeyres quantity")
    
    dataPP = select(data, -c("Laspeyres quantity", "Laspeyres price", "Paasche quantity", "Fisher quantity", "Fisher price", "Index value"))
    dataPP$Indices = "Paasche price"
    dataPP = rename(dataPP, Value = "Paasche price")
    
    dataPQ = select(data, -c("Laspeyres price", "Paasche price", "Laspeyres quantity", "Fisher quantity", "Fisher price", "Index value"))
    dataPQ$Indices = "Paasche quantity"
    dataPQ = rename(dataPQ, Value = "Paasche quantity")
    
    dataFP = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Fisher quantity", "Laspeyres price", "Index value"))
    dataFP$Indices = "Fisher price"
    dataFP = rename(dataFP, Value = "Fisher price")
    
    dataFQ = select(data, -c("Laspeyres quantity", "Paasche price", "Paasche quantity", "Laspeyres price", "Fisher price", "Index value"))
    dataFQ$Indices = "Fisher quantity"
    dataFQ = rename(dataFQ, Value = "Fisher quantity")
    
    dataIV = select(data, -c("Laspeyres price", "Paasche price", "Paasche quantity", "Laspeyres quantity", "Fisher price", "Fisher quantity"))
    dataIV$Indices = "Index value"
    dataIV = rename(dataIV, Value = "Index value")
    
    data = rbind(dataFP, dataFQ, dataPP, dataPQ, dataLP, dataLQ, dataIV)
    
    hchart(data, "line", hcaes(x = Date, y = c(Value), group = Indices))%>%
      hc_exporting(
        enabled = TRUE
      )
    }
  })

output$table_product <- renderDataTable({
  data = indices_table()
  data = filter(data, Product == input$var_indices_product, Partner == input$var_indices_partner, Reporter == input$var_indices_reporter)
  data
})