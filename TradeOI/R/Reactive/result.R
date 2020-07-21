## adding ended date on log
output$proc_report <- DT::renderDataTable(
  proc_report9(),
  options = list(
    pageLength=10, scrollX='400px'), filter = 'top'
)

output$downloadPR <- downloadHandler(
  filename = paste('proc_report', Sys.Date(),'.csv'),
  content = function(file) {
    write.csv(proc_report9(),file)
  })
###########

output$cover_rate <- renderDataTable(
  cover_rate(),
  options = list(
    pageLength=10, scrollX='400px'), filter = 'top'
)

output$downloadCR <- downloadHandler(
  filename = paste('cover_rate_', Sys.Date(),'.csv'),
  content = function(file) {
    write.csv(cover_rate(),file)
  })
###########

output$detected_outliers <- renderDataTable(
  detected_outliers(),
  options = list(
    pageLength=10, scrollX='400px'), filter = 'top'
)

output$downloadOL <- downloadHandler(
  filename = paste('detected_outliers', Sys.Date(),'.csv'),
  content = function(file) {
    write.csv(detected_outliers(),file)
  })
###########

output$series_hetero <- renderDataTable(
  series_hetero(),
  options = list(
    pageLength=10, scrollX='400px'), filter = 'top'
)

output$downloadHS <- downloadHandler(
  filename = paste('heteorgeneous_series_', Sys.Date(),'.csv'),
  content = function(file) {
    write.csv(series_hetero(),file)
  })
###########

output$indices_table <- renderDataTable(
  indices_table(),
  options = list(
    pageLength=10, scrollX='400px'), filter = 'top'
)

output$downloadIT <- downloadHandler(
  filename = paste('trade_indices_', Sys.Date(),'.csv'),
  content = function(file) {
    write.csv(indices_table(),file)
  })
