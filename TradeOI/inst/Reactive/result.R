## adding ended date on log
output$proc_report <- DT::renderDT(datatable(proc_report9()))

output$downloadPR <- downloadHandler(
  filename = paste('proc_report', Sys.Date(),'.csv'),
  content = function(file) {
    write.csv(proc_report9(),file)
  })
###########

output$cover_rate <- DT::renderDT(datatable(cover_rate()))

output$downloadCR <- downloadHandler(
  filename = paste('cover_rate_', Sys.Date(),'.csv'),
  content = function(file) {
    write.csv(cover_rate(),file)
  })
###########

output$detected_outliers <- DT::renderDT(datatable(detected_outliers()))

output$downloadOL <- downloadHandler(
  filename = paste('detected_outliers', Sys.Date(),'.csv'),
  content = function(file) {
    write.csv(detected_outliers(),file)
  })
###########

output$series_hetero <- DT::renderDT(datatable(series_hetero()))

output$downloadHS <- downloadHandler(
  filename = paste('heteorgeneous_series_', Sys.Date(),'.csv'),
  content = function(file) {
    write.csv(series_hetero(),file)
  })
###########

output$indices_table <- DT::renderDT(datatable(indices_table()))

output$downloadIT <- downloadHandler(
  filename = paste('trade_indices_', Sys.Date(),'.csv'),
  content = function(file) {
    write.csv(indices_table(),file)
  })
