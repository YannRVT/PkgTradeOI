#' Function to launch TradeOI app
#'
#' @param ui ui file 
#' @param server server file
#'
#' @import shiny
#'
#' @author Yann Rivallant <yrivallant@intracen.org>
#' @export

run_app <- function(options = list()) {
  shiny::shinyApp(ui = app_ui,
                  server = app_server,
                  options = options(shiny.maxRequestSize = 1000*1024^10, expressions=500000,
                                    shiny.suppressMissingContextError = TRUE
                  )) 
}