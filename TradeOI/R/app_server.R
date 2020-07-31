#' Function server to run TradeOi app
#'
#' Some options are programmed
#' Suppress missing test error
#' Maximum importable file size
#' Feature in parallel
#'
#' @param input file in
#' @param output file create
#' @param session for reactive and observe
#'
#' @import filehash
#' @import sqldf
#' @import stringr
#' @import data.table
#' @import robustbase
#' @import shiny
#' @import shinydashboard
#' @import doParallel
#' @import gsubfn
#' @import proto
#' @import RSQLite
#' @import iterators
#' @import shinycssloaders
#' @import shinyjs
#' @import dplyr
#' @import optiRum
#' @import compiler
#' @import shinyalert
#' @import DT
#' @import stringi
#' @import highcharter
#' @import parallel
#'
#' @author Yann Rivallant <yrivallant@intracen.org>
#' @export

#SERVER
app_server = function(input, output, session) {

  memory.limit(100000)

  no_cores = detectCores() - 1
  cl = makeCluster(no_cores, setup_strategy = "sequential")
  registerDoParallel(cl)

  source("inst/Reactive/Server_data_preparation.R", local = T)$value

  source("inst/Reactive/variablename.R", local = T)$value

  source("inst/Reactive/input_table.R", local = T)$value

  source("inst/Reactive/computation.R", local = T)$value

  source("inst/Reactive/result.R", local = T)$value

  source("inst/Reactive/graph_output.R", local = T)$value

  stopCluster(cl)

}
