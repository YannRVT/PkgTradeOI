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
#' @importFrom shiny shinyServer
#'
#' @author Yann Rivallant <yrivallant@intracen.org>
#' @export

##Increase size of data
options(shiny.maxRequestSize = 1000*1024^10, expressions=500000,
        shiny.suppressMissingContextError = TRUE
        )

app_server <- shiny::shinyServer(function(input,output,session)
  {
  memory.limit(100000)

  no_cores <- detectCores() - 1
  cl <- parallel::makeCluster(no_cores, setup_strategy = "sequential")
  registerDoParallel(cl)

  source("inst/Reactive/Server_data_preparation.R",local=T)$value

  source("inst/Reactive/variablename.R",local=T)$value

  source("inst/Reactive/input_table.R",local=T)$value

  source("external/Function/test_parameters.R",local=T)$value

  source("external/Function/empty.R",local=T)$value

  source("external/Function/strKey.R",local=T)$value

  source("external/Function/change_names.R",local=T)$value

  source("external/Function/verif_HS.R",local=T)$value

  source("external/Function/non_HS.R",local=T)$value

  source("external/Function/doublons.R",local=T)$value

  source("external/Function/unit_quant.R",local=T)$value

  source("external/Function/chang_rev.R",local=T)$value

  source("external/Function/time_code.R",local=T)$value

  source("external/Function/outliers_detection.R",local=T)$value

  source("external/Function/replacement.R",local=T)$value

  source("external/Function/heterogeneity.R",local=T)$value

  source("external/Function/out_outliers.R",local=T)$value

  source("external/Function/out_heterogeneous.R",local=T)$value

  source("external/Function/non_heterogeneous.R",local=T)$value

  source("external/Function/prepare_indices.R",local=T)$value

  source("external/Function/unit_value_indices.R",local=T)$value

  source("external/Function/covert_rate.R",local=T)$value

  source("external/Function/date_end.R",local=T)$value

  source("inst/Reactive/result.R",local=T)$value

  source("inst/Reactive/computation.R",local=T)$value

  source("inst/Reactive/graph_output.R",local=T)$value

  stopCluster(cl)

  session$onSessionEnded(stopApp)

  })
