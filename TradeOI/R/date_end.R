#' Function generating the date and time of the start of calculations
#'
#' @param log_prev last hour
#'
#' @import filehash
#' @import sqldf
#' @import stringr
#' @import data.table
#' @import robustbase
#' @import gsubfn
#' @import proto
#' @import RSQLite
#' @import iterators
#' @import dplyr
#' @import optiRum
#' @import compiler
#' @import shinyalert
#' @import DT
#' @import stringi
#'
#' @author Yann Rivallant <yrivallant@intracen.org>
#' @export

date_end1 <- compiler::cmpfun(date_end <- function(log_prev){
  print("Starting date_end")

  end_time <- as.character(Sys.time())
  log <- data.table(step = "Ended at", note = end_time, type="info" , stringsAsFactors = F)
  log <- rbind(log_prev,log)
  log <- log[,1:2]

  print("Ending date_end")
  return(log)
})
