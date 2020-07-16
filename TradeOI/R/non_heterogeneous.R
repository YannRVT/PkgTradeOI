#' Function to keep only heterogeneous series
#'
#' @param input_dataset last dataset
#' @param empty_parameter empty data
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
#'

non_heterogeneous1 <- compiler::cmpfun(non_heterogeneous <- function(
  input_dataset,
  empty_parameter){

  print("Starting non_heterogeneous")

  if(empty_parameter == 1){
    input_dataset <- data.table()
  }
  else{
    input_dataset <- input_dataset %>%
      filter(is.na(FG_HETEROGEN)) %>%
      select(-FG_OUTLIERS, -FG_HETEROGEN)
    }

  print("Ending non_heterogeneous")
  return(input_dataset)
})
